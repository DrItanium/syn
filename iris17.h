#ifndef _TARGET_IRIS32_IRIS_H
#define _TARGET_IRIS32_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include <cstdint>
#include <vector>
#include <memory>


namespace iris32 {
	using dword = int64_t;
	using word = int32_t;
	using hword = int16_t;
	constexpr word encodeWord(byte, byte, byte, byte) noexcept;
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 268435456 /* bytes */ / sizeof(word), // words
		InstructionPointerIndex = RegisterCount - 1,
		LinkRegisterIndex = RegisterCount - 2,
		StackPointerIndex = RegisterCount - 3,
		ConditionRegisterIndex = RegisterCount - 4,
		ThreadIndex = RegisterCount - 5,

		GroupMask = 0b00000111,
		RestMask = ~GroupMask,
		MaxInstructionsPerGroup = RestMask >> 3,
	};

	enum class InstructionGroup : byte {
#define X(e, __) e ,
#include "def/iris32/groups.def"
#undef X
	};
	template<byte index>
	struct DecodeByteToInstructionGroup { };

	template<InstructionGroup group>
	struct EncodeInstructionGroupAsByte { };

	template<InstructionGroup group>
	struct GroupToOp { };

#define X(group, __) \
	template<> \
	struct DecodeByteToInstructionGroup< static_cast< byte > ( InstructionGroup :: group ) > { \
		static constexpr InstructionGroup value = InstructionGroup :: group ; \
	}; \
	template<> \
	struct EncodeInstructionGroupAsByte< InstructionGroup :: group > { \
		static constexpr byte value = static_cast< byte > (InstructionGroup :: group ); \
	};
#include "def/iris32/groups.def"
#undef X

	class DecodedInstruction {
		public:
			DecodedInstruction(word rinst);
			word getRawValue() const { return raw; }
#define X(field, mask, shift, type, isreg, unused) \
			type get ## field () const; \
			void set ## field (type value);
#include "def/iris32/instruction.def"
#undef X
		private:
#define X(u0, u1, u2, type, u3, fieldName) type fieldName;
#include "def/iris32/instruction.def"
#undef X
			word raw;
	};
	/// Represents the execution state of a thread of execution
	struct ExecState {
		bool advanceIp = true;
		word gpr[ArchitectureConstants::RegisterCount] = { 0 };
	};

	template<byte control>
	struct DecodeControl {
		static constexpr byte value = control;
		static constexpr InstructionGroup group = iris::decodeBits<byte, InstructionGroup, 0b00000111, 0>(control);
		static constexpr byte op = iris::decodeBits<byte, byte, 0b11111000, 3>(control);
	};

	inline constexpr byte encodeControl(byte group, byte op) noexcept {
		return ((group | (op << 3)));
	}

	enum class CompareOp : byte {
#define X(title, operation, unused) title,
#define Y(title, operation, unused) title,
#include "def/iris32/compare.def"
#undef X
#undef Y
		Count,
	};
	static_assert(byte(CompareOp::Count) <= byte(MaxInstructionsPerGroup), "Too many compare operations defined!");
#define DefOp(cl, group, e) \
	using Decode ## cl ## e = DecodeControl<encodeControl( static_cast<byte>(InstructionGroup:: group), static_cast < byte > (cl :: e))>;
#define X(e, __, ___) DefOp(CompareOp, Compare, e)
#define Y(e, __, ___) X(e, __, ___)
#include "def/iris32/compare.def"
#undef Y
#undef X





	enum class ArithmeticOp : byte {
#define X(title, operation, type) title ,
#include "def/iris32/arithmetic.def"
#undef X
		Count,
	};
	static_assert(byte(ArithmeticOp::Count) <= byte(MaxInstructionsPerGroup), "Too many arithmetic operations defined!");

#define X(title, u0, u1) DefOp(ArithmeticOp, Arithmetic, title)
#include "def/iris32/arithmetic.def"
#undef X

	enum class MoveOp : byte {
#define X(title, operation, __, ___, ____) title ,
#include "def/iris32/move.def"
#undef X
		Count,
	};
	static_assert(byte(MoveOp::Count) <= byte(MaxInstructionsPerGroup), "Too many move operations defined!");

#define X(title, __, ___, ____, _____) DefOp(MoveOp, Move, title)
#include "def/iris32/move.def"
#undef X

	enum class JumpOp : byte {
#define X(title, u0, u1, u2, u3, u4) title ,
#include "def/iris32/jump.def"
#undef X
		Count,
	};
	static_assert(byte(JumpOp::Count) <= byte(MaxInstructionsPerGroup), "Too many jump operations defined!");

#define X(title, u0, u1, u2, u3, u4) DefOp(JumpOp, Jump, title)
#include "def/iris32/jump.def"
#undef X

	enum class MiscOp : byte {
#define X(title, __) title ,
#include "def/iris32/misc.def"
#undef X
		Count,
	};
	static_assert(byte(MiscOp::Count) <= byte(MaxInstructionsPerGroup), "Too many misc operations defined!");

#define X(title, __) DefOp(MiscOp, Misc, title)
#include "def/iris32/misc.def"
#undef X

	enum class SystemCalls : byte {
#define X(title) title ,
#include "def/iris32/syscalls.def"
#undef X
		Count,
	};
	static_assert(byte(SystemCalls::Count) <= 255, "Too many syscall operations defined!");

#define X(group, __) \
	template<> \
	struct GroupToOp< InstructionGroup :: group > { \
		using OpKind = group ## Op ; \
	};
#include "def/iris32/groups.def"
#undef X

	class Core : public iris::Core {
		public:
			Core(word memorySize, byte numThreads);
			~Core();
			virtual void initialize();
			virtual void installprogram(std::istream& stream);
			virtual void shutdown();
			virtual void dump(std::ostream& stream);
			virtual void run();
			virtual void link(std::istream& input);
			void write(word address, word value);
			word read(word address);
		private:
			void execBody();
			void decode();
			void dispatch();
			void systemCall(DecodedInstruction& inst);

		private:
			template<bool ifthenelse, bool conditional, bool iffalse, bool immediate, bool link>
			friend void invokeJump(Core* core, DecodedInstruction&& inst);
			template<MoveOp op>
			friend void invokeMove(Core* core, DecodedInstruction&& inst);
			template<CompareOp op>
			friend void invokeCompare(Core* core, DecodedInstruction&& inst);
			template<ArithmeticOp op, bool checkDenominator, bool immediate>
			friend void invokeArithmetic(Core* core, DecodedInstruction&& inst);
			template<MiscOp op>
			friend void invokeMisc(Core* core, DecodedInstruction&& inst);
		private:
			word memorySize;
			std::unique_ptr<word> memory;
			std::shared_ptr<ExecState> thread;
			std::vector<std::shared_ptr<ExecState>> threads;
			bool execute = true;
	};

	Core* newCore() noexcept;
	void assemble(FILE* input, std::ostream* output);
} // end namespace iris32
#undef DefOp
#endif // end _TARGET_IRIS32_IRIS_H
