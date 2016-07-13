#ifndef _TARGET_IRIS32_IRIS_H
#define _TARGET_IRIS32_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include <cstdint>
#include <vector>

typedef int64_t dword;
typedef int32_t word;
typedef int16_t hword;

namespace iris32 {
	word encodeWord(byte, byte, byte, byte);
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 268435456 /* bytes */ / sizeof(word), // words
		InstructionPointerIndex = RegisterCount - 1,
		LinkRegisterIndex = RegisterCount - 2,
		StackPointerIndex = RegisterCount - 3,
		ConditionRegisterIndex = RegisterCount - 4,
		ThreadIndex = RegisterCount - 5,
		
	};
	enum {
		GroupMask = 0b00000111,
		RestMask = ~GroupMask,
		MaxInstructionsPerGroup = RestMask >> 3,
	};
	class MemoryController {
		public:
			MemoryController(word memSize);
			~MemoryController();
			word read(word address);
			void write(word address, word value);
			void install(std::istream& stream);
			void dump(std::ostream& stream);
		private:
			word memorySize;
			word* memory;
	};
	enum class InstructionGroup : byte {
#define X(e, __) e ,
#include "iris32_groups.def"
#undef X
	};
	template<byte index>
	struct DecodeByteToInstructionGroup { };

	template<InstructionGroup group>
	struct EncodeInstructionGroupAsByte { };

#define X(group, __) \
	template<> \
	struct DecodeByteToInstructionGroup< static_cast< byte > ( InstructionGroup :: group ) > { \
		static const InstructionGroup value = InstructionGroup :: group ; \
	}; \
	template<> \
	struct EncodeInstructionGroupAsByte< InstructionGroup :: group > { \
		static const byte value = static_cast< byte > (InstructionGroup :: group ); \
	};
#include "iris32_groups.def"
#undef X

	class DecodedInstruction {
		enum class Fields {
#define X(en, u0, u1, u2, u3, u4) en ,
#include "iris32_instruction.def"
#undef X
			Count,
		};
		public:
			DecodedInstruction(word rinst);
#define X(field, mask, shift, type, isreg, unused) \
			type get ## field (); \
			void set ## field (type value);
#include "iris32_instruction.def"
#undef X
			word encodeInstruction();
		private:
#define X(u0, u1, u2, type, u3, fieldName) type fieldName; 
#include "iris32_instruction.def"
#undef X
			word raw;
	};
	/// Represents the execution state of a thread of execution
	struct ExecState {
		bool advanceIp = true;
		word gpr[ArchitectureConstants::RegisterCount] = { 0 };
	};
	template<byte index, InstructionGroup group>
		struct OpInfer { };

	template<typename T, T op>
		struct OpToIndex { };

	template<byte control>
	struct DecodeGroup : public std::integral_constant<byte, control & 0b00000111> { };

	template<byte control>
	struct DecodeOp : public std::integral_constant<byte, ((control & 0b11111000) >> 3)> { };

	template<byte control>
	struct DecodeControl {
		static const byte value = control;
		static const byte group = DecodeGroup<control>::value;
		static const byte op = DecodeOp<control>::value;
	};

	template<byte group, byte op>
	struct EncodeControl : public std::integral_constant<byte, ((group | (op << 3)))> { };


	enum class CompareOp : byte {
#define X(title, operation, unused) title,
#define Y(title, operation, unused) title,
#include "iris32_compare.def"
#undef X
#undef Y
		NumberOfCompareOps,
	};
	static_assert(byte(CompareOp::NumberOfCompareOps) <= byte(MaxInstructionsPerGroup), "Too many compare operations defined!");
#define DefOp(cl, group, e) \
	template<> \
	struct OpInfer<static_cast< byte >( cl :: e ), InstructionGroup:: group > { static const cl value = cl :: e ; }; \
	template<> \
	struct OpToIndex<cl, cl :: e > { static const byte value = static_cast < byte > (cl :: e); }; \
	typedef DecodeControl<EncodeControl< EncodeInstructionGroupAsByte < InstructionGroup:: group >::value, static_cast < byte > (cl :: e)>::value> Decode ## cl ## e;
#define X(e, __, ___) DefOp(CompareOp, Compare, e)
#define Y(e, __, ___) X(e, __, ___)
#include "iris32_compare.def"
#undef Y
#undef X





	enum class ArithmeticOp : byte {
#define X(title, operation, type) title ,
#include "iris32_arithmetic.def"
#undef X
		NumberOfArithmeticOps,
	};
	static_assert(byte(ArithmeticOp::NumberOfArithmeticOps) <= byte(MaxInstructionsPerGroup), "Too many arithmetic operations defined!");

#define X(title, u0, u1) DefOp(ArithmeticOp, Arithmetic, title) 
#include "iris32_arithmetic.def"
#undef X

	enum class MoveOp : byte {
#define X(title, operation, __, ___, ____) title ,
#include "iris32_move.def"
#undef X
		NumberOfMoveOps,
	};
	static_assert(byte(MoveOp::NumberOfMoveOps) <= byte(MaxInstructionsPerGroup), "Too many move operations defined!");

#define X(title, __, ___, ____, _____) DefOp(MoveOp, Move, title)
#include "iris32_move.def"
#undef X

	enum class JumpOp : byte {
#define X(title, u0, u1, u2, u3, u4) title ,
#include "iris32_jump.def"
#undef X
		NumberOfJumpOps,
	};
	static_assert(byte(JumpOp::NumberOfJumpOps) <= byte(MaxInstructionsPerGroup), "Too many jump operations defined!");

#define X(title, u0, u1, u2, u3, u4) DefOp(JumpOp, Jump, title)
#include "iris32_jump.def"
#undef X

	enum class MiscOp : byte {
#define X(title, __) title ,
#include "iris32_misc.def"
#undef X
		NumberOfMiscOps,
	};
	static_assert(byte(MiscOp::NumberOfMiscOps) <= byte(MaxInstructionsPerGroup), "Too many misc operations defined!");

#define X(title, __) DefOp(MiscOp, Misc, title)
#include "iris32_misc.def"
#undef X

	enum class SystemCalls : byte {
#define X(title) title ,
#include "iris32_syscalls.def"
#undef X
		NumberOfSyscalls,
	};
	static_assert(byte(SystemCalls::NumberOfSyscalls) <= 255, "Too many syscall operations defined!");


	class Core : public iris::Core {
		public:
			Core(word memorySize, std::initializer_list<ExecState*> execs);
			~Core();
			virtual void initialize();
			virtual void installprogram(std::istream& stream);
			virtual void shutdown();
			virtual void dump(std::ostream& stream);
			virtual void run();
			void write(word address, word value);
			bool debugEnabled();
			void toggleDebug();
			word read(word address);
		private:
			void execBody();
			void decode();
			void dispatch();
#define X(_, func) void func (DecodedInstruction& inst); 
#include "iris32_groups.def"
#undef X
			void systemCall(DecodedInstruction& inst);
		private:
			word memorySize;
			word* memory;
			ExecState *thread = 0;
			std::vector<ExecState*> threads;
			bool execute = true;
			bool debug = false;
	};

} // end namespace iris32
#endif // end _TARGET_IRIS32_IRIS_H
