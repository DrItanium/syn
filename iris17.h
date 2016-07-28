#ifndef _TARGET_IRIS17_IRIS_H
#define _TARGET_IRIS17_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include "Problem.h"
#include <cstdint>
#include <sstream>
namespace iris17 {
	typedef uint16_t word;
	typedef uint32_t dword;
	typedef word raw_instruction;
	typedef word immediate;
	inline dword encodeDword(byte a, byte b, byte c, byte d);
	inline word encodeWord(byte a, byte b);
	enum ArchitectureConstants  {
		RegisterCount = 64,
		AddressMax = 65535,
		SegmentCount = 256,
		InstructionPointer = RegisterCount - 1,
		LinkRegister = RegisterCount - 2,
		StackPointer = RegisterCount - 3,
		ConditionRegister = RegisterCount - 4,
		AddressRegister = RegisterCount - 5,
		DataRegister = RegisterCount - 6,
		DataSegmentRegister = RegisterCount - 7,
		CodeSegmentRegister = RegisterCount - 8,
		StackSegmentRegister = RegisterCount - 9,

		MaxGroups = 0x7,
		MaxOperations = 0x1F,
	};

	enum class InstructionGroup : byte {
#define X(title, _) title,
#include "iris17_groups.def"
#undef X
		Count,
	};
	static_assert((byte)InstructionGroup::Count < ((byte)ArchitectureConstants::MaxGroups), "too many instruction groups defined");
	enum class ArithmeticOp : byte {
#define X(name, __, ___) name,
#include "iris17_arithmetic.def"
#undef X
		Count
	};
	static_assert((byte)ArithmeticOp::Count < ((byte)ArchitectureConstants::MaxOperations), "too many Arithmetic operations defined");
	enum class MiscOp : byte {
#define X(title, func) title,
#include "iris17_misc.def"
#undef X
		Count
	};
	static_assert((byte)MiscOp::Count < ((byte)ArchitectureConstants::MaxOperations), "too many Misc operations defined");

	enum class JumpOp : byte {
#define X(name) name,
#include "iris17_jump.def"
#undef X
		Count
	};
	static_assert((byte)JumpOp::Count < ((byte)ArchitectureConstants::MaxOperations), "too many Jump operations defined");

	enum class SystemCalls : byte {
#define X(name) name,
#include "iris17_syscalls.def"
#undef X
		Count,
	};
	enum class MoveOp : byte {
#define X(name) name,
#include "iris17_move.def"
#undef X
		Count,
	};
	static_assert((byte)MoveOp::Count < ((byte)ArchitectureConstants::MaxOperations), "too many Move operations defined");
	enum class CompareOp : byte {
#define X(name, op, group) name,
#define Y(name, op, group) name,
#include "iris17_compare.def"
#undef Y
#undef X
		Count,
	};
	static_assert((byte)CompareOp::Count < ((byte)ArchitectureConstants::MaxOperations), "too many Compare operations defined");
	class DecodedInstruction {
		public:
			DecodedInstruction();
			void decode(raw_instruction input);
#define X(title, mask, shift, type, is_register, post) inline type get ## title () const { return _ ## post ; }
#include "iris17_instruction.def"
#undef X
		private:
#define X(title, mask, shift, type, is_register, post) type _ ## post ;
#include "iris17_instruction.def"
#undef X
	};
	template<InstructionGroup group>
	struct GetAssociatedOp { };
#define X(title, _) \
	template<> \
	struct GetAssociatedOp<InstructionGroup :: title > { \
		typedef title ## Op Association; \
	};
#include "iris17_groups.def"
#undef X

	template<InstructionGroup group,typename T, T op>
	struct ControlSignature {
		static constexpr byte groupValue = static_cast<byte>(group);
		static constexpr byte opValue = static_cast<byte>(op);
		static constexpr byte fullSignature = (opValue << 3) | groupValue;
	};
	class Core : public iris::Core {
		public:
			Core();
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override;
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
			virtual void link(std::istream& stream) override;
		private:
			void dispatch();
#define X(title, func) void func ();
#include "iris17_misc.def"
#undef X
		template<byte index>
		word& registerValue() {
			switch(index) {
#define X(index) case index : return gpr[index];
#include "iris17_registers.def"
#undef X
				default:
					std::stringstream msg;
					msg << "Out of range register index: " << index;
					throw iris::Problem(msg.str());
			}
		}
		inline byte getControl();
		inline word& getArg0Register();
		inline word& getArg1Register();
		inline word& registerValue(byte index);

		template<InstructionGroup group, typename T, T operation>
		void op() {
			throw iris::Problem("Unimplemented function!");
		}
		private:
			DecodedInstruction current;
			bool execute = true,
				 advanceIp = true;
			word gpr[ArchitectureConstants::RegisterCount] = { 0 };
			word memory[ArchitectureConstants::SegmentCount][ArchitectureConstants::AddressMax] = { { 0 }, };
	};

	Core* newCore();

}
#endif
