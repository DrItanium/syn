#ifndef _TARGET_IRIS64_IRIS_H
#define _TARGET_IRIS64_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include <cstdint>

typedef int64_t word;
typedef int32_t hword;
typedef int16_t quword;
typedef uint64_t raw_instruction;

typedef float hfword;
typedef double fword;

typedef struct {
	int64_t upper;
	int64_t lower;
} dword;
namespace iris64 {
	enum ArchitectureConstants  {
		RegisterCount = 256,
		FloatRegisterCount = 256,
		AddressMax = 268435456,
		InstructionPointerIndex = RegisterCount - 1,
		LinkRegisterIndex = RegisterCount - 2,
		StackPointerIndex = RegisterCount - 3,
		MaxGroups = 0x7,
		MaxOperations = 0x1F,
	};
	dword encodeDword(byte a, byte b, byte c, byte d);
	word encodeWord(byte a, byte b);

	enum class InstructionGroup : byte {
#define X(title, _) title,
#include "iris64_groups.def"
#undef X
		Count,
	};
	static_assert((byte)InstructionGroup::Count < ((byte)ArchitectureConstants::MaxGroups), "too many instruction groups defined");
	enum class ArithmeticOp : byte {
#define X(name, __, ___) name,
#include "iris64_arithmetic.def"
#undef X
		Count
	};
	static_assert((byte)ArithmeticOp::Count < ((byte)ArchitectureConstants::MaxOperations), "too many Arithmetic operations defined");
	enum class MiscOp : byte {
#define X(title, func) title, 
#include "iris64_misc.def"
#undef X
		Count
	};
	static_assert((byte)MiscOp::Count < ((byte)ArchitectureConstants::MaxOperations), "too many Misc operations defined");

	enum class JumpOp : byte {
#define X(name, ifthenelse, conditional, iffalse, immediate, link) name,
#include "iris64_jump.def"
#undef X
		Count
	};
	static_assert((byte)JumpOp::Count < ((byte)ArchitectureConstants::MaxOperations), "too many Jump operations defined");

	enum class SystemCalls : byte {
#define X(name) name,
#include "iris64_syscalls.def"
#undef X
		Count,
	};
	enum class MoveOp : byte {
#define X(name, type, target, dest, src) name,
#include "iris64_move.def"
#undef X
		Count,
	};
	static_assert((byte)MoveOp::Count < ((byte)ArchitectureConstants::MaxOperations), "too many Move operations defined");
	enum class CompareOp : byte {
#define X(name, op, group) name,
#define Y(name, op, group) name,
#include "iris64_compare.def"
#undef Y
#undef X
		Count,
	};
	static_assert((byte)CompareOp::Count < ((byte)ArchitectureConstants::MaxOperations), "too many Compare operations defined");
	class DecodedInstruction {
		public:
			DecodedInstruction();
			void decode(raw_instruction input);
#define X(title, mask, shift, type, is_register, post) type get ## title () const { return _ ## post ; }
#include "iris64_instruction.def"
#undef X
		private:
#define X(title, mask, shift, type, is_register, post) type _ ## post ;
#include "iris64_instruction.def"
#undef X
	};
	class Core : public iris::Core {
		public:
			Core(hword memSize);
			virtual ~Core();
			virtual void initialize();
			virtual void installprogram(std::istream& stream);
			virtual void shutdown();
			virtual void dump(std::ostream& stream);
			virtual void run();
		private:
			void dispatch();
#define X(_, op) void op();
#include "iris64_groups.def"
#undef X
#define X(title, func) void func ();
#include "iris64_misc.def"
#undef X

		private:
			hword memorySize;
			byte* memory;
			bool execute = true,
				 advanceIp = true;
			word gpr[ArchitectureConstants::RegisterCount] = {0};
			DecodedInstruction current;
			// removed data section, it is part of memory
	};

} // end namespace iris64
#endif // end _TARGET_IRIS64_IRIS_H
