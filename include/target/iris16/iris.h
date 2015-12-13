#ifndef _TARGET_IRIS16_IRIS_H
#define _TARGET_IRIS16_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include <cstdint>
typedef uint16_t word;
typedef uint32_t dword;
typedef dword raw_instruction;
typedef word immediate;
namespace iris16 {
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 65535,
		InstructionPointerIndex = 255,
		LinkRegisterIndex = 254,
		StackPointerIndex = 253,
	};

	enum class InstructionGroup : byte {
#define X(title, _, __) title,
#include "target/iris16/groups.def"
#undef X
	};
	enum class ArithmeticOp : byte {
#define X(name, _, __, ___) name,
#include "target/iris16/arithmetic.def"
#undef X
	};

	enum class JumpOp : byte {
#define X(name, id, ifthenelse, conditional, iffalse, immediate, link) name,
#include "target/iris16/jump.def"
#undef X
	};
	class DecodedInstruction {
		public:
			DecodedInstruction();
			void decode(raw_instruction input);
#define X(title, mask, shift, type, is_register, post) type get ## title () const { return _ ## post ; }
#include "target/iris16/instruction.def"
#undef X
		private:
#define X(title, mask, shift, type, is_register, post) type _ ## post ;
#include "target/iris16/instruction.def"
#undef X
	};
	class Core : public iris::Core {
		public:
			Core();
			virtual void initialize();
			virtual void installprogram(std::istream& stream);
			virtual void shutdown();
			virtual void dump(std::ostream& stream);
			virtual void run();
		private:
			void dispatch();
#define X(_, op, __) void op();
#include "target/iris16/groups.def"
#undef X

		private:
			DecodedInstruction current;
			bool execute = true,
				 advanceIp = true;
			word gpr[ArchitectureConstants::RegisterCount] = {0};
			word data[ArchitectureConstants::AddressMax] = { 0 };
			dword instruction[ArchitectureConstants::AddressMax] = { 0 };
			word stack[ArchitectureConstants::AddressMax] = { 0 };
	};

}
#endif
