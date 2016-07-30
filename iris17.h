#ifndef _TARGET_IRIS17_IRIS_H
#define _TARGET_IRIS17_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include "Problem.h"
#include <cstdint>
#include <sstream>
#include <memory>
namespace iris17 {
	typedef uint16_t hword;
	typedef uint32_t word;
	typedef word raw_instruction; // this is more of a packet!
	typedef hword immediate;
	inline word encodeWord (byte a, byte b, byte c, byte d);
	inline hword encodeHword(byte a, byte b);
	enum ArchitectureConstants  {
		RegisterCount = 16,
		AddressMax = 65535 * 256,
		// unlike iris16 and iris32, there is a limited set of registers with
		// a majority of them marked for explicit usage, instructions
		// themselves are still 16 bits wide but 32bits are extracted per
		// packet.
		InstructionPointer = RegisterCount - 1,
		LinkRegister = RegisterCount - 2,
		StackPointer = RegisterCount - 3,
		ConditionRegister = RegisterCount - 4,
		AddressRegister = RegisterCount - 5,
		ValueRegister = RegisterCount - 6,
	};

	enum class Operation : byte {
#define X(name) name,
#include "iris17_ops.def"
#undef X
	};

	enum class SystemCalls : byte {
#define X(name) name,
#include "iris17_syscalls.def"
#undef X
		Count,
	};
	class DecodedInstruction {
		public:
			DecodedInstruction();
			DecodedInstruction(raw_instruction input);
			void decode(raw_instruction input);
#define X(title, mask, shift, type, is_register, post) inline type get ## title () const { return _ ## post ; }
#include "iris17_instruction.def"
#undef X
		private:
#define X(title, mask, shift, type, is_register, post) type _ ## post ;
#include "iris17_instruction.def"
#undef X
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
		inline word& registerValue(byte index);

		template<Operation op>
		void op() {
			throw iris::Problem("Unimplemented function!");
		}
		inline word& getInstructionPointer();
		inline word& getStackPointer();
		inline word& getConditionRegister();
		inline word& getLinkRegister();
		inline word& getAddressRegister();
		inline word& getValueRegister();
		inline word getCurrentCodeWord();
		inline word getTopOfStack();

		private:
			DecodedInstruction current;
			bool execute = true,
				 advanceIp = true;
			word gpr[ArchitectureConstants::RegisterCount] = { 0 };
			std::unique_ptr<word[]> memory;
	};

	Core* newCore();

}
#endif
