#ifndef _TARGET_IRIS17_IRIS_H
#define _TARGET_IRIS17_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include "Problem.h"
#include <cstdint>
#include <sstream>
#include <memory>
namespace iris17 {
	typedef uint8_t hword;
	typedef uint16_t word;
	typedef uint32_t dword;
	typedef word raw_instruction; // this is more of a packet!
	typedef hword immediate;
	typedef uint32_t RegisterValue;
	inline word encodeWord (byte a, byte b);
	inline RegisterValue encodeRegisterValue(byte a, byte b, byte c, byte d);
	inline void decodeWord(word value, byte* storage);
	inline void decodeRegisterValue(RegisterValue value, byte* storage);
	enum ArchitectureConstants  {
		RegisterCount = 16,
		SegmentCount = 256,
		AddressMax = 65535 * SegmentCount,
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
			DecodedInstruction(raw_instruction input);
			inline raw_instruction getRawValue() const { return _rawValue; }
#define X(title, mask, shift, type, is_register, post) inline type get ## title () const { return _ ## post ; }
#include "iris17_instruction.def"
#undef X
		private:
#define X(title, mask, shift, type, is_register, post) type _ ## post ;
#include "iris17_instruction.def"
#undef X
			raw_instruction _rawValue;
	};

	class Core : public iris::Core {
		public:
			Core();
			virtual ~Core();
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override;
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
			virtual void link(std::istream& stream) override;
		private:
			void dispatch(DecodedInstruction&& inst);
#define X(title, func) void func ();
#include "iris17_misc.def"
#undef X
		template<byte index>
		RegisterValue& registerValue() {
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
		template<Operation operation>
		void op(DecodedInstruction&& inst) {
			throw iris::Problem("Unimplemented function!");
		}

		inline RegisterValue& registerValue(byte index);
		inline RegisterValue& getInstructionPointer();
		inline RegisterValue& getStackPointer();
		inline RegisterValue& getConditionRegister();
		inline RegisterValue& getLinkRegister();
		inline RegisterValue& getAddressRegister();
		inline RegisterValue& getValueRegister();
		inline word* getSegment(RegisterValue segment);
		inline word getCurrentCodeWord();
		inline word getTopOfStack();
		inline void storeWord(RegisterValue address, word value);
		inline word loadWord(RegisterValue address);
		inline RegisterValue loadRegisterValue(RegisterValue address);
		inline void storeRegisterValue(RegisterValue address, RegisterValue value);

		private:
			bool execute = true,
				 advanceIp = true;
			RegisterValue gpr[ArchitectureConstants::RegisterCount] = { 0 };
			word *memory = nullptr;
	};

	Core* newCore();

}
#endif
