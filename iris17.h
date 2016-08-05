#ifndef _TARGET_IRIS17_IRIS_H
#define _TARGET_IRIS17_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include "Problem.h"
#include <cstdint>
#include <sstream>
#include <memory>
#include <vector>
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
		MaxInstructionCount = 16,
		MaxSystemCalls = 64,
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
#include "def/iris17/ops.def"
#undef X
		Count,
	};
	static_assert(static_cast<byte>(Operation::Count) <= static_cast<byte>(ArchitectureConstants::MaxInstructionCount), "Too many operations defined!");

	enum class SystemCalls : byte {
#define X(name) name,
#include "def/iris17/syscalls.def"
#undef X
		Count,
	};
	static_assert(static_cast<byte>(SystemCalls::Count) <= static_cast<byte>(ArchitectureConstants::MaxSystemCalls), "Too many system calls defined!");

	enum class CompareCombine : byte {
		None,
		And,
		Or,
		Xor,
	};

	enum class CompareStyle : byte {
		Equals,
		NotEquals,
		LessThan,
		GreaterThan,
		LessThanOrEqualTo,
		GreaterThanOrEqualTo,
	};

	enum class ArithmeticOps : byte {
		Add,
		Sub,
		Mul,
		Div,
		Rem,
		Count,
	};
	enum class ImmediateLogicalOps : byte {
		And,
		Or,
		Xor,
		Nand,
		Count,
	};

	enum class LogicalOps : byte {
		And,
		Or,
		Xor,
		Nand,
		Not,
		Count,
	};

	enum class MemoryOperation : byte {
		Load,
		LoadMerge,
		Store,
		Push,
		Pop,
		Count,
	};
	class DecodedInstruction {
		public:
			DecodedInstruction(raw_instruction input);
			raw_instruction getRawValue() const;
#define X(title, mask, shift, type, is_register, post) type get ## title () const; 
#include "def/iris17/instruction.def"
#undef X
		private:
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
#include "def/iris17/misc.def"
#undef X
		template<byte index>
		RegisterValue& registerValue() {
			switch(index) {
#define X(index) case index : return gpr[index];
#include "def/iris17/registers.def"
#undef X
				default:
					std::stringstream msg;
					msg << "Out of range register index: " << index;
					throw iris::Problem(msg.str());
			}
		}
		template<Operation op>
		void operation(DecodedInstruction&& inst) {
			throw iris::Problem("Unimplemented function!");
		}

		RegisterValue& registerValue(byte index);
		RegisterValue& getInstructionPointer();
		RegisterValue& getStackPointer();
		RegisterValue& getConditionRegister();
		RegisterValue& getLinkRegister();
		RegisterValue& getAddressRegister();
		RegisterValue& getValueRegister();
		word getCurrentCodeWord();
		void storeWord(RegisterValue address, word value);
		word loadWord(RegisterValue address);
		RegisterValue loadRegisterValue(RegisterValue address);
		void storeRegisterValue(RegisterValue address, RegisterValue value);

		private:
			bool execute = true,
				 advanceIp = true;
			RegisterValue gpr[ArchitectureConstants::RegisterCount] = { 0 };
			word *memory = nullptr;
	};

	Core* newCore();

}
#endif
