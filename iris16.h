#ifndef _TARGET_IRIS16_IRIS_H
#define _TARGET_IRIS16_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include <cstdint>
#include <memory>
namespace iris16 {
	typedef uint16_t word;
	typedef uint32_t dword;
	typedef dword raw_instruction;
	typedef word immediate;
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 65535,
		InstructionPointerIndex = RegisterCount - 1,
		LinkRegisterIndex = RegisterCount - 2,
		StackPointerIndex = RegisterCount - 3,
		MaxGroups = 0b00000111,
		MaxOperations = 0b00011111,
	};
	inline constexpr dword encodeDword(byte a, byte b, byte c, byte d) noexcept {
		return iris::encodeUint32LE(a, b, c, d);
	}
	inline constexpr word encodeWord(byte a, byte b) noexcept {
		return iris::encodeUint16LE(a, b);
	}
	inline constexpr dword encodeDword(word lower, word upper) noexcept {
		return iris::encodeUint32LE(lower, upper);
	}
#include "def/iris16/enums.def"

	class Core : public iris::Core {
		public:
			Core() noexcept;
			Core(std::shared_ptr<word> data, dword size) noexcept;
			virtual ~Core();
			virtual void initialize() override { }
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override { }
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
			virtual void link(std::istream& input) override;
			word* registerMapping(byte index);
			word* dataMapping(word index);
			inline void setInstructionMemory(word address, dword value) noexcept { instruction[address] = value; }
			inline void setDataMemory(word address, word value) noexcept         { data[address] = value; }
			inline dword getInstructionMemory(word address) noexcept             { return instruction[address]; }
			inline word getDataMemory(word address) noexcept                     { return data[address]; }
			// TODO: add support for installing externally defined system calls
			void setExtendedDataMemory(dword address, word value);
			word getExtendedDataMemory(dword address);

		private:
			void dispatch();
#define X(title, mask, shift, type, is_register, post) \
			inline type get ## title () const noexcept { \
				return iris::decodeBits<raw_instruction, type, mask, shift>(current); \
			} 
#include "def/iris16/instruction.def"
#undef X

#define X(_, op) void op();
#include "def/iris16/groups.def"
#include "def/iris16/misc.def"
#undef X

		private:
			std::shared_ptr<word> extendedData;
			dword extendedMemorySize = 0;
			bool execute = true,
				 advanceIp = true;
			word gpr[ArchitectureConstants::RegisterCount] = {0};
			word data[ArchitectureConstants::AddressMax] = { 0 } ;
			dword instruction[ArchitectureConstants::AddressMax] = { 0 };
			word stack[ArchitectureConstants::AddressMax] = { 0 };
			raw_instruction current = 0;
	};

	Core* newCore() noexcept;
	void assemble(FILE* input, std::ostream* output);
	void installExtensions(void* theEnv);
}
#endif
