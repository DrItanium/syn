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
		//MaxGroups = 0x7,
		//MaxOperations = 0x1F,
		//
	};
	enum {
		DecodeMask = 0b00000111,
		RestCount = 0b00011111,
	};
	enum class OneByteOperations : byte {
		Return,
		Count,
	};
	static_assert(byte(OneByteOperations::Count) <= byte(RestCount), "too many one byte operations defined!");
	enum class TwoByteOperations : byte {
		PushByte,
		Increment,
		Decrement,
		Halve,
		Double,
		InvertBits,
		Pop,
		Square,
		CallRegister,
		Count,
	};
	static_assert(byte(TwoByteOperations::Count) <= byte(RestCount), "too many two byte operations defined!");
	enum class DecodeWidth : byte {
		Variable,
		OneByte,
		TwoByte,
		FourByte,
		EightByte,
		TenByte,
		NumberOfDecodeTypes,
	};
	static_assert((byte)DecodeWidth::NumberOfDecodeTypes <= ((byte)DecodeMask), "too many instruction widths defined");
	class MemoryController {
		public:
			MemoryController(hword memSize);
			~MemoryController();
			byte readByte(word address);
			void writeByte(word address, byte value);
			void install(std::istream& stream);
			void dump(std::ostream& stream);
		private:
			hword memorySize;
			byte* memory;
	};
	class Core : public iris::Core {
		public:
			Core(MemoryController* memC);
			virtual void initialize();
			virtual void installprogram(std::istream& stream);
			virtual void shutdown();
			virtual void dump(std::ostream& stream);
			virtual void run();
		private:
			void decode();
			void dispatch();
			void decodeVariable(byte rest);
			void decodeOneByte(OneByteOperations rest);
			void decodeTwoByte(TwoByteOperations rest);
			void decodeFourByte(byte rest);
			void decodeEightByte(byte rest);
			void decodeTenByte(byte rest);
		private:
			MemoryController* mc;
			bool execute = true,
				 advanceIp = true;
			word gpr[ArchitectureConstants::RegisterCount] = {0};
	};

} // end namespace iris64
#endif // end _TARGET_IRIS64_IRIS_H
