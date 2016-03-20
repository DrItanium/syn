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
	enum {
		DecodeMask = 0b00000111,
	};
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
			byte readByte(word address);
			void decode();
			void dispatch();
			void decodeVariable(byte rest);
			void decodeOneByte(byte rest);
			void decodeTwoByte(byte rest);
			void decodeFourByte(byte rest);
			void decodeEightByte(byte rest);
			void decodeTenByte(byte rest);
		private:
			hword memorySize;
			byte* memory;
			bool execute = true,
				 advanceIp = true;
			word gpr[ArchitectureConstants::RegisterCount] = {0};
	};

} // end namespace iris64
#endif // end _TARGET_IRIS64_IRIS_H
