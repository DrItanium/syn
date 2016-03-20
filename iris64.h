#ifndef _TARGET_IRIS32_IRIS_H
#define _TARGET_IRIS32_IRIS_H
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
namespace iris32 {
	enum ArchitectureConstants  {
		RegisterCount = 256,
		FloatRegisterCount = 256,
		AddressMax = 268435456,
		InstructionPointerIndex = RegisterCount - 1,
		LinkRegisterIndex = RegisterCount - 2,
		StackPointerIndex = RegisterCount - 3,
		CallPointerIndex = RegisterCount - 4,
		InternalTemporaryCount = 8,
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
			word readWord(word address);
			void writeByte(word address, byte value);
			void install(std::istream& stream);
			void dump(std::ostream& stream);
		private:
			hword memorySize;
			byte* memory;
	};

	/// Represents the execution state of a thread of execution
	class ExecState {
		public:
			ExecState(word numRegisters);
			~ExecState();
			word& operator[](word idx);
			bool shouldAdvanceIp();
			void setAdvanceIp(bool value);
		private:
			bool advanceIp = true;
			word regCount = 0;
			word* gpr = 0;
	};

	class Core : public iris::Core {
		public:
			Core(MemoryController* memC, ExecState&& t0, ExecState&& t1);
			virtual void initialize();
			virtual void installprogram(std::istream& stream);
			virtual void shutdown();
			virtual void dump(std::ostream& stream);
			virtual void run();
		private:
			void execBody(ExecState& thread);
			void decode(ExecState& curr);
			void dispatch(ExecState& curr);
			void decodeVariable(ExecState& curr, byte rest);
			void decodeOneByte(ExecState& curr, OneByteOperations rest);
			void decodeTwoByte(ExecState& curr, TwoByteOperations rest);
			void decodeFourByte(ExecState& curr, byte rest);
			void decodeEightByte(ExecState& curr, byte rest);
			void decodeTenByte(ExecState& curr, byte rest);
		private:
			MemoryController* mc;
			ExecState thread0,
					  thread1;
			bool execute = true;
	};

} // end namespace iris32
#endif // end _TARGET_IRIS32_IRIS_H
