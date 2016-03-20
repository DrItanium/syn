#ifndef _TARGET_IRIS32_IRIS_H
#define _TARGET_IRIS32_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include <cstdint>

typedef int64_t dword;
typedef int32_t word;
typedef int16_t hword;
typedef int64_t raw_instruction;

namespace iris32 {
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 268435456,
		InstructionPointerIndex = RegisterCount - 1,
		LinkRegisterIndex = RegisterCount - 2,
		StackPointerIndex = RegisterCount - 3,
		CallPointerIndex = RegisterCount - 4,
		InternalTemporaryCount = 8,
	};
	enum {
		WidthMask= 0b00000111,
		RestCount = ~(WidthMask) >> 3,
	};
	class MemoryController {
		public:
			MemoryController(word memSize);
			~MemoryController();
			word read(word address);
			void write(word address, word value);
			void install(std::istream& stream);
			void dump(std::ostream& stream);
		private:
			word memorySize;
			word* memory;
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
