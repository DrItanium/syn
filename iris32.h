#ifndef _TARGET_IRIS32_IRIS_H
#define _TARGET_IRIS32_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include <cstdint>

typedef int64_t dword;
typedef int32_t word;
typedef int16_t hword;

namespace iris32 {
	word encodeWord(byte, byte, byte, byte);
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 268435456 /* bytes */ / sizeof(word), // words
		InstructionPointerIndex = RegisterCount - 1,
		LinkRegisterIndex = RegisterCount - 2,
		StackPointerIndex = RegisterCount - 3,
		CallPointerIndex = RegisterCount - 4,
		
	};
	enum {
		GroupMask = 0b00000111,
		RestMask = ~GroupMask,
		MaxInstructionsPerGroup = RestMask >> 3,
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
	enum class InstructionGroup {
#define X(e, __) e ,
#include "iris32_groups.def"
#undef X
	};
	class DecodedInstruction {
		enum class Fields {
#define X(en, u0, u1, u2, u3, u4) en ,
#include "iris32_instruction.def"
#undef X
			Count,
		};
		public:
			DecodedInstruction(word rinst);
#define X(field, mask, shift, type, isreg, unused) \
			type get ## field (); \
			void set ## field (type value);
#include "iris32_instruction.def"
#undef X
			word encodeInstruction();
		private:
#define X(u0, u1, u2, type, u3, fieldName) type fieldName; 
#include "iris32_instruction.def"
#undef X
			word raw;
	};
	/// Represents the execution state of a thread of execution
	struct ExecState {
		bool advanceIp = true;
		word gpr[ArchitectureConstants::RegisterCount] = { 0 };
	};

	enum class CompareOp : byte {
#define X(title, operation, unused) title,
#define Y(title, operation, unused) title,
#include "iris32_compare.def"
#undef X
#undef Y
		NumberOfCompareOps,
	};
	static_assert(byte(CompareOp::NumberOfCompareOps) <= byte(MaxInstructionsPerGroup), "Too many compare operations defined!");

	enum class ArithmeticOp : byte {
#define X(title, operation, type) title ,
#include "iris32_arithmetic.def"
#undef X
		NumberOfArithmeticOps,
	};
	static_assert(byte(ArithmeticOp::NumberOfArithmeticOps) <= byte(MaxInstructionsPerGroup), "Too many arithmetic operations defined!");

	enum class MoveOp : byte {
#define X(title, operation, __, ___, ____) title ,
#include "iris32_move.def"
#undef X
		NumberOfMoveOps,
	};
	static_assert(byte(MoveOp::NumberOfMoveOps) <= byte(MaxInstructionsPerGroup), "Too many move operations defined!");

	enum class JumpOp : byte {
#define X(title, u0, u1, u2, u3, u4) title ,
#include "iris32_jump.def"
#undef X
		NumberOfJumpOps,
	};
	static_assert(byte(JumpOp::NumberOfJumpOps) <= byte(MaxInstructionsPerGroup), "Too many jump operations defined!");

	enum class MiscOp : byte {
#define X(title, __) title ,
#include "iris32_misc.def"
#undef X
		NumberOfMiscOps,
	};
	static_assert(byte(MiscOp::NumberOfMiscOps) <= byte(MaxInstructionsPerGroup), "Too many misc operations defined!");

	enum class SystemCalls : byte {
#define X(title) title ,
#include "iris32_syscalls.def"
#undef X
		NumberOfSyscalls,
	};
	static_assert(byte(SystemCalls::NumberOfSyscalls) <= 255, "Too many syscall operations defined!");


	class Core : public iris::Core {
		public:
			Core(word memorySize, ExecState* t0, ExecState* t1);
			~Core();
			virtual void initialize();
			virtual void installprogram(std::istream& stream);
			virtual void shutdown();
			virtual void dump(std::ostream& stream);
			virtual void run();
			void write(word address, word value);
			bool debugEnabled();
			void toggleDebug();
			word read(word address);
		private:
			void execBody();
			void decode();
			void dispatch();
#define X(_, func) void func (DecodedInstruction& inst); 
#include "iris32_groups.def"
#undef X
			void systemCall(DecodedInstruction& inst);
		private:
			word memorySize;
			word* memory;
			ExecState *thread = 0;
			ExecState *thread0,
					  *thread1;
			bool execute = true;
			bool debug = false;
	};

} // end namespace iris32
#endif // end _TARGET_IRIS32_IRIS_H
