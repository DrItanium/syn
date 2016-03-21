#include "iris32.h"
#include <functional>

namespace iris32 {
	word encodeWord(byte a, byte b, byte c, byte d) {
		return word(a) | word(b) << 8 | word(c) << 16 | word(d) << 24; 
	}
	DecodedInstruction::DecodedInstruction(word inst) :
#define X(title, mask, shift, type, isreg, field ) \
		field ( (type) ((inst & mask) >> shift) ),
#include "iris32_instruction.def"
#undef X
		raw(inst)
	{ }
#define X(title, mask, shift, type, isreg, field) \
	type DecodedInstruction:: get ## title ( ) { \
		return field ; \
	} \
	void DecodedInstruction:: set ## title ( type value ) { \
		field = value ; \
	}
#include "iris32_instruction.def"
#undef X
	word DecodedInstruction::encodeInstruction() {
		return
#define X(title, mask, shift, type, isreg, field) \
			((word(field) << shift) & mask) |
#include "iris32_instruction.def"
#undef X
			0;
	}

	void Core::write(word address, word value) {
		auto addr = address >> 2; // shift the address
		if (addr >= 0 && addr < memorySize) {
			memory[addr] = value;
		} else {
			std::cerr << "Address " << std::hex << address << " is out of range" << std::endl;
			execute = false;
			throw 0;
		}
	}
	word Core::read(word address) {
		auto addr = address >> 2; // modify the address
		if (addr >= 0 && addr < memorySize) {
			return memory[addr];
		} else {
			std::cerr << "Address " << std::hex << address << " is out of range" << std::endl;
			execute = false;
			throw 0;
		}
	}

	Core::Core(word msize, ExecState&& t0, ExecState&& t1) : 
		memorySize(msize / sizeof(word)),
		memory(new word[msize / sizeof(word)]),
		thread0(std::move(t0)),
		thread1(std::move(t1))
	{ }
	Core::~Core() {
		delete [] memory;
		memory = 0;
	}
	void Core::installprogram(std::istream& stream) {
		char storage[sizeof(word)] = { 0 };
		for (hword i = 0; i < memorySize; ++i) {
			if (!stream.good()) {
				std::cerr << "WARNING: Memory image is smaller than total memory!" << std::endl;
				break;
			} else {
				stream.read(storage, sizeof(word));
				memory[i] = word(storage[0]) | (word(storage[1]) << 8) | (word(storage[2]) << 16) | (word(storage[3]) << 24);
			}
		}
	}
	void Core::dump(std::ostream& stream) {
		char storage[sizeof(word)] = { 0 };
		for (word i = 0; i < memorySize; ++i) {
			auto cell = memory[i];
			for (int j = 0; j < int(sizeof(word)); ++j) {
				storage[j] = byte(cell >> (8 * j));
			}
			stream.write(storage, sizeof(word));
		}
	}
	void Core::dispatch(ExecState& thread) {
		// read a byte from the current instruction pointer address
		auto decoded = DecodedInstruction(read(thread.gpr[ArchitectureConstants::InstructionPointerIndex]));
		switch (static_cast<InstructionGroup>(decoded.getGroup())) {
#define X(en, fn) \
			case InstructionGroup:: en : \
				 fn (thread, decoded) ; \
			break;
#include "iris32_groups.def"
#undef X
			default:
				std::cerr << "Illegal instruction group " << std::hex << decoded.getGroup() << "!" << std::endl;
				execute = false;
				break;
		}
	}
	void Core::initialize() {

	}
	void Core::shutdown() {

	}
	void Core::run() {
		while(execute) {
			execBody(thread0);
			if (execute) {
				execBody(thread1);
			}
		}
	}
	void Core::execBody(ExecState& thread) {
		if (!thread.advanceIp) {
			thread.advanceIp = true;
		}
		dispatch(thread);
		if (thread.advanceIp) {
			++thread.gpr[ArchitectureConstants::InstructionPointerIndex];
		}
	}
	void Core::compare(ExecState& thread, DecodedInstruction& current) {
		switch(static_cast<CompareOp>(current.getOperation())) {
#define OpNone =
#define OpAnd &=
#define OpOr |=
#define OpXor ^=
#define X(type, compare, mod) \
			case CompareOp:: type: \
								   thread.gpr[current.getDestination()] INDIRECTOR(Op, mod) (thread.gpr[current.getSource0()] compare thread.gpr[current.getSource1()]); \
			break;
#define Y(type, compare, mod) \
			case CompareOp:: type: \
								   thread.gpr[current.getDestination()] INDIRECTOR(Op, mod) (thread.gpr[current.getSource0()] compare (word(current.getSource1()))); \
			break;

#include "iris32_compare.def"
#undef X
#undef Y
#undef OpNone
#undef OpAnd
#undef OpOr
#undef OrXor
			default:
				std::cerr << "Illegal compare code " << current.getOperation() << std::endl;
				execute = false;
				thread.advanceIp = false;
				break;
		}
	}
	template<ArithmeticOp op>
		word arithmeticOp(word a, word b) {
			return a;
		}
#define XNone(n, op) \
	template<> \
	word arithmeticOp<ArithmeticOp:: n>(word a, word b) { \
		return a op b; \
	}
#define XDenominator(n, op) XNone(n, op)
#define XUnary(n, op) \
	template<> \
	word arithmeticOp<ArithmeticOp:: n>(word a, word unused) { \
		return op a; \
	}
#define XImmediate(n, op) XNone(n, op) 
#define XDenominatorImmediate(n, op) XDenominator(n, op)
#define X(name, op, desc) INDIRECTOR(X, desc)(name, op)
#include "iris32_arithmetic.def"
#undef X
#undef XNone
#undef XDenominator
#undef XUnary
#undef XImmediate
#undef XDenominatorImmediate

	void Core::arithmetic(ExecState& thread, DecodedInstruction& inst) {
		switch(static_cast<ArithmeticOp>(inst.getOperation())) {
#define XNone(n) thread.gpr[inst.getDestination()] = arithmeticOp<ArithmeticOp:: n>( thread.gpr[inst.getSource0()], thread.gpr[inst.getSource1()]);
#define XImmediate(n) thread.gpr[inst.getDestination()] = arithmeticOp<ArithmeticOp:: n>(thread.gpr[inst.getSource0()], static_cast<word>(inst.getSource1()));
#define XUnary(n) thread.gpr[inst.getDestination()] = arithmeticOp<ArithmeticOp:: n>(thread.gpr[inst.getSource0()], 0);
#define XDenominator(n) \
			if (thread.gpr[inst.getSource1()] == 0) { \
				std::cerr << "denominator in for operation " << #n << " is zero!" << std::endl; \
				execute = false; \
			} else { \
				XNone(n) \
			}
#define XDenominatorImmediate(n) \
			if (thread.gpr[inst.getSource1()] == 0) { \
				std::cerr << "denominator in for operation " << #n << " is zero!" << std::endl; \
				execute = false; \
			} else { \
				XImmediate(n) \
			}
#define X(name, op, desc) \
			case ArithmeticOp:: name: \
						{ \
							INDIRECTOR(X, desc)(name) \
							break; \
						}
#include "iris32_arithmetic.def"
#undef X
#undef XNone
#undef XDenominator
#undef XUnary
#undef XImmediate
#undef XDenominatorImmediate

			default:
				std::cerr << "Illegal arithmetic operation " << inst.getOperation() << std::endl;
				execute = false;
				break;
		}
	}
	template<JumpOp op> 
		struct ConditionalStyle {
			static const bool isFalseForm = false;
		};
#define X(name, ifthenelse, conditional, iffalse, immediate, link) \
	template<> struct ConditionalStyle<JumpOp:: name> { static const bool isFalseForm = iffalse; };
#include "iris32_jump.def"
#undef X
	template<JumpOp op>
		bool jumpCond(word cond) {
			return ConditionalStyle<op>::isFalseForm ? (cond == 0) : (cond != 0);
		}

	void Core::jump(ExecState& thread, DecodedInstruction& inst) {
		word newAddr = 0;
		bool cond = true;
		thread.advanceIp = false;
		word ip = thread.gpr[ArchitectureConstants::InstructionPointerIndex];
		switch(static_cast<JumpOp>(inst.getOperation())) {
#define XImmediateCond_true (inst.getImmediate())
#define XImmediateCond_false (thread.gpr[inst.getSource0()])
#define XIfThenElse_false(immediate) \
			newAddr = cond ? INDIRECTOR(XImmediateCond, _ ## immediate) : ip + 1;
#define XIfThenElse_true(immediate) \
			newAddr = thread.gpr[cond ? inst.getSource0() : inst.getSource1()];
#define XImmediateUncond_false (thread.gpr[inst.getDestination()])
#define XImmediateUncond_true (inst.getImmediate())
#define XConditional_false(name, ifthenelse, immediate) \
			newAddr = INDIRECTOR(XImmediateUncond, _ ## immediate);
#define XConditional_true(name, ifthenelse, immediate) \
			cond = jumpCond<JumpOp:: name> (thread.gpr[inst.getDestination()]); \
			INDIRECTOR(XIfThenElse, _ ## ifthenelse)(immediate)
#define XLink_true \
			if (cond) { \
				thread.gpr[ArchitectureConstants::LinkRegisterIndex] = ip + 1; \
			}
#define XLink_false

#define X(name, ifthenelse, conditional, iffalse, immediate, link) \
			case JumpOp:: name: \
					 { \
						 INDIRECTOR(XConditional, _ ## conditional)(name, ifthenelse, immediate) \
						 thread.gpr[ArchitectureConstants::InstructionPointerIndex] = newAddr; \
						 INDIRECTOR(XLink, _ ## link)  \
						 break; \
					 }
#include "iris32_jump.def"
#undef X
			default:
				std::cerr << "Illegal jump code " << inst.getOperation() << std::endl;
				execute = false;
				break;
		}
	}
	void Core::misc(ExecState& thread, DecodedInstruction& inst) {
		switch(static_cast<MiscOp>(inst.getOperation())) {
#define X(name, func) \
			case MiscOp:: name: \
			func (thread, inst); \
			break;
#include "iris32_misc.def"
#undef X
			default:
				std::cerr << "Illegal misc code " << inst.getOperation() << std::endl;
				execute = false;
				thread.advanceIp = false;
				break;
		}
	}
	void Core::systemCall(ExecState& thread, DecodedInstruction& inst) {
		switch(static_cast<SystemCalls>(inst.getDestination())) {
			case SystemCalls::Terminate:
				execute = false;
				thread.advanceIp = false;
				break;
			case SystemCalls::PutC:
				// read register 0 and register 1
				std::cout.put((char)thread.gpr[inst.getSource0()]);
				break;
			case SystemCalls::GetC:
				byte value;
				std::cin >> std::noskipws >> value;
				thread.gpr[inst.getSource0()] = (word)value;
				break;
			default:
				std::cerr << "Illegal system call " << inst.getDestination() << std::endl;
				execute = false;
				thread.advanceIp = false;
				break;
		}
	}
	void Core::move(ExecState& thread, DecodedInstruction& inst) {
		word a = 0;
		switch(static_cast<MoveOp>(inst.getOperation())) {
#define GPRRegister0 (thread.gpr[inst.getDestination()])
#define GPRRegister1 (thread.gpr[inst.getSource0()])
#define GPRRegister2 (thread.gpr[inst.getSource1()])
#define GPRImmediate1 (inst.getImmediate())
#define DataRegister0 GPRRegister0
#define DataRegister1 GPRRegister1
#define DataImmediate1 GPRImmediate1
#define StackPushRegister0 (thread.gpr[ArchitectureConstants::StackPointerIndex])
#define StackPushRegister1 GPRRegister0
#define StackPushImmediate1 GPRImmediate1
#define StackPopRegister0 GPRRegister0
#define StackPopRegister1 (thread.gpr[ArchitectureConstants::StackPointerIndex])
#define StackPopImmediate1 GPRImmediate1
#define StoreRegister0  GPRRegister0
#define StoreRegister1 GPRRegister1
#define StoreImmediate1 GPRImmediate1
#define CodeRegister0 GPRRegister0
#define CodeUpperLowerRegisters1 GPRRegister1
#define CodeUpperLowerRegisters2 GPRRegister2

#define XMove(type, dest, src) \
			INDIRECTOR(type, dest ## 0) = INDIRECTOR(type, src ## 1);
#define XSetLower(type, dest, src) \
			INDIRECTOR(type, dest ## 0) |= (word(INDIRECTOR(type, src ## 1)));
#define XSetUpper(type, dest, src) \
			INDIRECTOR(type, dest ## 0) |= (word(INDIRECTOR(type, src ## 1)) << 16);

#define XSwap(type, dest, src) \
			a = INDIRECTOR(type, dest ##  0); \
			INDIRECTOR(type, dest ## 0) = INDIRECTOR(type, src ## 1); \
			INDIRECTOR(type, src ##  1) = a;
#define XLoad(type, dest, src) \
			INDIRECTOR(type, dest ## 0) = read(INDIRECTOR(type, src ## 1));
#define XPop(type, dest, src) \
			INDIRECTOR(type, Pop ## dest ## 0) = read(INDIRECTOR(type, Pop ## src ## 1)); \
			++INDIRECTOR(type, Pop ## src ## 1); 
#define XPush(type, dest, src) \
			--INDIRECTOR(type, Push ## dest ## 0); \
			write(INDIRECTOR(type, Push ## dest ## 0), INDIRECTOR(type, Push ## src ## 1)); 
#define XStore(type, dest, src) \
			write(INDIRECTOR(type, dest ## 0), INDIRECTOR(type, src ## 1));
#define X(name, type, target, dest, src) \
			case MoveOp:: name: \
					 { \
					 INDIRECTOR(X,type)(target, dest, src) \
			break; \
					 }
#include "iris32_move.def"
#undef X
#undef XMove
#undef XSwap
#undef XLoad
#undef XStore
#undef XPop
#undef XPush
#undef GPRRegister0 
#undef GPRRegister1 
#undef GPRImmediate1 
#undef DataRegister0 
#undef DataRegister1 
#undef DataImmediate1
#undef StackPushRegister0 
#undef StackPushRegister1  
#undef StackPushImmediate1 
#undef StackPopRegister0 
#undef StackPopRegister1 
#undef StackPopImmediate1 	
#undef StoreRegister0  	
#undef StoreRegister1 		
#undef StoreImmediate1 	
#undef XStoreCode
#undef XLoadCode
#undef CodeRegister0 
#undef CodeUpperLowerRegisters1 
#undef CodeUpperLowerRegisters2 
			default:
				std::cerr << "Illegal move code " << inst.getOperation() << std::endl;
				execute = false;
				thread.advanceIp = false;
				break;
		}
	}
} // end namespace iris32
