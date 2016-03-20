#include "iris32.h"
#include <functional>

namespace iris32 {
	ExecState::ExecState(word numRegs) : regCount(numRegs), gpr(new word[numRegs]) { }
	ExecState::~ExecState() {
		delete [] gpr;
		gpr = 0;
	}
	word& ExecState::operator[](word idx) {
		if (idx < 0 || idx >= regCount) {
			throw "Illegal register index!";
		} else {
			return gpr[idx];
		}
	}
	bool ExecState::shouldAdvanceIp() {
			return advanceIp;
	}
	void ExecState::setAdvanceIp(bool value) {
		advanceIp = value;
	}
	MemoryController::MemoryController(hword size) : memorySize(size), memory(new byte[size]) { }

	MemoryController::~MemoryController() {
		delete [] memory;
		memory = 0;
	}


	void MemoryController::writeByte(word addr, byte value) {
		if (addr >= 0 && addr < memorySize) {
			memory[addr] = value;
		} else {
			throw "Address out of range";
		}
	}
	byte MemoryController::readByte(word addr) {
		if (addr >= 0 && addr < memorySize) {
			return memory[addr];
		} else {
			throw "Address out of range";
		}
	}

	void MemoryController::install(std::istream& stream) {
		for (hword i = 0; i < memorySize; ++i) {
			if (!stream.good()) {
				throw "Memory size too small";
			} else {
				auto value = stream.get();
				memory[i] = byte(value);
			}
		}
	}
	void MemoryController::dump(std::ostream& stream) {
		char storage[1] = { 0 };
		for (hword i = 0; i < memorySize; ++i) {
			storage[0] = memory[i];
			stream.write(storage, 1);
		}
	}

	Core::Core(MemoryController* m, ExecState&& t0, ExecState&& t1) : 
		mc(m),
		thread0(std::move(t0)),
		thread1(std::move(t1))
	{ }
	void Core::installprogram(std::istream& stream) {
		mc->install(stream);
	}
	void Core::dump(std::ostream& stream) {
		mc->dump(stream);
	}
	void Core::decode(ExecState& thread) {
		// read a byte from the current instruction pointer address
		byte curr = mc->readByte(thread[ArchitectureConstants::InstructionPointerIndex]);
		++thread[ArchitectureConstants::InstructionPointerIndex];
		auto width = DecodeWidth(curr & DecodeMask);
		byte rest = (curr & (~DecodeMask)) >> 5;
		switch (width) {
			case DecodeWidth::Variable:
				decodeVariable(thread, rest);
				break;
			case DecodeWidth::OneByte: // one byte
				decodeOneByte(thread, OneByteOperations(rest));
				break;
			case DecodeWidth::TwoByte:
				decodeTwoByte(thread, TwoByteOperations(rest));
				break;
			case DecodeWidth::FourByte:
				decodeFourByte(thread, rest);
				break;
			case DecodeWidth::EightByte:
				decodeEightByte(thread, rest);
				break;
			case DecodeWidth::TenByte:
				decodeTenByte(thread, rest);
				break;
			default:
				throw "Illegal opcode";
		}
	}
	void Core::initialize() {

	}
	void Core::shutdown() {

	}
	void Core::decodeVariable(ExecState& curr, byte input) {

	}
	word MemoryController::readWord(word address) {
		// assume it is a byte address
		auto addr = address >> 3;
		if (addr < 0 || addr >= memorySize) {
			throw "Address out of range";
		} else {
			return ((word*)memory)[addr];
		}
	}
	void Core::decodeOneByte(ExecState& curr, OneByteOperations input) {
		switch (input) {
			case OneByteOperations::Return:
				{
				auto callTop = curr[ArchitectureConstants::CallPointerIndex];
				curr[ArchitectureConstants::InstructionPointerIndex] = mc->readWord(callTop);
				curr[ArchitectureConstants::CallPointerIndex] = ((callTop >> 3) + 1) << 3; // decrement the call pointer
				break;
				}
			default:
				{
				throw "Illegal operation";
				}
		}
	}
	void Core::decodeTwoByte(ExecState& curr, TwoByteOperations input) {
		byte reg = mc->readByte(curr[ArchitectureConstants::InstructionPointerIndex]);
		++curr[ArchitectureConstants::InstructionPointerIndex];
		switch(input) {
			case TwoByteOperations::PushByte:
				mc->writeByte(--curr[ArchitectureConstants::StackPointerIndex], reg);
				break;
			case TwoByteOperations::Increment:
				++curr[reg];
				break;
			case TwoByteOperations::Decrement:
				--curr[reg];
				break;
			case TwoByteOperations::Double:
				curr[reg] *= 2;
				break;
			case TwoByteOperations::Halve:
				curr[reg] /= 2;
				break;
			case TwoByteOperations::InvertBits:
				curr[reg] = ~curr[reg];
				break;
			case TwoByteOperations::Pop:
				curr[reg] = mc->readWord(curr[ArchitectureConstants::StackPointerIndex]);
				curr[ArchitectureConstants::StackPointerIndex] = ((curr[ArchitectureConstants::StackPointerIndex] >> 3) + 1) << 3;
				break;
			case TwoByteOperations::Square:
				curr[reg] = curr[reg] * curr[reg];
				break;
			case TwoByteOperations::CallRegister:
				curr[ArchitectureConstants::CallPointerIndex]
				break;
			default:
				throw "Illegal operation";
		}
	}
	void Core::decodeFourByte(ExecState& curr, byte input) {

	}
	void Core::decodeEightByte(ExecState& curr, byte input) {

	}
	void Core::decodeTenByte(ExecState& curr, byte input) {

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
		if (!thread.shouldAdvanceIp()) {
			thread.setAdvanceIp(true);
		}
		decode(thread);
		dispatch(thread);
		if (thread.shouldAdvanceIp()) {
			++thread[ArchitectureConstants::InstructionPointerIndex];
		}
	}
	void Core::dispatch(ExecState& thread) {
		/*
		switch(static_cast<InstructionGroup>(current.getGroup())) {
#define X(name, operation) case InstructionGroup:: name: operation(); break; 
#include "iris32_groups.def"
#undef X
			default:
				std::cerr << "Illegal instruction group " << current.getGroup() << std::endl;
				execute = false;
				break;
		}
		*/
	}
	/*
	void Core::compare() {
		switch(static_cast<CompareOp>(current.getOperation())) {
#define OpNone =
//#define OpAnd &=
//#define OpOr |=
//#define OpXor ^=
#define X(type, compare, mod) \
			case CompareOp:: type: \
								   gpr[current.getDestination()] INDIRECTOR(Op, mod) (gpr[current.getSource0()] compare gpr[current.getSource1()]); \
			break;
#define Y(type, compare, mod) \
			case CompareOp:: type: \
								   gpr[current.getDestination()] INDIRECTOR(Op, mod) (gpr[current.getSource0()] compare (word(current.getSource1()))); \
			break;

#include "iris32_compare.def"
#undef X
#undef Y
#undef OpNone
//#undef OpAnd
//#undef OpOr
//#undef OrXor
			default:
				std::cerr << "Illegal compare code " << current.getOperation() << std::endl;
				execute = false;
				advanceIp = false;
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

	void Core::arithmetic() {
		switch(static_cast<ArithmeticOp>(current.getOperation())) {
#define XNone(n) gpr[current.getDestination()] = arithmeticOp<ArithmeticOp:: n>( gpr[current.getSource0()], gpr[current.getSource1()]);
#define XImmediate(n) gpr[current.getDestination()] = arithmeticOp<ArithmeticOp:: n>(gpr[current.getSource0()], static_cast<word>(current.getSource1()));
#define XUnary(n) gpr[current.getDestination()] = arithmeticOp<ArithmeticOp:: n>(gpr[current.getSource0()], 0);
#define XDenominator(n) \
			if (gpr[current.getSource1()] == 0) { \
				std::cerr << "denominator in for operation " << #n << " is zero!" << std::endl; \
				execute = false; \
			} else { \
				XNone(n) \
			}
#define XDenominatorImmediate(n) \
			if (gpr[current.getSource1()] == 0) { \
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
				std::cerr << "Illegal arithmetic operation " << current.getOperation() << std::endl;
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

	void Core::jump() {
		word newAddr = 0;
		bool cond = true;
		advanceIp = false;
		word ip = gpr[ArchitectureConstants::InstructionPointerIndex];
		switch(static_cast<JumpOp>(current.getOperation())) {
#define XImmediateCond_true (current.getImmediate())
#define XImmediateCond_false (gpr[current.getSource0()])
#define XIfThenElse_false(immediate) \
			newAddr = cond ? INDIRECTOR(XImmediateCond, _ ## immediate) : ip + 1;
#define XIfThenElse_true(immediate) \
			newAddr = gpr[cond ? current.getSource0() : current.getSource1()];
#define XImmediateUncond_false (gpr[current.getDestination()])
#define XImmediateUncond_true (current.getImmediate())
#define XConditional_false(name, ifthenelse, immediate) \
			newAddr = INDIRECTOR(XImmediateUncond, _ ## immediate);
#define XConditional_true(name, ifthenelse, immediate) \
			cond = jumpCond<JumpOp:: name> (gpr[current.getDestination()]); \
			INDIRECTOR(XIfThenElse, _ ## ifthenelse)(immediate)
#define XLink_true \
			if (cond) { \
				gpr[ArchitectureConstants::LinkRegisterIndex] = ip + 1; \
			}
#define XLink_false

#define X(name, ifthenelse, conditional, iffalse, immediate, link) \
			case JumpOp:: name: \
					 { \
						 INDIRECTOR(XConditional, _ ## conditional)(name, ifthenelse, immediate) \
						 gpr[ArchitectureConstants::InstructionPointerIndex] = newAddr; \
						 INDIRECTOR(XLink, _ ## link)  \
						 break; \
					 }
#include "iris32_jump.def"
#undef X
			default:
				std::cerr << "Illegal jump code " << current.getOperation() << std::endl;
				execute = false;
				break;
		}
	}
	void Core::misc() {
		switch(static_cast<MiscOp>(current.getOperation())) {
#define X(name, func) \
			case MiscOp:: name: \
			func (); \
			break;
#include "iris32_misc.def"
#undef X
			default:
				std::cerr << "Illegal misc code " << current.getOperation() << std::endl;
				execute = false;
				advanceIp = false;
				break;
		}
	}
	void Core::systemCall() {
		switch(static_cast<SystemCalls>(current.getDestination())) {
			case SystemCalls::Terminate:
				execute = false;
				advanceIp = false;
				break;
			case SystemCalls::PutC:
				// read register 0 and register 1
				std::cout.put((char)gpr[current.getSource0()]);
				break;
			case SystemCalls::GetC:
				byte value;
				std::cin >> std::noskipws >> value;
				gpr[current.getSource0()] = (word)value;
				break;
			default:
				std::cerr << "Illegal system call " << current.getDestination() << std::endl;
				execute = false;
				advanceIp = false;
				break;
		}
	}
	void Core::move() {
		word a = 0;
		switch(static_cast<MoveOp>(current.getOperation())) {
#define GPRRegister0 (gpr[current.getDestination()])
#define GPRRegister1 (gpr[current.getSource0()])
#define GPRRegister2 (gpr[current.getSource1()])
#define GPRImmediate1 (current.getImmediate())
#define DataRegister0 GPRRegister0
#define DataRegister1 GPRRegister1
#define DataImmediate1 GPRImmediate1
#define StackPushRegister0 (gpr[ArchitectureConstants::StackPointerIndex])
#define StackPushRegister1 GPRRegister0
#define StackPushImmediate1 GPRImmediate1
#define StackPopRegister0 GPRRegister0
#define StackPopRegister1 (gpr[ArchitectureConstants::StackPointerIndex])
#define StackPopImmediate1 GPRImmediate1
#define StoreRegister0  GPRRegister0
#define StoreRegister1 GPRRegister1
#define StoreImmediate1 GPRImmediate1
#define CodeRegister0 GPRRegister0
#define CodeUpperLowerRegisters1 GPRRegister1
#define CodeUpperLowerRegisters2 GPRRegister2

#define XLoadCode(type, dest, src) \
			auto result = instruction[INDIRECTOR(type, dest ## 0)]; \
			INDIRECTOR(type, src ## 1) = (word)result; \
			INDIRECTOR(type, src ## 2) = (word)(result >> 16);

#define XStoreCode(type, dest, src) \
			instruction[INDIRECTOR(type, dest ## 0)] = ((((dword)INDIRECTOR(type, src ## 2)) << 16) | ((dword)INDIRECTOR(type, src ## 1)));

#define XMove(type, dest, src) \
			INDIRECTOR(type, dest ## 0) = INDIRECTOR(type, src ## 1);
#define XSwap(type, dest, src) \
			a = INDIRECTOR(type, dest ##  0); \
			INDIRECTOR(type, dest ## 0) = INDIRECTOR(type, src ## 1); \
			INDIRECTOR(type, src ##  1) = a; 
#define XLoad(type, dest, src) \
			INDIRECTOR(type, dest ## 0) = data[INDIRECTOR(type, src ## 1)];
#define XPop(type, dest, src) \
			INDIRECTOR(type, Pop ## dest ## 0) = stack[INDIRECTOR(type, Pop ## src ## 1)]; \
			--INDIRECTOR(type, Pop ## src ## 1); 
#define XPush(type, dest, src) \
			++INDIRECTOR(type, Push ## dest ## 0); \
			stack[INDIRECTOR(type, Push ## dest ## 0)] = INDIRECTOR(type, Push ## src ## 1);
#define XStore(type, dest, src) \
			data[INDIRECTOR(type, dest ##  0)] = INDIRECTOR(type, src ## 1); 
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
				std::cerr << "Illegal move code " << current.getOperation() << std::endl;
				execute = false;
				advanceIp = false;
				break;
		}
	}
	*/
} // end namespace iris32
