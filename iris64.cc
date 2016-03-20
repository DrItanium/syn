#include "iris16.h"
#include <functional>

namespace iris16 {

	word encodeWord(byte a, byte b) {
		return iris::encodeBits<word, byte, 0xFF00, 8>(iris::encodeBits<word, byte, 0x00FF>(word(0), a), b);
	}
	dword encodeDword(byte a, byte b, byte c, byte d) {
		return iris::encodeBits<dword, byte, 0xFF000000, 24>(iris::encodeBits<dword, byte, 0x00FF0000, 16>( iris::encodeBits<dword, byte, 0x0000FF00, 8>( iris::encodeBits<dword, byte, 0x000000FF>(dword(0), a), b), c), d);
	}

	DecodedInstruction::DecodedInstruction() { }

	void DecodedInstruction::decode(raw_instruction input) {
#define X(title, mask, shift, type, is_register, post) \
		_ ## post = iris::decodeBits<raw_instruction, type, mask, shift>(input);
#include "iris16_instruction.def"
#undef X
	}

	Core::Core() { }
	void Core::setInstructionMemory(word address, dword value) {
		instruction[address] = value;
	}
	void Core::setDataMemory(word address, word value) {
		data[address] = value;
	}
	void Core::initialize() {

	}
	void Core::shutdown() {

	}
	template<typename T, int count>
	void populateContents(T* contents, std::istream& stream, std::function<T(char*)> func) {
		char* buf = new char[sizeof(T)];
		for(int i = 0; i < count; ++i) {
			stream.read(buf, sizeof(T));
			contents[i] = func(buf);
		}
		delete[] buf;
	}
	void Core::installprogram(std::istream& stream) {
		auto encodeWord = [](char* buf) {
			return iris16::encodeWord(buf[0], buf[1]);
		};
		auto encodeDword = [](char* buf) {
			return iris16::encodeDword(buf[0], buf[1], buf[2], buf[3]);
		};
		populateContents<word, ArchitectureConstants::RegisterCount>(gpr, stream, encodeWord);
		populateContents<word, ArchitectureConstants::AddressMax>(data, stream, encodeWord);
		populateContents<dword, ArchitectureConstants::AddressMax>(instruction, stream, encodeDword);
		populateContents<word, ArchitectureConstants::AddressMax>(stack, stream, encodeWord);
	}

	template<typename T, int count>
	void dumpContents(T* contents, std::ostream& stream, std::function<char*(T,char*)> func) {
		char* buf = new char[sizeof(T)];
		for(int i = 0; i < count; ++i) {
			func(contents[i], buf);
			stream.write(buf, sizeof(T));
		}
		delete[] buf;
	}
	void Core::dump(std::ostream& stream) {
		// save the registers
		auto decomposeWord = [](word v, char* buf) {
			buf[0] = (char)v;
			buf[1] = (char)(v >> 8);
			return buf;
		};
		auto decomposeDword = [](dword v, char* buf) {
			buf[0] = (char)v;
			buf[1] = (char)(v >> 8);
			buf[2] = (char)(v >> 16);
			buf[3] = (char)(v >> 24);
			return buf;
		};
		dumpContents<word, ArchitectureConstants::RegisterCount>(gpr, stream, decomposeWord);
		dumpContents<word, ArchitectureConstants::AddressMax>(data, stream, decomposeWord);
		dumpContents<dword, ArchitectureConstants::AddressMax>(instruction, stream, decomposeDword);
		dumpContents<word, ArchitectureConstants::AddressMax>(stack, stream, decomposeWord);
	}
	void Core::run() {
		while(execute) {
			if (!advanceIp) {
				advanceIp = true;
			}
			current.decode(instruction[gpr[ArchitectureConstants::InstructionPointerIndex]]);
			dispatch();
			if (advanceIp) {
				gpr[ArchitectureConstants::InstructionPointerIndex]++;
			} 
		}
	}
	void Core::dispatch() {
		switch(static_cast<InstructionGroup>(current.getGroup())) {
#define X(name, operation) case InstructionGroup:: name: operation(); break; 
#include "iris16_groups.def"
#undef X
			default:
				std::cerr << "Illegal instruction group " << current.getGroup() << std::endl;
				execute = false;
				break;
		}
	}
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

#include "iris16_compare.def"
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
#include "iris16_arithmetic.def"
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
#include "iris16_arithmetic.def"
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
#include "iris16_jump.def"
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
#include "iris16_jump.def"
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
#include "iris16_misc.def"
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
#include "iris16_move.def"
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
}
