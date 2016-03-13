#include "iris16.h"
#include <functional>

namespace iris16 {

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
#include "groups.def"
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

#include "compare.def"
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
#include "arithmetic.def"
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
#include "arithmetic.def"
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
#include "jump.def"
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
#include "jump.def"
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
#include "misc.def"
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
}
