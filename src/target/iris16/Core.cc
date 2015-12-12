#include "target/iris16/iris.h"

namespace iris16 {
	Core::Core() { }

	void Core::initialize() {

	}

	void Core::installprogram(std::istream& stream) {

	}

	void Core::shutdown() {

	}
	void Core::dump(std::ostream& stream) {

	}
	void Core::run() {
		while(execute) {
			current.decode(instruction[gpr[ArchitectureConstants::InstructionPointerIndex]]);
			dispatch();
			if (advanceIp) {
				gpr[ArchitectureConstants::InstructionPointerIndex]++;
			}
		}
	}
	void Core::dispatch() {
		switch(static_cast<InstructionGroup>(current.getGroup())) {
#define X(_, operation, tag) case tag: operation(); break; 
#include "target/iris16/groups.def"
#undef X
			default:
				std::cerr << "Illegal instruction group " << current.getGroup() << std::endl;
				execute = false;
				break;
		}
	}
template<ArithmeticOp op>
word arithmeticOp(word a, word b) {
	return a;
}
#define XNone(n, op) \
	template<> \
	word arithmeticOp<n>(word a, word b) { \
		return a op b; \
	}
#define XDenominator(n, op) XNone(n, op)
#define XUnary(n, op) \
template<> \
	word arithmeticOp<n>(word a, word unused) { \
		return op a; \
	}
#define XImmediate(n, op) XNone(n, op) 
#define XDenominatorImmediate(n, op) XDenominator(n, op)
#define X(name, title, op, desc) INDIRECTOR(X, desc)(title, op)
#include "target/iris16/arithmetic.def"
#undef X
#undef XNone
#undef XDenominator
#undef XUnary
#undef XImmediate
#undef XDenominatorImmediate

	void Core::arithmetic() {
		switch(static_cast<ArithmeticOp>(current.getOperation())) {
#define XNone(n) gpr[current.getDestination()] = arithmeticOp<n>( gpr[current.getSource0()], gpr[current.getSource1()]);
#define XImmediate(n) gpr[current.getDestination()] = arithmeticOp<n>(gpr[current.getSource0()], static_cast<word>(current.getSource1()));
#define XUnary(n) gpr[current.getDestination()] = arithmeticOp<n>(gpr[current.getSource0()], 0);
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
#define X(name, title, op, desc) \
			case title: \
						{ \
							INDIRECTOR(X, desc)(title) \
							break; \
						}
#include "target/iris16/arithmetic.def"
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

	void Core::jump() {

	}
	void Core::misc() {

	}
	void Core::move() {

	}
	void Core::compare() {
	
	}
}
