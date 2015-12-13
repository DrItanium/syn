#include "target/iris16/iris.h"

namespace iris16 {
	template<JumpOp op>
	struct FormIfThenElse {
		static const bool is = false;
	};
	template<JumpOp op>
	struct FormConditional {
		static const bool is = false;
	};
	template<JumpOp op>
	struct JumpLinkData {
		static const bool isLinkRegisterForm = false;
	};
	template<JumpOp op> 
	struct ConditionalStyle {
		static const bool isFalseForm = false;
	};
	template<JumpOp op>
	struct AddressStyle {
		static const bool isImmediateForm = false;
	};
	template<JumpOp op>
	bool jumpCond(word cond) {
		return ConditionalStyle<op>::isFalseForm ? (cond == 0) : (cond != 0);
	}
	template<JumpOp op>
	word jumpOp(word cond, word onTrue, word onFalse) {
		if (FormConditional<op>::is) {
			return jumpCond<op>(cond) ? onTrue : onFalse;
		} else {
			return onTrue;
		}
	}
	void Core::jump() {
		word newAddr = 0;
		bool cond = true;
		word ip = gpr[ArchitectureConstants::InstructionPointerIndex];
#define X(op) \
		if (FormConditional<op>::is) { \
			cond = jumpCond<op>(gpr[current.getDestination()]); \
			if(FormIfThenElse<op>::is) { \
				newAddr = gpr[cond ? current.getSource0() : current.getSource1()]; \
			} else { \
				if (AddressStyle<op>::isImmediateForm) { \
					newAddr = cond ? current.getImmediate() : ip + 1; \
				} else { \
					newAddr = cond ? gpr[current.getSource0()] : ip + 1; \
				} \
			} \
		} else  { \
			if (AddressStyle<op>::isImmediateForm) { \
				newAddr = current.getImmediate(); \
			} else { \
				newAddr = gpr[current.getDestination()]; \
			} \
		} \
		gpr[ArchitectureConstants::InstructionPointerIndex] = newAddr; \
		if (JumpLinkData<op>::isLinkRegisterForm()) { \
			if (cond) { \
				gpr[ArchitectureConstants::LinkRegisterIndex] = ip + 1; \
			} \
		} 
		switch(static_cast<JumpOp>(current.getOperation())) {
			case JumpOp::UnconditionalImmediate:
				{
					break;
				}
			default:
				break;
		}
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
}
