#include "iris.h"

namespace iris16 {
	template<JumpOp op> 
		struct ConditionalStyle {
			static const bool isFalseForm = false;
		};
#define X(name, id, ifthenelse, conditional, iffalse, immediate, link) \
	template<> struct ConditionalStyle<id> { static const bool isFalseForm = iffalse; };
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
#define X(name, op, ifthenelse, conditional, iffalse, immediate, link) \
			case op: \
					 { \
						 if (conditional) { \
							 cond = jumpCond<op>(gpr[current.getDestination()]); \
							 if(ifthenelse) { \
								 newAddr = gpr[cond ? current.getSource0() : current.getSource1()]; \
							 } else { \
								 if (immediate) { \
									 newAddr = cond ? current.getImmediate() : ip + 1; \
								 } else { \
									 newAddr = cond ? gpr[current.getSource0()] : ip + 1; \
								 } \
							 } \
						 } else  { \
							 if (immediate) { \
								 newAddr = current.getImmediate(); \
							 } else { \
								 newAddr = gpr[current.getDestination()]; \
							 } \
						 } \
						 gpr[ArchitectureConstants::InstructionPointerIndex] = newAddr; \
						 if (link && cond) { \
								 gpr[ArchitectureConstants::LinkRegisterIndex] = ip + 1; \
						 }  \
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
}
