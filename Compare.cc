#include "iris.h"

namespace iris16 {
	void Core::compare() {
		switch(static_cast<CompareOp>(current.getOperation())) {
#define OpNone =
#define OpAnd &=
#define OpOr |=
#define OpXor ^=
#define X(type, compare, mod) \
			case CompareOp:: type: \
								   gpr[current.getDestination()] INDIRECTOR(Op, mod) (gpr[current.getSource0()] compare gpr[current.getSource1()]); \
			break;

#include "compare.def"
#undef X
#undef OpNone
#undef OpAnd
#undef OpOr
#undef OrXor
			default:
				std::cerr << "Illegal compare code " << current.getOperation() << std::endl;
				execute = false;
				advanceIp = false;
				break;
		}
	}
}
