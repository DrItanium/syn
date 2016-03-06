#include "iris.h"

namespace iris16 {
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
#define X(name, id, type, target, dest, src) \
			case id: \
					 { \
					 INDIRECTOR(X,type)(target, dest, src) \
			break; \
					 }
#include "move.def"
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
			default:
				std::cerr << "Illegal move code " << current.getOperation() << std::endl;
				execute = false;
				advanceIp = false;
				break;
		}
	}
}
