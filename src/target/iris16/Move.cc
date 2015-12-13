#include "target/iris16/iris.h"

namespace iris16 {
	void Core::move() {
		word a = 0;
		switch(static_cast<MoveOp>(current.getOperation())) {
#define GPRRegister0 (gpr[current.getDestination()])
#define GPRRegister1 (gpr[current.getSource0()])
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
				INDIRECTOR(X,type)(target, dest, src) \
			break;
#include "target/iris16/move.def"
#undef X
#undef XMoveGPRRegisterRegister 
#undef XMoveGPRRegisterImmediate 
#undef XSwapGPRRegisterRegister 
#undef XLoadDataRegisterRegister 
#undef XLoadDataRegisterImmediate 
#undef XLoadStackRegisterRegister 
#undef XStoreDataRegisterRegister 
#undef XStoreDataRegisterImmediate 
#undef XStoreStackRegisterRegister 
#undef XStoreStackRegisterImmediate 
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
				std::cout << (char)gpr[current.getSource0()];
				break;
			case SystemCalls::GetC:
				byte value;
				std::cin >> value;
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
