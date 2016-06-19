#include "iris32.h"
#include <functional>

namespace iris32 {
	void Core::toggleDebug() {
		debug = !debug;
	}
	bool Core::debugEnabled() {
		return debug;
	}
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

	void Core::write(word addr, word value) {
		if (addr >= 0 && addr < memorySize) {
			memory[addr] = value;
		} else {
			std::cerr << "write: Address " << std::hex << addr << " is out of range of" << std::hex << memorySize << std::endl;
			execute = false;
			throw 0;
		}
	}
	word Core::read(word address) {
		if (address >= 0 && address < memorySize) {
			return memory[address];
		} else {
			std::cerr << "read: Address " << std::hex << address << " is out of range of " << std::hex << memorySize << std::endl;
			execute = false;
			throw 0;
		}
	}

	Core::Core(word msize, std::initializer_list<ExecState*> execs) :
		memorySize(msize),
		memory(new word[msize]),
		threads(execs)
	{ }
	Core::~Core() {
		delete [] memory;
		memory = 0;
	}
	void Core::installprogram(std::istream& stream) {
		// read directly from the assembler's output
		char a[sizeof(word)] = { 0 };
		char b[sizeof(word)] = { 0 };
		while(stream.good()) {
			stream.read(a, sizeof(word));
			if (!stream.good()) {
				if (stream.gcount() > 0) {
					std::cerr << "panic: provided data is not valid iris32 encoded assembler" << std::endl;
					exit(1);
				} else {
					break;
				}
			}
			auto addr = word(a[0]);
			addr = (addr & 0xFFFF00FF) | (word(a[1]) << 8);
			addr = (addr & 0xFF00FFFF) | (word(a[2]) << 16);
			addr = (addr & 0x00FFFFFF) | (word(a[3]) << 24);
			stream.read(b, sizeof(word));
			if (stream.gcount() != sizeof(word)) {
				std::cerr << "panic: provided data is not valid iris32 encoded assembler" << std::endl;
				exit(1);
			}
			auto data = word(b[0]);
			data = (data & 0xFFFF00FF) | (word(b[1]) << 8);
			data = (data & 0xFF00FFFF) | (word(b[2]) << 16);
			data = (data & 0x00FFFFFF) | (word(b[3]) << 24);
			if (debug) {
				std::cerr << "install 0x" << std::hex << data 
						  << " @ 0x" << std::hex << addr << std::endl;
			}
			memory[addr] = data;
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
	void Core::dispatch() {
		// read a byte from the current instruction pointer address
		auto result = read(thread->gpr[ArchitectureConstants::InstructionPointerIndex]);
		if (debug) {
			std::cerr << "ip: 0x" << std::hex << thread->gpr[ArchitectureConstants::InstructionPointerIndex] 
				      << ", instruction raw: 0x" << std::hex << result << std::endl;
		}
		auto decoded = DecodedInstruction(result);
		if (debug) { 
			std::cerr << "destination register index: r" << std::dec << (int)  decoded.getDestination() 
				<< ", value: " << (thread->gpr[decoded.getDestination()]) << "\n" 
				<< "source0 register index: r" <<  std::dec << (int)  decoded.getSource0()  
				<< ", value: " << (thread->gpr[decoded.getSource0()]) << "\n" 
				<< "source 1 register index: r" <<  std::dec << (int)  decoded.getSource1() 
				<< ", value: " << (thread->gpr[decoded.getSource1()]) << std::endl; 
		} 
		switch (static_cast<InstructionGroup>(decoded.getGroup())) {
#define X(en, fn) \
			case InstructionGroup:: en : \
				if (debug) std::cerr << "Calling " << "InstructionGroup::" << #en << std::endl; \
				 fn (decoded) ; \
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
		while (execute) {
			for (auto &cthread : threads) {
				if (!execute) {
					return;
				} else {
					thread = cthread;
					execBody();
				}
			}
		}
	}
	void Core::execBody() {
		if (debug) {
			std::cerr << "current thread " << std::hex << thread << std::endl;
			std::cerr << "{" << std::endl;
		}
		if (!thread->advanceIp) {
			thread->advanceIp = true;
		}
		dispatch();
		if (thread->advanceIp) {
			++thread->gpr[ArchitectureConstants::InstructionPointerIndex];
			if (thread->gpr[ArchitectureConstants::InstructionPointerIndex] >= memorySize) {
				thread->gpr[ArchitectureConstants::InstructionPointerIndex] = 0;
			}
		}
		if (debug) {
			std::cerr << "}" << std::endl;
		}
	}
	void Core::compare( DecodedInstruction& current) {
		switch(static_cast<CompareOp>(current.getOperation())) {
#define OpNone =
#define OpAnd &=
#define OpOr |=
#define OpXor ^=
#define X(type, compare, mod) \
			case CompareOp:: type: \
								   if (debug) std::cerr << "Called CompareOp::" << #type << std::endl; \
								   thread->gpr[current.getDestination()] INDIRECTOR(Op, mod) (thread->gpr[current.getSource0()] compare thread->gpr[current.getSource1()]); \
			break;
#define Y(type, compare, mod) \
			case CompareOp:: type: \
								   if (debug) std::cerr << "Called CompareOp::" << #type << std::endl; \
								   thread->gpr[current.getDestination()] INDIRECTOR(Op, mod) (thread->gpr[current.getSource0()] compare (word(current.getSource1()))); \
			break;

#include "iris32_compare.def"
#undef X
#undef Y
#undef OpNone
#undef OpAnd
#undef OpOr
#undef OrXor
			default:
				std::cerr << "Illegal compare code " << std::hex << current.getOperation() << std::endl;
				execute = false;
				thread->advanceIp = false;
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

	void Core::arithmetic( DecodedInstruction& inst) {
		switch(static_cast<ArithmeticOp>(inst.getOperation())) {
#define XNone(n) thread->gpr[inst.getDestination()] = arithmeticOp<ArithmeticOp:: n>( thread->gpr[inst.getSource0()], thread->gpr[inst.getSource1()]);
#define XImmediate(n) thread->gpr[inst.getDestination()] = arithmeticOp<ArithmeticOp:: n>(thread->gpr[inst.getSource0()], static_cast<word>(inst.getSource1()));
#define XUnary(n) thread->gpr[inst.getDestination()] = arithmeticOp<ArithmeticOp:: n>(thread->gpr[inst.getSource0()], 0);
#define XDenominator(n) \
			if (thread->gpr[inst.getSource1()] == 0) { \
				if (debug) std::cerr << "denominator in for operation " << std::hex <<  #n << " is zero!" << std::endl; \
				execute = false; \
			} else { \
				XNone(n) \
			}
#define XDenominatorImmediate(n) \
			if (thread->gpr[inst.getSource1()] == 0) { \
				if (debug) std::cerr << "denominator in for operation " << std::hex << #n << " is zero!" << std::endl; \
				execute = false; \
			} else { \
				XImmediate(n) \
			}
#define X(name, op, desc) \
			case ArithmeticOp:: name: \
						{ \
							if (debug) std::cerr << "Called ArithmeticOp::" << #name << std::endl; \
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
				std::cerr << "Illegal arithmetic operation " << std::hex << inst.getOperation() << std::endl;
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

	void Core::jump( DecodedInstruction& inst) {
		word newAddr = 0;
		bool cond = true;
		thread->advanceIp = false;
		word ip = thread->gpr[ArchitectureConstants::InstructionPointerIndex];
		switch(static_cast<JumpOp>(inst.getOperation())) {
#define XImmediateCond_true (inst.getImmediate())
#define XImmediateCond_false \
			(thread->gpr[inst.getSource0()])
#define XIfThenElse_false(immediate) \
			newAddr = cond ? INDIRECTOR(XImmediateCond, _ ## immediate) : ip + 1;
#define XIfThenElse_true(immediate) \
			newAddr = thread->gpr[cond ? inst.getSource0() : inst.getSource1()];
#define XImmediateUncond_false (thread->gpr[inst.getDestination()])
#define XImmediateUncond_true (inst.getImmediate())
#define XConditional_false(name, ifthenelse, immediate) \
			newAddr = INDIRECTOR(XImmediateUncond, _ ## immediate);
#define XConditional_true(name, ifthenelse, immediate) \
			cond = jumpCond<JumpOp:: name> (thread->gpr[inst.getDestination()]); \
			INDIRECTOR(XIfThenElse, _ ## ifthenelse)(immediate)
#define XLink_true \
			if (cond) { \
				thread->gpr[ArchitectureConstants::LinkRegisterIndex] = ip + 1; \
				if (debug) { \
					std::cerr << "update link register index to "  << std::hex << thread->gpr[ArchitectureConstants::LinkRegisterIndex] << std::endl; \
				} \
			}
#define XLink_false

#define X(name, ifthenelse, conditional, iffalse, immediate, link) \
			case JumpOp:: name: \
					 { \
						 if (debug) { \
							 std::cerr << "Called JumpOp::" << #name << std::endl; \
						 } \
						 INDIRECTOR(XConditional, _ ## conditional)(name, ifthenelse, immediate) \
						 thread->gpr[ArchitectureConstants::InstructionPointerIndex] = newAddr; \
						 INDIRECTOR(XLink, _ ## link)  \
						 break; \
					 }
#include "iris32_jump.def"
#undef X
			default:
				std::cerr << "Illegal jump code " << std::hex << inst.getOperation() << std::endl;
				execute = false;
				break;
		}
	}
	void Core::misc( DecodedInstruction& inst) {
		switch(static_cast<MiscOp>(inst.getOperation())) {
#define X(name, func) \
			case MiscOp:: name: \
			func (inst); \
			break;
#include "iris32_misc.def"
#undef X
			default:
				std::cerr << "Illegal misc code " << std::hex << inst.getOperation() << std::endl;
				execute = false;
				thread->advanceIp = false;
				break;
		}
	}
	void Core::systemCall(DecodedInstruction& inst) {
		switch(static_cast<SystemCalls>(inst.getDestination())) {
			case SystemCalls::Terminate:
				execute = false;
				thread->advanceIp = false;
				break;
			case SystemCalls::PutC:
				// read register 0 and register 1
				std::cout << (char)thread->gpr[inst.getSource0()];
				//std::cout.put((char)thread->gpr[inst.getSource0()]);
				break;
			case SystemCalls::GetC:
				byte value;
				std::cin >> std::noskipws >> value;
				thread->gpr[inst.getSource0()] = (word)value;
				break;
			default:
				std::cerr << "Illegal system call " << std::hex << inst.getDestination() << std::endl;
				execute = false;
				thread->advanceIp = false;
				break;
		}
	}
	void Core::move( DecodedInstruction& inst) {
		word a = 0;
		switch(static_cast<MoveOp>(inst.getOperation())) {
#define GPRRegister0 (thread->gpr[inst.getDestination()])
#define GPRRegister1 (thread->gpr[inst.getSource0()])
#define GPRRegister2 (thread->gpr[inst.getSource1()])
#define GPRImmediate1 (inst.getImmediate())
#define DataRegister0 GPRRegister0
#define DataRegister1 GPRRegister1
#define DataImmediate1 GPRImmediate1
#define StackPushRegister0 (thread->gpr[ArchitectureConstants::StackPointerIndex])
#define StackPushRegister1 GPRRegister0
#define StackPushImmediate1 GPRImmediate1
#define StackPopRegister0 GPRRegister0
#define StackPopRegister1 (thread->gpr[ArchitectureConstants::StackPointerIndex])
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
			auto value = INDIRECTOR(type, dest ## 0); \
			value &= 0x0000FFFF; \
			value |= (word(INDIRECTOR(type, src ## 1)) << 16); \
			INDIRECTOR(type, dest ## 0) = value;

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
						 if (debug) std::cerr << "Called CompareOp::" << #name << std::endl;  \
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
				std::cerr << "Illegal move code " << std::hex << inst.getOperation() << std::endl;
				execute = false;
				thread->advanceIp = false;
				break;
		}
	}
} // end namespace iris32
