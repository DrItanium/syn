#include "iris32.h"
#include "sim_registration.h"
#include <functional>

namespace iris {
	template<>
	Core* getCore<Architecture::iris32>() {
		iris32::ExecState t0, t1, t2, t3, t4, t5, t6, t7;
		// make sure they all start at the same position
		t0.gpr[iris32::ArchitectureConstants::ThreadIndex] = 0;
		t1.gpr[iris32::ArchitectureConstants::ThreadIndex] = 1;
		t2.gpr[iris32::ArchitectureConstants::ThreadIndex] = 2;
		t3.gpr[iris32::ArchitectureConstants::ThreadIndex] = 3;
		t4.gpr[iris32::ArchitectureConstants::ThreadIndex] = 4;
		t5.gpr[iris32::ArchitectureConstants::ThreadIndex] = 5;
		t6.gpr[iris32::ArchitectureConstants::ThreadIndex] = 6;
		t7.gpr[iris32::ArchitectureConstants::ThreadIndex] = 7;
		return new iris32::Core(iris32::ArchitectureConstants::AddressMax, 8);
	}
}
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

	Core::Core(word msize, byte numThreads) :
		memorySize(msize),
		memory(new word[msize]),
		threads(numThreads)
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
	template<typename T, T value>
		void invoke(Core* c, DecodedInstruction&& inst) {
			throw "unimplemented invocation";
		}

// jump operations
	template<JumpOp op>
		struct ConditionalStyle {
			static const bool isFalseForm = false;
		};

#define XImmediateCond_true (inst.getImmediate())
#define XImmediateCond_false (thread->gpr[inst.getSource0()])
#define XIfThenElse_false(immediate) \
			newAddr = cond ? INDIRECTOR(XImmediateCond, _ ## immediate) : ip + 1;
#define XIfThenElse_true(immediate) \
			newAddr = thread->gpr[cond ? inst.getSource0() : inst.getSource1()];
#define XImmediateUncond_false (thread->gpr[inst.getDestination()])
#define XImmediateUncond_true (inst.getImmediate())
#define XConditional_false(name, ifthenelse, immediate) \
			newAddr = INDIRECTOR(XImmediateUncond, _ ## immediate);
#define XConditional_true(name, ifthenelse, immediate) \
            auto dest = thread->gpr[inst.getDestination()]; \
            cond = ConditionalStyle< JumpOp :: name >::isFalseForm ? (dest == 0) : (dest != 0); \
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
            template<> \
            struct ConditionalStyle< JumpOp :: name > { static const bool isFalseForm = iffalse ; } ; \
			template<> \
			void invoke<JumpOp, JumpOp :: name>(Core* core, DecodedInstruction&& inst) { \
				auto thread = core->thread; \
				word newAddr = 0; \
				bool cond = true; \
				thread->advanceIp = false; \
				bool debug = core->debug; \
				word ip = thread->gpr[ArchitectureConstants::InstructionPointerIndex]; \
				if (debug) { \
					std::cerr << "Called JumpOp::" << #name << std::endl; \
				} \
				INDIRECTOR(XConditional, _ ## conditional)(name, ifthenelse, immediate) \
				thread->gpr[ArchitectureConstants::InstructionPointerIndex] = newAddr; \
				if (debug) { \
					std::cerr << "newAddr = " << std::hex << newAddr << std::endl; \
				} \
				INDIRECTOR(XLink, _ ## link)  \
			}
#include "iris32_jump.def"
#undef X
// move operations
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
	auto value = INDIRECTOR(type, dest ## 0); \
	value &= 0xFFFF0000; \
	value |= (word(INDIRECTOR(type, src ## 1))); \
	INDIRECTOR(type, dest ## 0) = value;
#define XSetUpper(type, dest, src) \
	auto value = INDIRECTOR(type, dest ## 0); \
	value &= 0x0000FFFF; \
	value |= (word(INDIRECTOR(type, src ## 1)) << 16); \
	INDIRECTOR(type, dest ## 0) = value;

#define XSwap(type, dest, src) \
	auto a = INDIRECTOR(type, dest ##  0); \
	INDIRECTOR(type, dest ## 0) = INDIRECTOR(type, src ## 1); \
	INDIRECTOR(type, src ##  1) = a;
#define XLoad(type, dest, src) \
	INDIRECTOR(type, dest ## 0) = core->read(INDIRECTOR(type, src ## 1));
#define XPop(type, dest, src) \
	INDIRECTOR(type, Pop ## dest ## 0) = core->read(INDIRECTOR(type, Pop ## src ## 1)); \
	++INDIRECTOR(type, Pop ## src ## 1);
#define XPush(type, dest, src) \
	--INDIRECTOR(type, Push ## dest ## 0); \
	core->write(INDIRECTOR(type, Push ## dest ## 0), INDIRECTOR(type, Push ## src ## 1));
#define XStore(type, dest, src) \
	core->write(INDIRECTOR(type, dest ## 0), INDIRECTOR(type, src ## 1));
#define X(name, type, target, dest, src) \
	template<> \
	void invoke<MoveOp, MoveOp :: name>(Core* core, DecodedInstruction&& inst) { \
		auto thread = core->thread; \
		if (core->debug) {\
			std::cerr << "Called MoveOp::" << #name << std::endl;  \
		}\
		INDIRECTOR(X,type)(target, dest, src) \
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

#define OpNone =
#define OpAnd &=
#define OpOr |=
#define OpXor ^=
#define X(type, compare, mod) \
	template<> \
	void invoke<CompareOp, CompareOp:: type>(Core* core, DecodedInstruction&& current) { \
		auto thread = core->thread; \
		if (core->debug) { \
			std::cerr << "Called CompareOp::" << #type << std::endl; \
		} \
		thread->gpr[current.getDestination()] INDIRECTOR(Op, mod) (thread->gpr[current.getSource0()] compare thread->gpr[current.getSource1()]); \
	}
#define Y(type, compare, mod) \
	template<> \
	void invoke<CompareOp, CompareOp:: type> (Core* core, DecodedInstruction&& current) { \
		auto thread = core->thread; \
		if (core->debug) { \
			std::cerr << "Called CompareOp::" << #type << std::endl; \
		} \
		thread->gpr[current.getDestination()] INDIRECTOR(Op, mod) (thread->gpr[current.getSource0()] compare (word(current.getSource1()))); \
	}

#include "iris32_compare.def"
#undef X
#undef Y
#undef OpNone
#undef OpAnd
#undef OpOr
#undef OrXor

#define XNone(n, op) thread->gpr[inst.getDestination()] = thread->gpr[inst.getSource0()] op thread->gpr[inst.getSource1()];
#define XImmediate(n, op) thread->gpr[inst.getDestination()] = thread->gpr[inst.getSource0()] op  static_cast<word>(inst.getSource1());
#define XUnary(n, op) thread->gpr[inst.getDestination()] = op thread->gpr[inst.getSource0()];
#define XDenominator(n, op) \
			if (thread->gpr[inst.getSource1()] == 0) { \
				if (debug) { \
					std::cerr << "denominator in for operation " << std::hex <<  #n << " is zero!" << std::endl; \
				} \
				core->execute = false; \
			} else { \
				XNone(n, op) \
			}
#define XDenominatorImmediate(n, op) \
			if (thread->gpr[inst.getSource1()] == 0) { \
				if (debug) { \
					std::cerr << "denominator in for operation " << std::hex << #n << " is zero!" << std::endl; \
				} \
				core->execute = false; \
			} else { \
				XImmediate(n, op) \
			}
#define X(name, op, desc) \
	template<> \
	void invoke<ArithmeticOp, ArithmeticOp :: name >(Core* core, DecodedInstruction&& inst) { \
		auto thread = core->thread; \
		auto debug = core->debug; \
		if (debug) { \
			std::cerr << "Called ArithmeticOp::" << #name << std::endl; \
		}\
		INDIRECTOR(X, desc)(name, op) \
	}
#include "iris32_arithmetic.def"
#undef X
#undef XNone
#undef XDenominator
#undef XUnary
#undef XImmediate
#undef XDenominatorImmediate

	template<>
	void invoke<MiscOp, MiscOp::SystemCall>(Core* core, DecodedInstruction&& inst) {
		auto thread = core->thread;
		switch(static_cast<SystemCalls>(inst.getDestination())) {
			case SystemCalls::Terminate:
				core->execute = false;
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
				core->execute = false;
				thread->advanceIp = false;
				break;
		}
	}


	void Core::dispatch() {
		// read a byte from the current instruction pointer address
		auto decoded = DecodedInstruction(read(thread->gpr[ArchitectureConstants::InstructionPointerIndex]));
		auto printInst = [this](const std::string& msg, DecodedInstruction&& decoded) {
			std::cerr << msg << "\n"
				<< "\tdestination register index: r" << std::dec << (int)  decoded.getDestination()
				<< ", value: " << (thread->gpr[decoded.getDestination()]) << "\n"
				<< "\tsource0 register index: r" <<  std::dec << (int)  decoded.getSource0()
				<< ", value: " << (thread->gpr[decoded.getSource0()]) << "\n"
				<< "\tsource 1 register index: r" <<  std::dec << (int)  decoded.getSource1()
				<< ", value: " << (thread->gpr[decoded.getSource1()]) << std::endl;
			std::cerr << "ip: 0x" << std::hex << thread->gpr[ArchitectureConstants::InstructionPointerIndex]
				<< ", instruction raw: 0x" << std::hex << decoded.getRawValue() << std::endl;
			std::cerr << "group: " << std::hex << (int)decoded.getGroup() << std::endl;
			std::cerr << "op: " << std::hex << (int)decoded.getOperation() << std::endl;
		};
		if (debug) {
			printInst("before", std::move(decoded));
		}
#define DispatchDecode(cl, val) \
	case Decode ## cl ## val :: value :  \
	     invoke < GroupToOp < \
	DecodeByteToInstructionGroup < Decode ## cl ## val :: group > :: value > :: OpKind, \
	OpInfer < Decode ## cl ## val :: op , DecodeByteToInstructionGroup < Decode ## cl ## val :: group > :: value > :: value >(this, std::move(decoded));\
	break;
		switch (decoded.getControl()) {
#define X(title, operation, unused) DispatchDecode(CompareOp, title)
#define Y(title, operation, unused) X(title, operation, unused)
#include "iris32_compare.def"
#undef X
#undef Y
#define X(title, operation, type) DispatchDecode(ArithmeticOp, title)
#include "iris32_arithmetic.def"
#undef X
#define X(title, operation, __, ___, ____) DispatchDecode(MoveOp, title)
#include "iris32_move.def"
#undef X
#define X(title, u0, u1, u2, u3, u4) DispatchDecode(JumpOp, title)
#include "iris32_jump.def"
#undef X
#define X(title, __) DispatchDecode(MiscOp, title)
#include "iris32_misc.def"
#undef X
			default:
				throw "Undefined control!";
		}
#undef DispatchDecode
		if (debug) {
			printInst("after", std::move(decoded));
		}
	}
	void Core::initialize() {
		int threadIndex = 0;
		for (auto &cthread : threads) {
			cthread.gpr[iris32::ArchitectureConstants::ThreadIndex] = threadIndex;
			++threadIndex;
		}
	}
	void Core::shutdown() {

	}
	void Core::run() {
		while (execute) {
			for (auto &cthread : threads) {
				if (!execute) {
					return;
				} else {
					thread = &cthread;
					execBody();
				}
			}
		}
	}
	void Core::execBody() {
		if (debug) {
			std::cerr << "{" << std::endl;
			std::cerr << "current thread " << std::hex << thread << std::endl;
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
} // end namespace iris32
