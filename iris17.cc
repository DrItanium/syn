#include "iris17.h"
#include <functional>
#include <sstream>
#include "Problem.h"

namespace iris17 {
/*
 * Iris17 is a variable length encoding 16 bit architecture.
 * It has a 24 bit memory space across 256 16-bit sections. The variable length
 * encoding comes from different register choices. The reserved registers are
 * used to compress the encoding.
 */
	Core* newCore() {
		return new Core();
	}
	word encodeWord(byte a, byte b) {
		return iris::encodeBits<word, byte, 0xFF00, 8>(iris::encodeBits<word, byte, 0x00FF>(word(0), a), b);
	}

	DecodedInstruction::DecodedInstruction() { }

	void DecodedInstruction::decode(raw_instruction input) {
#define X(title, mask, shift, type, is_register, post) \
		_ ## post = iris::decodeBits<raw_instruction, type, mask, shift>(input);
#include "iris17_instruction.def"
#undef X
	}

	Core::Core() : memory(new word[ArchitectureConstants::AddressMax]) {

	}

	void Core::initialize() { }

	void Core::shutdown() { }

	template<int count>
	void populateContents(word* contents, std::istream& stream) {
		char buf[sizeof(word)] = { 0 };
		for(int i = 0; i < count; ++i) {
			stream.read(buf, sizeof(word));
			contents[i] = iris17::encodeWord(buf[0], buf[1]);
		}
	}
	void Core::installprogram(std::istream& stream) {
		populateContents<ArchitectureConstants::RegisterCount>(gpr, stream);
		for (int i = 0 ; i < ArchitectureConstants::SegmentCount; ++i) {
			populateContents<ArchitectureConstants::AddressMax>(memory[i], stream);
		}
	}

	template<int count>
	void dumpContents(word* contents, std::ostream& stream) {
		char buf[sizeof(word)] = { 0 };
		for(int i = 0; i < count; ++i) {
			buf[0] = static_cast<char>(contents[i]);
			buf[1] = static_cast<char>(contents[i] >> 8);
			stream.write(buf, sizeof(word));
		}
	}
	void Core::dump(std::ostream& stream) {
		// save the registers
		dumpContents<ArchitectureConstants::RegisterCount>(gpr, stream);
		for (int i = 0 ; i < ArchitectureConstants::SegmentCount; ++i) {
			dumpContents<ArchitectureConstants::AddressMax>(memory[i], stream);
		}
	}
	void Core::run() {
		while(execute) {
			if (!advanceIp) {
				advanceIp = true;
			}
			auto lower = getCurrentCodeWord();
			++getInstructionPointer();
			auto upper = getCurrentCodeWord();
			
			current.decode(raw_instruction(upper << 16) | raw_instruction(lower));
			dispatch();
			if (advanceIp) {
				++getInstructionPointer();
			}
		}
	}
#define OpNone =
#define OpAnd &=
#define OpOr |=
#define OpXor ^=
#define DefCompareOp(type, compare, mod, action) \
	template<> \
	void Core::op<InstructionGroup::Compare, GetAssociatedOp<InstructionGroup::Compare>::Association, GetAssociatedOp<InstructionGroup::Compare>::Association:: type>() { \
		++getInstructionPointer(); \
		auto rest = getCurrentCodeWord(); \
		DecodedInstruction second;\
		second.decode(rest); \
		getConditionRegister() INDIRECTOR(Op, mod) (registerValue(current.getEmbeddedArg()) compare action ); \
	}
#define X(op, compare, mod) DefCompareOp(op, compare, mod, (second.getSpecificArg0()))
#define Y(op, compare, mod) DefCompareOp(op, compare, mod, (rest))
#include "iris17_compare.def"
#undef X
#undef Y
#undef DefCompareOp
#undef OpNone
#undef OpAnd
#undef OpOr
#undef OrXor

#define DefMoveOp(title) \
	template<> \
	void Core::op<InstructionGroup::Move, GetAssociatedOp<InstructionGroup::Move>::Association, GetAssociatedOp<InstructionGroup::Move>::Association:: title>()

	DefMoveOp(Move)  {
		registerValue(current.getArg0()) = registerValue(current.getArg1());
	}

	DefMoveOp(Swap) {
        word a = registerValue(current.getArg0());
        registerValue(current.getArg0()) = registerValue(current.getArg1());
        registerValue(current.getArg1()) = a;
	}

    DefMoveOp(Set) {
        ++getInstructionPointer();
        registerValue(current.getArg0()) = getCurrentCodeWord();
    }


	DefMoveOp(Load) {
		// use arg0 to denote which data segment to use
		getValueRegister() = getSegment(registerValue(current.getArg0()))[getAddressRegister()];
	}

	DefMoveOp(Store) {
		getDataSegment()[getAddressRegister()] = getValueRegister();
	}

	DefMoveOp(Push) {
		word& stackPointer = getStackPointer();
		++stackPointer;
		getStackSegment()[stackPointer] = registerValue(current.getEmbeddedArg());
	}

	DefMoveOp(Pop) {
		registerValue(current.getEmbeddedArg()) = getStackSegment()[getStackPointer()];
		--getStackPointer();
	}

#define XNone(n, op) registerValue(current.getEmbeddedArg()) = ( src0 op  registerValue(next.getSpecificArg1()));
#define XImmediate(n, op) registerValue(current.getEmbeddedArg()) = (src0 op static_cast<word>(next.getSpecificArg1()));
#define XUnary(n, op) registerValue(current.getEmbeddedArg()) = (op src0);
#define XDenominator(n, op) \
			if (registerValue(next.getSpecificArg1()) == 0) { \
				throw iris::Problem("denominator for operation " #n " is zero!"); \
				execute = false; \
			} else { \
				XNone(n, op) \
			}
#define XDenominatorImmediate(n, op) \
			if (registerValue(next.getSpecificArg1()) == 0) { \
				execute = false; \
				throw iris::Problem("denominator for operation " #n " is zero!"); \
			} else { \
				XImmediate(n, op) \
			}
#define X(name, title, desc) \
	template<> \
	void Core::op<InstructionGroup::Arithmetic, GetAssociatedOp<InstructionGroup::Arithmetic>::Association, GetAssociatedOp<InstructionGroup::Arithmetic>::Association:: name>() { \
		++getInstructionPointer(); \
		DecodedInstruction next; \
		next.decode(getCurrentCodeWord()); \
		auto src0 = registerValue(next.getSpecificArg0()); \
		INDIRECTOR(X, desc)(name, title) \
	}
#define Y(name) 
#include "iris17_arithmetic.def"
#undef Y
#undef X
#undef XNone
#undef XDenominator
#undef XUnary
#undef XImmediate
#undef XDenominatorImmediate

#define DefArithmeticOp(name) \
	template<> \
	void Core::op<InstructionGroup::Arithmetic, GetAssociatedOp<InstructionGroup::Arithmetic>::Association, GetAssociatedOp<InstructionGroup::Arithmetic>::Association:: name>()

DefArithmeticOp(Increment) {
	registerValue(current.getEmbeddedArg()) = registerValue(current.getEmbeddedArg()) + 1;
}

DefArithmeticOp(Decrement) {
	registerValue(current.getEmbeddedArg()) = registerValue(current.getEmbeddedArg()) - 1;
}

#define DefJumpOp(title) \
	template<> \
	void Core::op<InstructionGroup::Jump, GetAssociatedOp<InstructionGroup::Jump>::Association, GetAssociatedOp<InstructionGroup::Jump>::Association:: title>()

DefJumpOp(Branch) {
	advanceIp = false;
	++getInstructionPointer();
	// need to read the next "instruction"
	getInstructionPointer() = getCurrentCodeWord();
}

DefJumpOp(Call) {
	advanceIp = false;
    // read the next word
	//
	++getInstructionPointer();
	word ip = getInstructionPointer();
	getInstructionPointer() = getCurrentCodeWord();
	getLinkRegister() = ip + 1;
}

DefJumpOp(IndirectBranch) {
    advanceIp = false;
	getInstructionPointer() = registerValue(current.getEmbeddedArg());
}

DefJumpOp(IndirectCall) {
    advanceIp = false;
	word ip = getInstructionPointer() + 1;
	getInstructionPointer() = registerValue(current.getEmbeddedArg());
	getLinkRegister() = ip;
}

DefJumpOp(ConditionalBranch) {
	advanceIp = false;
	if (getConditionRegister() != 0) {
		++getInstructionPointer();
		getInstructionPointer() = getCurrentCodeWord();
	} else {
		getInstructionPointer() += 2;
	}
}

DefJumpOp(ConditionalIndirectBranch) {
	advanceIp = false;
	if (getConditionRegister() != 0) {
		getInstructionPointer() = registerValue(current.getEmbeddedArg());
	} else {
		++getInstructionPointer();
	}
}

DefJumpOp(IfThenElse) {
	advanceIp = false;
	++getInstructionPointer();
	DecodedInstruction next;
	next.decode(getCurrentCodeWord());
	getInstructionPointer() = registerValue(((registerValue(current.getEmbeddedArg()) != 0) ? next.getSpecificArg0() : next.getSpecificArg1()));
}

DefJumpOp(IfThenElseLink) {
	advanceIp = false;
	word ip = getInstructionPointer() + 2;
	++getInstructionPointer();
	DecodedInstruction next;
	next.decode(getCurrentCodeWord());
	getInstructionPointer() = registerValue(((registerValue(current.getEmbeddedArg()) != 0) ? next.getSpecificArg0() : next.getSpecificArg1()));
	getLinkRegister() = ip;
}

	template<>
	void Core::op<InstructionGroup::Misc, GetAssociatedOp<InstructionGroup::Misc>::Association, GetAssociatedOp<InstructionGroup::Misc>::Association::SystemCall>() {
		++getInstructionPointer();
		DecodedInstruction next;
		next.decode(getCurrentCodeWord());
		switch(static_cast<SystemCalls>(current.getEmbeddedImmediate())) {
			case SystemCalls::Terminate:
				execute = false;
				advanceIp = false;
				break;
			case SystemCalls::PutC:
				// read register 0 and register 1
				std::cout.put(static_cast<char>(registerValue(next.getSpecificArg0())));
				break;
			case SystemCalls::GetC:
				byte value;
				std::cin >> std::noskipws >> value;
				registerValue(next.getSpecificArg1()) = static_cast<word>(value);
				break;
			default:
				std::stringstream ss;
				ss << "Illegal system call " << current.getEmbeddedImmediate();
				execute = false;
				advanceIp = false;
				throw iris::Problem(ss.str());
		}
	}

	void Core::dispatch() {
		switch(current.getControl()) {
#define X(type, compare, mod) \
			case ControlSignature<InstructionGroup::Compare, GetAssociatedOp<InstructionGroup::Compare>::Association, GetAssociatedOp<InstructionGroup::Compare>::Association:: type>::fullSignature: \
			op<InstructionGroup::Compare, GetAssociatedOp<InstructionGroup::Compare>::Association, GetAssociatedOp<InstructionGroup::Compare>::Association:: type>(); \
			break;

#define Y(type, compare, mod) X(type, compare, mod)
#include "iris17_compare.def"
#undef Y
#undef X

#define Y(name) \
			case ControlSignature<InstructionGroup::Arithmetic, GetAssociatedOp<InstructionGroup::Arithmetic>::Association, GetAssociatedOp<InstructionGroup::Arithmetic>::Association:: name>::fullSignature: \
			op<InstructionGroup::Arithmetic, GetAssociatedOp<InstructionGroup::Arithmetic>::Association, GetAssociatedOp<InstructionGroup::Arithmetic>::Association:: name>(); \
			break;
#define X(name, __, ____) Y(name)
#include "iris17_arithmetic.def"
#undef X

#define X(title) \
			case ControlSignature<InstructionGroup::Jump, GetAssociatedOp<InstructionGroup::Jump>::Association, GetAssociatedOp<InstructionGroup::Jump>::Association:: title>::fullSignature: \
			op<InstructionGroup::Jump, GetAssociatedOp<InstructionGroup::Jump>::Association, GetAssociatedOp<InstructionGroup::Jump>::Association:: title>(); \
			break;
#include "iris17_jump.def"
#undef X
#define X(title, func) \
			case ControlSignature<InstructionGroup::Misc, GetAssociatedOp<InstructionGroup::Misc>::Association, GetAssociatedOp<InstructionGroup::Misc>::Association:: title>::fullSignature: \
			op<InstructionGroup::Misc, GetAssociatedOp<InstructionGroup::Misc>::Association, GetAssociatedOp<InstructionGroup::Misc>::Association:: title>(); \
			break;
#include "iris17_misc.def"
#undef X

#define X(name) \
			case ControlSignature<InstructionGroup::Move, GetAssociatedOp<InstructionGroup::Move>::Association, GetAssociatedOp<InstructionGroup::Move>::Association:: name>::fullSignature: op<InstructionGroup::Move, GetAssociatedOp<InstructionGroup::Move>::Association, GetAssociatedOp<InstructionGroup::Move>::Association:: name>(); break;
#include "iris17_move.def"
#undef X

			default:
				std::stringstream str;
				str << "Illegal instruction " << current.getControl();
				execute = false;
				throw iris::Problem(str.str());
		}
	}


	enum class Segment  {
		Code,
		Data,
		Count,
	};

	void Core::link(std::istream& input) {
		dword result = 0;
		word result0 = 0;
		char buf[8] = {0};
		for(int lineNumber = 0; input.good(); ++lineNumber) {
			input.read(buf, 8);
			if (input.gcount() < 8 && input.gcount() > 0) {
				throw iris::Problem("unaligned object file found!");
			} else if (input.gcount() == 0) {
				if (input.eof()) {
					break;
				} else {
					throw iris::Problem("something bad happened while reading input file!");
				}
			}
			//ignore the first byte, it is always zero
			byte tmp = buf[1];
			Segment target = static_cast<Segment>(buf[1]);
			word address = iris17::encodeWord(buf[2], buf[3]);
			if (debugEnabled()) {
				std::cerr << "current target = " << static_cast<int>(target) << "\tcurrent address = 0x" << std::hex << address << std::endl;
			}
			switch(target) {
				case Segment::Code:
					result = iris17::encodeWord(buf[4], buf[5]);
					if (debugEnabled()) {
						std::cerr << " code result: 0x" << std::hex << result << std::endl;
					}
					//setInstructionMemory(address, result);
					break;
				case Segment::Data:
					result0 = iris17::encodeWord(buf[4], buf[5]);
					if (debugEnabled()) {
						std::cerr << " data result: 0x" << std::hex << result0 << std::endl;
					}
					//setDataMemory(address, result0);
					break;
				default:
					std::stringstream str;
					str << "error: line " << lineNumber << ", unknown segment " << static_cast<int>(target) << "/" << static_cast<int>(tmp) << std::endl;
					str << "current address: " << std::hex << address << std::endl;
					throw iris::Problem(str.str());
			}
		}
	}
	byte Core::getControl() {
		return current.getControl();
	}
	word& Core::registerValue(byte index) {
		return gpr[index];
	}
	word* Core::getSegment(byte segment) {
		return memory[segment];
	}
	word& Core::getInstructionPointer() {
		return registerValue<ArchitectureConstants::InstructionPointer>();
	}
	word& Core::getStackPointer() {
		return registerValue<ArchitectureConstants::StackPointer>();
	}
	word& Core::getConditionRegister() {
		return registerValue<ArchitectureConstants::ConditionRegister>();
	}
	word& Core::getLinkRegister() {
		return registerValue<ArchitectureConstants::LinkRegister>();
	}
	word& Core::getAddressRegister() {
		return registerValue<ArchitectureConstants::AddressRegister>();
	}
	word& Core::getValueRegister() {
		return registerValue<ArchitectureConstants::ValueRegister>();
	}
	word Core::getCurrentCodeWord() {
		return getCodeSegment()[getInstructionPointer()];
	}
	word Core::getTopOfStack() {
		return getStackSegment()[getStackPointer()];
	}
}
