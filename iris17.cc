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

	Core::Core() { }

	void Core::initialize() {

	}

	void Core::shutdown() {

	}
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
			current.decode(memory[registerValue<ArchitectureConstants::CodeSegmentRegister>()][registerValue<ArchitectureConstants::InstructionPointer>()]);
			dispatch();
			if (advanceIp) {
				++registerValue<ArchitectureConstants::InstructionPointer>();
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
		++registerValue<ArchitectureConstants::InstructionPointer>(); \
		auto rest = memory[registerValue<ArchitectureConstants::CodeSegmentRegister>()][registerValue<ArchitectureConstants::InstructionPointer>()]; \
		DecodedInstruction second;\
		second.decode(rest); \
		registerValue<ConditionRegister>() INDIRECTOR(Op, mod) (registerValue(current.getEmbeddedArg()) compare action ); \
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
        ++registerValue<ArchitectureConstants::InstructionPointer>();
		auto instruction = memory[registerValue<ArchitectureConstants::CodeSegmentRegister>()];
        DecodedInstruction next;
        next.decode(instruction[registerValue<ArchitectureConstants::InstructionPointer>()]);
        registerValue(current.getEmbeddedArg()) = registerValue(next.getSpecificArg0());
	}

    DefMoveOp(SetFull) {
		auto instruction = memory[registerValue<ArchitectureConstants::CodeSegmentRegister>()];
        ++registerValue<ArchitectureConstants::InstructionPointer>();
        registerValue(current.getEmbeddedArg()) = instruction[registerValue<ArchitectureConstants::InstructionPointer>()];
    }
	DefMoveOp(SetLower_AddressRegister) {
		registerValue<ArchitectureConstants::AddressRegister>() = iris17::encodeWord(current.getEmbeddedImmediate(), static_cast<byte>(registerValue<ArchitectureConstants::AddressRegister>() >> 8));
	}

	DefMoveOp(SetUpper_AddressRegister) {
		registerValue<ArchitectureConstants::AddressRegister>() = iris17::encodeWord(static_cast<byte>(registerValue<ArchitectureConstants::AddressRegister>()), current.getEmbeddedImmediate());
	}

	DefMoveOp(Swap) {
		auto instruction = memory[registerValue<ArchitectureConstants::CodeSegmentRegister>()];
        ++registerValue<ArchitectureConstants::InstructionPointer>();
        DecodedInstruction next;
        next.decode(instruction[registerValue<ArchitectureConstants::InstructionPointer>()]);
        word a = registerValue(current.getEmbeddedArg());
        registerValue(current.getEmbeddedArg()) = registerValue(next.getSpecificArg0());
        registerValue(next.getSpecificArg0()) = a;
	}

	DefMoveOp(Load) {
		registerValue<ArchitectureConstants::DataRegister>() = memory[registerValue<ArchitectureConstants::DataSegmentRegister>()][registerValue<ArchitectureConstants::AddressRegister>()];
	}

	DefMoveOp(Push) {
		word& stackPointer = registerValue<ArchitectureConstants::StackPointer>();
		++stackPointer;
		auto stackSeg = registerValue<ArchitectureConstants::StackSegmentRegister>();
		memory[stackSeg][stackPointer] = registerValue(current.getEmbeddedArg());
	}

	DefMoveOp(Pop) {
		getArg0Register() = memory[registerValue<ArchitectureConstants::StackPointer>()][registerValue<ArchitectureConstants::StackPointer>()];
		--registerValue<ArchitectureConstants::StackPointer>();
	}

#define XNone(n, op) getArg0Register() = ( getArg0Register() op  getArg1Register());
#define XImmediate(n, op) getArg0Register() = (getArg0Register() op static_cast<word>(current.getArg1()));
#define XUnary(n, op) getArg0Register() = (op getArg0Register());
#define XDenominator(n, op) \
			if (getArg1Register() == 0) { \
				throw iris::Problem("denominator for operation " #n " is zero!"); \
				execute = false; \
			} else { \
				XNone(n, op) \
			}
#define XDenominatorImmediate(n, op) \
			if (getArg1Register() == 0) { \
				execute = false; \
				throw iris::Problem("denominator for operation " #n " is zero!"); \
			} else { \
				XImmediate(n, op) \
			}
#define X(name, title, desc) \
	template<> \
	void Core::op<InstructionGroup::Arithmetic, GetAssociatedOp<InstructionGroup::Arithmetic>::Association, GetAssociatedOp<InstructionGroup::Arithmetic>::Association:: name>() { \
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
#define DefArithmeticOp(Increment) \
    template<> \
    void Core::op<InstructionGroup::Arithmetic, GetAssociatedOp<InstructionGroup::Arith

}

#define DefJumpOp(title) \
	template<> \
	void Core::op<InstructionGroup::Jump, GetAssociatedOp<InstructionGroup::Jump>::Association, GetAssociatedOp<InstructionGroup::Jump>::Association:: title>()

DefJumpOp(Branch) {
	advanceIp = false;
	++registerValue<ArchitectureConstants::InstructionPointer>();
	// need to read the next "instruction"
	registerValue<ArchitectureConstants::InstructionPointer>() = instruction[registerValue<ArchitectureConstants::InstructionPointer>()];
}

DefJumpOp(Call) {
	advanceIp = false;
    // read the next word
	++registerValue<ArchitectureConstants::InstructionPointer>();
	registerValue<ArchitectureConstants::InstructionPointer> = instruction[registerValue<ArchitectureConstants::InstructionPointer>()];
}

DefJumpOp(IndirectBranch) {
    advanceIp = false;
    registerValue<ArchitectureConstants::InstructionPointer> = registerValue(current.getArg0());
}

DefJumpOp(IndirectCall) {
    advanceIp = false;

}

DefJumpOp(ConditionalBranch) {

}

DefJumpOp(ConditionalIndirectBranch) {

}

DefJumpOp(IfThenElse) {

}

DefJumpOp(IfThenElseLink) {

}
	template<>
	void Core::op<InstructionGroup::Misc, GetAssociatedOp<InstructionGroup::Misc>::Association, GetAssociatedOp<InstructionGroup::Misc>::Association::SystemCall>() {
		switch(static_cast<SystemCalls>(current.getArg0())) {
			case SystemCalls::Terminate:
				execute = false;
				advanceIp = false;
				break;
			case SystemCalls::PutC:
				// read register 0 and register 1
				std::cout.put(static_cast<char>(getArg1Register()));
				break;
			case SystemCalls::GetC:
				byte value;
				std::cin >> std::noskipws >> value;
				getArg1Register() = static_cast<word>(value);
				break;
			default:
				std::stringstream ss;
				ss << "Illegal system call " << current.getArg0();
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

#define X(name, __, ____) \
			case ControlSignature<InstructionGroup::Arithmetic, GetAssociatedOp<InstructionGroup::Arithmetic>::Association, GetAssociatedOp<InstructionGroup::Arithmetic>::Association:: name>::fullSignature: \
			op<InstructionGroup::Arithmetic, GetAssociatedOp<InstructionGroup::Arithmetic>::Association, GetAssociatedOp<InstructionGroup::Arithmetic>::Association:: name>(); \
			break;
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
					setInstructionMemory(address, result);
					break;
				case Segment::Data:
					result0 = iris17::encodeWord(buf[4], buf[5]);
					if (debugEnabled()) {
						std::cerr << " data result: 0x" << std::hex << result0 << std::endl;
					}
					setDataMemory(address, result0);
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
	word& Core::getArg0Register() {
		return registerValue(current.getArg0());
	}
	word& Core::getArg1Register() {
		return registerValue(current.getArg1());
	}
	word& Core::registerValue(byte index) {
		return gpr[index];
	}
}
