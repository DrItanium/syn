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
	word encodeWord(byte a, byte b, byte c, byte d) {
		return iris::encodeUint32LE(a, b, c, d);
	}
	hword encodeHword(byte a, byte b) {
		return iris::encodeUint16LE(a, b);
	}
	void decodeWord(word value, byte* storage) {
		return iris::decodeUint32LE(value, storage);
	}
	void decodeWord(RegisterValue value, byte* storage) {
		return iris::decodeInt32LE(value, storage);
	}
	void decodeHword(hword value, byte* storage) {
		return iris::decodeUint16LE(value, storage);
	}

	DecodedInstruction::DecodedInstruction(raw_instruction input) :
#define X(title, mask, shift, type, is_register, post) \
		_ ## post (iris::decodeBits<raw_instruction, type, mask, shift>(input)),
#include "iris17_instruction.def"
#undef X
		_rawValue(input) { }


	Core::Core() : memory(new word[ArchitectureConstants::AddressMax]) {

	}

	void Core::initialize() { }

	void Core::shutdown() { }

	template<int count>
	void populateContents(RegisterValue* contents, std::istream& stream) {
		char buf[sizeof(word)] = { 0 };
		for(int i = 0; i < count; ++i) {
			stream.read(buf, sizeof(word));
			contents[i] = iris17::encodeWord(buf[0], buf[1], buf[2], buf[3]);
		}
	}
	template<int count>
	void populateContents(const std::unique_ptr<word[]>& contents, std::istream& stream) {
		char buf[sizeof(word)] = { 0 };
		for(int i = 0; i < count; ++i) {
			stream.read(buf, sizeof(word));
			contents[i] = iris17::encodeWord(buf[0], buf[1], buf[2], buf[3]);
		}
	}
	void Core::installprogram(std::istream& stream) {
		populateContents<ArchitectureConstants::RegisterCount>(gpr, stream);
		populateContents<ArchitectureConstants::AddressMax>(memory, stream);
	}

	void Core::dump(std::ostream& stream) {
		// save the registers
		static char rBuf[sizeof(RegisterValue)] = { 0 };
		static char memBuf[sizeof(word)] = { 0 };
		for (int i = 0; i < ArchitectureConstants::RegisterCount; ++i) {
			decodeWord(gpr[i], rBuf);
			stream.write(rBuf, sizeof(RegisterValue));
		}
		for (int i = 0; i < ArchitectureConstants::AddressMax; ++i) {
			decodeWord(memory[i], memBuf);
			stream.write(memBuf, sizeof(word));
		}
	}
	void Core::run() {
		while(execute) {
			if (!advanceIp) {
				advanceIp = true;
			}
			current.decode(getCurrentCodeWord());
			dispatch();
			if (advanceIp) {
				++getInstructionPointer();
			}
		}
	}





#define DefOp(title) \
	template<> \
	void Core::op<Operation:: title>() 

	DefOp(Add){
		
	}

	DefOp(Increment) {
		registerValue(current.getArg0()) = registerValue(current.getArg0()) + 1;
	}
	
	DefOp(Decrement) {
		registerValue(current.getEmbeddedArg()) = registerValue(current.getEmbeddedArg()) - 1;
	}

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

DefOp(Branch) {
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
#define X(type) \
			case Operation:: type : \
				op<Operation:: type>(); \
			break;
#include "iris17_ops.def"
#undef X
			default:
				std::stringstream str;
				str << "Illegal instruction " << current.getControl();
				execute = false;
				throw iris::Problem(str.str());
		}
	}


	//enum class Segment  {
	//	Code,
	//	Data,
	//	Count,
	//};

	void Core::link(std::istream& input) {
		//dword result = 0;
		//word result0 = 0;
		//char buf[8] = {0};
		//for(int lineNumber = 0; input.good(); ++lineNumber) {
		//	input.read(buf, 8);
		//	if (input.gcount() < 8 && input.gcount() > 0) {
		//		throw iris::Problem("unaligned object file found!");
		//	} else if (input.gcount() == 0) {
		//		if (input.eof()) {
		//			break;
		//		} else {
		//			throw iris::Problem("something bad happened while reading input file!");
		//		}
		//	}
		//	//ignore the first byte, it is always zero
		//	byte tmp = buf[1];
		//	Segment target = static_cast<Segment>(buf[1]);
		//	word address = iris17::encodeWord(buf[2], buf[3]);
		//	if (debugEnabled()) {
		//		std::cerr << "current target = " << static_cast<int>(target) << "\tcurrent address = 0x" << std::hex << address << std::endl;
		//	}
		//	switch(target) {
		//		case Segment::Code:
		//			result = iris17::encodeWord(buf[4], buf[5]);
		//			if (debugEnabled()) {
		//				std::cerr << " code result: 0x" << std::hex << result << std::endl;
		//			}
		//			//setInstructionMemory(address, result);
		//			break;
		//		case Segment::Data:
		//			result0 = iris17::encodeWord(buf[4], buf[5]);
		//			if (debugEnabled()) {
		//				std::cerr << " data result: 0x" << std::hex << result0 << std::endl;
		//			}
		//			//setDataMemory(address, result0);
		//			break;
		//		default:
		//			std::stringstream str;
		//			str << "error: line " << lineNumber << ", unknown segment " << static_cast<int>(target) << "/" << static_cast<int>(tmp) << std::endl;
		//			str << "current address: " << std::hex << address << std::endl;
		//			throw iris::Problem(str.str());
		//	}
		//}
	}
	byte Core::getControl() {
		return current.getControl();
	}
	word& Core::registerValue(byte index) {
		return gpr[index];
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
		return memory[getInstructionPointer()];
	}
	word Core::getTopOfStack() {
		return memory[getStackPointer()];
	}
}
