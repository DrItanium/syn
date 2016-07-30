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


	Core::Core() { }

	void Core::initialize() { }

	void Core::shutdown() { }

	template<typename T, int count>
	void populateContents(T* contents, std::istream& stream, std::function<T(byte*)> encode) {
		static char buf[sizeof(word)] = { 0 };
		for(int i = 0; i < count; ++i) {
			stream.read(buf, sizeof(word));
			contents[i] = encode((byte*)buf);
		}
	}
	void Core::installprogram(std::istream& stream) {

		populateContents<RegisterValue, ArchitectureConstants::RegisterCount>(gpr, stream, [](byte* buf) { return iris::encodeUint32LE(buf); });
		for (int i =0 ; i < ArchitectureConstants::SegmentCount; ++i) {
			populateContents<word, ArchitectureConstants::AddressMax>(memory[i], stream, [](byte* buf) { return iris::encodeUint16LE(buf); });
		}
	}

	template<typename T, int count>
	void dumpContents(T* contents, std::ostream& stream, std::function<void(T value, byte* buf)> decompose) {
		static byte buf[sizeof(T)];
		for (int i = 0; i < count; ++i) {
			decompose(contents[i], (byte*)buf);
			stream.write(buf, sizeof(T));
		}
	}

	void Core::dump(std::ostream& stream) {
		// save the registers
		dumpContents<RegisterValue, ArchitectureConstants::RegisterCount>(gpr, stream, iris::decodeUint32LE);
		for (int i = 0; i < ArchitectureConstants::SegmentCount; ++i) {
			dumpContents<word, ArchitectureConstants::AddressMax>(memory[i], stream, iris::decodeUint16LE);
		}
	}
	void Core::run() {
		while(execute) {
			if (!advanceIp) {
				advanceIp = true;
			}
			DecodedInstruction di(getCurrentCodeWord());
			dispatch(std::move(di));
			if (advanceIp) {
				++getInstructionPointer();
			}
		}
	}



#define mValueArg0 (current.getDestination())
#define mValueArg1 (current.getSrc0())
#define mValueArg2 (current.getSrc1())
#define mValueArg3 (current.getSrc2())
#define mValueArg4 (current.getSrc3())
#define mRegisterArg0 (registerValue(mValueArg0))
#define mRegisterArg1 (registerValue(mValueArg1))
#define mRegisterArg2 (registerValue(mValueArg2))
#define mRegisterArg3 (registerValue(mValueArg3))
#define mRegisterArg4 (registerValue(mValueArg4))
#define mAsRegisterValue(value) static_cast<RegisterValue>(value)

#define DefOp(title) \
	template<> \
	void Core::op<Operation:: title>(DecodedInstruction&& current) 
	
	DefOp(Nop) { 
	}

#define ArithmeticOp(title, operation, src1) \
	DefOp(title) { \
		mRegisterArg0 = mRegisterArg0 operation src1 ; \
	}
#define X(title, operation) ArithmeticOp(title, operation, mRegisterArg1)
#define Y(title, operation) ArithmeticOp(title, operation, mAsRegisterValue(mValueArg1))
// for the cases where we have an immediate form
#define Z(title, operation) \
	X(title, operation) \
	Y(title ## Immediate , operation)
#define Div(title, operation, src1) \
	DefOp(title) { \
		if (src1 == 0) { \
			throw iris::Problem("Denominator is zero!"); \
		} else { \
			mRegisterArg0 = mRegisterArg0 operation src1 ; \
		} \
	}
#define W(title, operation) \
	Div(title, operation,  mRegisterArg1) \
	Div(title ## Immediate, operation, mAsRegisterValue(mValueArg1))

	Z(Add, +)
	Z(Sub, -)
	Z(Mul, *)
	W(Div, /)
	W(Rem, %)
	Z(ShiftLeft, <<)
	Z(ShiftRight, >>)
	X(BinaryAnd, &)
	X(BinaryOr, |)
	X(BinaryXor, ^)
#undef Z
#undef X
#undef Y
#undef W
#undef Div
#undef ArithmeticOp
	DefOp(BinaryNot) {
		mRegisterArg0 = ~ mRegisterArg0;
	}

	DefOp(Increment) {
		++mRegisterArg0;
	}
	
	DefOp(Decrement) {
		--mRegisterArg0;
	}

	DefOp(Double) {
		mRegisterArg0 *= 2;
	}

	DefOp(Halve) {
		mRegisterArg0 /= 2;
	}

	DefOp(Move)  {
		mRegisterArg0 = mRegisterArg1;
	}

	DefOp(Swap) {
		RegisterValue tmp = mRegisterArg0;
		mRegisterArg0 = mRegisterArg1;
		mRegisterArg1 = tmp;
	}

	template<byte bitmask> 
	struct SetBitmaskToWordMask {
		static constexpr bool decomposedBits[] = {
			(bitmask & 0b0001),
			((bitmask & 0b0010) >> 1),
			((bitmask & 0b0100) >> 2),
			((bitmask & 0b1000) >> 3),
		};
		static constexpr byte determineMaskValue(bool value) { return value ? 0xFF : 0x00; }
		static constexpr RegisterValue mask = (determineMaskValue(decomposedBits[3]) << 24) |
				(determineMaskValue(decomposedBits[2]) << 16) | 
				(determineMaskValue(decomposedBits[1]) << 8) | 
				(determineMaskValue(decomposedBits[0]));
		static constexpr bool readLower = decomposedBits[1] || decomposedBits[0];
		static constexpr bool readUpper = decomposedBits[2] || decomposedBits[3];
	};

    DefOp(Set) {
		auto bitmask = mRegisterArg1;
		switch (bitmask) {
#define X(value) \
			case value: \
			{ \
				RegisterValue lower = 0; \
				RegisterValue upper = 0; \
				if (SetBitmaskToWordMask<value>::readLower) { \
					++getInstructionPointer(); \
					lower = getCurrentCodeWord(); \
				} \
				if (SetBitmaskToWordMask<value>::readUpper) { \
					++getInstructionPointer(); \
					upper = RegisterValue(getCurrentCodeWord()) << 16; \
				} \
				mRegisterArg0 = SetBitmaskToWordMask<value>::mask & (lower | upper); \
				break; \
			}
			X(0b0000)
			X(0b0001)
			X(0b0010)
			X(0b0011)
			X(0b0100)
			X(0b0101)
			X(0b0110)
			X(0b0111)
			X(0b1000)
			X(0b1001)
			X(0b1010)
			X(0b1011)
			X(0b1100)
			X(0b1101)
			X(0b1110)
			X(0b1111)
#undef X
			default:
				throw iris::Problem("unknown mask!");
		}
    }


	DefOp(Load) {
		// use arg0 to denote which data segment to use
		//getValueRegister() = getSegment(registerValue(current.getArg0()))[getAddressRegister()];
	}

	DefOp(Store) {
		//getDataSegment()[getAddressRegister()] = getValueRegister();
	}

	DefOp(Push) {
		//word& stackPointer = getStackPointer();
		//++stackPointer;
		//getStackSegment()[stackPointer] = registerValue(current.getEmbeddedArg());
	}

	DefOp(Pop) {
		//registerValue(current.getEmbeddedArg()) = getStackSegment()[getStackPointer()];
		//--getStackPointer();
	}
DefOp(Branch) {
	advanceIp = false;
	getInstructionPointer() = current.getAddress24();
}

DefJumpOp(Call) {
	advanceIp = false;
	word ip = getInstructionPointer();
	getInstructionPointer() = current.getAddress24();
	getLinkRegister() = ip + 1;
}

DefJumpOp(IndirectBranch) {
    advanceIp = false;
	getInstructionPointer() = registerValue(current.getEmbeddedArg());
}
//
//DefJumpOp(IndirectCall) {
//    advanceIp = false;
//	word ip = getInstructionPointer() + 1;
//	getInstructionPointer() = registerValue(current.getEmbeddedArg());
//	getLinkRegister() = ip;
//}
//
//DefJumpOp(ConditionalBranch) {
//	advanceIp = false;
//	if (getConditionRegister() != 0) {
//		++getInstructionPointer();
//		getInstructionPointer() = getCurrentCodeWord();
//	} else {
//		getInstructionPointer() += 2;
//	}
//}
//
//DefJumpOp(ConditionalIndirectBranch) {
//	advanceIp = false;
//	if (getConditionRegister() != 0) {
//		getInstructionPointer() = registerValue(current.getEmbeddedArg());
//	} else {
//		++getInstructionPointer();
//	}
//}
//
//DefJumpOp(IfThenElse) {
//	advanceIp = false;
//	++getInstructionPointer();
//	DecodedInstruction next;
//	next.decode(getCurrentCodeWord());
//	getInstructionPointer() = registerValue(((registerValue(current.getEmbeddedArg()) != 0) ? next.getSpecificArg0() : next.getSpecificArg1()));
//}
//
//DefJumpOp(IfThenElseLink) {
//	advanceIp = false;
//	word ip = getInstructionPointer() + 2;
//	++getInstructionPointer();
//	DecodedInstruction next;
//	next.decode(getCurrentCodeWord());
//	getInstructionPointer() = registerValue(((registerValue(current.getEmbeddedArg()) != 0) ? next.getSpecificArg0() : next.getSpecificArg1()));
//	getLinkRegister() = ip;
//}

	template<>
	void Core::op<Operation::SystemCall>(DecodedInstruction&& current) {
		switch(static_cast<SystemCalls>(current.getByte1())) {
			case SystemCalls::Terminate:
				execute = false;
				advanceIp = false;
				break;
			case SystemCalls::PutC:
				// read register 0 and register 1
				std::cout.put(static_cast<char>(registerValue(current.getArg2())));
				break;
			case SystemCalls::GetC:
				byte value;
				std::cin >> std::noskipws >> value;
				registerValue(current.getArg3()) = static_cast<word>(value);
				break;
			default:
				std::stringstream ss;
				ss << "Illegal system call " << current.getByte1();
				execute = false;
				advanceIp = false;
				throw iris::Problem(ss.str());
		}
	}

	void Core::dispatch(DecodedInstruction&& current) {
		auto controlValue = current.getControl();
		switch(controlValue) {
#define X(type) \
			case Operation:: type : \
				op<Operation:: type>(std::move(current)); \
			break;
#include "iris17_ops.def"
#undef X
			default:
				std::stringstream str;
				str << "Illegal instruction " << std::hex << static_cast<byte>(controlValue);
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
	RegisterValue& Core::registerValue(byte index) {
		return gpr[index];
	}
	RegisterValue& Core::getInstructionPointer() {
		return registerValue<ArchitectureConstants::InstructionPointer>();
	}
	RegisterValue& Core::getStackPointer() {
		return registerValue<ArchitectureConstants::StackPointer>();
	}
	RegisterValue& Core::getConditionRegister() {
		return registerValue<ArchitectureConstants::ConditionRegister>();
	}
	RegisterValue& Core::getLinkRegister() {
		return registerValue<ArchitectureConstants::LinkRegister>();
	}
	RegisterValue& Core::getAddressRegister() {
		return registerValue<ArchitectureConstants::AddressRegister>();
	}
	RegisterValue& Core::getValueRegister() {
		return registerValue<ArchitectureConstants::ValueRegister>();
	}
	word Core::getCurrentCodeWord() {
		return memory[getInstructionPointer()];
	}
	word Core::getTopOfStack() {
		return memory[getStackPointer()];
	}
}
