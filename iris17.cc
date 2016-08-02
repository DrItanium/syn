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
	RegisterValue encodeRegisterValue(byte a, byte b, byte c, byte d) {
		return iris::encodeUint32LE(a, b, c, d);
	}
	word encodeWord(byte a, byte b) {
		return iris::encodeUint16LE(a, b);
	}
	void decodeWord(word value, byte* storage) {
		return iris::decodeUint32LE(value, storage);
	}
	void decodeWord(RegisterValue value, byte* storage) {
		return iris::decodeInt32LE(value, storage);
	}

	DecodedInstruction::DecodedInstruction(raw_instruction input) :
#define X(title, mask, shift, type, is_register, post) \
		_ ## post (iris::decodeBits<raw_instruction, type, mask, shift>(input)),
#include "iris17_instruction.def"
#undef X
		_rawValue(input) { }


	Core::Core() : memory(new word[ArchitectureConstants::AddressMax]) { }
	Core::~Core() {
		delete [] memory;
	}

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
		populateContents<word, ArchitectureConstants::AddressMax>(memory, stream, [](byte* buf) { return iris::encodeUint16LE(buf); });
	}

	template<typename T, int count>
	void dumpContents(T* contents, std::ostream& stream, std::function<void(T value, byte* buf)> decompose) {
		static byte buf[sizeof(T)];
		for (int i = 0; i < count; ++i) {
			decompose(contents[i], (byte*)buf);
			stream.write((char*)buf, sizeof(T));
		}
	}

	void Core::dump(std::ostream& stream) {
		// save the registers
		dumpContents<RegisterValue, ArchitectureConstants::RegisterCount>(gpr, stream, iris::decodeUint32LE);
		dumpContents<word, ArchitectureConstants::AddressMax>(memory, stream, iris::decodeUint16LE);
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
#define mRegisterArg0 (registerValue(mValueArg0))
#define mRegisterArg1 (registerValue(mValueArg1))
#define mAsRegisterValue(value) static_cast<RegisterValue>(value)

#define DefOp(title) \
	template<> \
	void Core::operation<Operation:: title>(DecodedInstruction&& current) 
	
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
#include "iris17_arithmetic_ops.def"
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
		static constexpr word lowerMask = (determineMaskValue(decomposedBits[1]) << 8) | (determineMaskValue(decomposedBits[0]));
		static constexpr word upperMask = (determineMaskValue(decomposedBits[3]) << 8) | (determineMaskValue(decomposedBits[2]));
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
#include "iris17_bitmask4bit.def"
#undef X
			default:
				throw iris::Problem("unknown mask!");
		}
    }
	void Core::storeWord(RegisterValue address, word value) {
		if (address > ArchitectureConstants::AddressMax) {
			throw iris::Problem("Attempted to write outside of memory!");
		} else {
			memory[address] = value;
		}
	}
	word Core::loadWord(RegisterValue address) {
		if (address > ArchitectureConstants::AddressMax) {
			throw iris::Problem("Attempted to read from outside of memory!");
		} else {
			return memory[address];
		}
	}
	RegisterValue Core::loadRegisterValue(RegisterValue address) {
		return iris::encodeBits<RegisterValue, word, SetBitmaskToWordMask<0b1111>::mask, 16>(RegisterValue(loadWord(address)), loadWord(address + 1));
	}
	void Core::storeRegisterValue(RegisterValue address, RegisterValue value) {
		storeWord(address, iris::decodeBits<RegisterValue, word, SetBitmaskToWordMask<0b0011>::mask, 0>(value));
		storeWord(address + 1, iris::decodeBits<RegisterValue, word, SetBitmaskToWordMask<0b1100>::mask, 16>(value));
	}
	DefOp(Load) {
		// use the destination field of the instruction to denote offset, thus we need
		// to use the Address and Value registers
		auto offset = current.getDestination();
		RegisterValue address = getAddressRegister() + offset;
		// use the src0 field of the instruction to denote the bitmask
		auto bitmask = current.getSrc0();
		switch (bitmask) {
#define X(value) \
			case value: \
			{ \
				RegisterValue lower = 0; \
				RegisterValue upper = 0; \
				if (SetBitmaskToWordMask<value>::readLower) { \
					lower = loadWord(address); \
				} \
				if (SetBitmaskToWordMask<value>::readUpper) { \
					upper = RegisterValue(loadWord(address + 1)) << 16; \
				} \
				getValueRegister() = (SetBitmaskToWordMask<value>::mask & (lower | upper)); \
				break; \
			}
#include "iris17_bitmask4bit.def"
#undef X
			default:
				throw iris::Problem("illegal bitmask");
		}
	}

	DefOp(LoadMerge) {
		// use the destination field of the instruction to denote offset, thus we need
		// to use the Address and Value registers
		auto offset = current.getDestination();
		RegisterValue address = getAddressRegister() + offset;
		// use the src0 field of the instruction to denote the bitmask
		auto bitmask = current.getSrc0();
		switch (bitmask) {
			// 0b1101 implies that we have to leave 0x0000FF00 around in the
			// value register since it isn't necessary
#define X(value) \
			case value: \
			{ \
				RegisterValue lower = 0; \
				RegisterValue upper = 0; \
				if (SetBitmaskToWordMask<value>::readLower) { \
					lower = loadWord(address); \
				} \
				if (SetBitmaskToWordMask<value>::readUpper) { \
					upper = RegisterValue(loadWord(address + 1)) << 16; \
				} \
				getValueRegister() = (SetBitmaskToWordMask<value>::mask & (lower | upper)) | (getValueRegister() & ~SetBitmaskToWordMask<value>::mask); \
				break; \
			}
#include "iris17_bitmask4bit.def"
#undef X
			default:
				throw iris::Problem("illegal bitmask");
		}
	}

	DefOp(Store) {
		auto offset = current.getDestination();
		auto bitmask = current.getSrc0();
		RegisterValue address = getAddressRegister() + offset;
		switch(bitmask) {
			// 0b1101 implies that we have to leave 0x0000FF00 around in the
			// value register since it isn't necessary
#define X(value) \
			case value: \
			{ \
				RegisterValue lower = 0; \
				RegisterValue upper = 0; \
				if (SetBitmaskToWordMask<value>::readLower) { \
					lower = SetBitmaskToWordMask<value>::lowerMask & iris::decodeBits<RegisterValue, word, SetBitmaskToWordMask<0b0011>::mask, 0>(getValueRegister()); \
					auto loader = loadWord(address) & ~SetBitmaskToWordMask<value>::lowerMask; \
					storeWord(address, lower | loader); \
				} \
				if (SetBitmaskToWordMask<value>::readUpper) { \
					upper = SetBitmaskToWordMask<value>::upperMask & iris::decodeBits<RegisterValue, word, SetBitmaskToWordMask<0b1100>::mask, 16>(getValueRegister()); \
					auto loader = loadWord(address) & ~SetBitmaskToWordMask<value>::upperMask; \
					storeWord(address + 1, upper | loader); \
				} \
				break; \
			}
#include "iris17_bitmask4bit.def"
#undef X
			default:
				throw iris::Problem("illegal bitmask");
		}
	}

	DefOp(Push) {
		auto stackPointer = getStackPointer();
		auto bitmask = current.getSrc0();
		auto pushToStack = registerValue(current.getSrc1());
		switch (bitmask) {
#define X(value) \
			case value: \
			{ \
				if (SetBitmaskToWordMask<value>::readUpper) { \
					--stackPointer; \
					stackPointer &= SetBitmaskToWordMask<0b0111>::mask; \
					RegisterValue upper = SetBitmaskToWordMask<value>::upperMask & iris::decodeBits<RegisterValue, word, SetBitmaskToWordMask<0b1100>::mask, 16>(pushToStack); \
					storeWord(stackPointer, upper); \
				} \
				if (SetBitmaskToWordMask<value>::readLower) { \
					--stackPointer; \
					stackPointer &= SetBitmaskToWordMask<0b0111>::mask; \
					RegisterValue lower = SetBitmaskToWordMask<value>::lowerMask & iris::decodeBits<RegisterValue, word, SetBitmaskToWordMask<0b0011>::mask, 0>(pushToStack); \
					storeWord(stackPointer, lower); \
				} \
				break; \
			}
#include "iris17_bitmask4bit.def"
#undef X
			default:
				throw iris::Problem("illegal bitmask");
		}
	}

	DefOp(Pop) {
		auto stackPointer = getStackPointer();
		auto bitmask = current.getSrc0();
		switch (bitmask) {
			// pop the entries off of the stack and store it in the register
#define X(value) \
			case value: \
			{ \
				RegisterValue lower = 0; \
				RegisterValue upper = 0; \
				if (SetBitmaskToWordMask<value>::readLower) { \
					auto val = loadWord(stackPointer); \
					++stackPointer; \
					stackPointer &= SetBitmaskToWordMask<0b0111>::mask; \
					lower = SetBitmaskToWordMask<value>::lowerMask & val; \
				} \
				if (SetBitmaskToWordMask<value>::readUpper) { \
					auto val = loadWord(stackPointer); \
					++stackPointer; \
					stackPointer &= SetBitmaskToWordMask<0b0111>::mask; \
					upper = SetBitmaskToWordMask<value>::upperMask & val; \
				} \
				registerValue(current.getSrc1()) = iris::encodeBits<word, RegisterValue, SetBitmaskToWordMask<0b1100>::mask, 16>(iris::encodeBits<word, RegisterValue, SetBitmaskToWordMask<0b0011>::mask, 0>(RegisterValue(0), lower), upper); \
				break; \
			}
#include "iris17_bitmask4bit.def"
#undef X
			default:
				throw iris::Problem("illegal bitmask");
		}
	}
DefOp(Branch) {
	advanceIp = false;
	++getInstructionPointer();
	// make a 24 bit number
	auto bottom = RegisterValue(current.getUpper());
	auto upper = RegisterValue(getCurrentCodeWord()) << 8;
	getInstructionPointer() = SetBitmaskToWordMask<0b0111>::mask & (upper | bottom);

}

DefOp(Call) {
	advanceIp = false;
	++getInstructionPointer();
	// make a 24 bit number
	auto ip = getInstructionPointer();
	auto bottom = RegisterValue(current.getUpper());
	auto upper = RegisterValue(getCurrentCodeWord()) << 8;
	getInstructionPointer() = SetBitmaskToWordMask<0b0111>::mask & (upper | bottom);
	getLinkRegister() = ip + 1;
	if (getLinkRegister() > SetBitmaskToWordMask<0b0111>::mask) {
		getLinkRegister() &= SetBitmaskToWordMask<0b0111>::mask; // make sure that we aren't over the memory setup
	}
}

DefOp(IndirectBranch) {
    advanceIp = false;
	auto target = registerValue(current.getDestination());
	getInstructionPointer() = SetBitmaskToWordMask<0b0111>::mask & target;
}

DefOp(IndirectCall) {
    advanceIp = false;
	auto ip = getInstructionPointer();
	auto target = registerValue(current.getDestination());
	getInstructionPointer() = SetBitmaskToWordMask<0b0111>::mask & target;
	getLinkRegister() = ip + 1;
	if (getLinkRegister() > SetBitmaskToWordMask<0b0111>::mask) {
		getLinkRegister() &= SetBitmaskToWordMask<0b0111>::mask; // make sure that we aren't over the memory setup
	}
}

DefOp(ConditionalBranch) {
	++getInstructionPointer();
	if (getConditionRegister() != 0) {
		advanceIp = false;
		// make a 24 bit number
		auto bottom = RegisterValue(current.getUpper());
		auto upper = RegisterValue(getCurrentCodeWord()) << 8;
		getInstructionPointer() = SetBitmaskToWordMask<0b0111>::mask & (upper | bottom);
	} 
}

DefOp(ConditionalIndirectBranch) {
	if (getConditionRegister() != 0) {
		advanceIp = false;
		// make a 24 bit number
		auto target = registerValue(current.getDestination());
		getInstructionPointer() = SetBitmaskToWordMask<0b0111>::mask & target;
	}
}
DefOp(IfThenElse) {
	advanceIp = false;
	auto onTrue = registerValue(current.getDestination());
	auto onFalse = registerValue(current.getSrc0());
	if (getConditionRegister() != 0) {
		getInstructionPointer() = SetBitmaskToWordMask<0b0111>::mask & onTrue;
	} else {
		getInstructionPointer() = SetBitmaskToWordMask<0b0111>::mask & onFalse;
	}
}
DefOp(IfThenElseLink) {
	advanceIp = false;
	auto onTrue = registerValue(current.getDestination());
	auto onFalse = registerValue(current.getSrc0());
	getLinkRegister() = getInstructionPointer() + 1;
	if (getConditionRegister() != 0) {
		getInstructionPointer() = SetBitmaskToWordMask<0b0111>::mask & onTrue;
	} else {
		getInstructionPointer() = SetBitmaskToWordMask<0b0111>::mask & onFalse;
	}
}
template<CompareCombine compareOp> 
bool combine(bool newValue, bool existingValue) {
	throw iris::Problem("Undefined combine operation");
}
template<>
bool combine<CompareCombine::None>(bool newValue, bool existingValue) {
	return newValue;
}

template<>
bool combine<CompareCombine::And>(bool newValue, bool existingValue) {
	return newValue && existingValue;
}

template<>
bool combine<CompareCombine::Or>(bool newValue, bool existingValue) {
	return newValue || existingValue;
}

template<>
bool combine<CompareCombine::Xor>(bool newValue, bool existingValue) {
	return newValue ^ existingValue;
}

template<CompareStyle style>
bool compare(RegisterValue a, RegisterValue b) {
	throw iris::Problem("Undefined comparison style!");
}

template<>
bool compare<CompareStyle::Equals>(RegisterValue a, RegisterValue b) {
	return a == b;
}

template<>
bool compare<CompareStyle::NotEquals>(RegisterValue a, RegisterValue b) {
	return a != b;
}

template<>
bool compare<CompareStyle::LessThan>(RegisterValue a, RegisterValue b) {
	return a < b;
}

template<>
bool compare<CompareStyle::GreaterThan>(RegisterValue a, RegisterValue b) {
	return a > b;
}

template<>
bool compare<CompareStyle::LessThanOrEqualTo>(RegisterValue a, RegisterValue b) {
	return a <= b;
}
template<>
bool compare<CompareStyle::GreaterThanOrEqualTo>(RegisterValue a, RegisterValue b) {
	return a >= b;
}

DefOp(Compare) {
	++getInstructionPointer();
	DecodedInstruction next(getCurrentCodeWord());
	switch (current.getConditionalCompareType()) {
#define X(type) \
		case CompareStyle:: type : { \
									   RegisterValue first = registerValue(next.getSrc1()); \
									   RegisterValue second = current.getConditionalImmediateFlag() ? next.getUpper() : registerValue(next.getSrc2()); \
									   bool result = compare<CompareStyle:: type>(first, second); \
									   switch (current.getConditionalCombineFlag()) { \
										   case CompareCombine::None: \
											   getConditionRegister() = combine<CompareCombine::None>(result, getConditionRegister()); \
											   break; \
										   case CompareCombine::And: \
											   getConditionRegister() = combine<CompareCombine::And>(result, getConditionRegister()); \
											   break; \
										   case CompareCombine::Or: \
											   getConditionRegister() = combine<CompareCombine::Or>(result, getConditionRegister()); \
											   break; \
										   case CompareCombine::Xor: \
											   getConditionRegister() = combine<CompareCombine::Xor>(result, getConditionRegister()); \
											   break; \
										   default: \
													throw iris::Problem("Illegal Compare Combine Operation"); \
									   } \
									   break; \
								   }
		X(Equals)
		X(NotEquals)
		X(LessThan)
		X(GreaterThan)
		X(LessThanOrEqualTo)
		X(GreaterThanOrEqualTo)
#undef X
		default:
			throw iris::Problem("illegal compare type!");
	}
}

DefOp(Return) {
	advanceIp = false;
	// jump to the link register
	getInstructionPointer() = getLinkRegister();
}

DefOp(Square) {
	registerValue(current.getDestination()) = registerValue(current.getDestination()) * registerValue(current.getDestination());
}

DefOp(Cube) {
	registerValue(current.getDestination()) = registerValue(current.getDestination()) * registerValue(current.getDestination()) * registerValue(current.getDestination());
}

	template<>
	void Core::operation<Operation::SystemCall>(DecodedInstruction&& current) {
		switch(static_cast<SystemCalls>(getAddressRegister())) {
			case SystemCalls::Terminate:
				execute = false;
				advanceIp = false;
				break;
			case SystemCalls::PutC:
				// read register 0 and register 1
				std::cout.put(static_cast<char>(registerValue(current.getDestination())));
				break;
			case SystemCalls::GetC:
				byte value;
				std::cin >> std::noskipws >> value;
				registerValue(current.getDestination()) = static_cast<word>(value);
				break;
			default:
				std::stringstream ss;
				ss << "Illegal system call " << std::hex << getAddressRegister();
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
				operation<Operation:: type>(std::move(current)); \
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

	void Core::link(std::istream& input) {
		// two address system, 1 RegisterValue -> address, 1 word -> value
		constexpr int bufSize = sizeof(RegisterValue) + sizeof(word);
		char buf[bufSize] = { 0 };
		for(int lineNumber = 0; input.good(); ++lineNumber) {
			input.read(buf, bufSize);
			if (input.gcount() < bufSize && input.gcount() > 0) {
				throw iris::Problem("unaligned object file found!");
			} else if (input.gcount() == 0) {
				if (input.eof()) {
					break;
				} else {
					throw iris::Problem("something bad happened while reading input file!");
				}
			}
			//ignore the first byte, it is always zero
			RegisterValue address = encodeRegisterValue(buf[0], buf[1], buf[2], buf[3]);
			word value = encodeWord(buf[4], buf[5]);
			this->storeWord(address, value);
		}
	}
	RegisterValue& Core::registerValue(byte index) {
		if (index >= ArchitectureConstants::RegisterCount) {
			throw iris::Problem("Attempted to access an out of range register!");
		} else {
			return gpr[index];
		}
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
}
