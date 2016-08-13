#include "iris17.h"
#include <functional>
#include <sstream>
#include "Problem.h"
#include <utility>

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

	DecodedInstruction::DecodedInstruction(raw_instruction input) : _rawValue(input) { }

	raw_instruction DecodedInstruction::getRawValue() const {
		return _rawValue;
	}

#define X(title, mask, shift, type, post) \
		type DecodedInstruction:: get ## title () const { \
			return iris::decodeBits<raw_instruction, type, mask, shift>(_rawValue); \
		}
#include "def/iris17/instruction.def"
#undef X


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
			DecodedInstruction di(getCurrentCodeWord());
			dispatch(std::move(di));
			if (advanceIp) {
				++getInstructionPointer();
			} else {
				// just re-enable it
				advanceIp = true;
			}
		}
	}


#define DefOp(title) \
	template<> \
	void Core::operation<Operation:: title>(DecodedInstruction&& current) 
	
	DefOp(Nop) { 
	}
	DefOp(Shift) {
		auto destination = registerValue(current.getShiftRegister0());
		RegisterValue source = (current.getShiftFlagImmediate() ? static_cast<RegisterValue>(current.getShiftImmediate()) : registerValue(current.getShiftRegister1()));
		destination = (current.getShiftFlagLeft() ? (destination << source) : (destination >> source));
	}


	DefOp(Logical) {
		switch(current.getLogicalSignature()) {
#define X(datum) case datum: logicalOperation<datum>(std::move(current)); break;
#include "def/iris17/bitmask8bit.def"
#undef X
				default:
					throw iris::Problem("Illegal logical signature!");
		}
	}



	DefOp(Arithmetic) {
		switch (current.getArithmeticSignature()) {
#define X(value) case value: arithmeticOperation< value > (std::move(current)); break; 
#include "def/iris17/bitmask4bit.def"
#undef X
			default:
				throw iris::Problem("Illegal Arithmetic Signature");
		}
	}
	DefOp(Move)  {
		switch (current.getMoveSignature()) {
#define X(value) case value : moveOperation<value>(std::move(current)); break;
#include "def/iris17/bitmask4bit.def"
#undef X
			default: 
				throw iris::Problem("Illegal move signature!");
		}
	}

	DefOp(Swap) {
		if (current.getSwapDestination() != current.getSwapSource()) {
			RegisterValue tmp = registerValue(current.getSwapDestination());
			registerValue(current.getSwapDestination()) = registerValue(current.getSwapSource());
			registerValue(current.getSwapSource()) = tmp;
		}
	}

    DefOp(Set) {
		switch (current.getSetSignature()) {
#define X(value) setOperation<value>(std::move(current)); break;
#include "def/iris17/bitmask8bit.def"
#undef X
			default:
				throw iris::Problem("Illegal set signature!");
		}
    }


	DefOp(Memory) {
		switch (current.getMemorySignature()) {
#define X(value) case value: memoryOperation<value>(std::move(current)); break;
#include "def/iris17/bitmask8bit.def"
#undef X
			default:
				throw iris::Problem("Illegal memory signature!");
		}
	}

	template<byte flags>
	bool branchSpecificOperation(RegisterValue& ip, RegisterValue& linkRegister, RegisterValue& cond, std::function<RegisterValue()> getUpper16, std::function<RegisterValue&(byte)> registerValue, DecodedInstruction&& current) {
		constexpr bool isIf = static_cast<bool>(flags & 0b0001);
		constexpr bool isCall = static_cast<bool>((flags & 0b0010) >> 1);
		constexpr bool isImmediate = static_cast<bool>((flags & 0b0100) >> 2);
		constexpr bool isConditional = static_cast<bool>((flags & 0b1000) >> 3);
		bool advanceIp = true;
		if (isIf) {
			// if instruction
			advanceIp = false;
			if (isCall) {
				linkRegister = ip + 1;
				if (linkRegister > bitmask24) {
					linkRegister &= bitmask24;
				}
			} 
			ip = bitmask24 & ((cond != 0) ? registerValue(current.getBranchIfOnTrue()) : registerValue(current.getBranchIfOnFalse())); 
		} else if (isCall) {
			// call instruction
			advanceIp = false;
			// determine next
			linkRegister = isImmediate ? ip + 2 : ip + 1;
			if (linkRegister > bitmask24) {
				linkRegister &= bitmask24; // make sure that we aren't over the memory setup
			}
			if (isImmediate) {
				++ip;
				// make a 24 bit number
				ip = bitmask24 & ((static_cast<RegisterValue>(current.getUpper())) | (getUpper16() << 8));
			} else {
				ip = bitmask24 & registerValue(current.getBranchIndirectDestination());
			}
		} else {
			// jump instruction
			if (isImmediate) {
				++ip;
				if (isConditional) {
					if (cond != 0) {
						advanceIp = false;
						auto bottom = current.getUpper();
						auto upper = getUpper16() << 8;
						ip = bitmask24 & (upper | bottom);
					}
				} else {
					advanceIp = false;
					auto bottom = RegisterValue(current.getUpper());
					auto upper = getUpper16() << 8;
					ip = bitmask24 & (upper | bottom);
				}
			}  else {
				if (isConditional) {
					if (cond != 0) {
						advanceIp = false;
						auto target = registerValue(current.getBranchIndirectDestination());
						ip = bitmask24 & target;
					}
				} else {
					advanceIp = false;
					auto target = registerValue(current.getBranchIndirectDestination());
					ip = bitmask24 & target;
				}
			}
		}
		return advanceIp;
	}


	DefOp(Branch) {
		auto upper16fn = [this]() { return static_cast<RegisterValue>(getCurrentCodeWord()); };
		auto regValFn = [this](byte index) -> RegisterValue& { return registerValue(index); };

		switch (current.getBranchFlags()) {
#define X(value) \
			case value :: flags : { \
							 advanceIp = branchSpecificOperation< value :: flags >(getInstructionPointer(), getLinkRegister(), getConditionRegister(), upper16fn, regValFn, std::move(current)); \
							 break; \
						 }
			X(IfJump)
			X(IfCall)
			X(CallIndirect)
			X(CallDirect)
			X(JumpDirect)
			X(JumpIndirect)
			X(ConditionalJumpDirect)
			X(ConditionalJumpIndirect)
#undef X
			default:
				throw iris::Problem("Undefined branch flag setup!");
		}
	}

template<CompareCombine compareOp> 
bool combine(bool newValue, bool existingValue) {
	switch (compareOp) {
		case CompareCombine::None:
			return newValue;
		case CompareCombine::And:
			return newValue && existingValue;
		case CompareCombine::Or:
			return newValue || existingValue;
		case CompareCombine::Xor:
			return newValue ^ existingValue;
		default:
			throw iris::Problem("Undefined combine operation");
	}
}

template<CompareStyle style>
bool compare(RegisterValue a, RegisterValue b) {
	switch (style) {
		case CompareStyle::Equals:
			return a == b;
		case CompareStyle::NotEquals:
			return a != b;
		case CompareStyle::LessThan:
			return a < b;
		case CompareStyle::LessThanOrEqualTo:
			return a <= b;
		case CompareStyle::GreaterThan:
			return a > b;
		case CompareStyle::GreaterThanOrEqualTo:
			return a >= b;
		default:
			throw iris::Problem("Undefined comparison style!");
	}
}


DefOp(Compare) {
	++getInstructionPointer();
	DecodedInstruction next(getCurrentCodeWord());
	switch (current.getCompareType()) {
#define combineOp(flag) \
		case CompareCombine:: flag : \
									 getConditionRegister() = combine<CompareCombine:: flag>(result, getConditionRegister()); \
		break;
#define X(type) \
		case CompareStyle:: type : { \
									   RegisterValue first = registerValue(next.getCompareRegister0()); \
									   RegisterValue second = current.getCompareImmediateFlag() ? next.getUpper() : registerValue(next.getCompareRegister1()); \
									   bool result = compare<CompareStyle:: type>(first, second); \
									   switch (current.getCompareCombineFlag()) { \
										   combineOp(None) \
										   combineOp(And) \
										   combineOp(Or) \
										   combineOp(Xor) \
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
#undef combineOp
		default:
			throw iris::Problem("illegal compare type!");
	}
}

DefOp(Return) {
	advanceIp = false;
	// jump to the link register
	getInstructionPointer() = getLinkRegister();
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
				std::cout.put(static_cast<char>(registerValue(current.getSystemArg0())));
				break;
			case SystemCalls::GetC:
				byte value;
				std::cin >> std::noskipws >> value;
				registerValue(current.getSystemArg0()) = static_cast<word>(value);
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
#include "def/iris17/ops.def"
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
		return iris::encodeBits<RegisterValue, word, bitmask32, 16>(RegisterValue(loadWord(address)), loadWord(address + 1));
	}
	void Core::storeRegisterValue(RegisterValue address, RegisterValue value) {
		storeWord(address, iris::decodeBits<RegisterValue, word, lower16Mask, 0>(value));
		storeWord(address + 1, iris::decodeBits<RegisterValue, word, upper16Mask, 16>(value));
	}

	dynamicop::EncodedInstruction
	dynamicop::encodeArithmetic() {
		auto first = encodeControl(0, type);
		first = encodeArithmeticFlagImmediate(first, Arithmetic.immediate);
		first = encodeArithmeticFlagType(first, Arithmetic.subType);
		first = encodeArithmeticDestination(first, Arithmetic.destination);
		if (Arithmetic.immediate) {
			first = encodeArithmeticImmediate(first, Arithmetic.immediateValue);
		} else {
			first = encodeArithmeticSource(first, Arithmetic.source);
		}
		return std::make_tuple(1, first, 0, 0);
	}

	dynamicop::EncodedInstruction
	dynamicop::encodeMove() {
		auto first = encodeControl(0, type);
		first = encodeMoveBitmask(first, Move.bitmask);
		first = encodeMoveRegister0(first, Move.register0);
		first = encodeMoveRegister1(first, Move.register1);
		return std::make_tuple(1, first, 0, 0);
	}

	dynamicop::EncodedInstruction
	dynamicop::encodeSwap() {
		return std::make_tuple(1, encodeSwapSource( encodeSwapDestination( encodeControl(0, type), Swap.dest), Swap.source), 0, 0);
	}

	dynamicop::EncodedInstruction
	dynamicop::encodeShift() {
		auto first = encodeControl(0, type);
		first = encodeShiftFlagImmediate(first, Shift.immediate);
		first = encodeShiftFlagLeft(first, Shift.shiftLeft);
		first = encodeShiftRegister0(first, Shift.register0);
		if (Shift.immediate) {
			first = encodeShiftImmediate(first, Shift.immediateValue);
		} else {
			first = encodeShiftRegister1(first, Shift.register1);
		}
		return std::make_tuple(1, first, 0, 0);
	}

	dynamicop::EncodedInstruction
	dynamicop::encodeSystem() {
		return std::make_tuple(1, encodeSystemArg0(encodeControl(0, type), System.arg0), 0, 0);
	}

	dynamicop::EncodedInstruction
	dynamicop::encodeCompare() {
		auto first = encodeControl(0, type);
		first = encodeCompareType(first, Compare.subType);
		first = encodeCompareCombineFlag(first, Compare.combineType);
		first = encodeCompareImmediateFlag(first, Compare.immediate);
		auto second = encodeCompareRegister0(0, Compare.register0);
		if (Compare.immediate) {
			second = encodeCompareImmediate(second, Compare.immediateValue);
		} else {
			second = encodeCompareRegister1(second, Compare.register1);
		}
		return std::make_tuple(2, first, second, 0);
	}
	
	dynamicop::EncodedInstruction
	dynamicop::encodeSet() {
		int count = instructionSizeFromImmediateMask(Set.bitmask);
		auto first = encodeControl(0, type);
		first = encodeSetBitmask(first, Set.bitmask);
		first = encodeSetDestination(first, Set.destination);
		// use the mask during encoding since we know how many words the
		// instruction is made up of
		auto maskedValue = getMask(Set.bitmask) & Set.immediate;
		auto second = static_cast<word>(maskedValue);
		auto third = static_cast<word>(maskedValue >> 16);
		return std::make_tuple(count, first, second, third);
	}

	dynamicop::EncodedInstruction
	dynamicop::encodeMemory() {
		auto first = encodeControl(0, type);
		first = encodeMemoryFlagType(first, Memory.subType);
		first = encodeMemoryFlagBitmask(first, Memory.bitmask);
		// the register and offset occupy the same space
		first = encodeMemoryOffset(first, Memory.offset);
		return std::make_tuple(1, first, 0, 0);
	}

	dynamicop::EncodedInstruction
	dynamicop::encode() {
		// always encode the type
		switch (type) {
			case Operation::Nop:
			case Operation::Return:
				return std::make_tuple(1, encodeControl(0, type), 0, 0);
			case Operation::Arithmetic:
				return encodeArithmetic();
			case Operation::Move:
				return encodeMove();
			case Operation::Swap:
				return encodeSwap();
			case Operation::Shift:
				return encodeShift();
			case Operation::SystemCall:
				return encodeSystem();
			case Operation::Compare:
				return encodeCompare();
			case Operation::Set:
				return encodeSet();
			case Operation::Memory:
				return encodeMemory();
		}
	}
	//int 
	//dynamicop::numberOfBytes() {
	//	switch (type) {
	//		case Operation::Memory:
	//		case Operation::Logical:
	//		case Operation::Branch:
	//			// TODO: this...
	//			return 3;
	//		default:
	//			throw iris::Problem("Illegal operation!");
	//	}
	//
	//}
#define X(title, mask, shift, type, post) \
	word encode ## title (word input, type value) { \
		return iris::encodeBits<word, type, mask, shift>(input, value); \
	}
#include "def/iris17/instruction.def"
#undef X

			static int instructionSizeFromImmediateMask(byte bitmask) {
				switch(bitmask) {
#define X(bits) case bits : return instructionSizeFromImmediateMask<bits>();
#include "def/iris17/bitmask4bit.def"
#undef X
					default:
						throw iris::Problem("illegal bitmask value!");
				}
			}
	RegisterValue
	getMask(byte bitmask) {
		switch (bitmask) {
#define X(bits) case bits : return mask<bits>();
#include "def/iris17/bitmask4bit.def"
#undef X
			
			default:
				throw iris::Problem("Illegal bitmask provided!");
		}
	}
}
