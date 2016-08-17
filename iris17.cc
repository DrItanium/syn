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
		static char buf[sizeof(T)] = { 0 };
		for(int i = 0; i < count; ++i) {
			stream.read(buf, sizeof(T));
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

	DefOp(Shift) {
		auto destination = registerValue(current.getShiftRegister0());
		auto source = (current.getShiftFlagImmediate() ? static_cast<RegisterValue>(current.getShiftImmediate()) : registerValue(current.getShiftRegister1()));
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
#define X(value) case value: setOperation<value>(std::move(current)); break;
#include "def/iris17/bitmask8bit.def"
#undef X
			default: {
						 std::stringstream stream;
						 stream << "Illegal set signature 0x" << std::hex << static_cast<int>(current.getSetSignature()) << "\n";
						 auto str = stream.str();
						 throw iris::Problem(str);
					 }
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
		using decodedFlags = BranchFlagsDecoder<flags>;
		bool advanceIp = true;
		if (decodedFlags::isIf) {
			// if instruction
			advanceIp = false;
			if (decodedFlags::isCall) {
				linkRegister = ip + 1;
				if (linkRegister > bitmask24) {
					linkRegister &= bitmask24;
				}
			}
			ip = bitmask24 & ((cond != 0) ? registerValue(current.getBranchIfOnTrue()) : registerValue(current.getBranchIfOnFalse()));
		} else if (decodedFlags::isCall) {
			// call instruction
			advanceIp = false;
			// determine next
			linkRegister = decodedFlags::isImmediate ? ip + 2 : ip + 1;
			if (linkRegister > bitmask24) {
				linkRegister &= bitmask24; // make sure that we aren't over the memory setup
			}
			if (decodedFlags::isImmediate) {
				++ip;
				// make a 24 bit number
				auto bottom = static_cast<RegisterValue>(current.getUpper());
				auto upper = getUpper16() << 8;
				ip = bitmask24 & (upper | bottom);
			} else {
				ip = bitmask24 & registerValue(current.getBranchIndirectDestination());
			}
		} else {
			// jump instruction
			if (decodedFlags::isImmediate) {
				++ip;
				if ((decodedFlags::isConditional && cond != 0) || !decodedFlags::isConditional) {
					advanceIp = false;
					auto bottom = current.getUpper();
					auto upper = getUpper16() << 8;
					ip = bitmask24 & (upper | bottom);
				}
			}  else {
				if ((decodedFlags::isConditional && cond != 0) || !decodedFlags::isConditional) {
						advanceIp = false;
						ip = bitmask24 & registerValue(current.getBranchIndirectDestination());
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
	//std::cout << "Compare Operation" << std::endl;
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
		switch(current.getControl()) {
#define X(type) \
			case Operation:: type : \
				operation<Operation:: type>(std::move(current)); \
			break;
#include "def/iris17/ops.def"
#undef X
			default:
				std::stringstream str;
				str << "Illegal instruction " << std::hex << static_cast<byte>(current.getControl());
				execute = false;
				throw iris::Problem(str.str());
		}
	}

	void Core::link(std::istream& input) {
		// we have some more data to read through
		// two address system, 1 RegisterValue -> address, 1 word -> value
		static constexpr int bufSize = 8;
		char buf[bufSize] = { 0 };
		for(int lineNumber = 0; input.good(); ++lineNumber) {
			input.read(buf, bufSize);
			if (input.gcount() == 0) {
				break;
			} else if (input.gcount() != bufSize) {
				throw iris::Problem("unaligned object file found");
			} else {
				// use the first byte to determine what sort of installation
				// should occur
				switch (buf[0]) {
					case 0: // memory value
						storeWord(encodeRegisterValue(buf[2], buf[3], buf[4], buf[5]), encodeWord(buf[6], buf[7]));
						break;
					case 1: // register value
						gpr[static_cast<byte>(buf[1])] = encodeRegisterValue(buf[2], buf[3], buf[4], buf[5]);
						break;
					default:
						throw iris::Problem("undefined link class!");
				}
			}
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

	InstructionEncoder::Encoding InstructionEncoder::encodeArithmetic() {
		auto first = encodeControl(0, type);
		first = encodeArithmeticFlagImmediate(first, immediate);
		first = encodeArithmeticFlagType(first, static_cast<ArithmeticOps>(subType));
		first = encodeArithmeticDestination(first, arg0);
		if (immediate) {
			first = encodeArithmeticImmediate(first, arg1);
		} else {
			first = encodeArithmeticSource(first, arg1);
		}
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeMove() {
		auto first = encodeControl(0, type);
		first = encodeMoveBitmask(first, bitmask);
		first = encodeMoveRegister0(first, arg0);
		first = encodeMoveRegister1(first, arg1);
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeSwap() {
		return std::make_tuple(1, encodeSwapSource( encodeSwapDestination( encodeControl(0, type), arg0), arg1), 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeShift() {
		auto first = encodeControl(0, type);
		first = encodeShiftFlagImmediate(first, immediate);
		first = encodeShiftFlagLeft(first, shiftLeft);
		first = encodeShiftRegister0(first, arg0);
		if (immediate) {
			first = encodeShiftImmediate(first, arg1);
		} else {
			first = encodeShiftRegister1(first, arg1);
		}
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeSystem() {
		return std::make_tuple(1, encodeSystemArg0(encodeControl(0, type), arg0), 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeCompare() {
		auto first = encodeControl(0, type);
		first = encodeCompareType(first, static_cast<CompareStyle>(subType));
		first = encodeCompareCombineFlag(first, combineType);
		first = encodeCompareImmediateFlag(first, immediate);
		auto second = encodeCompareRegister0(0, arg0);
		if (immediate) {
			second = encodeCompareImmediate(second, arg1);
		} else {
			second = encodeCompareRegister1(second, arg1);
		}
		return std::make_tuple(2, first, second, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeSet() {
		int count = instructionSizeFromImmediateMask(bitmask);
		auto first = encodeControl(0, type);
		first = encodeSetBitmask(first, bitmask);
		first = encodeSetDestination(first, arg0);
		// use the mask during encoding since we know how many words the
		// instruction is made up of
		auto maskedValue = getMask(bitmask) & fullImmediate;
		auto second = static_cast<word>(maskedValue);
		auto third = static_cast<word>(maskedValue >> 16);
		return std::make_tuple(count, first, second, third);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeMemory() {
		auto first = encodeControl(0, type);
		first = encodeMemoryFlagType(first, static_cast<MemoryOperation>(subType));
		first = encodeMemoryFlagBitmask(first, bitmask);
		// the register and offset occupy the same space
		first = encodeMemoryOffset(first, arg0);
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeLogical() {
		auto first = encodeControl(0, type);
		first = encodeLogicalFlagImmediate(first, immediate);
		if (immediate) {
			first = encodeLogicalFlagImmediateType(first, static_cast<ImmediateLogicalOps>(subType));
			first = encodeLogicalFlagImmediateMask(first, bitmask);
			first = encodeLogicalImmediateDestination(first, arg0);
			auto maskedImmediate = getMask(bitmask) & fullImmediate;
			auto second = static_cast<word>(maskedImmediate);
			auto third = static_cast<word>(maskedImmediate >> 16);
			return std::make_tuple(instructionSizeFromImmediateMask(bitmask), first, second, third);
		} else {
			first = encodeLogicalFlagType(first, static_cast<LogicalOps>(subType));
			first = encodeLogicalRegister0(first, arg0);
			first = encodeLogicalRegister1(first, arg1);
			return std::make_tuple(1, first, 0, 0);
		}
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeBranch() {
		auto first = encodeControl(0, type);
		first = encodeBranchFlagIsConditional(first, isConditional);
		first = encodeBranchFlagIsIfForm(first, isIf);
		first = encodeBranchFlagIsImmediate(first, immediate);
		first = encodeBranchFlagIsCallForm(first, isCall);
		if (isIf) {
			first = encodeBranchIfOnTrue(first, arg0);
			first = encodeBranchIfOnFalse(first, arg1);
			return std::make_tuple(1, first, 0, 0);
		} else {
			if (immediate) {
				// encode the 24-bit number
				first = encodeUpper(first, static_cast<byte>(fullImmediate));
				auto second = static_cast<word>(fullImmediate >> 8);
				return std::make_tuple(2, first, second, 0);
			} else {
				first = encodeBranchIndirectDestination(first, arg0);
				return std::make_tuple(1, first, 0, 0);
			}
		}
	}

	InstructionEncoder::Encoding InstructionEncoder::encode() {
		// always encode the type
		switch (type) {
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
			case Operation::Logical:
				return encodeLogical();
			case Operation::Branch:
				return encodeBranch();
			default:
				throw iris::Problem("Illegal type to encode!");
		}
	}
#define X(title, mask, shift, type, post) \
	word encode ## title (word input, type value) { \
		return iris::encodeBits<word, type, mask, shift>(input, value); \
	}
#include "def/iris17/instruction.def"
#undef X

	int instructionSizeFromImmediateMask(byte bitmask) {
		switch(bitmask) {
#define X(bits) case bits : return instructionSizeFromImmediateMask<bits>();
#include "def/iris17/bitmask4bit.def"
#undef X
			default:
				throw iris::Problem("illegal bitmask value!");
		}
	}
	RegisterValue getMask(byte bitmask) {
		switch (bitmask) {
#define X(bits) case bits : return SetBitmaskToWordMask<bits>::mask;
#include "def/iris17/bitmask4bit.def"
#undef X

			default:
				throw iris::Problem("Illegal bitmask provided!");
		}
	}
	int InstructionEncoder::numWords() {
		return std::get<0>(encode());
	}
	void InstructionEncoder::clear() {
		currentLine = 0;
		address = 0;
		type = Operation::Memory;
		immediate = false;
		shiftLeft = false;
		isIf = false;
		isCall = false;
		isConditional = false;
		bitmask = 0b0000;
		arg0 = 0;
		arg1 = 0;
		isLabel = false;
		labelValue.clear();
		subType = 0;
		combineType = CompareCombine::Xor;
		fullImmediate = 0;
	}
}
