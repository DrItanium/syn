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
#define X(value) \
			setOperation<value>(std::move(current)); \
			break;
#include "def/iris17/bitmask8bit.def"
#undef X
			default:
				throw iris::Problem("Illegal set signature!");
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
		return iris::encodeBits<RegisterValue, word, bitmask32, 16>(RegisterValue(loadWord(address)), loadWord(address + 1));
	}
	void Core::storeRegisterValue(RegisterValue address, RegisterValue value) {
		storeWord(address, iris::decodeBits<RegisterValue, word, lower16Mask, 0>(value));
		storeWord(address + 1, iris::decodeBits<RegisterValue, word, upper16Mask, 16>(value));
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

	template<bool isConditional, bool ifForm, bool callForm, bool immediateForm>
	struct BranchFlagsEncoder {
		static constexpr byte flags = (static_cast<byte>(isConditional) << 3) | (static_cast<byte>(ifForm) << 2) | (static_cast<byte>(callForm) << 1) | static_cast<byte>(immediateForm);
	};
	typedef BranchFlagsEncoder<false, true, false, false> IfJump;
	typedef BranchFlagsEncoder<false, true, true, false> IfCall;

	typedef BranchFlagsEncoder<false, false, true, false> CallIndirect;
	typedef BranchFlagsEncoder<false, false, true, true> CallDirect;

	typedef BranchFlagsEncoder<false, false, false, true> JumpDirect;
	typedef BranchFlagsEncoder<false, false, false, false> JumpIndirect;

	typedef BranchFlagsEncoder<true, false, false, true> ConditionalJumpDirect;
	typedef BranchFlagsEncoder<true, false, false, false> ConditionalJumpIndirect;

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
	switch (current.getConditionalCompareType()) {
#define combineOp(flag) \
		case CompareCombine:: flag : \
									 getConditionRegister() = combine<CompareCombine:: flag>(result, getConditionRegister()); \
		break;
#define X(type) \
		case CompareStyle:: type : { \
									   RegisterValue first = registerValue(next.getConditionalRegister0()); \
									   RegisterValue second = current.getConditionalImmediateFlag() ? next.getUpper() : registerValue(next.getConditionalRegister1()); \
									   bool result = compare<CompareStyle:: type>(first, second); \
									   switch (current.getConditionalCombineFlag()) { \
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
}
