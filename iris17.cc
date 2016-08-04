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

	DecodedInstruction::DecodedInstruction(raw_instruction input) : _rawValue(input) { }

	raw_instruction DecodedInstruction::getRawValue() const {
		return _rawValue;
	}

#define X(title, mask, shift, type, is_register, post) \
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
	template<byte bitmask>
	constexpr RegisterValue mask() { return SetBitmaskToWordMask<bitmask>::mask; }
	template<byte bitmask>
	constexpr word lowerMask() { return SetBitmaskToWordMask<bitmask>::lowerMask; }
	template<byte bitmask>
	constexpr word upperMask() { return SetBitmaskToWordMask<bitmask>::upperMask; }
	template<byte bitmask>
	constexpr bool readLower() { return SetBitmaskToWordMask<bitmask>::readLower; }
	template<byte bitmask>
	constexpr bool readUpper() { return SetBitmaskToWordMask<bitmask>::readUpper; }

	constexpr RegisterValue bitmask32 = mask<0b1111>();
	constexpr RegisterValue bitmask24 = mask<0b0111>();
	constexpr RegisterValue upper16Mask = mask<0b1100>();
	constexpr RegisterValue lower16Mask = mask<0b0011>();

#define DefOp(title) \
	template<> \
	void Core::operation<Operation:: title>(DecodedInstruction&& current) 
	
	DefOp(Nop) { 
	}

	DefOp(Shift) {
		auto destination = registerValue(current.getShiftRegister0());
		RegisterValue source = (current.getShiftFlagImmediate() ? static_cast<RegisterValue>(current.getShiftImmediate()) : registerValue(current.getShiftRegister1()));
		if (current.getShiftFlagLeft()) {
			destination = destination << source;
		} else {
			destination = destination >> source;
		}
	}

	DefOp(Logical) {
		if (current.getLogicalFlagImmediate()) {
			auto dest = registerValue(current.getLogicalImmediateDestination());
			RegisterValue immediate = 0;
			switch (current.getLogicalFlagImmediateMask()) {
#define X(value) \
				case value : { \
								 RegisterValue lower = 0; \
								 RegisterValue upper = 0; \
								if (readLower<value>()) { \
									++getInstructionPointer(); \
									lower = RegisterValue(getCurrentCodeWord()); \
								} \
								if (readUpper<value>()) { \
									++getInstructionPointer(); \
									upper = (RegisterValue(getCurrentCodeWord()) << 16) & lower16Mask; \
								} \
								immediate = upper | lower; \
								break; \
							 }
#include "def/iris17/bitmask4bit.def"
#undef X
				default:
					throw iris::Problem("Illegal bit mask provided!");
			}
			switch (current.getLogicalFlagImmediateType()) {
				case ImmediateLogicalOps::And:
					dest = dest & immediate;
					break;
				case ImmediateLogicalOps::Or:
					dest = dest | immediate;
					break;
				case ImmediateLogicalOps::Xor:
					dest = dest ^ immediate;
					break;
				case ImmediateLogicalOps::Nand:
					dest = ~(dest & immediate);
					break;
				default:
					throw iris::Problem("Illegal immediate logical op!");
			}
		} else {
			switch (current.getLogicalFlagType()) {
				case LogicalOps::And:
					registerValue(current.getLogicalRegister0()) = (registerValue(current.getLogicalRegister0()) & registerValue(current.getLogicalRegister1()));
					break;
				case LogicalOps::Or:
					registerValue(current.getLogicalRegister0()) = (registerValue(current.getLogicalRegister0()) | registerValue(current.getLogicalRegister1()));
					break;
				case LogicalOps::Xor:
					registerValue(current.getLogicalRegister0()) = (registerValue(current.getLogicalRegister0()) ^ registerValue(current.getLogicalRegister1()));
					break;
				case LogicalOps::Nand:
					registerValue(current.getLogicalRegister0()) = ~(registerValue(current.getLogicalRegister0()) & registerValue(current.getLogicalRegister1()));
					break;
				case LogicalOps::Not:
					registerValue(current.getLogicalRegister0()) = ~registerValue(current.getLogicalRegister0());
					break;
				default:
					throw iris::Problem("Illegal logical op!");
			}
		}
	}
	DefOp(Arithmetic) {
		auto destination = registerValue(current.getArithmeticDestination());
		RegisterValue source = (current.getArithmeticFlagImmediate() ? current.getArithmeticImmediate() : registerValue(current.getArithmeticSource()));
		switch (current.getArithmeticFlagType()) {
#define X(title, operation) \
			case ArithmeticOps:: title : { \
											 destination = destination operation source; \
											 break; \
										 }
#define Y(title, operation) \
			case ArithmeticOps:: title : { \
											 if (source == 0) { \
												 throw iris::Problem("Denominator is zero!"); \
											 } else { \
												destination = destination operation source; \
											 } \
										 break; \
										 }
#include "def/iris17/arithmetic_ops.def"
#undef X
#undef Y
			default:
				throw iris::Problem("Illegal Arithmetic Operation");
		}
	}

	DefOp(Move)  {
		switch (current.getMoveBitmask()) {
#define X(value) \
			case value : { \
							 registerValue(current.getMoveRegister0()) = registerValue(current.getMoveRegister1()) & mask<value>(); \
							 break; \
						 }
#include "def/iris17/bitmask4bit.def"
#undef X
			default: 
				throw iris::Problem("Illegal bitmask!");
		}
	}

	DefOp(Swap) {
		RegisterValue tmp = registerValue(current.getSwapDestination());
		registerValue(current.getSwapDestination()) = registerValue(current.getSwapSource());
		registerValue(current.getSwapSource()) = tmp;
	}

    DefOp(Set) {
		switch (current.getSetBitmask()) {
#define X(value) \
			case value: \
			{ \
				RegisterValue lower = 0; \
				RegisterValue upper = 0; \
				if ( readLower<value>() ) { \
					++getInstructionPointer(); \
					lower = getCurrentCodeWord(); \
				} \
				if (readUpper<value>() ) { \
					++getInstructionPointer(); \
					upper = RegisterValue(getCurrentCodeWord()) << 16; \
				} \
				registerValue(current.getSetDestination()) = mask<value>() & (lower | upper); \
				break; \
			}
#include "def/iris17/bitmask4bit.def"
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
		return iris::encodeBits<RegisterValue, word, bitmask32, 16>(RegisterValue(loadWord(address)), loadWord(address + 1));
	}
	void Core::storeRegisterValue(RegisterValue address, RegisterValue value) {
		storeWord(address, iris::decodeBits<RegisterValue, word, lower16Mask, 0>(value));
		storeWord(address + 1, iris::decodeBits<RegisterValue, word, upper16Mask, 16>(value));
	}
	template<byte bitmask> 
	void loadOperation(RegisterValue& value, RegisterValue address, std::function<word(RegisterValue)> loadWord) {
		// use the destination field of the instruction to denote offset, thus we need
		// to use the Address and Value registers
		RegisterValue lower = readLower<bitmask>() ? loadWord(address) : 0;
		RegisterValue upper = readUpper<bitmask>() ? (static_cast<RegisterValue>(loadWord(address + 1)) << 16) : 0;
		value = mask<bitmask>() & (lower | upper);
	}
	template<byte bitmask>
	void loadMergeOperation(RegisterValue &value, RegisterValue address, std::function<word(RegisterValue)> loadWord) {
		// 0b1101 implies that we have to leave 0x0000FF00 around in the
		// value register since it isn't necessary
		auto constexpr cMask = mask<bitmask>();
		// use the destination field of the instruction to denote offset, thus we need
		// to use the Address and Value registers
		RegisterValue lower = readLower<bitmask>() ? loadWord(address) : 0;
		RegisterValue upper = readUpper<bitmask>() ? (static_cast<RegisterValue>(loadWord(address + 1)) << 16) : 0;
		value = (cMask & (lower | upper)) | (value & ~cMask);
	}
	template<byte bitmask>
	void storeOperation(RegisterValue& value, RegisterValue address, std::function<word(RegisterValue)> loadWord, std::function<void(RegisterValue, word)> storeWord) {
		if (readLower<bitmask>()) { 
			auto constexpr lmask = lowerMask<bitmask>();
			word lower = lmask & iris::decodeBits<RegisterValue, word, lower16Mask, 0>(value); 
			auto loader = loadWord(address) & ~lmask;
			storeWord(address, lower | loader); 
		} 
		if (readUpper<bitmask>()) { 
			auto constexpr umask = upperMask<bitmask>();
			word upper = umask & iris::decodeBits<RegisterValue, word, upper16Mask, 16>(value);
			auto loader = loadWord(address + 1) & ~umask;
			storeWord(address + 1, upper | loader); 
		} 
	}
	template<byte bitmask>
	void pushOperation(RegisterValue& stackPointer, RegisterValue& pushToStack, std::function<void(RegisterValue, word)> storeWord) {
		// read backwards because the stack grows upward towards zero
		if (readUpper<bitmask>()) {
			--stackPointer;
			stackPointer &= bitmask24;
			word upper = upperMask<bitmask>() & iris::decodeBits<RegisterValue, word, upper16Mask, 16>(pushToStack);
			storeWord(stackPointer, upper);
		}
		if (readLower<bitmask>()) {
			--stackPointer;
			stackPointer &= bitmask24;
			word lower = lowerMask<bitmask>() & iris::decodeBits<RegisterValue, word, lower16Mask, 0>(pushToStack);
			storeWord(stackPointer, lower);
		}
	}

	template<byte bitmask>
	void popOperation(RegisterValue& stackPointer, RegisterValue& storage, std::function<word(RegisterValue)> loadWord) {
		RegisterValue lower = 0; 
		RegisterValue upper = 0; 
		if (readLower<bitmask>()) { 
			lower = lowerMask<bitmask>() & loadWord(stackPointer); 
			++stackPointer; 
			stackPointer &= bitmask24; 
		} 
		if (readUpper<bitmask>()) { 
			upper = upperMask<bitmask>() & loadWord(stackPointer);
			++stackPointer; 
			stackPointer &= bitmask24; 
		} 
		storage = iris::encodeBits<word, RegisterValue, upper16Mask, 16>(iris::encodeBits<word, RegisterValue, lower16Mask, 0>(RegisterValue(0), lower), upper); 
	}
	DefOp(Memory) {
		auto loadWordFn = [this](RegisterValue address) { return loadWord(address); };
		auto storeWordFn = [this](RegisterValue address, word value) { storeWord(address, value); };
		switch (current.getMemoryFlagBitmask()) {
#define X(value) \
			case value : { \
							 switch (current.getMemoryFlagType()) { \
								 case MemoryOperation::Load: \
								 								loadOperation<value>(getValueRegister(), getAddressRegister() + current.getMemoryOffset(), loadWordFn); \
								 break; \
								 case MemoryOperation::LoadMerge: \
																  loadMergeOperation<value>(getValueRegister(), getAddressRegister() + current.getMemoryOffset(), loadWordFn); \
								 break; \
								 case MemoryOperation::Store: \
															  storeOperation<value>(getValueRegister(), getAddressRegister() + current.getMemoryOffset(), loadWordFn, storeWordFn); \
								 break; \
								 case MemoryOperation::Push: \
															 pushOperation<value>(getStackPointer(), registerValue(current.getMemoryRegister()), storeWordFn); \
								 break; \
								 case MemoryOperation::Pop: \
															popOperation<value>(getStackPointer(), registerValue(current.getMemoryRegister()), loadWordFn); \
								 break; \
								default: \
										 throw iris::Problem("Illegal memory operation type!"); \
							 } \
							 break; \
						 }
#include "def/iris17/bitmask4bit.def"
#undef X
			default:
				throw iris::Problem("Illegal bitmask!");
		}
	}

	template<byte value>
	struct BranchFlags {
		static constexpr bool isIf = static_cast<bool>(value & 0b0001);
		static constexpr bool isCall = static_cast<bool>((value & 0b0010) >> 1);
		static constexpr bool isImmediate = static_cast<bool>((value & 0b0100) >> 2);
		static constexpr bool isConditional = static_cast<bool>((value & 0b1000) >> 3);
	};
	template<byte flags>
	bool branchSpecificOperation(RegisterValue& ip, RegisterValue& linkRegister, RegisterValue& cond, std::function<RegisterValue()> getUpper16, std::function<RegisterValue&(byte)> registerValue, DecodedInstruction&& current) {
		bool advanceIp = true;
		if (BranchFlags<flags>::isIf) {
			// if instruction
			advanceIp = false;
			if (BranchFlags<flags>::isCall) {
				linkRegister = ip + 1;
				if (linkRegister > bitmask24) {
					linkRegister &= bitmask24;
				}
			} 
			ip = bitmask24 & ((cond != 0) ? registerValue(current.getBranchIfOnTrue()) : registerValue(current.getBranchIfOnFalse())); 
		} else if (BranchFlags<flags>::isCall) {
			// call instruction
			advanceIp = false;
			// determine next
			linkRegister = BranchFlags<flags>::isImmediate ? ip + 2 : ip + 1;
			if (linkRegister > bitmask24) {
				linkRegister &= bitmask24; // make sure that we aren't over the memory setup
			}
			if (BranchFlags<flags>::isImmediate) {
				++ip;
				// make a 24 bit number
				ip = bitmask24 & ((static_cast<RegisterValue>(current.getUpper())) | (getUpper16() << 8));
			} else {
				ip = bitmask24 & registerValue(current.getBranchIndirectDestination());
			}
		} else {
			// jump instruction
			if (BranchFlags<flags>::isImmediate) {
				++ip;
				if (BranchFlags<flags>::isConditional) {
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
				if (BranchFlags<flags>::isConditional) {
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
									   RegisterValue first = registerValue(next.getConditionalRegister0()); \
									   RegisterValue second = current.getConditionalImmediateFlag() ? next.getUpper() : registerValue(next.getConditionalRegister1()); \
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
