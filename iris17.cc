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
	Core* newCore() noexcept {
		return new Core();
	}
	constexpr RegisterValue encodeRegisterValue(byte a, byte b, byte c, byte d) noexcept {
		return iris::encodeUint32LE(a, b, c, d);
	}
	constexpr Word encodeWord(byte a, byte b) noexcept {
		return iris::encodeUint16LE(a, b);
	}
	void decodeWord(Word value, byte* storage) noexcept {
		return iris::decodeUint32LE(value, storage);
	}
	void decodeWord(RegisterValue value, byte* storage) noexcept {
		return iris::decodeInt32LE(value, storage);
	}

	Word decodeUpperHalf(RegisterValue value) noexcept {
		return iris::decodeBits<RegisterValue, Word, upper16Mask, 16>(value);
	}
	Word decodeLowerHalf(RegisterValue value) noexcept {
		return iris::decodeBits<RegisterValue, Word, lower16Mask, 16>(value);
	}

	constexpr RegisterValue encodeUpperHalf(RegisterValue value, Word upperHalf) noexcept {
		return iris::encodeBits<RegisterValue, Word, upper16Mask, 16>(value, upperHalf);
	}
	constexpr RegisterValue encodeLowerHalf(RegisterValue value, Word lowerHalf) noexcept {
		return iris::encodeBits<RegisterValue, Word, lower16Mask, 0>(value, lowerHalf);
	}

	constexpr RegisterValue encodeRegisterValue(Word upper, Word lower) noexcept {
		return encodeUpperHalf(encodeLowerHalf(0, lower), upper);
	}

	RegisterValue Core::retrieveImmediate(byte bitmask) noexcept {
		switch(bitmask) {
#define X(value) case value : return retrieveImmediate<value>(); 
#include "def/iris17/bitmask4bit.def"
#undef X
			default:
				throw iris::Problem("Illegal bitmask defined!");
		}
	}

	Core::Core() : memory(new Word[ArchitectureConstants::AddressMax]) { }
	Core::~Core() { }

	void Core::initialize() {
		// setup the default system handlers
		for (auto i = 0; i < ArchitectureConstants::MaxSystemCalls; ++i) {
			installSystemHandler(i, Core::defaultSystemHandler);
		}
		installSystemHandler(Core::DefaultHandlers::Terminate, Core::terminate);
		installSystemHandler(Core::DefaultHandlers::GetC, Core::getc);
		installSystemHandler(Core::DefaultHandlers::PutC, Core::putc);
	}

	void Core::defaultSystemHandler(Core* core, DecodedInstruction&& inst) {
		throw iris::Problem("Unimplemented system call!");
	}


	void Core::shutdown() { }

	template<typename T, int count>
		void populateContents(T* contents, std::istream& stream, std::function<T(byte*)> encode) {
			static char buf[sizeof(T)] = { 0 };
			for(int i = 0; i < count; ++i) {
				stream.read(buf, sizeof(T));
				contents[i] = encode((byte*)buf);
			}
		}
	template<typename T, int count>
		void populateContents(const std::shared_ptr<T>& contents, std::istream& stream, std::function<T(byte*)> encode) {
			static char buf[sizeof(T)] = { 0 };
			for (auto i = 0; i < count; ++i) {
				stream.read(buf, sizeof(T));
				contents.get()[i] = encode((byte*)buf);
			}
		}
	void Core::installprogram(std::istream& stream) {
		populateContents<RegisterValue, ArchitectureConstants::RegisterCount>(gpr, stream, [](byte* buf) { return iris::encodeUint32LE(buf); });
		populateContents<Word, ArchitectureConstants::AddressMax>(memory, stream, [](byte* buf) { return iris::encodeUint16LE(buf); });
	}

	template<typename T, int count>
		void dumpContents(T* contents, std::ostream& stream, std::function<void(T value, byte* buf)> decompose) {
			static byte buf[sizeof(T)];
			for (int i = 0; i < count; ++i) {
				decompose(contents[i], (byte*)buf);
				stream.write((char*)buf, sizeof(T));
			}
		}

	template<typename T, int count>
		void dumpContents(const std::shared_ptr<T>& contents, std::ostream& stream, std::function<void(T value, byte* buf)> decompose) {
			static byte buf[sizeof(T)];
			for (auto i = 0; i < count; ++i) {
				decompose(contents.get()[i], buf);
				stream.write((char*)buf, sizeof(T));
			}
		}

	void Core::dump(std::ostream& stream) {
		// save the registers
		dumpContents<RegisterValue, ArchitectureConstants::RegisterCount>(gpr, stream, iris::decodeUint32LE);
		dumpContents<Word, ArchitectureConstants::AddressMax>(memory, stream, iris::decodeUint16LE);
	}
	void Core::run() {
		while(execute) {
			cycle();
		}
	}
	void Core::cycle() {
		dispatch(std::move(DecodedInstruction(getCurrentCodeWord())));
		if (advanceIp) {
			incrementInstructionPointer();
		} else {
			// just re-enable it
			advanceIp = true;
		}
	}
    inline void mask24(RegisterValue& ref) noexcept {
        ref &= bitmask24;
    }
	void Core::incrementInstructionPointer() noexcept {
		++getInstructionPointer();
        mask24(getInstructionPointer());
	}
	void Core::incrementStackPointer() noexcept {
		++getStackPointer();
        mask24(getStackPointer());
	}

	void Core::decrementStackPointer() noexcept {
		--getStackPointer();
        mask24(getStackPointer());
	}

	void Core::dispatch(DecodedInstruction&& current) {
		auto tControl = current.getControl();
		if (tControl == Operation::Shift) {
			auto &destination = registerValue(current.getShiftRegister0());
			auto source = (current.getShiftFlagImmediate() ? static_cast<RegisterValue>(current.getShiftImmediate()) : registerValue(current.getShiftRegister1()));
			if (current.getShiftFlagLeft()) {
				destination <<= source;
			} else {
				destination >>= source;
			}
		} else if (tControl == Operation::Swap) {
			if (current.getSwapDestination() != current.getSwapSource()) {
				auto tmp = registerValue(current.getSwapDestination());
				registerValue(current.getSwapDestination()) = registerValue(current.getSwapSource());
				registerValue(current.getSwapSource()) = tmp;
			}
		} else if (tControl == Operation::Arithmetic) {
			switch (current.getArithmeticSignature()) {
#define X(value) case value: arithmeticOperation< value > (std::move(current)); break;
#include "def/iris17/bitmask4bit.def"
#undef X
				default:
					throw iris::Problem("Illegal Arithmetic Signature");
			}
		} else if (tControl == Operation::Logical) {
#define X(datum) \
			if (datum == current.getLogicalSignature()) { \
				using lflags = LogicalFlags<datum>; \
				if (lflags::immediate) { \
					if (lflags::immediateError) { \
						throw iris::Problem("Illegal bit set for immediate mode logicalOperation!"); \
					} else { \
						logicalImmediateOperation<lflags::immediateType, lflags::bitmask>(std::move(current)); \
						return; \
					} \
				} else { \
					if (lflags::indirectError) { \
						throw iris::Problem("Illegal bits set for indirect mode logicalOperation!"); \
					} else { \
						logicalIndirectOperation<lflags::indirectType>(std::move(current)); \
						return; \
					} \
				} \
			}
#include "def/iris17/bitmask8bit.def"
#undef X
			throw iris::Problem("Illegal logical signature!");
		} else if (tControl == Operation::Move) {
			auto &dest = registerValue(current.getMoveRegister0());
			switch (current.getMoveSignature()) {
#define X(value) case value : \
				if (MoveFlags< value >::isError) { \
					throw iris::Problem("Illegal move signature"); \
				} else { \
					dest = iris::decodeBits<RegisterValue, RegisterValue, mask<MoveFlags< value >::bitmask>(), 0>(registerValue(current.getMoveRegister1())); \
				}
#include "def/iris17/bitmask4bit.def"
#undef X
				default:
					throw iris::Problem("Illegal move signature!");
			}
		} else if (tControl == Operation::Set) {
			switch (current.getSetSignature()) {
#define X(value) case value: \
				registerValue<SetFlags<value>::destination>() = retrieveImmediate<SetFlags<value>::bitmask>(); \
				break;
#include "def/iris17/bitmask8bit.def"
#undef X
				default:
					std::stringstream stream;
					stream << "Illegal set signature 0x" << std::hex << static_cast<int>(current.getSetSignature()) << "\n";
					throw iris::Problem(stream.str());
			}
		} else if (tControl == Operation::Memory) {
#define X(value) \
			if (value == current.getMemorySignature()) { \
				if (!MemoryFlags<value>::errorState) { \
					memoryOperation<MemoryFlags<value>::type, MemoryFlags<value>::bitmask, MemoryFlags<value>::indirect>(std::move(current)); \
					return; \
				} else { \
					throw iris::Problem("Undefined bits set in memory operation!"); \
				} \
			}
#include "def/iris17/bitmask8bit.def"
#undef X
			throw iris::Problem("Illegal memory signature!");
		} else if (tControl == Operation::Branch) {
			auto instFlags = current.getBranchFlags();
			if (instFlags == IfJump::flags) {
				branchSpecificOperation<IfJump::flags>(std::move(current));
			} else if(instFlags == CallIndirect::flags) {
				branchSpecificOperation<CallIndirect::flags>(std::move(current));
			} else if (instFlags == CallDirect::flags) {
				branchSpecificOperation<CallDirect::flags>(std::move(current));
			} else if(instFlags == JumpIndirect::flags) {
				branchSpecificOperation<JumpIndirect::flags>(std::move(current));
			} else if (instFlags == JumpDirect::flags) {
				branchSpecificOperation<JumpDirect::flags>(std::move(current));
			} else if(instFlags == ConditionalJumpIndirect::flags) {
				branchSpecificOperation<ConditionalJumpIndirect::flags>(std::move(current));
			} else if (instFlags == ConditionalJumpDirect::flags) {
				branchSpecificOperation<ConditionalJumpDirect::flags>(std::move(current));
			} else {
				throw iris::Problem("Undefined branch flag setup!");
			}
		} else if (tControl == Operation::Compare) {
			incrementInstructionPointer();
			DecodedInstruction next(getCurrentCodeWord());
			auto first = registerValue(next.getCompareRegister0());
			auto second = current.getCompareImmediateFlag() ? next.getUpper() : registerValue(next.getCompareRegister1());
			auto result = false;
			auto compareType = current.getCompareType();
			if (compareType == CompareStyle::Equals) {
				result = iris::eq(first, second);
			} else if (compareType == CompareStyle::NotEquals) {
				result = iris::neq(first, second);
			} else if (compareType == CompareStyle::LessThan) {
				result = iris::lt(first, second);
			} else if (compareType == CompareStyle::GreaterThan) {
				result = iris::gt(first, second);
			} else if (compareType == CompareStyle::LessThanOrEqualTo) {
				result = iris::le(first, second);
			} else if (compareType == CompareStyle::GreaterThanOrEqualTo) {
				result = iris::ge(first, second);
			} else {
				throw iris::Problem("illegal compare type!");
			}
			auto combineType = current.getCompareCombineFlag();
			if (combineType == CompareCombine::None) {
				getConditionRegister() = result;
			} else if (combineType == CompareCombine::And) {
				getConditionRegister() &= result;
			} else if (combineType == CompareCombine::Or) {
				getConditionRegister() |= result;
			} else if (combineType == CompareCombine::Xor) {
				getConditionRegister() ^= result;
			} else {
				throw iris::Problem("Illegal Compare Combine Operation");
			}
		} else if (tControl == Operation::SystemCall) {
			if (getAddressRegister() >= ArchitectureConstants::MaxSystemCalls) {
				throw iris::Problem("ERROR: system call index out of range!");
			} else {
				systemHandlers[getAddressRegister()](this, std::move(current));
			}
        } else if (tControl == Operation::Complex) {
            complexOperation(std::move(current));
		} else {
			std::stringstream str;
			str << "Illegal instruction " << std::hex << static_cast<byte>(current.getControl());
			execute = false;
			throw iris::Problem(str.str());
		}
	}

	void Core::complexOperation(DecodedInstruction&& inst) {
		auto type = inst.getComplexSubClass();
		if (type == ComplexSubTypes::Encoding) {
			encodingOperation(std::move(inst));
		} else {
			throw iris::Problem("Undefined complex subtype!");
		}
	}
    void Core::encodingOperation(DecodedInstruction&& inst) {
        switch (inst.getComplexClassEncoding_Type()) {
            case EncodingOperation::Decode:
                getValueRegister() = (getAddressRegister() & getMaskRegister()) >> getShiftRegister();
                break;
            case EncodingOperation::Encode:
                getAddressRegister() = (getAddressRegister() & ~getMaskRegister()) | ((getValueRegister() << getShiftRegister()) & getMaskRegister());
                break;
            case EncodingOperation::BitSet:
                // use the shift register as the field select
                getConditionRegister() = static_cast<RegisterValue>(((getAddressRegister() >> getFieldRegister()) & 0x1) == 1);
                break;
            case EncodingOperation::BitUnset:
                getConditionRegister() = static_cast<RegisterValue>(((getAddressRegister() >> getFieldRegister()) & 0x1) != 1);
                break;
            default:
                throw iris::Problem("Illegal complex encoding operation defined!");
        }
    }

	void Core::terminate(Core* core, DecodedInstruction&& inst) {
		core->execute = false;
		core->advanceIp = false;
	}

	void Core::putc(Core* core, DecodedInstruction&& current) {
		std::cout.put(static_cast<char>(core->registerValue(current.getSystemArg0())));
	}
	void Core::getc(Core* core, DecodedInstruction&& current) {
		byte value = 0;
		std::cin >> std::noskipws >> value;
		core->registerValue(current.getSystemArg0()) = static_cast<Word>(value);
	}


	void Core::link(std::istream& input) {
		// we have some more data to read through
		// two address system, 1 RegisterValue -> address, 1 Word -> value
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
	Word Core::getCurrentCodeWord() noexcept {
		return memory.get()[getInstructionPointer()];
	}
	void Core::storeWord(RegisterValue address, Word value) {
		if (address >= ArchitectureConstants::AddressMax) {
			throw iris::Problem("Attempted to write outside of memory!");
		} else {
			memory.get()[address] = value;
		}
	}
	Word Core::loadWord(RegisterValue address) {
		if (address >= ArchitectureConstants::AddressMax) {
			throw iris::Problem("Attempted to read from outside of memory!");
		} else {
			return memory.get()[address];
		}
	}
	RegisterValue Core::loadRegisterValue(RegisterValue address) {
		return iris::encodeBits<RegisterValue, Word, bitmask32, 16>(static_cast<RegisterValue>(loadWord(address)), loadWord(address + 1));
	}
	void Core::storeRegisterValue(RegisterValue address, RegisterValue value) {
		storeWord(address, iris::decodeBits<RegisterValue, Word, lower16Mask, 0>(value));
		storeWord(address + 1, iris::decodeBits<RegisterValue, Word, upper16Mask, 16>(value));
	}

	std::shared_ptr<Word> Core::getMemory() {
		return memory;
	}

	void Core::installSystemHandler(byte index, Core::SystemFunction func) {
		if (index >= ArchitectureConstants::MaxSystemCalls) {
			throw iris::Problem("Can't install to out of range system handler index!");
		} else {
			systemHandlers[index] = func;
		}
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeArithmetic() {
		auto first = encodeControl(0, type);
		first = encodeArithmeticFlagImmediate(first, immediate);
		first = encodeArithmeticFlagType(first, static_cast<ArithmeticOps>(subType));
		first = encodeArithmeticDestination(first, arg0);
		first = immediate ? encodeArithmeticImmediate(first, arg1) : encodeArithmeticSource(first, arg1);
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
		first = immediate ? encodeShiftImmediate(first, arg1) : encodeShiftRegister1(first, arg1);
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeSystemCall() {
		return std::make_tuple(1, encodeSystemArg0(encodeControl(0, type), arg0), 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeCompare() {
		auto first = encodeControl(0, type);
		first = encodeCompareType(first, static_cast<CompareStyle>(subType));
		first = encodeCompareCombineFlag(first, combineType);
		first = encodeCompareImmediateFlag(first, immediate);
		auto second = encodeCompareRegister0(0, arg0);
		second = immediate ? encodeCompareImmediate(second, arg1) : encodeCompareRegister1(second, arg1);
		return std::make_tuple(2, first, second, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeSet() {
		int count = instructionSizeFromImmediateMask(bitmask);
		auto first = encodeControl(0, type);
		first = encodeSetBitmask(first, bitmask);
		first = encodeSetDestination(first, arg0);
		// use the mask during encoding since we know how many Words the
		// instruction is made up of
		auto maskedValue = getMask(bitmask) & fullImmediate;
		auto second = static_cast<Word>(maskedValue);
		auto third = static_cast<Word>(maskedValue >> 16);
		return std::make_tuple(count, first, second, third);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeMemory() {
		auto first = encodeControl(0, type);
		first = encodeMemoryFlagType(first, static_cast<MemoryOperation>(subType));
		first = encodeMemoryFlagBitmask(first, bitmask);
		first = encodeMemoryFlagIndirect(first, indirect);
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
			auto second = static_cast<Word>(maskedImmediate);
			auto third = static_cast<Word>(maskedImmediate >> 16);
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
				auto second = static_cast<Word>(fullImmediate >> 8);
				return std::make_tuple(2, first, second, 0);
			} else {
				first = encodeBranchIndirectDestination(first, arg0);
				return std::make_tuple(1, first, 0, 0);
			}
		}
	}
    InstructionEncoder::Encoding InstructionEncoder::encodeComplex() {
        auto sType = static_cast<ComplexSubTypes>(subType);
        auto first = encodeControl(0, type);
        first = encodeComplexSubClass(first, sType);
        if (sType == ComplexSubTypes::Encoding) {
            // right now it is a single word
            first = encodeComplexClassEncoding_Type(first, static_cast<EncodingOperation>(bitmask));
            return std::make_tuple(1, first, 0, 0);
        } else {
            throw iris::Problem("Attempted to encode an unsupported value as a complex type!");
        }
    }

	InstructionEncoder::Encoding InstructionEncoder::encode() {
		// always encode the type
#define DefEnum(a, b)
#define EndDefEnum(a, b, c)
#define EnumEntry(compareType) if (type == Operation:: compareType) { return encode ## compareType () ; }
#include "def/iris17/ops.def"
#undef DefEnum
#undef EndDefEnum
#undef EnumEntry
		throw iris::Problem("Illegal type to encode!");
	}

	int instructionSizeFromImmediateMask(byte bitmask) {

#define X(bits) if (bitmask == bits) { return instructionSizeFromImmediateMask<bits>(); }
#include "def/iris17/bitmask4bit.def"
#undef X
		throw iris::Problem("Illegal bitmask provided!");
	}
	RegisterValue getMask(byte bitmask) {
#define X(bits) if (bitmask == bits) { return SetBitmaskToWordMask<bits>::mask; }
#include "def/iris17/bitmask4bit.def"
#undef X
		throw iris::Problem("Illegal bitmask provided!");
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
		indirect = false;
	}
}
