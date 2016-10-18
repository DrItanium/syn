#include "iris19.h"
#include <functional>
#include <sstream>
#include "Problem.h"
#include <utility>

namespace iris19 {
	/*
	 * Iris19 is a variable length encoding 16 bit architecture.
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
		return iris::decodeBits<RegisterValue, Word, upper32Mask, 32>(value);
	}
	Word decodeLowerHalf(RegisterValue value) noexcept {
		return iris::decodeBits<RegisterValue, Word, lower32Mask, 0>(value);
	}

	constexpr RegisterValue encodeUpperHalf(RegisterValue value, Word upperHalf) noexcept {
		return iris::encodeBits<RegisterValue, Word, upper32Mask, 32>(value, upperHalf);
	}
	constexpr RegisterValue encodeLowerHalf(RegisterValue value, Word lowerHalf) noexcept {
		return iris::encodeBits<RegisterValue, Word, lower32Mask, 0>(value, lowerHalf);
	}

	constexpr RegisterValue encodeRegisterValue(Word upper, Word lower) noexcept {
		return encodeUpperHalf(encodeLowerHalf(0, lower), upper);
	}

	RegisterValue Core::retrieveImmediate(byte bitmask) noexcept {
		switch(bitmask) {
#define X(value) case value : return retrieveImmediate<value>();
#include "def/iris19/bitmask8bit.def"
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
#ifdef DEBUG
		std::cout << "Current Instruction Location: " << std::hex << getInstructionPointer() << std::endl;
		std::cout << "\tCurrent word value: " << std::hex << getCurrentCodeWord() << std::endl;
#endif
		DecodedInstruction di(getCurrentCodeWord());
		dispatch(std::move(di));
		if (advanceIp) {
			incrementInstructionPointer();
		} else {
			// just re-enable it
			advanceIp = true;
		}
	}
	inline void maskMemory(RegisterValue& ref) noexcept {
		ref &= memoryMaxBitmask;
	}
	void Core::incrementAddress(RegisterValue& ptr) noexcept {
		++ptr;
		maskMemory(ptr);
	}
	void Core::decrementAddress(RegisterValue& ptr) noexcept {
		--ptr;
		maskMemory(ptr);
	}
	void Core::incrementInstructionPointer() noexcept {
		incrementAddress(getInstructionPointer());
	}
	void Core::incrementStackPointer() noexcept {
		incrementStackPointer(getStackPointer());
	}
	void Core::incrementStackPointer(RegisterValue& ptr) noexcept {
		incrementAddress(ptr);
	}

	void Core::decrementStackPointer() noexcept {
		decrementStackPointer(getStackPointer());
	}
	void Core::decrementStackPointer(RegisterValue& ptr) noexcept {
		decrementAddress(ptr);
	}

	void Core::dispatch(DecodedInstruction&& current) {
		auto tControl = current.getControl();
#ifdef DEBUG
		std::cout << "Current control value: " << std::hex << static_cast<int>(tControl) << std::endl;
#endif
		if (tControl == Operation::Shift) {
			auto result = 0u;
			auto source0 = genericRegisterGet(current.getShiftSource0());
			auto source1 = (current.getShiftFlagImmediate() ? current.getShiftImmediate() : genericRegisterGet(current.getShiftSource1()));
			if (current.getShiftFlagLeft()) {
				result = source0 << source1;
			} else {
				result = source0 >> source1;
			}
			genericRegisterSet(current.getShiftDestination(), result);
		} else if (tControl == Operation::Swap) {
			if (current.getSwapDestination() != current.getSwapSource()) {
				auto src = genericRegisterGet(current.getSwapSource());
				auto dest = genericRegisterGet(current.getSwapDestination()); // destination is always last
				genericRegisterSet(current.getSwapDestination(), src);
				genericRegisterSet(current.getSwapSource(), dest);
			}
		} else if (tControl == Operation::Arithmetic) {
			auto result = 0u;
			auto src0 = genericRegisterGet(current.getArithmeticSource());
			auto src1 = current.getArithmeticFlagImmediate() ? current.getArithmeticImmediate() : genericRegisterGet(current.getArithmeticSource1());
			switch (current.getArithmeticFlagType()) {
				case ArithmeticOps::Add:
					result = iris::add(src0, src1);
					break;
				case ArithmeticOps::Sub:
					result = iris::sub(src0, src1);
					break;
				case ArithmeticOps::Mul:
					result = iris::mul(src0, src1);
					break;
				case ArithmeticOps::Div:
					result = iris::div(src0, src1);
					break;
				case ArithmeticOps::Rem:
					result = iris::rem(src0, src1);
					break;
				default:
					throw iris::Problem("Illegal Arithmetic Signature");
			}
			genericRegisterSet(current.getArithmeticDestination(), result);
		} else if (tControl == Operation::Logical) {
			auto result = 0u;
			if (current.getLogicalFlagImmediate()) {
				auto src0 = genericRegisterGet(current.getLogicalRegister0());
				auto immediate = retrieveImmediate(current.getLogicalFlagImmediateMask());
				switch (current.getLogicalFlagImmediateType()) {
					case ImmediateLogicalOps::And:
						result = iris::binaryAnd(src0, immediate);
						break;
					case ImmediateLogicalOps::Or:
						result = iris::binaryOr(src0, immediate);
						break;
					case ImmediateLogicalOps::Nand:
						result = iris::binaryNand(src0, immediate);
						break;
					case ImmediateLogicalOps::Xor:
						result = iris::binaryXor(src0, immediate);
						break;
					default:
						throw iris::Problem("Illegal immediate logical flag type");
				}
				genericRegisterSet(current.getLogicalImmediateDestination(), result);
			} else {
				auto src0 = genericRegisterGet(current.getLogicalRegister0());
				auto type = current.getLogicalFlagType();
				if (type == LogicalOps::Not) {
					result = iris::binaryNot(src0);
				} else {
					auto src1 = genericRegisterGet(current.getLogicalRegister1());
					if (type == LogicalOps::And) {
						result = iris::binaryAnd(src0, src1);
					} else if (type == LogicalOps::Or) {
						result = iris::binaryOr(src0, src1);
					} else if (type == LogicalOps::Xor) {
						result = iris::binaryXor(src0, src1);
					} else if (type == LogicalOps::Nand) {
						result = iris::binaryNand(src0, src1);
					} else {
						throw iris::Problem("Illegal indirect logical operation!");
					}
				}
				genericRegisterSet(current.getLogicalRegisterDestination(), result);
			}
		} else if (tControl == Operation::Move) {
			auto src = genericRegisterGet(current.getMoveSource());
			genericRegisterSet(current.getMoveDestination(), iris::decodeBits<RegisterValue, RegisterValue>(src, current.getMoveBitmask(), 0));
		} else if (tControl == Operation::Set) {
			genericRegisterSet(current.getSetDestination(), retrieveImmediate(current.getSetBitmask()));
		} else if (tControl == Operation::Branch) {
			auto instFlags = current.getBranchFlags();

#ifdef DEBUG
			std::cout << "Branch flags: " << std::hex << static_cast<int>(instFlags) << std::endl;
#endif
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
			DecodedInstruction next(tryReadNext<true>());
			auto src0 = genericRegisterGet(next.getCompareRegisterSrc0());
			auto src1 = current.getCompareImmediateFlag() ? static_cast<RegisterValue>(next.getCompareImmediate()) : genericRegisterGet(next.getCompareRegisterSrc1());
			auto compareType = current.getCompareType();
			auto result = false;
			switch (compareType) {
				case CompareStyle::Equals:
					result = iris::eq(src0, src1);
					break;
				case CompareStyle::NotEquals:
					result = iris::neq(src0, src1);
					break;
				case CompareStyle::LessThan:
					result = iris::lt(src0, src1);
					break;
				case CompareStyle::GreaterThan:
					result = iris::gt(src0, src1);
					break;
				case CompareStyle::LessThanOrEqualTo:
					result = iris::le(src0, src1);
					break;
				case CompareStyle::GreaterThanOrEqualTo:
					result = iris::ge(src0, src1);
					break;
				default:
					throw iris::Problem("illegal compare type!");
			}
			genericRegisterSet(current.getCompareRegisterDest(), result);
		} else if (tControl == Operation::SystemCall) {
			auto field = genericRegisterGet(current.getSystemAction());
			if (field >= ArchitectureConstants::MaxSystemCalls) {
				throw iris::Problem("ERROR: system call index out of range!");
			} else {
				systemHandlers[field](this, std::move(current));
			}
		} else {
			std::stringstream str;
			str << "Illegal instruction " << std::hex << static_cast<int>(current.getControl()) << std::endl;
			str << "Location: " << std::hex << getInstructionPointer() << std::endl;
			execute = false;
			throw iris::Problem(str.str());
		}
	}

	void Core::terminate(Core* core, DecodedInstruction&& inst) {
		core->execute = false;
		core->advanceIp = false;
	}

	void Core::putc(Core* core, DecodedInstruction&& current) {
		std::cout.put(static_cast<char>(core->genericRegisterGet(current.getSystemArg0())));
	}
	void Core::getc(Core* core, DecodedInstruction&& current) {
		byte value = 0;
		std::cin >> std::noskipws >> value;
		core->genericRegisterSet(current.getSystemArg0(), static_cast<Word>(value));
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
		return iris::encodeBits<RegisterValue, Word, bitmask64, 32>(static_cast<RegisterValue>(loadWord(address)), loadWord(address + 1));
	}
	void Core::storeRegisterValue(RegisterValue address, RegisterValue value) {
		storeWord(address, iris::decodeBits<RegisterValue, Word, lower32Mask, 0>(value));
		storeWord(address + 1, iris::decodeBits<RegisterValue, Word, upper32Mask, 32>(value));
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
	void Core::pushWord(Word value) {
		pushWord(value, getStackPointer());
	}
	void Core::pushWord(Word value, RegisterValue& ptr) {
		decrementStackPointer(ptr);
		storeWord(ptr, value);
	}
	void Core::pushDword(DWord value, RegisterValue& ptr) {
		pushWord(decodeUpperHalf(value), ptr);
		pushWord(decodeLowerHalf(value), ptr);
	}
	void Core::pushDword(DWord value) {
		return pushDword(value, getStackPointer());
	}

	Word Core::popWord() {
		return popWord(getStackPointer());
	}
	Word Core::popWord(RegisterValue& ptr) {
		auto result = loadWord(ptr);
		incrementStackPointer(ptr);
		return result;
	}

	DWord Core::popDword() {
		return popDword(getStackPointer());
	}
	DWord Core::popDword(RegisterValue& ptr) {
		auto upper = popWord(ptr);
		auto lower = popWord(ptr);
		return encodeRegisterValue(upper, lower);
	}
	void Core::genericRegisterSet(byte registerTarget, RegisterValue value) {
		if (registerIsMarkedStack(registerTarget)) {
			if (registerIsMarkedIndirect(registerTarget)) {
				throw iris::Problem("Unable to do both stack and indirect operations at the same time!");
			} else {
				pushDword(value, registerValue(registerGetActualIndex(registerTarget)));
			} 
		} else {
			if (registerIsMarkedIndirect(registerTarget)) {
				storeRegisterValue(registerValue(registerGetActualIndex(registerTarget)), value);
			} else {
				registerValue(registerGetActualIndex(registerTarget)) = value;
			}
		}
	}

	RegisterValue Core::genericRegisterGet(byte registerTarget) {
		if (registerIsMarkedStack(registerTarget)) {
			if (registerIsMarkedIndirect(registerTarget)) {
				throw iris::Problem("Unable to do both stack and indirect operations at the same time!");
			} else {
				return popDword(registerValue(registerGetActualIndex(registerTarget)));
			}
		} else {
			if (registerIsMarkedIndirect(registerTarget)) {
				return loadRegisterValue(registerValue(registerGetActualIndex(registerTarget)));
			} else {
				return registerValue(registerGetActualIndex(registerTarget));
			}
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
		first = encodeMoveDestination(first, arg0);
		first = encodeMoveSource(first, arg1);
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeSwap() {
		return std::make_tuple(1, encodeSwapSource( encodeSwapDestination( encodeControl(0, type), arg0), arg1), 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeShift() {
		auto first = encodeControl(0, type);
		first = encodeShiftFlagImmediate(first, immediate);
		first = encodeShiftFlagLeft(first, shiftLeft);
		first = encodeShiftDestination(first, arg0);
		first = encodeShiftSource0(first, arg1);
		first = immediate ? encodeShiftImmediate(first, arg2) : encodeShiftSource1(first, arg2);
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeSystemCall() {
		auto first = encodeControl(0, type);
		first = encodeSystemAction(first, arg0);
		first = encodeSystemArg0(first, arg1);
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeCompare() {
		auto first = encodeControl(0, type);
		first = encodeCompareType(first, static_cast<CompareStyle>(subType));
		first = encodeCompareImmediateFlag(first, immediate);
		first = encodeCompareRegisterDest(first, this->arg0);
		first = encodeCompareRegisterSrc0(first, this->arg1);
		first = encodeCompareRegisterSrc1(first, this->arg2);
		return std::make_tuple(1, first, 0, 0);
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

	InstructionEncoder::Encoding InstructionEncoder::encode() {
		// always encode the type
#define DefEnum(a, b)
#define EndDefEnum(a, b, c)
#define EnumEntry(compareType) if (type == Operation:: compareType) { return encode ## compareType () ; }
#include "def/iris19/ops.def"
#undef DefEnum
#undef EndDefEnum
#undef EnumEntry
		throw iris::Problem("Illegal type to encode!");
	}

	int instructionSizeFromImmediateMask(byte bitmask) {

#define X(bits) if (bitmask == bits) { return instructionSizeFromImmediateMask<bits>(); }
#include "def/iris19/bitmask4bit.def"
#undef X
		throw iris::Problem("Illegal bitmask provided!");
	}
	RegisterValue getMask(byte bitmask) {
#define X(bits) if (bitmask == bits) { return SetBitmaskToWordMask<bits>::mask; }
#include "def/iris19/bitmask4bit.def"
#undef X
		throw iris::Problem("Illegal bitmask provided!");
	}
	int InstructionEncoder::numWords() {
		return std::get<0>(encode());
	}
	void InstructionEncoder::clear() {
		currentLine = 0;
		address = 0;
		type = Operation::Arithmetic;
		immediate = false;
		shiftLeft = false;
		isIf = false;
		isCall = false;
		isConditional = false;
		bitmask = 0b0000;
		arg0 = 0;
		arg1 = 0;
		arg2 = 0;
		isLabel = false;
		labelValue.clear();
		subType = 0;
		fullImmediate = 0;
		indirect = false;
		readNextWord = false;
	}
}
