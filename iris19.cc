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

	RegisterValue Core::retrieveImmediate(byte bitmask) noexcept {
		auto useLower = readLower(bitmask);
		auto useUpper = readUpper(bitmask);
		if (!useLower && !useUpper) {
			return 0;
		} else {
			auto lower = tryReadNext(useLower);
			auto upper = static_cast<RegisterValue>(tryReadNext(useUpper)) << 32;
			return mask(bitmask) & (lower | upper);
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

	Word Core::tryReadNext(bool readNext) noexcept {
		if (readNext) {
			incrementInstructionPointer();
			return getCurrentCodeWord();
		} else {
			return 0;
		}
	}

	void Core::dispatch(DecodedInstruction&& current) {
		auto hasError = false;
		switch (current.getOperation()) {
			case Operation::Arithmetic:
				arithmeticOperation(std::move(current));
				break;
			case Operation::Shift:
				shiftOperation(std::move(current));
				break;
			case Operation::Logical:
				logicalOperation(std::move(current));
				break;
			case Operation::Compare:
				compareOperation(std::move(current));
				break;
			case Operation::Branch:
				branchSpecificOperation(std::move(current));
				break;
			case Operation::Move:
				moveOperation(std::move(current));
				break;
			default:
				hasError = true;
				break;
		}
		if (hasError) {
			std::stringstream str;
			str << "Illegal instruction " << std::hex << static_cast<int>(current.getOperation()) << std::endl;
			str << "Location: " << std::hex << getInstructionPointer() << std::endl;
			execute = false;
			throw iris::Problem(str.str());
		}
	}
	void Core::moveOperation(DecodedInstruction&& current) {
		auto moveType = current.getSubType<MoveOperation>();
		auto bitmask = current.getBitmask();
		auto mDest = current.getDestination();
		if (moveType == MoveOperation::Move) {
			auto mSrc = current.getSource0();
			if (!((bitmask == ArchitectureConstants::Bitmask) && (mDest == mSrc && mDest < ArchitectureConstants::RegisterCount))) {
				auto src = genericRegisterGet(mSrc);
				genericRegisterSet(mDest, iris::decodeBits<RegisterValue, RegisterValue>(src, bitmask, 0));
			} 
			// no operation otherwise
		} else if (moveType == MoveOperation::Set) {
			// check and see if it is zero, if so then just set it to zero
			auto result = bitmask == 0 ? 0 : retrieveImmediate(bitmask);
			genericRegisterSet(mDest, result);
		} else if (moveType == MoveOperation::Swap) {
			if (bitmask != 0) {
				throw iris::Problem("Swap Operation: Bitmask must be set to zero since it has no bearing on this operation!");
			}
			// this can do swap the first and second dwords of the given
			// stack pointer in one instruction :)
			auto mSrc = current.getSource0();
			auto src = genericRegisterGet(mSrc);
			auto dest = genericRegisterGet(mDest);
			genericRegisterSet(mDest, src);
			genericRegisterSet(mSrc, dest);
		} else if (moveType == MoveOperation::SystemCall) {
			if (bitmask != 0) {
				throw iris::Problem("System Call Operation: Bitmask must be set to zero since it has no bearing on this operation!");
			}
			auto field = genericRegisterGet(mDest);
			if (field >= ArchitectureConstants::MaxSystemCalls) {
				throw iris::Problem("ERROR: system call index out of range!");
			} else {
				systemHandlers[field](this, std::move(current));
			}
		} else {
			throw iris::Problem("Move Superclass: Undefined subtype!");
		}
	}

	void Core::shiftOperation(DecodedInstruction&& current) {
		auto result = 0u;
		auto source0 = genericRegisterGet(current.getSource0());
		auto source1 = current.markedImmediate() ? current.getImmediate8() : genericRegisterGet(current.getSource1());
		if (current.shiftLeft()) {
			result = source0 << source1;
		} else {
			result = source0 >> source1;
		}
		genericRegisterSet(current.getDestination(), result);
	}

	void Core::arithmeticOperation(DecodedInstruction&& current) {
		auto result = 0u;
		auto src0 = genericRegisterGet(current.getSource0());
		auto src1 = current.markedImmediate() ? current.getImmediate8() : genericRegisterGet(current.getSource1());
		switch (current.getSubType<ArithmeticOps>()) {
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
		genericRegisterSet(current.getDestination(), result);
	}

	void Core::logicalOperation(DecodedInstruction&& current) {
		auto result = 0u;
		auto src0 = genericRegisterGet(current.getSource0());
		if (current.markedImmediate()) {
			auto immediate = retrieveImmediate(current.getBitmask());
			switch (current.getSubType<ImmediateLogicalOps>()) {
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
		} else {
			auto type = current.getSubType<LogicalOps>();
			if (type == LogicalOps::Not) {
				result = iris::binaryNot(src0);
			} else {
				auto src1 = genericRegisterGet(current.getSource1());
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
		}
		genericRegisterSet(current.getDestination(), result);
	}

	void Core::compareOperation(DecodedInstruction&& current) {
		auto src0 = genericRegisterGet(current.getSource0());
		auto src1 = current.markedImmediate() ? current.getImmediate8() : genericRegisterGet(current.getSource1());
		auto compareType = current.getSubType<CompareStyle>();
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
		genericRegisterSet(current.getDestination(), result);
	}

	void Core::branchSpecificOperation(DecodedInstruction&& current) {
		advanceIp = true;
		auto isIf = current.branchMarkedIf();
		auto isCall = current.branchMarkedIf();
		auto isImmediate = current.markedImmediate();
		auto isConditional = current.branchMarkedConditional();

		if (isIf) {
			if (isImmediate) {
				throw iris::Problem("Branch if conditional form doesn't support the immediate flag!");
			} 
			if (isConditional) {
				throw iris::Problem("Branch if conditional form can't also be marked as conditional!");
			}
			// if instruction
			advanceIp = false;
			// process them all even if we don't use them all, it is then
			// straightforward what is is happening
			auto cond = genericRegisterGet(current.getConditional());
			auto onTrue = genericRegisterGet(current.getOnTrue());
			auto onFalse = genericRegisterGet(current.getOnFalse());
			if (isCall) {
				pushDword((getInstructionPointer() + 1) & memoryMaxBitmask);
			}
			getInstructionPointer() = memoryMaxBitmask & (cond ? onTrue : onFalse);
#ifdef DEBUG
						std::cout << "if: jumping to " << std::hex << getInstructionPointer() << std::endl;
#endif
		} else if (isCall) {
			if (isIf) {
				throw iris::Problem("Should never ever get here, but somehow call was set to true then if o_O, you may have bad ram!");
			} 
			if (isConditional) {
				throw iris::Problem("Just like x86, call instructions are not conditional!");
			}
			// call instruction
			advanceIp = false;
			auto returnAddress = getInstructionPointer() + (isImmediate ? 2 : 1);
			auto address = 0u;
			if (isImmediate) {
				address = retrieveImmediate(0b00001111);
			} else {
				address = genericRegisterGet(current.getDestination());
			}
			pushDword(returnAddress); // push the return value onto the default stack
			getInstructionPointer() = memoryMaxBitmask & address;
#ifdef DEBUG
						std::cout << "call: Jumping to " << std::hex << getInstructionPointer() << std::endl;
#endif
		} else {
			if (isCall) {
				throw iris::Problem("Unconditional Jump: Somehow call was set to false as well as if yet you're here o_O, so call was reset to true! You may have bad ram!");
			}
			if (isIf) {
				throw iris::Problem("Unconditional Jump: Somehow call was set to false as well as if yet you're here o_O, so if was reset to true! You may have bad ram!");
			} 
			// jump instruction, including cond versions
			if ((isConditional && genericRegisterGet(current.getConditional()) != 0) || !isConditional) {
				advanceIp = false;
				auto value = 0u;
				if (isImmediate) {
					value = retrieveImmediate(0b00001111);
				} else {
					value = genericRegisterGet(current.getDestination());
				}
				getInstructionPointer() = memoryMaxBitmask & value;
#ifdef DEBUG
						std::cout << "one way Jumping to " << std::hex << getInstructionPointer() << std::endl;
#endif
			}
		}
	}

	void Core::terminate(Core* core, DecodedInstruction&& inst) {
		core->execute = false;
		core->advanceIp = false;
	}

	void Core::putc(Core* core, DecodedInstruction&& current) {
		std::cout.put(static_cast<char>(core->genericRegisterGet(current.getSource0())));
	}
	void Core::getc(Core* core, DecodedInstruction&& current) {
		byte value = 0;
		std::cin >> std::noskipws >> value;
		core->genericRegisterSet(current.getSource0(), static_cast<Word>(value));
	}


	void Core::link(std::istream& input) {
		// we have some more data to read through
		// two address system, 1 RegisterValue -> address, 1 Word -> value
		static constexpr int bufSize = 14;
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
						storeWord(encodeRegisterValue(buf[2], buf[3], buf[4], buf[5], buf[6], buf[7], buf[8], buf[9]), encodeWord(static_cast<byte>(buf[10]), buf[11], buf[12], buf[13]));
						break;
					case 1: // register value
						gpr[static_cast<byte>(buf[1])] = encodeRegisterValue(buf[2], buf[3], buf[4], buf[5], buf[6], buf[7], buf[8], buf[9]);
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
        auto dest = registerGetActualIndex(registerTarget);
		if (registerIsMarkedStack(registerTarget)) {
			if (registerIsMarkedIndirect(registerTarget)) {
				throw iris::Problem("Unable to do both stack and indirect operations at the same time!");
			} else {
				pushDword(value, registerValue(dest));
			}
		} else {
			if (registerIsMarkedIndirect(registerTarget)) {
				storeRegisterValue(dest, value);
			} else {
				if (dest == ArchitectureConstants::InstructionPointer) {
					// if it is a normal store then don't advance ip if
					// registerTarget == ip. That way we don't do too surprising
					// things
					getInstructionPointer() = value & memoryMaxBitmask;
					advanceIp = false;
				} else {
					registerValue(dest) = value;
					advanceIp = true;
				}
			}
		}
	}

	RegisterValue Core::genericRegisterGet(byte registerTarget) {
        auto &value = registerValue(registerGetActualIndex(registerTarget));
		if (registerIsMarkedStack(registerTarget)) {
			if (registerIsMarkedIndirect(registerTarget)) {
				throw iris::Problem("Unable to do both stack and indirect operations at the same time!");
			} else {
				return popDword(value);
			}
		} else {
			if (registerIsMarkedIndirect(registerTarget)) {
				return loadRegisterValue(value);
			} else {
				return value;
			}
		}
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeArithmetic() {
		auto first = encodeControl(0, type);
		first = encodeArithmeticFlagImmediate(first, immediate);
		first = encodeArithmeticFlagType(first, subType);
		first = encodeArithmeticDestination(first, arg0);
		first = immediate ? encodeArithmeticImmediate(first, arg1) : encodeArithmeticSource(first, arg1);
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeMove() {
		auto first = encodeControl(0, type);
		auto second = 0u;
		auto third = 0u;
		auto count = instructionSizeFromImmediateMask(bitmask);
		auto memOp = static_cast<MoveOperation>(subType);
		first = encodeMoveSubtype(first, subType);
		first = encodeMoveBitmask(first, bitmask);
		first = encodeMoveDestination(first, arg0);
		if (memOp == MoveOperation::Move) {
			first = encodeMoveSource(first, arg1);
		} else if (memOp == MoveOperation::Swap) {
			if (bitmask != 0) {
				throw iris::Problem("The bitmask of a swap operation must always be zero!");
			}
			first = encodeMoveSource(first, arg1);
		} else if (memOp == MoveOperation::Set) {
			// use the mask during encoding since we know how many Words the
			// instruction is made up of
			auto maskedValue = mask(bitmask) & fullImmediate;
			second = static_cast<Word>(maskedValue);
			third = static_cast<Word>(maskedValue >> 32);
		} else if (memOp == MoveOperation::SystemCall) {
			if (bitmask != 0) {
				throw iris::Problem("The bitmask of a system call operation must always be zero!");
			}
			first = encodeMoveSource(first, arg1);
		} else {
			throw iris::Problem("Undefined MoveOperation requested during encoding!");
		}
		return std::make_tuple(count, first, second, third);
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

	InstructionEncoder::Encoding InstructionEncoder::encodeCompare() {
		auto first = encodeControl(0, type);
		first = encodeCompareType(first, subType);
		first = encodeCompareImmediateFlag(first, immediate);
		first = encodeCompareRegisterDest(first, this->arg0);
		first = encodeCompareRegisterSrc0(first, this->arg1);
		first = encodeCompareRegisterSrc1(first, this->arg2);
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeLogical() {
		auto first = encodeControl(0, type);
		first = encodeLogicalFlagImmediate(first, immediate);
		if (immediate) {
			first = encodeLogicalFlagImmediateType(first, subType);
			first = encodeLogicalFlagImmediateMask(first, bitmask);
			first = encodeLogicalImmediateDestination(first, arg0);
			auto maskedImmediate = mask(bitmask) & fullImmediate;
			auto second = static_cast<Word>(maskedImmediate);
			auto third = static_cast<Word>(maskedImmediate >> 16);
			return std::make_tuple(instructionSizeFromImmediateMask(bitmask), first, second, third);
		} else {
			first = encodeLogicalFlagType(first, subType);
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
		switch (type) {
#define DefEnum(a, b)
#define EndDefEnum(a, b, c)
#define EnumEntry(compareType) case Operation:: compareType : return encode ## compareType () ; 
#include "def/iris19/ops.def"
#undef DefEnum
#undef EndDefEnum
#undef EnumEntry
			default:
				throw iris::Problem("Illegal type to encode!");
		}
	}

	int instructionSizeFromImmediateMask(byte bitmask) {
        return 1 + (readLower(bitmask) ? 1 : 0) + (readUpper(bitmask) ? 1 : 0);
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
	// BEGIN DECODED INSTRUCTION
	bool DecodedInstruction::markedImmediate() const noexcept {
		switch (getControl()) {
			case Operation::Arithmetic:
				return getArithmeticFlagImmediate();
			case Operation::Logical:
				return getLogicalFlagImmediate();
			case Operation::Shift:
				return getShiftImmediate();
			case Operation::Compare:
				return getCompareImmediateFlag();
			case Operation::Branch:
				return getBranchFlagIsImmediate();
			default:
				return false;
		}
	}
	RegisterValue DecodedInstruction::getImmediate8() const noexcept {
		switch(getControl()) {
			case Operation::Arithmetic:
				return getArithmeticFlagImmediate() ? getArithmeticImmediate() : 0u;
			case Operation::Shift:
				return getShiftFlagImmediate() ? getShiftImmediate() : 0u;
			case Operation::Compare:
				return getCompareImmediateFlag() ? getCompareImmediate() : 0u;
			default:
				return 0u;
		}
	}
	byte DecodedInstruction::getBitmask() const noexcept {
		switch(getControl()) {
			case Operation::Move:
				return getMoveBitmask();
			case Operation::Logical:
				return getLogicalFlagImmediate() ?  getLogicalFlagImmediateMask() : ArchitectureConstants::Bitmask;
			default:
				return ArchitectureConstants::Bitmask;
		}
	}
	byte DecodedInstruction::getDestination() const noexcept {
		switch(getControl()) {
			case Operation::Arithmetic:
				return getArithmeticDestination();
			case Operation::Logical:
				return getLogicalFlagImmediate() ? getLogicalImmediateDestination() : getLogicalRegisterDestination();
			case Operation::Compare:
				return getCompareRegisterDest();
			case Operation::Move:
				return getMoveDestination();
			case Operation::Shift:
				return getShiftDestination();
			case Operation::Branch:
				return getBranchIndirectDestination();
			default:
				return 0;
		}
	}
	byte DecodedInstruction::getSource0() const noexcept {
		switch (getControl()) {
			case Operation::Arithmetic:
				return getArithmeticSource();
			case Operation::Compare:
				return getCompareRegisterSrc0();
			case Operation::Logical:
				return getLogicalFlagImmediate() ? getLogicalImmediateSource0() : getLogicalRegister0();
			case Operation::Move:
				return getSubType<MoveOperation>() == MoveOperation::Set ? 0u : getMoveSource();
			default:
				return 0;
		}
	}
	byte DecodedInstruction::getSource1() const noexcept {
		auto imm = markedImmediate();
		switch (getControl()) {
			case Operation::Arithmetic:
				return imm ? 0 : getArithmeticSource1();
			case Operation::Shift:
				return imm ? 0 : getShiftSource1();
			case Operation::Compare:
				return imm ? 0 : getCompareRegisterSrc1();
			case Operation::Logical:
				return imm ? 0 : getLogicalRegister1();
			default:
				return 0;
		}
	}
	byte DecodedInstruction::getSubType() const noexcept {
		switch (getControl()) {
			case Operation::Arithmetic:
				return getArithmeticFlagType();
			case Operation::Move:
				return getMoveSubtype();
			case Operation::Compare:
				return getCompareType();
			case Operation::Logical:
				return markedImmediate() ? getLogicalFlagImmediateType() : getLogicalFlagType();
			default:
				return 0;
		}
	}
}
