#include "iris19.h"
#include <functional>
#include <sstream>
#include "Problem.h"
#include <utility>
extern "C" {
	#include <clips.h>
}

namespace iris19 {
	// BEGIN INSTRUCTION
	RegisterValue Instruction::getImmediate8() const noexcept {
		return getShortImmediate();
	}
	byte Instruction::getBitmask() const noexcept {
		switch(getControl()) {
			case Operation::Move:
			case Operation::Logical:
				return getRawBitmask();
			default:
				return ArchitectureConstants::Bitmask;
		}
	}
	byte Instruction::getDestination() const noexcept {
		return getDestinationIndex();
	}
	byte Instruction::getSource0() const noexcept {
		return getSource0Index();
	}
	byte Instruction::getSource1() const noexcept {
		return markedImmediate() ? getShortImmediate() : getSource1Index();
	}
	byte Instruction::getSubType() const noexcept {
		switch (getControl()) {
			case Operation::Arithmetic:
				return getArithmeticFlagType();
			case Operation::Move:
				return getMoveSubtype();
			case Operation::Compare:
				return getCompareType();
			case Operation::Logical:
				return getLogicalFlagType();
			default:
				return 0;
		}
	}
	// BEGIN CORE
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

	void Core::defaultSystemHandler(Core* core, Instruction&& inst) {
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
		populateContents<RegisterValue, ArchitectureConstants::RegisterCount>(gpr, stream, [](byte* buf) { return iris::encodeUint64LE(buf); });
		populateContents<Word, ArchitectureConstants::AddressMax>(memory, stream, [](byte* buf) { return iris::encodeUint32LE(buf); });
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
		dumpContents<RegisterValue, ArchitectureConstants::RegisterCount>(gpr, stream, iris::decodeUint64LE);
		dumpContents<Word, ArchitectureConstants::AddressMax>(memory, stream, iris::decodeUint32LE);
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
		Instruction di(getCurrentCodeWord());
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

	void Core::dispatch(Instruction&& current) {
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
	void Core::moveOperation(Instruction&& current) {
		auto moveType = current.getSubType<MoveOperation>();
		auto bitmask = current.getBitmask();
		auto mDest = current.getDestination();
		if (moveType == MoveOperation::Move) {
			auto mSrc = current.getSource0();
			if (!((bitmask == ArchitectureConstants::Bitmask) && (mDest == mSrc && mDest < ArchitectureConstants::RegisterCount))) {
				auto src = genericRegisterGet(mSrc);
				genericRegisterSet(mDest, iris::decodeBits<RegisterValue, RegisterValue>(src, mask(bitmask), 0));
			} 
		} else if (moveType == MoveOperation::Set) {
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

	void Core::shiftOperation(Instruction&& current) {
		auto result = 0u;
		auto source0 = genericRegisterGet(current.getSource0());
		// Cap the shift value to between 0-64
		auto source1 = current.markedImmediate() ? current.getImmediate8() : genericRegisterGet(current.getSource1());
		if (current.shiftLeft()) {
			result = source0 << source1;
		} else {
			result = source0 >> source1;
		}
		genericRegisterSet(current.getDestination(), result);
	}

	void Core::arithmeticOperation(Instruction&& current) {
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

	void Core::logicalOperation(Instruction&& current) {
		auto result = 0u;
		auto src0 = genericRegisterGet(current.getSource0());
		auto type = current.getSubType<LogicalOps>();
		if (type == LogicalOps::Not) {
			if (current.markedImmediate()) {
				throw iris::Problem("Can't do an immediate of a binary not!");
			} else {
				result = iris::binaryNot(src0);
			}
		} else {
			auto src1 = current.markedImmediate() ? retrieveImmediate(current.getBitmask()) : genericRegisterGet(current.getSource1());
			switch(type) {
				case LogicalOps::And:
					result = iris::binaryAnd(src0, src1);
					break;
				case LogicalOps::Or:
					result = iris::binaryOr(src0, src1);
					break;
				case LogicalOps::Nand:
					result = iris::binaryNand(src0, src1);
					break;
				case LogicalOps::Xor:
					result = iris::binaryXor(src0, src1);
					break;
				case LogicalOps::Not:
					throw iris::Problem("FATAL ERROR: logicalOperation, type was not LogicalOps::Not so else condition was taken, but type is now LogicalOps::Not!!!!");
				default:
					throw iris::Problem("Undefined logical operation!");
			}
		}
		genericRegisterSet(current.getDestination(), result);
	}

	void Core::compareOperation(Instruction&& current) {
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

	void Core::branchSpecificOperation(Instruction&& current) {
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
			auto cond = genericRegisterGet(current.getDestination()) != 0;
			auto onTrue = genericRegisterGet(current.getSource0());
			auto onFalse = genericRegisterGet(current.getSource1());
			if (isCall) {
				pushDword((getInstructionPointer() + 1) & memoryMaxBitmask);
			}
			getInstructionPointer() = memoryMaxBitmask & (cond ? onTrue : onFalse);
		} else {
			advanceIp = false;
			auto next = 0u;
			auto invoke = (isConditional && (genericRegisterGet(current.getDestination()) != 0)) || !isConditional;
			if (invoke) {
				if (isCall) {
					pushDword(next);
				}
				next = (isImmediate ? retrieveImmediate(ArchitectureConstants::Bitmask) : genericRegisterGet(current.getSource0()));
			}  else {
				next = getInstructionPointer() + instructionSizeFromImmediateMask(isImmediate ? 0b11111111 : 0b00000000);
			}
			getInstructionPointer() = memoryMaxBitmask & next;
		}
	}

	void Core::terminate(Core* core, Instruction&& inst) {
		core->execute = false;
		core->advanceIp = false;
	}

	void Core::putc(Core* core, Instruction&& current) {
		std::cout.put(static_cast<char>(core->genericRegisterGet(current.getSource0())));
	}
	void Core::getc(Core* core, Instruction&& current) {
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
		auto lower32 = loadWord(address);
		auto upper32 = loadWord(address + 1);
		return iris::encodeUint64LE(lower32, upper32);
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
		return std::make_tuple(1, singleWordEncoding<Operation::Arithmetic>(), 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeShift() {
		return std::make_tuple(1, singleWordEncoding<Operation::Shift>(), 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeCompare() {
		return std::make_tuple(1, singleWordEncoding<Operation::Compare>(), 0, 0);
	}
	Word InstructionEncoder::lowerHalf() const noexcept {
		return static_cast<Word>(fullImmediate);
	}
	Word InstructionEncoder::upperHalf() const noexcept {
		return static_cast<Word>(fullImmediate >> 32);
	}
	Word InstructionEncoder::maskedLowerHalf() const noexcept {
		return static_cast<Word>(mask(bitmask) & fullImmediate);
	}
	Word InstructionEncoder::maskedUpperHalf() const noexcept {
		return static_cast<Word>((mask(bitmask) & fullImmediate) >> 32);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeMove() {
		auto memOp = static_cast<MoveOperation>(subType);
		auto isSet = memOp == MoveOperation::Set;
		auto second = isSet ? maskedLowerHalf() : 0;
		auto third = isSet ? maskedUpperHalf() : 0;
		auto count = isSet ? instructionSizeFromImmediateMask(bitmask) : 1;
		auto first = setSubType<Operation::Move>(setControl());
		first = setBitmask(first);
		first = setDestination(first);
		first = setSource0(first); // safe for set operations
		switch(memOp) {
			case MoveOperation::Move:
			case MoveOperation::SystemCall:
			case MoveOperation::Swap:
			case MoveOperation::Set:
				return std::make_tuple(count, first, second, third);
			default:
				throw iris::Problem("Undefined MoveOperation requested during encoding!");
		}
	}


	InstructionEncoder::Encoding InstructionEncoder::encodeLogical() {
		auto first = setSubType<Operation::Logical>(setControl());
		first = setDestination(first);
		first = setSource0(first);
		if (immediate) {
			first = setBitmask(first);
			return std::make_tuple(instructionSizeFromImmediateMask(bitmask), first, maskedLowerHalf(), maskedUpperHalf());
		} else {
			first = setSource1(first);
			return std::make_tuple(1, first, 0, 0);
		}
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeBranch() {
		auto first = setSubType<Operation::Branch>(setControl());
		first = setDestination(first); // conditional, but safe even for immediate calls
		if (isIf) {
			first = setSource0(first); // onTrue
			first = setSource1(first); // onFalse
			return std::make_tuple(1, first, 0, 0);
		} else {
			if (immediate) {
				// upper 16 of branch direct immediate is all zeros, use the
				// other two words for address storage, even if 16 bits were
				// encoded inside the first word, the third word would be
				// necessary! This architecture needs to not be dependent on
				// internal constants. This prevents breakage if more ram is
				// added
				return std::make_tuple(3, first, lowerHalf(), upperHalf());
			} else {
				first = setSource0(first); // register destination
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
		bitmask = 0b00000000;
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
	Word InstructionEncoder::setBitmask(Word value) const noexcept {
		return encodeRawBitmask(value, bitmask);
	}
	Word InstructionEncoder::setDestination(Word value) const noexcept {
		return encodeDestinationIndex(value, arg0);
	}
	Word InstructionEncoder::setSource0(Word value) const noexcept {
		return encodeSource0Index(value, arg1);
	}
	Word InstructionEncoder::setSource1(Word value) const noexcept {
		return encodeSource1Index(value, arg2);
	}
	Word InstructionEncoder::setImmediateFlag(Word value) const noexcept {
		return encodeImmediateFlag(value, immediate);
	}
	Word InstructionEncoder::setControl(Word value) const noexcept {
		return encodeControl(value, type);
	}
	Word InstructionEncoder::setShortImmediate(Word value) const noexcept {
		return encodeShortImmediate(value, arg2);
	}
#define X(title, mask, shift, type, post) \
	void CLIPS_encode ## title (UDFContext* context, CLIPSValue* ret) { \
		CLIPSValue input, value; \
		if (!UDFFirstArgument(context, NUMBER_TYPES, &input)) { \
			CVSetBoolean(ret, false); \
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &value)) { \
			CVSetBoolean(ret, false); \
		} else { \
			CVSetInteger(ret, encode ## title ( static_cast<Word>(CVToInteger(&input)), static_cast< type >(CVToInteger(&value)))); \
		} \
	}
#include "def/iris19/instruction.def"
#undef X
// to integer
#define DefEnum(type, width) \
	void CLIPS_translateEnumToInteger_ ## type (UDFContext* context, CLIPSValue* ret) { \
		static bool init = true; \
		static std::map<std::string, type > collection; \
		static int count = 0; \
		if (init) { \
			init = false; \
			auto convert = [](int value) { return static_cast < type > (value) ; } ;
#define EnumEntry(type) collection.emplace( #type , convert(count)); ++count;
#define EndDefEnum(type, width, maxCount) \
		} \
		CLIPSValue in; \
		if (!UDFFirstArgument(context, LEXEME_TYPES, &in)) { \
			CVSetBoolean(ret, false); \
		} else { \
			std::string str = CVToString(&in); \
			auto loc = collection.find(str); \
			if (loc == collection.end()) { \
				CVSetBoolean(ret, false); \
			} else { \
				CVSetInteger(ret, static_cast<int>(loc->second)); \
			} \
		} \
	}
#include "def/iris19/ops.def"
#include "def/iris19/arithmetic_ops.def"
#include "def/iris19/compare.enum"
#include "def/iris19/logical.enum"
#include "def/iris19/move.def"
#undef EnumEntry
#undef DefEnum
#undef EndDefEnum
// integer to enum
#define DefEnum(type, width) \
	void CLIPS_translateIntegerToEnum_ ## type (UDFContext* context, CLIPSValue* ret) { \
		static bool init = true; \
		static std::map<type , std::string > collection; \
		static int count = 0; \
		if (init) { \
			init = false; \
			auto convert = [](int key) { return static_cast< type > (key) ; };
#define EnumEntry(type) collection.emplace(convert(count), #type ); ++count;
#define EndDefEnum(type, width, maxCount) \
		} \
		CLIPSValue in; \
		if (!UDFFirstArgument(context, NUMBER_TYPES, &in)) { \
			CVSetBoolean(ret, false); \
		} else { \
			auto integer = CVToInteger(&in); \
			auto loc = collection.find(static_cast< type > (integer)) ; \
			if (loc == collection.end()) { \
				CVSetBoolean(ret, false); \
			} else { \
				CVSetString(ret, loc->second.c_str()); \
			} \
		} \
	}
#include "def/iris19/ops.def"
#include "def/iris19/arithmetic_ops.def"
#include "def/iris19/compare.enum"
#include "def/iris19/logical.enum"
#include "def/iris19/move.def"
#undef EnumEntry
#undef DefEnum
#undef EndDefEnum
	void installExtensions(void* theEnv) {
		Environment* env = static_cast<Environment*>(theEnv);
#define X(title, mask, shift, type, post) \
		EnvAddUDF(env, "iris19:encode" #title , "l", CLIPS_encode ## title, "CLIPS_encode" #title, 2, 2, "l;l;l", nullptr);
#include "def/iris19/instruction.def"
#undef X
#define EnumEntry(unused)
#define EndDefEnum(a, b, c)
#define DefEnum(type, unused) \
		EnvAddUDF(env, "iris19:convertEnumToInt_" #type , "l", CLIPS_translateEnumToInteger_ ## type , "CLIPS_translateEnumToInteger_" #type , 1, 1, "s", nullptr); \
		EnvAddUDF(env, "iris19:convertIntToEnum_" #type , "y", CLIPS_translateIntegerToEnum_ ## type , "CLIPS_translateIntegerToEnum_" #type , 1, 1, "l", nullptr);
#include "def/iris19/ops.def"
#include "def/iris19/arithmetic_ops.def"
#include "def/iris19/compare.enum"
#include "def/iris19/logical.enum"
#include "def/iris19/move.def"
#undef DefEnum
#undef EnumEntry
#undef EndDefEnum
	}
}
