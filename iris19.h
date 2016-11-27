#ifndef _TARGET_IRIS19_IRIS_H
#define _TARGET_IRIS19_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include "Problem.h"
#include <cstdint>
#include <sstream>
#include <memory>
#include <vector>
#include <tuple>
#include <map>
#include "sim_registration.h"
#include "iris_xunits.h"

namespace iris19 {
	using HWord = uint16_t;
	using Word = uint32_t;
	using DWord = uint64_t;
	using RawInstruction = Word; // this is more of a packet!
	using immediate = HWord;
	using RegisterValue = DWord;
	inline constexpr Word encodeWord (byte b0, byte b1, byte b2, byte b3) noexcept {
		return iris::encodeUint32LE(b0, b1, b2, b3);
	}
	inline constexpr RegisterValue encodeRegisterValue(byte b0, byte b1, byte b2, byte b3, byte b4, byte b5, byte b6, byte b7) noexcept {
		return iris::encodeUint64LE(b0, b1, b2, b3, b4, b5, b6, b7);
	}
	inline void decodeRegisterValue(RegisterValue value, byte* storage) noexcept;
	inline constexpr RegisterValue encodeRegisterValue(Word lower, Word upper) noexcept {
		return iris::encodeUint64LE(lower, upper);
	}
	enum ArchitectureConstants  {
		RegisterCount = 64,
		SegmentCount = 4096, // 1 gigabyte
		AddressMax = 65536 * SegmentCount,
		MaxInstructionCount = 16,
		MaxSystemCalls = 255,
		Bitmask = 0b11111111,
		InstructionPointer = RegisterCount  - 1,
		StackPointer = RegisterCount - 2,
	};
}

#include "iris19_defines.h"

namespace iris19 {
	class Instruction {
		public:
			Instruction(RawInstruction input) noexcept : _rawValue(input) { }
			Instruction(const Instruction&) = delete;
			RawInstruction getRawValue() const noexcept { return _rawValue; }
			bool markedImmediate() const noexcept { return decodeImmediateFlag(_rawValue); }
			byte getBitmask() const noexcept;
			byte getSubType() const noexcept;
			inline Operation getOperation() const noexcept { return decodeControl(_rawValue); }
			inline bool branchMarkedIf() const noexcept { return isOperation<Operation::Branch>() && decodeBranchFlagIsIfForm(_rawValue); }
			inline bool branchMarkedCall() const noexcept { return isOperation<Operation::Branch>() && decodeBranchFlagIsCallForm(_rawValue); }
			inline bool branchMarkedConditional() const noexcept { return isOperation<Operation::Branch>() && decodeBranchFlagIsConditional(_rawValue); }
			inline bool shiftLeft() const noexcept { return isOperation<Operation::Shift>() && decodeShiftFlagLeft(_rawValue); }
			byte getSource0() const noexcept;
			byte getSource1() const noexcept;
			RegisterValue getImmediate8() const noexcept;
			byte getDestination() const noexcept;

			template<typename T>
			inline T getSubType() const noexcept {
				return static_cast<T>(getSubType());
			}

			template<Operation op>
			inline bool isOperation() const noexcept {
				return decodeControl(_rawValue) == op;
			}


		private:
			inline Operation getControl() const noexcept { return decodeControl(_rawValue); }
			inline byte getDestinationIndex() const noexcept { return decodeDestinationIndex(_rawValue); }
			inline byte getSource0Index() const noexcept { return decodeSource0Index(_rawValue); }
			inline byte getSource1Index() const noexcept { return decodeSource1Index(_rawValue); }
			inline byte getShortImmediate() const noexcept { return decodeShortImmediate(_rawValue); }
			inline byte getRawBitmask() const noexcept { return decodeRawBitmask(_rawValue); }
			inline byte getArithmeticFlagType() const noexcept { return decodeArithmeticFlagType(_rawValue); }
			inline byte getLogicalFlagType() const noexcept { return decodeLogicalFlagType(_rawValue); }
			inline byte getCompareType() const noexcept { return decodeCompareType(_rawValue); }
			inline byte getMoveSubtype() const noexcept { return decodeMoveSubtype(_rawValue); }
		private:
			RawInstruction _rawValue;
	};

    inline constexpr Word encodeWord(bool lower, bool upperLower, bool lowerUpper, bool upperMost) noexcept {
        return iris::expandUInt32LE(lower, upperLower, lowerUpper, upperMost);
    }
	inline constexpr Word lowerMask(byte bitmask) {
        return encodeWord(iris::getBit<byte, 0>(bitmask), iris::getBit<byte, 1>(bitmask), iris::getBit<byte, 2>(bitmask), iris::getBit<byte, 3>(bitmask));
	}
	inline constexpr Word upperMask(byte bitmask) {
        return encodeWord(iris::getBit<byte, 4>(bitmask), iris::getBit<byte, 5>(bitmask), iris::getBit<byte, 6>(bitmask), iris::getBit<byte, 7>(bitmask));
	}
	inline constexpr RegisterValue mask(byte bitmask) { return iris::encodeUint64LE(lowerMask(bitmask), upperMask(bitmask)); }
	inline constexpr bool readLower(byte bitmask) noexcept { return iris::getLowerHalf(bitmask) > 0; }
	inline constexpr bool readUpper(byte bitmask) noexcept { return iris::getUpperHalf(bitmask) > 0; }
	inline constexpr byte registerGetActualIndex(byte value) { return iris::decodeBits<byte, byte, 0b00111111, 0>(value); }
	inline constexpr bool registerIsMarkedIndirect(byte value) { return iris::getBit<byte, 6>(value); }
	inline constexpr bool registerIsMarkedStack(byte value) { return iris::getBit<byte, 7>(value); }
	inline constexpr byte encodeRegisterIndex(byte index, bool indirect, bool stack) {
		return iris::setBit<byte, 7>(iris::setBit<byte, 6>(iris::encodeBits<byte, byte, 0b00111111, 0>(0, index), indirect), stack);
	}
	constexpr auto bitmask64 = mask(0b11111111);
	constexpr auto memoryMaxBitmask = 0b00001111111111111111111111111111;
	constexpr auto lower32Mask = mask(0b00001111);
	constexpr auto upper32Mask = mask(0b11110000);

	inline constexpr Word decodeUpperHalf(RegisterValue value) noexcept {
		return iris::decodeBits<RegisterValue, Word, upper32Mask, 32>(value);
	}
	inline constexpr Word decodeLowerHalf(RegisterValue value) noexcept {
		return iris::decodeBits<RegisterValue, Word, lower32Mask, 0>(value);
	}

	int instructionSizeFromImmediateMask(byte bitmask);

    using RegisterFile = iris::FixedSizeLoadStoreUnit<RegisterValue, byte, ArchitectureConstants::RegisterCount>;
    using MemorySpace = iris::FixedSizeLoadStoreUnit<Word, RegisterValue, ArchitectureConstants::AddressMax>;
    using CompareUnit = iris::Comparator<RegisterValue>;
    using ALU = iris::ALU<RegisterValue>;

	class Core : public iris::Core {
		public:
			using SystemFunction = std::function<void(Core*, Instruction&&)>;
			enum DefaultHandlers {
				Terminate,
				GetC,
				PutC,
				Count,
			};
		public:
			Core() noexcept;
			virtual ~Core() noexcept;
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override;
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
			virtual void link(std::istream& stream) override;
			void installSystemHandler(byte index, SystemFunction fn);
			void cycle();
			bool shouldExecute() const { return execute; }
		private:
			void push(RegisterValue value, RegisterValue& ptr);
			void pushWord(Word value);
            void pushWord(Word value, RegisterValue& ptr);
			void pushDword(DWord value);
            void pushDword(DWord value, RegisterValue& ptr);
			Word popWord();
			Word popWord(RegisterValue& ptr);
			DWord popDword();
			DWord popDword(RegisterValue& ptr);
			static void defaultSystemHandler(Core* core, Instruction&& inst);
			static void terminate(Core* core, Instruction&& inst);
			static void getc(Core* core, Instruction&& inst);
			static void putc(Core* core, Instruction&& inst);
			SystemFunction getSystemHandler(byte index);
			void dispatch(Instruction&& inst);
			inline Word readNext() noexcept { return tryReadNext(true); }
			Word tryReadNext(bool readNext) noexcept;
			RegisterValue retrieveImmediate(byte bitmask) noexcept;
			void genericRegisterSet(byte registerTarget, RegisterValue value);
			RegisterValue genericRegisterGet(byte registerTarget);
			void branchSpecificOperation(Instruction&& current);
			void compareOperation(Instruction&& current);
			void arithmeticOperation(Instruction&& current);
			void logicalOperation(Instruction&& current);
			void shiftOperation(Instruction&& current);
			void moveOperation(Instruction&& current);

			RegisterValue& registerValue(byte index);
			inline RegisterValue& getInstructionPointer() noexcept     { return gpr[ArchitectureConstants::InstructionPointer]; }
			inline RegisterValue& getStackPointer() noexcept           { return gpr[ArchitectureConstants::StackPointer]; }

			void incrementInstructionPointer() noexcept;
			void incrementStackPointer() noexcept;
			void decrementStackPointer() noexcept;
			void decrementStackPointer(RegisterValue& ptr) noexcept;
			void incrementStackPointer(RegisterValue& ptr) noexcept;
			void incrementAddress(RegisterValue& ptr) noexcept;
			void decrementAddress(RegisterValue& ptr) noexcept;
			Word getCurrentCodeWord() noexcept;
			void storeWord(RegisterValue address, Word value);
			Word loadWord(RegisterValue address);
			RegisterValue loadRegisterValue(RegisterValue address);
			void storeRegisterValue(RegisterValue address, RegisterValue value);
        private:
            void baseALUOperation(Instruction&& current, ALU::Operation op, RegisterValue s0, RegisterValue s1);

		private:
			bool execute = true,
				 advanceIp = true;
            RegisterFile gpr;
            MemorySpace memory;
            CompareUnit _compare;
            ALU _alu;
			SystemFunction systemHandlers[ArchitectureConstants::MaxSystemCalls] =  { 0 };
	};

	struct InstructionEncoder {
		int currentLine;
		RegisterValue address;
		Operation type;
		bool immediate;
		bool shiftLeft;
		bool isIf;
		bool isCall;
		bool isConditional;
		bool indirect;
		bool readNextWord;
		byte bitmask;
		byte arg0;
		byte arg1;
		byte arg2;
		bool isLabel;
		std::string labelValue;
		byte subType;
		RegisterValue fullImmediate;
		using Encoding = std::tuple<int, Word, Word, Word>;
		int numWords();
		Encoding encode();
		void clear();
		private:
#define DefEnum(a, b)
#define EndDefEnum(a, b, c)
#define EnumEntry(type) Encoding encode ## type ();
#include "def/iris19/ops.def"
#undef DefEnum
#undef EndDefEnum
#undef EnumEntry
		Word setControl(Word value = 0) const noexcept;
		Word setBitmask(Word value) const noexcept;
		Word setDestination(Word value) const noexcept;
		Word setSource0(Word value) const noexcept;
		Word setSource1(Word value) const noexcept;
		Word setImmediateFlag(Word value) const noexcept;
		Word setShortImmediate(Word value) const noexcept;
		Word lowerHalf() const noexcept;
		Word upperHalf() const noexcept;
		Word maskedLowerHalf() const noexcept;
		Word maskedUpperHalf() const noexcept;
		template<Operation op>
		Word singleWordEncoding() const noexcept {
			auto enc = setSubType<op>(setControl());
			enc = setDestination(enc);
			enc = setSource0(enc);
			enc = immediate ? setShortImmediate(enc) : setSource1(enc);
			return enc;
		}
		template<Operation op>
		inline Word setSubType(Word value) const noexcept {
			switch(op) {
				case Operation::Arithmetic:
					return setImmediateFlag(encodeArithmeticFlagType(value, subType));
				case Operation::Move:
					return encodeMoveSubtype(value, subType);
				case Operation::Compare:
					return setImmediateFlag(encodeCompareType(value, subType));
				case Operation::Shift:
					return setImmediateFlag(encodeShiftFlagLeft(value, shiftLeft));
				case Operation::Logical:
					return setImmediateFlag(encodeLogicalFlagType(value, subType));
				case Operation::Branch:
					return setImmediateFlag(
							encodeBranchFlagIsConditional(
								encodeBranchFlagIsCallForm(
									encodeBranchFlagIsIfForm(value, isIf),
									isCall),
								isConditional));

				default:
					return value;
			}
		}
	};
	Core* newCore() noexcept;
	void assemble(FILE* input, std::ostream* output);
} // end namespace iris19

#endif // end _TARGET_IRIS19_IRIS_H
