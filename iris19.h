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
#define X(index) \
		R ## index = (RegisterCount - (RegisterCount - index)),
#include "def/iris19/registers.def"
#undef X

		InstructionPointer = R63,
		StackPointer = R62,
	};

#define DefEnum(type, width) \
	enum class type : width {
#define EndDefEnum(type, width, maxCount) \
		Count, \
	};  \
	static_assert(static_cast<width>(type :: Count) <= static_cast<width>( maxCount ), "Too many " #type " entries defined!");
#define EnumEntry(type) type,
#include "def/iris19/ops.def"
#include "def/iris19/arithmetic_ops.def"
#include "def/iris19/compare.enum"
#include "def/iris19/logical.enum"
#include "def/iris19/move.def"
#undef DefEnum
#undef EnumEntry
#undef EndDefEnum

	class DecodedInstruction {
		public:
			DecodedInstruction(RawInstruction input) noexcept : _rawValue(input) { }
			DecodedInstruction(const DecodedInstruction&) = delete;
			RawInstruction getRawValue() const noexcept { return _rawValue; }
#define X(title, mask, shift, type, post) inline type get ## title () const noexcept { return iris::decodeBits<RawInstruction, type, mask, shift>(_rawValue); }
#include "def/iris19/instruction.def"
#undef X
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
	template<byte bitmask>
		struct SetBitmaskToWordMask {
			static_assert(bitmask <= ArchitectureConstants::Bitmask, "Bitmask is too large and must be less than or equals to 0b11111111");
			static constexpr RegisterValue mask = iris::encodeUint64LE(lowerMask(bitmask), upperMask(bitmask));
			SetBitmaskToWordMask() = delete;
			~SetBitmaskToWordMask() = delete;
		};
	inline constexpr RegisterValue mask(byte bitmask) { return iris::encodeUint64LE(upperMask(bitmask), lowerMask(bitmask)); }
	inline constexpr bool readLower(byte bitmask) noexcept { return iris::getLowerHalf(bitmask) != 0; }
	inline constexpr bool readUpper(byte bitmask) noexcept { return iris::getUpperHalf(bitmask) != 0; }
	inline constexpr byte registerGetActualIndex(byte value) { return iris::decodeBits<byte, byte, 0b00111111, 0>(value); }
	inline constexpr bool registerIsMarkedIndirect(byte value) { return iris::getBit<byte, 6>(value); }
	inline constexpr bool registerIsMarkedStack(byte value) { return iris::getBit<byte, 7>(value); }
	inline constexpr byte encodeRegisterValue(byte index, bool indirect, bool stack) {
		return iris::setBit<byte, 7>(iris::setBit<byte, 6>(iris::encodeBits<byte, byte, 0b00111111, 0>(0, index), indirect), stack);
	}
	constexpr auto bitmask64 =   SetBitmaskToWordMask<ArchitectureConstants::Bitmask>::mask;
	constexpr auto memoryMaxBitmask = 0b00001111111111111111111111111111;
	constexpr auto upper32Mask = SetBitmaskToWordMask<iris::upperByteHalf>::mask;
	constexpr auto lower32Mask = SetBitmaskToWordMask<iris::lowerByteHalf>::mask;

	inline constexpr Word decodeUpperHalf(RegisterValue value) noexcept {
		return iris::decodeBits<RegisterValue, Word, upper32Mask, 32>(value);
	}
	inline constexpr Word decodeLowerHalf(RegisterValue value) noexcept {
		return iris::decodeBits<RegisterValue, Word, lower32Mask, 0>(value);
	}

	int instructionSizeFromImmediateMask(byte bitmask);


	class Core : public iris::Core {
		public:
			using SystemFunction = std::function<void(Core*, DecodedInstruction&&)>;
			enum DefaultHandlers {
				Terminate,
				GetC,
				PutC,
				Count,
			};
		public:
			Core();
			virtual ~Core();
			virtual void initialize() override;
			virtual void installprogram(std::istream& stream) override;
			virtual void shutdown() override;
			virtual void dump(std::ostream& stream) override;
			virtual void run() override;
			virtual void link(std::istream& stream) override;
			std::shared_ptr<Word> getMemory();
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
			static void defaultSystemHandler(Core* core, DecodedInstruction&& inst);
			static void terminate(Core* core, DecodedInstruction&& inst);
			static void getc(Core* core, DecodedInstruction&& inst);
			static void putc(Core* core, DecodedInstruction&& inst);
			SystemFunction getSystemHandler(byte index);
			void dispatch(DecodedInstruction&& inst);
			inline Word readNext() noexcept { return tryReadNext(true); }
			Word tryReadNext(bool readNext) noexcept;
			RegisterValue retrieveImmediate(byte bitmask) noexcept;
			void genericRegisterSet(byte registerTarget, RegisterValue value);
			RegisterValue genericRegisterGet(byte registerTarget);
			void branchSpecificOperation(DecodedInstruction&& current);
			void compareOperation(DecodedInstruction&& current);
			void arithmeticOperation(DecodedInstruction&& current);
			void logicalOperation(DecodedInstruction&& current);
			void shiftOperation(DecodedInstruction&& current);
			void moveOperation(DecodedInstruction&& current);

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
			bool execute = true,
				 advanceIp = true;
			RegisterValue gpr[ArchitectureConstants::RegisterCount] = { 0 };
			std::shared_ptr<Word> memory;
			SystemFunction systemHandlers[ArchitectureConstants::MaxSystemCalls] =  { 0 };
	};


#define X(title, mask, shift, type, post) \
	constexpr inline Word encode ## title (Word input, type value) noexcept { \
		return iris::encodeBits<Word, type, mask, shift>(input, value); \
	}
#include "def/iris19/instruction.def"
#undef X
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
	};
	Core* newCore() noexcept;
	void assemble(FILE* input, std::ostream* output);
} // end namespace iris19

#endif // end _TARGET_IRIS19_IRIS_H
