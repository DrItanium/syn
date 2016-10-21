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
	inline constexpr Word encodeWord (byte a, byte b) noexcept;
	inline constexpr RegisterValue encodeRegisterValue(byte a, byte b, byte c, byte d) noexcept;
	inline void decodeWord(Word value, byte* storage) noexcept;
	inline void decodeRegisterValue(RegisterValue value, byte* storage) noexcept;
	inline Word decodeUpperHalf(RegisterValue value) noexcept;
	inline Word decodeLowerHalf(RegisterValue value) noexcept;
	inline constexpr RegisterValue encodeUpperHalf(RegisterValue value, Word upperHalf) noexcept;
	inline constexpr RegisterValue encodeLowerHalf(RegisterValue value, Word lowerHalf) noexcept;
	inline constexpr RegisterValue encodeRegisterValue(Word upper, Word lower) noexcept;
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
		AddressRegister = R61,
		ValueRegister = R60,
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
#include "def/iris19/syscalls.def"
#include "def/iris19/compare.enum"
#include "def/iris19/logical.enum"
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
	template<byte bitmask>
		struct SetBitmaskToWordMask {
			static_assert(bitmask <= ArchitectureConstants::Bitmask, "Bitmask is too large and must be less than or equals to 0b11111111");
			static constexpr Word upperMask = encodeWord(iris::getBit<byte,4>(bitmask), iris::getBit<byte,5>(bitmask), iris::getBit<byte,6>(bitmask), iris::getBit<byte,7>(bitmask));
			static constexpr Word lowerMask = encodeWord(iris::getBit<byte,0>(bitmask), iris::getBit<byte,1>(bitmask), iris::getBit<byte,2>(bitmask), iris::getBit<byte,3>(bitmask));
			static constexpr RegisterValue mask = iris::encodeUint64LE(lowerMask, upperMask);

			static constexpr bool readLower = lowerMask != 0;
            static constexpr bool readUpper = upperMask != 0;
			SetBitmaskToWordMask() = delete;
			~SetBitmaskToWordMask() = delete;
		};
	inline constexpr Word lowerMask(byte bitmask) {
        return encodeWord(iris::getBit<byte, 0>(bitmask), iris::getBit<byte, 1>(bitmask), iris::getBit<byte, 2>(bitmask), iris::getBit<byte, 3>(bitmask));
	}
	inline constexpr Word upperMask(byte bitmask) {
        return encodeWord(iris::getBit<byte, 4>(bitmask), iris::getBit<byte, 5>(bitmask), iris::getBit<byte, 6>(bitmask), iris::getBit<byte, 7>(bitmask));
	}
	inline constexpr RegisterValue mask(byte bitmask) { return iris::encodeUint64LE(upperMask(bitmask), lowerMask(bitmask)); }
	inline constexpr bool readLower(byte bitmask) noexcept { return iris::getLowerHalf(bitmask) != 0; }
	inline constexpr bool readUpper(byte bitmask) noexcept { return iris::getUpperHalf(bitmask) != 0; }
	inline constexpr byte registerGetActualIndex(byte value) { return iris::decodeBits<byte, byte, 0b00111111, 0>(value); }
	inline constexpr bool registerIsMarkedIndirect(byte value) { return iris::getBit<byte, 6>(value); }
	inline constexpr bool registerIsMarkedStack(byte value) { return iris::getBit<byte, 7>(value); }
	constexpr auto bitmask64 =   SetBitmaskToWordMask<ArchitectureConstants::Bitmask>::mask;
	constexpr auto memoryMaxBitmask = 0b00001111111111111111111111111111;
	constexpr auto upper32Mask = SetBitmaskToWordMask<iris::upperByteHalf>::mask;
	constexpr auto lower32Mask = SetBitmaskToWordMask<iris::lowerByteHalf>::mask;

	template<bool isConditional, bool ifForm, bool callForm, bool immediateForm>
		struct BranchFlagsEncoder {
			static constexpr byte flags =
				iris::setBit<byte, 3>(
						iris::setBit<byte, 2>(
							iris::setBit<byte, 1>(
								iris::setBit<byte, 0>(0,
									immediateForm),
								callForm),
							ifForm),
						isConditional);
		};
	template<byte flags>
		struct BranchFlagsDecoder {
			static constexpr bool isImmediate = iris::getBit<byte, 0>(flags);
			static constexpr bool isCall = iris::getBit<byte, 1>(flags);
			static constexpr bool isIf = iris::getBit<byte, 2>(flags);
			static constexpr bool isConditional = iris::getBit<byte, 3>(flags);
		};
	using IfJump = BranchFlagsEncoder<false, true, false, false>;
	using IfCall = BranchFlagsEncoder<false, true, true, false>;

	using CallIndirect = BranchFlagsEncoder<false, false, true, false>;
	using CallDirect = BranchFlagsEncoder<false, false, true, true>;

	using JumpDirect = BranchFlagsEncoder<false, false, false, true>;
	using JumpIndirect = BranchFlagsEncoder<false, false, false, false>;

	using ConditionalJumpDirect = BranchFlagsEncoder<true, false, false, true>;
	using ConditionalJumpIndirect = BranchFlagsEncoder<true, false, false, false>;

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
#define X(title, func) void func ();
#include "def/iris19/misc.def"
#undef X
			template<byte rindex>
				inline RegisterValue& registerValue() noexcept {
					static_assert(rindex < ArchitectureConstants::RegisterCount, "Not a legal register index!");
                    switch (rindex) {
#define X(index) case index : return gpr[index];
#include "def/iris19/registers.def"
#undef X
                        default: {
                                     // if this is ever fired then we will get a std::terminate
                                     // invoked!
                                     std::stringstream msg;
                                     msg << "Out of range register index: " << rindex;
                                     throw iris::Problem(msg.str());
                                 }
                    }
				}
			inline Word readNext() noexcept { return tryReadNext(true); }
			Word tryReadNext(bool readNext) noexcept;
			RegisterValue retrieveImmediate(byte bitmask) noexcept;
			void genericRegisterSet(byte registerTarget, RegisterValue value);
			RegisterValue genericRegisterGet(byte registerTarget);
			void branchSpecificOperation(DecodedInstruction&& current);

			RegisterValue& registerValue(byte index);
			inline RegisterValue& getInstructionPointer() noexcept     { return registerValue<ArchitectureConstants::InstructionPointer>(); }
			inline RegisterValue& getStackPointer() noexcept           { return registerValue<ArchitectureConstants::StackPointer>(); }

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
