#ifndef _TARGET_CISC0_IRIS_H
#define _TARGET_CISC0_IRIS_H
#include "syn_base.h"
#include "ExecutionUnits.h"
#include "Core.h"
#include "Problem.h"
#include <cstdint>
#include <sstream>
#include <memory>
#include <vector>
#include <tuple>

namespace cisc0 {
	using HWord = uint8_t;
	using Word = uint16_t;
	using DWord = uint32_t;
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
		RegisterCount = 16,
		SegmentCount = 256,
		AddressMax = 65536 * SegmentCount,
		MaxInstructionCount = 16,
		MaxSystemCalls = 64,
		Bitmask = 0b1111,
		// unlike iris16 and iris32, there is a limited set of registers with
		// a majority of them marked for explicit usage, instructions
		// themselves are still 16 bits wide but 32bits are extracted per
		// packet.
		R15 = RegisterCount - 1,
		R14 = RegisterCount - 2,
		R13 = RegisterCount - 3,
		R12 = RegisterCount - 4,
		R11 = RegisterCount - 5,
		R10 = RegisterCount - 6,
		R9  = RegisterCount - 7,
		R8  = RegisterCount - 8,
		R7  = RegisterCount - 9,
		R6  = RegisterCount - 10,
		R5  = RegisterCount - 11,
		R4  = RegisterCount - 12,
		R3  = RegisterCount - 13,
		R2  = RegisterCount - 14,
		R1  = RegisterCount - 15,
		R0  = RegisterCount - 16,
		InstructionPointer = R15,
		StackPointer = R14,
		ConditionRegister = R13,
		AddressRegister = R12,
		ValueRegister = R11,
		MaskRegister = R10,
		ShiftRegister = R9,
		FieldRegister = R9,
	};
} // end namespace cisc0

#include "cisc0_defines.h"

namespace cisc0 {
	class DecodedInstruction {
		public:
			DecodedInstruction(RawInstruction input) noexcept : _rawValue(input) { }
			DecodedInstruction(const DecodedInstruction&) = delete;
			RawInstruction getRawValue() const noexcept { return _rawValue; }
#define X(title, mask, shift, type, post) inline type get ## title () const noexcept { return decode ## title ( _rawValue ); }
#include "def/cisc0/instruction.def"
#undef X
		private:
			RawInstruction _rawValue;
	};

	template<byte bitmask>
		struct SetBitmaskToWordMask {
			static_assert(bitmask <= ArchitectureConstants::Bitmask, "Bitmask is too large and must be less than or equals to 0b1111");
			static constexpr bool decomposedBits[] = {
                syn::getBit<byte, 0>(bitmask),
                syn::getBit<byte, 1>(bitmask),
                syn::getBit<byte, 2>(bitmask),
                syn::getBit<byte, 3>(bitmask),
			};
			static constexpr Word encodeWord(bool upper, bool lower) noexcept {
				return syn::encodeUint16LE(syn::expandBit(lower), syn::expandBit(upper));
			}
			static constexpr Word lowerMask = encodeWord(decomposedBits[1], decomposedBits[0]);
			static constexpr Word upperMask = encodeWord(decomposedBits[3], decomposedBits[2]);
			static constexpr RegisterValue mask = syn::encodeUint32LE(lowerMask, upperMask);

			static constexpr bool readLower = decomposedBits[1] || decomposedBits[0];
			static constexpr bool readUpper = decomposedBits[2] || decomposedBits[3];
		};
	template<byte bitmask>
		inline constexpr RegisterValue mask() noexcept { return SetBitmaskToWordMask<bitmask>::mask; }
	template<byte bitmask>
		inline constexpr Word lowerMask() noexcept { return SetBitmaskToWordMask<bitmask>::lowerMask; }
	template<byte bitmask>
		inline constexpr Word upperMask() noexcept { return SetBitmaskToWordMask<bitmask>::upperMask; }
	template<byte bitmask>
		inline constexpr bool readLower() noexcept { return SetBitmaskToWordMask<bitmask>::readLower; }
	template<byte bitmask>
		inline constexpr bool readUpper() noexcept { return SetBitmaskToWordMask<bitmask>::readUpper; }

	inline constexpr Word lowerMask(byte bitmask) noexcept {
		return syn::encodeUint16LE(syn::expandBit(syn::getBit<byte, 0>(bitmask)),
									syn::expandBit(syn::getBit<byte, 1>(bitmask)));
	}
	inline constexpr Word upperMask(byte bitmask) noexcept {
		return syn::encodeUint16LE(syn::expandBit(syn::getBit<byte, 2>(bitmask)),
									syn::expandBit(syn::getBit<byte, 3>(bitmask)));
	}

	inline constexpr RegisterValue mask(byte bitmask) noexcept {
		return syn::encodeUint32LE(lowerMask(bitmask), upperMask(bitmask));
	}

	inline constexpr bool readLower(byte bitmask) noexcept {
		return lowerMask(bitmask) != 0;
	}
	inline constexpr bool readUpper(byte bitmask) noexcept {
		return upperMask(bitmask) != 0;
	}

	constexpr auto bitmask32 =   SetBitmaskToWordMask<ArchitectureConstants::Bitmask>::mask;
	constexpr auto bitmask24 =   SetBitmaskToWordMask<0b0111>::mask;
	constexpr auto upper16Mask = SetBitmaskToWordMask<0b1100>::mask;
	constexpr auto lower16Mask = SetBitmaskToWordMask<0b0011>::mask;

	RegisterValue getMask(byte bitmask);

	int instructionSizeFromImmediateMask(byte bitmask);

	template<byte bitmask>
		static constexpr int instructionSizeFromImmediateMask() {
			return 1 + (readLower<bitmask>() ? 1 : 0) + (readUpper<bitmask>() ? 1 : 0);
		}

	using ALU = syn::ALU<RegisterValue>;
	using CompareUnit = syn::Comparator<RegisterValue>;
	using RegisterFile = syn::FixedSizeLoadStoreUnit<RegisterValue, byte, ArchitectureConstants::RegisterCount>;
	using MemorySpace = syn::FixedSizeLoadStoreUnit<Word, DWord, ArchitectureConstants::AddressMax>;
	class Core : public syn::Core {
		public:
			using SystemFunction = std::function<void(Core*, DecodedInstruction&&)>;
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
			virtual void link(std::istream& stream) override;
			void installSystemHandler(byte index, SystemFunction fn);
			virtual bool cycle() override;
			bool shouldExecute() const { return execute; }
		private:
			void pushWord(Word value);
            void pushWord(Word value, RegisterValue& ptr);
			void pushDword(DWord value);
            void pushDword(DWord value, RegisterValue& ptr);
			Word popWord();
			Word popWord(RegisterValue& ptr);
			static void defaultSystemHandler(Core* core, DecodedInstruction&& inst);
			static void terminate(Core* core, DecodedInstruction&& inst);
			static void getc(Core* core, DecodedInstruction&& inst);
			static void putc(Core* core, DecodedInstruction&& inst);
			SystemFunction getSystemHandler(byte index);
			void dispatch(DecodedInstruction&& inst);
#define X(title, func) void func ();
#include "def/cisc0/misc.def"
#undef X
			template<byte rindex>
				inline RegisterValue& registerValue() noexcept {
					static_assert(rindex < ArchitectureConstants::RegisterCount, "Not a legal register index!");
					return gpr[rindex];
				}
            template<bool readNext>
            inline Word tryReadNext() noexcept {
                if (readNext) {
                    incrementInstructionPointer();
                    return getCurrentCodeWord();
                } else {
                    return 0;
                }
            }
			inline Word tryReadNext(bool readNext) noexcept {
				if (readNext) {
					return tryReadNext<true>();
				} else {
					return tryReadNext<false>();
				}
			}
			template<byte bitmask>
				RegisterValue retrieveImmediate() noexcept {
					static_assert(bitmask <= ArchitectureConstants::Bitmask, "Wider masks are being provided to retrieveImmediate!");
					static constexpr auto useLower = readLower<bitmask>();
					static constexpr auto useUpper = readUpper<bitmask>();
					if (!useLower && !useUpper) {
						return 0;
					} else {
						auto lower = tryReadNext<useLower>();
						auto upper = static_cast<RegisterValue>(tryReadNext<useUpper>()) << 16;
						return mask<bitmask>() & (lower | upper);
					}
				}
			RegisterValue retrieveImmediate(byte bitmask) noexcept;

			RegisterValue& registerValue(byte index);
			inline RegisterValue& getInstructionPointer() noexcept     { return registerValue<ArchitectureConstants::InstructionPointer>(); }
			inline RegisterValue& getStackPointer() noexcept           { return registerValue<ArchitectureConstants::StackPointer>(); }
			inline RegisterValue& getConditionRegister() noexcept      { return registerValue<ArchitectureConstants::ConditionRegister>(); }
			inline RegisterValue& getAddressRegister() noexcept        { return registerValue<ArchitectureConstants::AddressRegister>(); }
			inline RegisterValue& getValueRegister() noexcept          { return registerValue<ArchitectureConstants::ValueRegister>(); }
			inline RegisterValue& getMaskRegister() noexcept           { return registerValue<ArchitectureConstants::MaskRegister>(); }

			inline RegisterValue getShiftRegister() noexcept           { return 0b11111 & registerValue<ArchitectureConstants::ShiftRegister>(); }
			inline RegisterValue getFieldRegister() noexcept           { return 0b11111 & registerValue<ArchitectureConstants::FieldRegister>(); }

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
			void complexOperation(DecodedInstruction&& inst);
			void encodingOperation(DecodedInstruction&& inst);
			void performEncodeOp(DecodedInstruction&& inst);
			void memoryManipulationOperation(DecodedInstruction&& inst);
		private:
			bool execute = true,
				 advanceIp = true;
			RegisterFile gpr;
			ALU _alu;
			ALU _shifter;
			ALU _logicalOps;
			CompareUnit _compare;
			syn::BooleanCombineUnit _bCombine;
			MemorySpace memory;
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
		CompareCombine combineType;
		RegisterValue fullImmediate;
		using Encoding = std::tuple<int, Word, Word, Word>;
		int numWords();
		Encoding encode();
		void clear();
		private:
#define DefEnum(a, b)
#define EndDefEnum(a, b, c)
#define EnumEntry(type) Encoding encode ## type ();
#include "def/cisc0/ops.def"
#undef DefEnum
#undef EndDefEnum
#undef EnumEntry
	};
	Core* newCore() noexcept;
	void assemble(const std::string& iName, FILE* input, std::ostream* output);
} // end namespace cisc0

#endif // end _TARGET_CISC0_IRIS_H
