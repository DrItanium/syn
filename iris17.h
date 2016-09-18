#ifndef _TARGET_IRIS17_IRIS_H
#define _TARGET_IRIS17_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include "Problem.h"
#include <cstdint>
#include <sstream>
#include <memory>
#include <vector>
#include <tuple>
#include "sim_registration.h"

namespace iris17 {
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
		LinkRegister = R14,
		StackPointer = R13,
		ConditionRegister = R12,
		AddressRegister = R11,
		ValueRegister = R10,
		MaskRegister = R9,
		ShiftRegister = R8,
		FieldRegister = R8,
		CountRegister = R7,
	};

#define DefEnum(type, width) \
	enum class type : width {
#define EndDefEnum(type, width, maxCount) \
		Count, \
	};  \
	static_assert(static_cast<width>(type :: Count) <= static_cast<width>( maxCount ), "Too many " #type " entries defined!");
#define EnumEntry(type) type,

#include "def/iris17/ops.def"
#include "def/iris17/arithmetic_ops.def"
#include "def/iris17/syscalls.def"
#include "def/iris17/compare.enum"
#include "def/iris17/logical.enum"
#include "def/iris17/memory.enum"
#include "def/iris17/complex.def"
#undef DefEnum
#undef EnumEntry
#undef EndDefEnum

	class DecodedInstruction {
		public:
			DecodedInstruction(RawInstruction input) noexcept : _rawValue(input) { }
			DecodedInstruction(const DecodedInstruction&) = delete;
			RawInstruction getRawValue() const noexcept { return _rawValue; }
#define X(title, mask, shift, type, post) inline type get ## title () const noexcept { return iris::decodeBits<RawInstruction, type, mask, shift>(_rawValue); }
#include "def/iris17/instruction.def"
#undef X
		private:
			RawInstruction _rawValue;
	};

	template<byte bitmask>
		struct SetBitmaskToWordMask {
			static_assert(bitmask <= ArchitectureConstants::Bitmask, "Bitmask is too large and must be less than or equals to 0b1111");
			static constexpr bool decomposedBits[] = {
				iris::decodeBits<byte, bool, 0b0001, 0>(bitmask),
				iris::decodeBits<byte, bool, 0b0010, 1>(bitmask),
				iris::decodeBits<byte, bool, 0b0100, 2>(bitmask),
				iris::decodeBits<byte, bool, 0b1000, 3>(bitmask)
			};
			static constexpr byte determineMaskValue(bool value) noexcept { return value ? 0xFF : 0x00; }
			static constexpr Word encodeWord(bool upper, bool lower) noexcept {
				return iris::encodeUint16LE(determineMaskValue(lower), determineMaskValue(upper));
			}
			static constexpr Word lowerMask = encodeWord(decomposedBits[1], decomposedBits[0]);
			static constexpr Word upperMask = encodeWord(decomposedBits[3], decomposedBits[2]);
			static constexpr RegisterValue mask = iris::encodeUint32LE(lowerMask, upperMask);

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

	constexpr auto bitmask32 =   SetBitmaskToWordMask<ArchitectureConstants::Bitmask>::mask;
	constexpr auto bitmask24 =   SetBitmaskToWordMask<0b0111>::mask;
	constexpr auto upper16Mask = SetBitmaskToWordMask<0b1100>::mask;
	constexpr auto lower16Mask = SetBitmaskToWordMask<0b0011>::mask;

	RegisterValue getMask(byte bitmask);

	template<bool isConditional, bool ifForm, bool callForm, bool immediateForm>
		struct BranchFlagsEncoder {
			static constexpr byte flags =
				iris::encodeBits<byte, bool, 0b1000, 3>(
						iris::encodeBits<byte, bool, 0b0100, 2>(
							iris::encodeBits<byte, bool, 0b0010, 1>(
								iris::encodeBits<byte, bool, 0b0001, 0>(0,
									immediateForm),
								callForm),
							ifForm),
						isConditional);
		};
	template<byte flags>
		struct BranchFlagsDecoder {
			static constexpr bool isImmediate = iris::decodeBits<byte, bool, 0b0001, 0>(flags);
			static constexpr bool isCall = iris::decodeBits<byte, bool, 0b0010, 1>(flags);
			static constexpr bool isIf = iris::decodeBits<byte, bool, 0b0100, 2>(flags);
			static constexpr bool isConditional = iris::decodeBits<byte, bool, 0b1000, 3>(flags);
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

	template<byte bitmask>
		static constexpr int instructionSizeFromImmediateMask() {
			return 1 + (readLower<bitmask>() ? 1 : 0) + (readUpper<bitmask>() ? 1 : 0);
		}

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
			static void defaultSystemHandler(Core* core, DecodedInstruction&& inst);
			static void terminate(Core* core, DecodedInstruction&& inst);
			static void getc(Core* core, DecodedInstruction&& inst);
			static void putc(Core* core, DecodedInstruction&& inst);
			SystemFunction getSystemHandler(byte index);
			void dispatch(DecodedInstruction&& inst);
#define X(title, func) void func ();
#include "def/iris17/misc.def"
#undef X
			template<byte rindex>
				inline RegisterValue& registerValue() noexcept {
					static_assert(rindex < ArchitectureConstants::RegisterCount, "Not a legal register index!");
#define X(index) if (index == rindex) { return gpr[index]; }
#include "def/iris17/registers.def"
#undef X
					// if this is ever fired then we will get a std::terminate
					// invoked!
					std::stringstream msg;
					msg << "Out of range register index: " << rindex;
					throw iris::Problem(msg.str());
				}
			template<byte bitmask>
				RegisterValue retrieveImmediate() noexcept {
					static_assert(bitmask <= ArchitectureConstants::Bitmask, "Wider masks are being provided to retrieveImmediate!");
					if (!readLower<bitmask>() && !readUpper<bitmask>()) {
						return 0;
					} else {
						auto lower = 0u;
						auto upper = 0u;
						if (readLower<bitmask>()) {
							incrementInstructionPointer();
							lower = getCurrentCodeWord();
						}
						if (readUpper<bitmask>()) {
							incrementInstructionPointer();
							upper = static_cast<RegisterValue>(getCurrentCodeWord()) << 16;
						}
						return mask<bitmask>() & ( lower | upper );
					}
				}
			RegisterValue retrieveImmediate(byte bitmask) noexcept;

#define DefFlags(name) template<byte signature> struct name {
#define EndDefFlags(name) };
#define Component(fieldName, mask, shift, type) static constexpr type fieldName = (iris::decodeBits<byte, type, mask, shift>(signature));
#define Field(fieldName, type, value) static constexpr type fieldName = value ;
#include "def/iris17/logical_generic.sig"
#include "def/iris17/arithmetic.sig"
#include "def/iris17/move.sig"
#include "def/iris17/memory.sig"
#include "def/iris17/set.sig"
#undef Field
#undef Component
#undef DefFlags
#undef EndDefFlags
			template<ImmediateLogicalOps type, byte bitmask>
				void logicalImmediateOperation(DecodedInstruction&& inst) {
					auto &dest = registerValue(inst.getLogicalImmediateDestination());
					auto immediate = retrieveImmediate<bitmask>();
					if (type == ImmediateLogicalOps::And) {
						dest = iris::binaryAnd(dest, immediate);
					} else if (type == ImmediateLogicalOps::Or) {
						dest = iris::binaryOr(dest, immediate);
					} else if (type == ImmediateLogicalOps::Nand) {
						dest = iris::binaryNand(dest, immediate);
					} else if (type == ImmediateLogicalOps::Xor) {
						dest = iris::binaryXor(dest, immediate);
					} else {
						throw iris::Problem("Illegal immediate logical flag type");
					}
				}

			template<LogicalOps type>
				void logicalIndirectOperation(DecodedInstruction&& inst) {
					auto &dest = registerValue(inst.getLogicalRegister0());
					if (type == LogicalOps::Not) {
						dest = iris::binaryNot(dest);
					} else {
						auto src = registerValue(inst.getLogicalRegister1());
						if (type == LogicalOps::And) {
							dest = iris::binaryAnd(dest, src);
						} else if (type == LogicalOps::Or) {
							dest = iris::binaryOr(dest, src);
						} else if (type == LogicalOps::Xor) {
							dest = iris::binaryXor(dest, src);
						} else if (type == LogicalOps::Nand) {
							dest = iris::binaryNand(dest, src);
						} else {
							throw iris::Problem("Illegal indirect logical operation!");
						}
					}
				}

			template<byte signature>
				void arithmeticOperation(DecodedInstruction&& inst) {
					using aflags = ArithmeticFlags<signature>;
					auto src = aflags::immediate ? inst.getArithmeticImmediate() : registerValue(inst.getArithmeticSource());
					auto &dest = registerValue(inst.getArithmeticDestination());
					if (aflags::op == ArithmeticOps::Add) {
						dest = iris::add(dest, src);
					} else if (aflags::op == ArithmeticOps::Sub) {
						dest = iris::sub(dest, src);
					} else if (aflags::op == ArithmeticOps::Mul) {
						dest = iris::mul(dest, src);
					} else if (aflags::op == ArithmeticOps::Div) {
						dest = iris::div(dest, src);
					} else if (aflags::op == ArithmeticOps::Rem) {
						dest = iris::rem(dest, src);
					} else {
						throw iris::Problem("Illegal arithmetic operation!");
					}
				}

			template<MemoryOperation type, byte bitmask>
				inline void memoryOperation(DecodedInstruction&& inst) {
					static_assert(bitmask <= ArchitectureConstants::Bitmask, "bitmask is too large!");
					static constexpr auto useLower = readLower<bitmask>();
					static constexpr auto useUpper = readUpper<bitmask>();
					static constexpr auto lmask = lowerMask<bitmask>();
					static constexpr auto umask = upperMask<bitmask>();
					static constexpr auto fullMask = mask<bitmask>();
					auto upper = 0u;
					auto lower = 0u;
					if (type == MemoryOperation::Load) {
						auto address = getAddressRegister() + inst.getMemoryOffset();
						// use the destination field of the instruction to denote offset, thus we need
						// to use the Address and Value registers
						lower = useLower ? encodeLowerHalf(0, loadWord(address)) : 0;
						upper = useUpper ? encodeUpperHalf(0, loadWord(address + 1)) : 0;
						getValueRegister() = iris::encodeBits<RegisterValue, RegisterValue, fullMask, 0>(0, lower | upper);
					} else if (type == MemoryOperation::Store) {
						auto address = getAddressRegister() + inst.getMemoryOffset();
						if (useLower) {
							lower = lmask & decodeLowerHalf(getValueRegister());
							auto loader = loadWord(address) & ~(lmask);
							storeWord(address, lower | loader);
						}
						if (useUpper) {
							upper = umask & decodeUpperHalf(getValueRegister());
							auto loader = loadWord(address + 1) & ~(umask);
							storeWord(address + 1, upper | loader);
						}
					} else if (type == MemoryOperation::Push) {
						auto pushToStack = registerValue(inst.getMemoryOffset());
						// read backwards because the stack grows upward towards zero
						if (useUpper) {
							decrementStackPointer();
							storeWord(getStackPointer(), umask & decodeUpperHalf(pushToStack));
						}
						if (useLower) {
							decrementStackPointer();
							storeWord(getStackPointer(), lmask & decodeLowerHalf(pushToStack));
						}
					} else if (type == MemoryOperation::Pop) {
						if (useLower) {
							lower = lmask & loadWord(getStackPointer());
							incrementStackPointer();
						}
						if (useUpper) {
							upper = umask & loadWord(getStackPointer());
							incrementStackPointer();
						}
						registerValue(inst.getMemoryOffset()) = encodeRegisterValue(upper, lower);
					} else {
						throw iris::Problem("Illegal memory operation type!");
					}
				}
				inline void maskLinkRegister() noexcept {
					if (getLinkRegister() > bitmask24) {
						getLinkRegister() &= bitmask24; // make sure that we aren't over the memory setup
					}
				}
			template<byte flags>
				void branchSpecificOperation(DecodedInstruction&& current) {
					using decodedFlags = BranchFlagsDecoder<flags>;
					advanceIp = true;
					if (decodedFlags::isIf) {
						// if instruction
						advanceIp = false;
						if (decodedFlags::isCall) {
							getLinkRegister() = getInstructionPointer() + 1;
							maskLinkRegister();
						}
						getInstructionPointer() = bitmask24 & ((getConditionRegister() != 0) ? registerValue(current.getBranchIfOnTrue()) : registerValue(current.getBranchIfOnFalse()));
					} else if (decodedFlags::isCall) {
						// call instruction
						advanceIp = false;
						// determine next
						getLinkRegister() = getInstructionPointer() + decodedFlags::isImmediate ?  2 : 1;
						maskLinkRegister();
						if (decodedFlags::isImmediate) {
							incrementInstructionPointer();
							// make a 24 bit number
							getInstructionPointer() = bitmask24 & ((static_cast<RegisterValue>(getCurrentCodeWord()) << 8) | static_cast<RegisterValue>(current.getUpper()));
						} else {
							getInstructionPointer() = bitmask24 & registerValue(current.getBranchIndirectDestination());
						}
					} else {
						// jump instruction
						if (decodedFlags::isImmediate) {
							incrementInstructionPointer();
							if ((decodedFlags::isConditional && getConditionRegister() != 0) || !decodedFlags::isConditional) {
								advanceIp = false;
								getInstructionPointer() = bitmask24 & (current.getUpper() | static_cast<RegisterValue>(getCurrentCodeWord()) << 8);
							}
						}  else {
							if ((decodedFlags::isConditional && getConditionRegister() != 0) || !decodedFlags::isConditional) {
								advanceIp = false;
								getInstructionPointer() = bitmask24 & registerValue(current.getBranchIndirectDestination());
							}
						}
					}
				}

			RegisterValue& registerValue(byte index);
			inline RegisterValue& getInstructionPointer() noexcept     { return registerValue<ArchitectureConstants::InstructionPointer>(); }
			inline RegisterValue& getStackPointer() noexcept           { return registerValue<ArchitectureConstants::StackPointer>(); }
			inline RegisterValue& getConditionRegister() noexcept      { return registerValue<ArchitectureConstants::ConditionRegister>(); }
			inline RegisterValue& getLinkRegister() noexcept           { return registerValue<ArchitectureConstants::LinkRegister>(); }
			inline RegisterValue& getAddressRegister() noexcept        { return registerValue<ArchitectureConstants::AddressRegister>(); }
			inline RegisterValue& getValueRegister() noexcept          { return registerValue<ArchitectureConstants::ValueRegister>(); }
			inline RegisterValue& getMaskRegister() noexcept           { return registerValue<ArchitectureConstants::MaskRegister>(); }
			inline RegisterValue& getCountRegister() noexcept          { return registerValue<ArchitectureConstants::CountRegister>(); }

			inline RegisterValue getShiftRegister() noexcept           { return 0b11111 & registerValue<ArchitectureConstants::ShiftRegister>(); }
			inline RegisterValue getFieldRegister() noexcept           { return 0b11111 & registerValue<ArchitectureConstants::FieldRegister>(); }

			void incrementInstructionPointer() noexcept;
			void incrementStackPointer() noexcept;
			void decrementStackPointer() noexcept;
			Word getCurrentCodeWord() noexcept;
			void storeWord(RegisterValue address, Word value);
			Word loadWord(RegisterValue address);
			RegisterValue loadRegisterValue(RegisterValue address);
			void storeRegisterValue(RegisterValue address, RegisterValue value);
		private:
			void complexOperation(DecodedInstruction&& inst);
			void encodingOperation(DecodedInstruction&& inst);
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
#include "def/iris17/instruction.def"
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
		byte bitmask;
		byte arg0;
		byte arg1;
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
#include "def/iris17/ops.def"
#undef DefEnum
#undef EndDefEnum
#undef EnumEntry
	};
	Core* newCore() noexcept;
	void assemble(FILE* input, std::ostream* output);
}

#endif
