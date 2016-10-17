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
#include "def/iris19/memory.enum"
#include "def/iris19/complex.def"
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

	template<byte bitmask>
		struct SetBitmaskToWordMask {
			static_assert(bitmask <= ArchitectureConstants::Bitmask, "Bitmask is too large and must be less than or equals to 0b1111");
			static constexpr bool decomposedBits[] = {
				iris::decodeBits<byte, bool, 0b00000001, 0>(bitmask),
				iris::decodeBits<byte, bool, 0b00000010, 1>(bitmask),
				iris::decodeBits<byte, bool, 0b00000100, 2>(bitmask),
				iris::decodeBits<byte, bool, 0b00001000, 3>(bitmask),
				iris::decodeBits<byte, bool, 0b00010000, 4>(bitmask),
				iris::decodeBits<byte, bool, 0b00100000, 5>(bitmask),
				iris::decodeBits<byte, bool, 0b01000000, 6>(bitmask),
				iris::decodeBits<byte, bool, 0b10000000, 7>(bitmask),
			};
			static constexpr byte determineMaskValue(bool value) noexcept { return value ? 0xFF : 0x00; }
			static constexpr Word encodeWord(bool upper, bool upperlower, bool lowerupper, bool lower) noexcept {
				return iris::encodeUint32LE(determineMaskValue(upper), determineMaskValue(upperlower), determineMaskValue(lowerupper), determineMaskValue(lower));
			}
			static constexpr Word upperMask = encodeWord(decomposedBits[7], decomposedBits[6], decomposedBits[5], decomposedBits[4]);
			static constexpr Word lowerMask = encodeWord(decomposedBits[3], decomposedBits[2], decomposedBits[1], decomposedBits[0]);
			static constexpr RegisterValue mask = iris::encodeUint64LE(lowerMask, upperMask);

			static constexpr bool readLower = decomposedBits[1] || decomposedBits[0] || decomposedBits[2] || decomposedBits[3];
			static constexpr bool readUpper = decomposedBits[4] || decomposedBits[5] || decomposedBits[6] || decomposedBits[7];
			SetBitmaskToWordMask() = delete;
			~SetBitmaskToWordMask() = delete;
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
	template<byte registerValue>
	struct DecodedRegister {
		static constexpr bool isIndirect = iris::decodeBits<byte, bool, 0b01000000, 6>(registerValue);
		static constexpr bool isStack = iris::decodeBits<byte, bool, 0b10000000, 7>(registerValue);
		static constexpr byte rawValue = registerValue;
		static constexpr byte actualIndex = iris::decodeBits<byte, byte, 0b00111111, 0>(registerValue);
	};
	inline byte decodeRegisterIndex(byte index) noexcept {
		switch (index) {
#define X(index) case index: return DecodedRegister<index>::actualIndex;
#include "def/iris19/bitmask8bit.def"
#undef X
			default:
				throw iris::Problem("Register index out of range, Should never ever get here!");
		}
	}
	inline bool registerMarkedIndirect(byte index) noexcept {
		switch (index) {
#define X(index) case index: return DecodedRegister<index>::isIndirect;
#include "def/iris19/bitmask8bit.def"
#undef X
			default:
				throw iris::Problem("Register index out of range, Should never ever get here!");
		}
	}

	inline bool registerMarkedStack(byte index) noexcept {
		switch (index) {
#define X(index) case index: return DecodedRegister<index>::isStack;
#include "def/iris19/bitmask8bit.def"
#undef X
			default:
				throw iris::Problem("Register index out of range, Should never ever get here!");
		}
	}

	constexpr auto bitmask64 =   SetBitmaskToWordMask<ArchitectureConstants::Bitmask>::mask;
	constexpr auto memoryMaxBitmask = 0b00001111111111111111111111111111;
	constexpr auto upper32Mask = SetBitmaskToWordMask<0b11110000>::mask;
	constexpr auto lower32Mask = SetBitmaskToWordMask<0b00001111>::mask;

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
#define X(index) if (index == rindex) { return gpr[index]; }
#include "def/iris19/registers.def"
#undef X
					// if this is ever fired then we will get a std::terminate
					// invoked!
					std::stringstream msg;
					msg << "Out of range register index: " << rindex;
					throw iris::Problem(msg.str());
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

#define DefFlags(name) template<byte signature> struct name {
#define EndDefFlags(name) };
#define Component(fieldName, mask, shift, type) static constexpr type fieldName = (iris::decodeBits<byte, type, mask, shift>(signature));
#define Field(fieldName, type, value) static constexpr type fieldName = value ;
#include "def/iris19/logical_generic.sig"
#include "def/iris19/arithmetic.sig"
#include "def/iris19/move.sig"
#include "def/iris19/memory.sig"
#include "def/iris19/set.sig"
#undef Field
#undef Component
#undef DefFlags
#undef EndDefFlags
			template<ImmediateLogicalOps type, byte bitmask>
				inline void logicalImmediateOperation(DecodedInstruction&& inst) {
					auto &dest = registerValue(inst.getLogicalImmediateDestination());
					if (type == ImmediateLogicalOps::And) {
						dest = iris::binaryAnd(dest, retrieveImmediate<bitmask>());
					} else if (type == ImmediateLogicalOps::Or) {
						dest = iris::binaryOr(dest, retrieveImmediate<bitmask>());
					} else if (type == ImmediateLogicalOps::Nand) {
						dest = iris::binaryNand(dest, retrieveImmediate<bitmask>());
					} else if (type == ImmediateLogicalOps::Xor) {
						dest = iris::binaryXor(dest, retrieveImmediate<bitmask>());
					} else {
						throw iris::Problem("Illegal immediate logical flag type");
					}
				}

			template<LogicalOps type>
				inline void logicalIndirectOperation(DecodedInstruction&& inst) {
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
					auto result = 0u;
					auto src0 = genericRegisterGet(inst.getArithmeticSource());
					auto src1 = aflags::immediate ? inst.getArithmeticImmediate() : genericRegisterGet(inst.getArithmeticSource());
					switch (aflags::op) {
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
							throw iris::Problem("Illegal arithmetic operation!");
					}
					genericRegisterSet(inst.getArithmeticDestination(), result);
				}
			RegisterValue registerStackPop(byte reg);
			RegisterValue registerIndirectLoad(byte reg);
			void registerIndirectStore(byte reg, RegisterValue value);
			void registerStackPush(byte reg, RegisterValue value);
			template<byte reg>
			RegisterValue registerValueGet() {
				return registerValue<reg>();
			}
			template<byte reg>
			void registerValueSet(RegisterValue value) {
				registerValue<reg>() = value;
			}
			template<byte reg>
			void registerStackPush(RegisterValue value) {
				pushDword(value, registerValue<reg>());
			}
			template<byte reg>
			RegisterValue registerStackPop() {
				return popDword(registerValue<reg>());
			}
			template<byte reg>
			void registerIndirectStore(RegisterValue value) {
				storeRegisterValue(registerValue<reg>(), value);
			}
			template<byte reg>
			RegisterValue registerIndirectLoad() {
				return loadRegisterValue(registerValue<reg>());
			}
			template<byte fullReg>
			void genericRegisterSet(RegisterValue value) {
				if (DecodedRegister<fullReg>::isStack) {
					if (DecodedRegister<fullReg>::isIndirect) {
						throw iris::Problem("Unable to do both stack and indirect operations at the same time!");
					} else {
						registerStackPush<DecodedRegister<fullReg>::actualIndex>(value);
					}
				} else {
					if (DecodedRegister<fullReg>::isIndirect) {
						registerIndirectStore<DecodedRegister<fullReg>::actualIndex>(value);
					} else {
						registerValueSet<DecodedRegister<fullReg>::actualIndex>(value);
					}
				}
			}
			template<byte fullReg>
			RegisterValue genericRegisterGet() {
				if (DecodedRegister<fullReg>::isStack) {
					if (DecodedRegister<fullReg>::isIndirect) {
						throw iris::Problem("Unable to do both stack and indirect operations at the same time!");
					} else {
						return registerStackPop<DecodedRegister<fullReg>::actualIndex>();
					}
				} else {
					if (DecodedRegister<fullReg>::isIndirect) {
						return registerIndirectLoad<DecodedRegister<fullReg>::actualIndex>();
					} else {
						return registerValueGet<DecodedRegister<fullReg>::actualIndex>();
					}
				}
			}
			void genericRegisterSet(byte registerTarget, RegisterValue value);
			RegisterValue genericRegisterGet(byte registerTarget);
			template<byte flags>
				void branchSpecificOperation(DecodedInstruction&& current) {
					using decodedFlags = BranchFlagsDecoder<flags>;
					advanceIp = true;
					if (decodedFlags::isIf) {
						// if instruction
						advanceIp = false;
						if (decodedFlags::isCall) {
							// push the instruction pointer plus one onto the
							// stack
							pushDword((getInstructionPointer() + 1) & memoryMaxBitmask);
						}
						getInstructionPointer() = memoryMaxBitmask & ((getConditionRegister(current.getBranchCondition()) != 0) ? registerValue(current.getBranchIfOnTrue()) : registerValue(current.getBranchIfOnFalse()));
#ifdef DEBUG
						std::cout << "if: jumping to " << std::hex << getInstructionPointer() << std::endl;
#endif
					} else if (decodedFlags::isCall) {
						// call instruction
						advanceIp = false;
						// determine next
						pushDword((getInstructionPointer() + decodedFlags::isImmediate ? 2 : 1) & memoryMaxBitmask);
						auto address = 0u;
						if (decodedFlags::isImmediate) {
							// make a 24 bit number
							auto upper16 = static_cast<RegisterValue>(tryReadNext<decodedFlags::isImmediate>()) << 8;
							auto lower8 = static_cast<RegisterValue>(current.getUpper());
							address = upper16 | lower8;
						} else {
							address = registerValue(current.getBranchIndirectDestination());
						}
						getInstructionPointer() = memoryMaxBitmask & address;
#ifdef DEBUG
						std::cout << "call: Jumping to " << std::hex << getInstructionPointer() << std::endl;
#endif
					} else {
						// jump instruction
						if (decodedFlags::isImmediate) {
							incrementInstructionPointer();
							if ((decodedFlags::isConditional && getConditionRegister(current.getBranchCondition()) != 0) || !decodedFlags::isConditional) {
								advanceIp = false;
								getInstructionPointer() = memoryMaxBitmask & (current.getUpper() | static_cast<RegisterValue>(getCurrentCodeWord()) << 8);
							}
						} else {
							if ((decodedFlags::isConditional && getConditionRegister(current.getBranchCondition()) != 0) || !decodedFlags::isConditional) {
								advanceIp = false;
								getInstructionPointer() = memoryMaxBitmask & registerValue(current.getBranchIndirectDestination());
							}
						}
#ifdef DEBUG
						std::cout << "one way Jumping to " << std::hex << getInstructionPointer() << std::endl;
#endif
					}
				}

			RegisterValue& registerValue(byte index);
			inline RegisterValue& getInstructionPointer() noexcept     { return registerValue<ArchitectureConstants::InstructionPointer>(); }
			inline RegisterValue& getStackPointer() noexcept           { return registerValue<ArchitectureConstants::StackPointer>(); }
			inline RegisterValue& getConditionRegister(byte index) noexcept      { return registerValue(index); }
			inline RegisterValue& getAddressRegister() noexcept        { return registerValue<ArchitectureConstants::AddressRegister>(); }
			inline RegisterValue& getValueRegister() noexcept          { return registerValue<ArchitectureConstants::ValueRegister>(); }

			inline void incrementInstructionPointer() noexcept;
			inline void incrementStackPointer() noexcept;
			inline void decrementStackPointer() noexcept;
			inline void decrementStackPointer(RegisterValue& ptr) noexcept;
			inline void incrementStackPointer(RegisterValue& ptr) noexcept;
			inline void incrementAddress(RegisterValue& ptr) noexcept;
			inline void decrementAddress(RegisterValue& ptr) noexcept;
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
