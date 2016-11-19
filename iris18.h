#ifndef _TARGET_IRIS18_IRIS_H
#define _TARGET_IRIS18_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include "Problem.h"
#include <cstdint>
#include <sstream>
#include <memory>
#include <vector>
#include <tuple>
#include "sim_registration.h"

namespace iris18 {
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
} // end namespace iris18

#include "iris18_defines.h"

namespace iris18 {
	class DecodedInstruction {
		public:
			DecodedInstruction(RawInstruction input) noexcept : _rawValue(input) { }
			DecodedInstruction(const DecodedInstruction&) = delete;
			RawInstruction getRawValue() const noexcept { return _rawValue; }
#define X(title, mask, shift, type, post) inline type get ## title () const noexcept { return decode ## title ( _rawValue ); }
#include "def/iris18/instruction.def"
#undef X
		private:
			RawInstruction _rawValue;
	};

	template<byte bitmask>
		struct SetBitmaskToWordMask {
			static_assert(bitmask <= ArchitectureConstants::Bitmask, "Bitmask is too large and must be less than or equals to 0b1111");
			static constexpr bool decomposedBits[] = {
                iris::getBit<byte, 0>(bitmask),
                iris::getBit<byte, 1>(bitmask),
                iris::getBit<byte, 2>(bitmask),
                iris::getBit<byte, 3>(bitmask),
			};
			static constexpr Word encodeWord(bool upper, bool lower) noexcept {
				return iris::encodeUint16LE(iris::expandBit(lower), iris::expandBit(upper));
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
            static constexpr byte flags = iris::setBit<byte, 3>(
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
#include "def/iris18/misc.def"
#undef X
			template<byte rindex>
				inline RegisterValue& registerValue() noexcept {
					static_assert(rindex < ArchitectureConstants::RegisterCount, "Not a legal register index!");
#define X(index) if (index == rindex) { return gpr[index]; }
#include "def/iris18/registers.def"
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
#include "def/iris18/logical_generic.sig"
#include "def/iris18/arithmetic.sig"
#include "def/iris18/move.sig"
#include "def/iris18/memory.sig"
#include "def/iris18/set.sig"
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
			template<MemoryOperation type, byte bitmask, bool indirect, bool readNext>
				inline void memoryOperation(DecodedInstruction&& inst) {
					static_assert(bitmask <= ArchitectureConstants::Bitmask, "bitmask is too large!");
					static constexpr auto useLower = readLower<bitmask>();
					static constexpr auto useUpper = readUpper<bitmask>();
					static constexpr auto lmask = lowerMask<bitmask>();
					static constexpr auto umask = upperMask<bitmask>();
					static constexpr auto fullMask = mask<bitmask>();
					auto upper = 0u;
					auto lower = 0u;
					auto memOffset = inst.getMemoryOffset();
                    DecodedInstruction next(tryReadNext<readNext>());
					if (type == MemoryOperation::Load) {
                        auto addr = readNext ? registerValue(next.getMemoryAddress()) : getAddressRegister();
                        auto& value = readNext ? registerValue(next.getMemoryValue()) : getValueRegister();
						if (!useLower && !useUpper) {
							value = 0; // zero out the register if nothing is going to be happening
						} else {
							auto address = addr + memOffset;
							if (indirect) {
								address = encodeRegisterValue(loadWord(address + 1), loadWord(address)) & bitmask24;
							}
							// use the value field of the instruction to denote offset, thus we need
							// to use the Address and Value registers
							lower = useLower ? encodeLowerHalf(0, loadWord(address)) : 0u;
							upper = useUpper ? encodeUpperHalf(0, loadWord(address + 1)) : 0u;
							value = iris::encodeBits<RegisterValue, RegisterValue, fullMask, 0>(0u, lower | upper);
						}
					} else if (type == MemoryOperation::Store) {
                        auto addr = readNext ? registerValue(next.getMemoryAddress()) : getAddressRegister();
                        auto value = readNext ? registerValue(next.getMemoryValue()) : getValueRegister();
						auto address = addr + memOffset;
						if (indirect) {
							address = encodeRegisterValue(loadWord(address + 1), loadWord(address)) & bitmask24;
						}
						if (useLower) {
							if (lmask == 0x0000FFFF) {
								storeWord(address, value);
							} else {
								storeWord(address, (lmask & value) | (loadWord(address) & ~lmask));
							}
						}
						if (useUpper) {
							if (umask == 0x0000FFFF) {
								storeWord(address, value);
							} else {
								storeWord(address, (umask & value) | (loadWord(address) & ~umask));
							}
						}
					} else if (type == MemoryOperation::Push) {
						if (indirect) {
							throw iris::Problem("Can't perform an indirect push");
						} else {
							// update the target stack to something different
							auto pushToStack = registerValue(memOffset);
							auto &stackPointer = readNext ? registerValue(next.getMemoryAddress()) : getStackPointer();
							// read backwards because the stack grows upward towards zero
							if (useUpper) {
								pushWord(umask & decodeUpperHalf(pushToStack), stackPointer);
							}
							if (useLower) {
								pushWord(lmask & decodeLowerHalf(pushToStack), stackPointer);
							}
						}
					} else if (type == MemoryOperation::Pop) {
						if (indirect) {
							throw iris::Problem("Can't perform an indirect pop!");
						} else {
							auto &stackPointer = readNext ? registerValue(next.getMemoryAddress()) : getStackPointer();
							if (useLower) {
								lower = lmask & popWord(stackPointer);
							}
							if (useUpper) {
								upper = umask & popWord(stackPointer);
							}
							registerValue(memOffset) = encodeRegisterValue(upper, lower);
							// can't think of a case where we should
							// restore the instruction pointer and then
							// immediate advance so just don't do it
							advanceIp = memOffset != ArchitectureConstants::InstructionPointer;
						}
					} else {
						throw iris::Problem("Illegal memory operation type!");
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
							// push the instruction pointer plus one onto the
							// stack
							pushDword((getInstructionPointer() + 1) & bitmask24);
						}
						getInstructionPointer() = bitmask24 & ((getConditionRegister() != 0) ? registerValue(current.getBranchIfOnTrue()) : registerValue(current.getBranchIfOnFalse()));
#ifdef DEBUG
						std::cout << "if: jumping to " << std::hex << getInstructionPointer() << std::endl;
#endif
					} else if (decodedFlags::isCall) {
						// call instruction
						advanceIp = false;
						// determine next
						pushDword((getInstructionPointer() + decodedFlags::isImmediate ? 2 : 1) & bitmask24);
						auto address = 0u;
						if (decodedFlags::isImmediate) {
							// make a 24 bit number
							auto upper16 = static_cast<RegisterValue>(tryReadNext<decodedFlags::isImmediate>()) << 8;
							auto lower8 = static_cast<RegisterValue>(current.getUpper());
							address = upper16 | lower8;
						} else {
							address = registerValue(current.getBranchIndirectDestination());
						}
						getInstructionPointer() = bitmask24 & address;
#ifdef DEBUG
						std::cout << "call: Jumping to " << std::hex << getInstructionPointer() << std::endl;
#endif
					} else {
						// jump instruction
						if (decodedFlags::isImmediate) {
							incrementInstructionPointer();
							if ((decodedFlags::isConditional && getConditionRegister() != 0) || !decodedFlags::isConditional) {
								advanceIp = false;
								getInstructionPointer() = bitmask24 & (current.getUpper() | static_cast<RegisterValue>(getCurrentCodeWord()) << 8);
							}
						} else {
							if ((decodedFlags::isConditional && getConditionRegister() != 0) || !decodedFlags::isConditional) {
								advanceIp = false;
								getInstructionPointer() = bitmask24 & registerValue(current.getBranchIndirectDestination());
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
			inline RegisterValue& getConditionRegister() noexcept      { return registerValue<ArchitectureConstants::ConditionRegister>(); }
			inline RegisterValue& getAddressRegister() noexcept        { return registerValue<ArchitectureConstants::AddressRegister>(); }
			inline RegisterValue& getValueRegister() noexcept          { return registerValue<ArchitectureConstants::ValueRegister>(); }
			inline RegisterValue& getMaskRegister() noexcept           { return registerValue<ArchitectureConstants::MaskRegister>(); }

			inline RegisterValue getShiftRegister() noexcept           { return 0b11111 & registerValue<ArchitectureConstants::ShiftRegister>(); }
			inline RegisterValue getFieldRegister() noexcept           { return 0b11111 & registerValue<ArchitectureConstants::FieldRegister>(); }

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
			void complexOperation(DecodedInstruction&& inst);
			void encodingOperation(DecodedInstruction&& inst);
			void memoryManipulationOperation(DecodedInstruction&& inst);
		private:
			bool execute = true,
				 advanceIp = true;
			RegisterValue gpr[ArchitectureConstants::RegisterCount] = { 0 };
			std::shared_ptr<Word> memory;
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
#include "def/iris18/ops.def"
#undef DefEnum
#undef EndDefEnum
#undef EnumEntry
	};
	Core* newCore() noexcept;
	void assemble(FILE* input, std::ostream* output);
} // end namespace iris18

#endif // end _TARGET_IRIS18_IRIS_H
