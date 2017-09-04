/**
 * @file
 * The base of the cisc0 architecture and implementation.
 * The cisc0 architecture is a 32-bit variable length instruction cisc
 * architecture. Registers are 32-bits wide and memory words are 16-bits wide.
 * This makes loading data much easier.
 * @copyright
 * syn
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#ifndef _TARGET_CISC0_IRIS_H
#define _TARGET_CISC0_IRIS_H
#include <sstream>
#include <memory>
#include <vector>
#include <tuple>

#include "Problem.h"
#include "Base.h"
#include "ClipsCore.h"
#include "ExecutionUnits.h"
#include "IODevice.h"
#include "IOController.h"
#include "Cisc0CoreConstants.h"
#include "Cisc0CoreDecodedInstruction.h"


/**
 * A CISC style instruction set architecture with odd edge cases and a limited
 * register set
 */
namespace cisc0 {

    /**
     * Prints out a message stating that the given instruction is illegal and
     * the address it was at.
     * @param current the illegal instruction
     * @param ip the address it was found at!
     */
    void illegalInstruction(const DecodedInstruction& current, RegisterValue ip);

    /**
     * Given a four bit bitmask, expand the lower two bits into field mask
     * format; so 0m0110 will return 0xFF00.
     * @param bitmask the bitmask to extract and expand the lower two bits from
     * @return the lower two bits of the bitmask expanded into a Word.
     */
	constexpr Word lowerMask(byte bitmask) noexcept {
		return syn::encodeUint16LE(syn::expandBit(syn::getBit<byte, 0>(bitmask)),
									syn::expandBit(syn::getBit<byte, 1>(bitmask)));
	}

    /**
     * Given a four bit bitmask, expand the upper two bits into field mask
     * format so 0m0101 will return 0x00FF.
     * @param bitmask the bitmask to extract and expand the upper two bits from
     * @return the upper two bits of the bitmask expanded into a Word.
     */
	constexpr Word upperMask(byte bitmask) noexcept {
		return syn::encodeUint16LE(syn::expandBit(syn::getBit<byte, 2>(bitmask)),
									syn::expandBit(syn::getBit<byte, 3>(bitmask)));
	}

    /**
     * Construct a register wide number from the given bitmask encoding; so
     * 0m1010 will be expanded to 0xFF00FF00.
     * @param bitmask the four bit bitmask to expand to register width
     * @return the four bits that make up the mask expanded into a register
     * wide value.
     */
	constexpr RegisterValue mask(byte bitmask) noexcept {
		return syn::encodeUint32LE(lowerMask(bitmask), upperMask(bitmask));
	}

    /**
     * Should we read the lower word based off of the supplied bitmask's lower
     * two bits; so 0m1101 would return true, where 0m1100 would return false.
     * @param bitmask the mask to parse and check
     * @return a bool value signifying that the lower word should be read
     */
	constexpr bool readLower(byte bitmask) noexcept {
		return lowerMask(bitmask) != 0;
	}

    /**
     * Should we read the upper word based off of the supplied bitmask's lower
     * two bits; so 0m1101 would return true, where 0m0011 would return false.
     * @param bitmask the mask to parse and check
     * @return a bool value signifying that the upper word should be read
     */
	constexpr bool readUpper(byte bitmask) noexcept {
		return upperMask(bitmask) != 0;
	}
    /**
     * Given four bytes, encode them into a register wide value
     * @param a the byte that makes up bits 0-7
     * @param b the byte that makes up bits 8-15
     * @param c the byte that makes up bits 16-23
     * @param d the byte that makes up bits 24-31
     * @return a register wide encoded value from the given bytes
     */
    constexpr RegisterValue encodeRegisterValue(byte a, byte b, byte c, byte d) noexcept {
        return syn::encodeUint32LE(a, b, c, d);
    }

    /**
     * Given two bytes, encode them into a word wide value
     * @param a the lower half of the word
     * @param b the upper half of the word
     * @return the word encoded from the supplied two halves
     */
    constexpr Word encodeWord(byte a, byte b) noexcept {
        return syn::encodeUint16LE(a, b);
    }

    /**
     * Extract the upper half of the register value.
     * @param value the register value to extract the upper half from
     * @return the word that makes up the upper half of the register value
     */
    constexpr Word decodeUpperHalf(RegisterValue value) noexcept {
        return syn::decodeBits<RegisterValue, Word, mask(0b1100), 16>(value);
    }

    /**
     * Extract the lower half of the register value.
     * @param value the register value to extract the lower half from
     * @return the word that makes up the lower half of the register value
     */
    constexpr Word decodeLowerHalf(RegisterValue value) noexcept {
        return syn::decodeBits<RegisterValue, Word, mask(0b0011), 0>(value);
    }

    /**
     * Puts the given word value into the upper half of a register
     * @param value the register to encode the upper half into
     * @param upperHalf the value to be encoded into the register
     * @return the newly encoded register value
     */
    constexpr RegisterValue encodeUpperHalf(RegisterValue value, Word upperHalf) noexcept {
        return syn::encodeBits<RegisterValue, Word, mask(0b1100), 16>(value, upperHalf);
    }
    /**
     * Puts the given word value into the lower half of a register
     * @param value the register to encode the lower half into
     * @param lowerHalf the value to be encoded into the register
     * @return the newly encoded register value
     */
    constexpr RegisterValue encodeLowerHalf(RegisterValue value, Word lowerHalf) noexcept {
        return syn::encodeBits<RegisterValue, Word, mask(0b0011), 0>(value, lowerHalf);
    }

    constexpr RegisterValue encodeRegisterValue(Word upper, Word lower) noexcept {
        if (upper == 0 && lower == 0) {
            return 0;
        }
        return encodeUpperHalf(encodeLowerHalf(0, lower), upper);
    }

    /**
     * Given a register value, convert it to 0xFFFFFFFF if it is not zero,
     * otherwise return zero; this is used when parsing the result of the
     * condition register (which is a full 32-bits wide).
     * Thus someone can easily stash more data inside the condition register if
     * necessary.
     * @param input the register value to normalize to 0xFFFFFFFF or 0
     * @return If the input is not zero then 0xFFFFFFFF otherwise zero
     */
    constexpr RegisterValue normalizeCondition(RegisterValue input) noexcept {
        return input != 0 ? 0xFFFFFFFF : 0x00000000;
    }

    /**
     * Parse the given input word as ascii value and returns the numeric value
     * it is meant to represent; so 'f' -> 0xF.
     * @param input the word to parse as a character and convert to its numeric
     * concept
     * @return the numeric concept associated with the given symbol, if not one
     * of the legal ascii chars, then zero is returned.
     */
	constexpr byte convertTextToHex(Word input) noexcept {
		switch(input) {
			case 'f':
			case 'F':
				return 0xF;
			case 'e':
			case 'E':
				return 0xE;
			case 'd':
			case 'D':
				return 0xD;
			case 'c':
			case 'C':
				return 0xC;
			case 'b':
			case 'B':
				return 0xB;
			case 'a':
			case 'A':
				return 0xA;
			case '9': return 9;
			case '8': return 8;
			case '7': return 7;
			case '6': return 6;
			case '5': return 5;
			case '4': return 4;
			case '3': return 3;
			case '2': return 2;
			case '1': return 1;
			case '0':
			default:
				return 0x0;
		}
	}

    /**
     * Converts the given input byte to its corresponding hex character in
     * ASCII, if the input is not 0-15 then ascii zero is returned.
     * @param input the byte value to translate to the ascii hex value, so 0d1 will return '1'.
     * @return the ascii value of the provided input as a Word
     */
	constexpr Word hexToText(byte input) noexcept {
		switch(syn::decodeBits<byte, byte, 0x0F, 0>(input)) {
			case 0x1: return static_cast<Word>('1');
			case 0x2: return static_cast<Word>('2');
			case 0x3: return static_cast<Word>('3');
			case 0x4: return static_cast<Word>('4');
			case 0x5: return static_cast<Word>('5');
			case 0x6: return static_cast<Word>('6');
			case 0x7: return static_cast<Word>('7');
			case 0x8: return static_cast<Word>('8');
			case 0x9: return static_cast<Word>('9');
			case 0xA: return static_cast<Word>('A');
			case 0xB: return static_cast<Word>('B');
			case 0xC: return static_cast<Word>('C');
			case 0xD: return static_cast<Word>('D');
			case 0xE: return static_cast<Word>('E');
			case 0xF: return static_cast<Word>('F');
			case 0x0:
			default:
				return static_cast<Word>('0');
		}
	}

	template<RegisterValue mask, RegisterValue shift>
	constexpr Word extractHexAndConvertToText(RegisterValue value) noexcept {
		return hexToText(syn::decodeBits<RegisterValue, byte, mask, shift>(value));
	}

    template<syn::Comparator::StandardOperations op>
    constexpr RegisterValue sliceBitAndCheck(RegisterValue a, RegisterValue b) noexcept {
        using T = RegisterValue;
        return syn::Comparator::performOperation<op, T>(
                syn::ALU::performOperation<translate(LogicalOps::And), T>(
                    syn::ALU::performOperation<ALUOperation::ShiftRight, T>(
                        a,
                        b), 0x1), 1);
    }

    /**
     * A cisc0 core which describes registers in terms of banks; each bank
     * contains 8 registers; there can be between 0 and 32 banks; model 0
     * conceptually has two banks; this class allows register banks to be
     * described in a concise and consistent manner across different cisc0
     * implementations.
     * @tparam bankCount the number of register banks
     * @tparam R the type of the register
     */
	template<byte bankCount, typename R>
	class BankedCore : public syn::ClipsCore<Word, R> {
		static_assert(bankCount <= ArchitectureConstants::MaxRegisterBanks, "Too many register banks specified!");
		public:
			using RegisterType = R;
			using RegisterFile = syn::FixedSizeLoadStoreUnit<RegisterType, byte, ArchitectureConstants::RegistersPerBank * bankCount>;
			using Parent = syn::ClipsCore<Word, RegisterType>; 
			static constexpr auto targetBankCount = bankCount;
		public:
			BankedCore(syn::CLIPSIOController& bus) noexcept : Parent(bus) { }
			virtual ~BankedCore() noexcept { }
		protected:
            virtual void storeWord(RegisterType address, Word value) {
				if (isTerminateAddress(address)) {
					Parent::execute = false;
					advanceIp = false;
				} else {
					Parent::writeToBus(address, value);
				}
			}
			virtual Word loadWord(RegisterType address) {
				if (isTerminateAddress(address)) {
					return 0;
				} else {
					return Parent::readFromBus(address);
				}
			}
            /**
             * Given a bank index and offset, retrieve a register value
             * reference.
             * @param bank the bank id of the given register
             * @param offset the index of the register within the given bank
             * @return a Register reference to the described register value if
             * it is there
             * @throw syn::Problem register in the given bank and offset does
             * not exist or can't be accessed.
             */
			virtual RegisterType& registerValue(byte bank, byte offset) = 0;
            virtual void incrementAddress(RegisterValue& ptr) noexcept = 0;
            virtual void decrementAddress(RegisterValue& ptr) noexcept = 0;
            virtual void incrementInstructionPointer() noexcept {
				incrementAddress(getInstructionPointer());
			}
            virtual RegisterType& getInstructionPointer() noexcept = 0;
            virtual RegisterType& getStackPointer() noexcept = 0;
            virtual RegisterType& getCallStackPointer() noexcept = 0;
            virtual RegisterType& getAddressRegister() noexcept = 0;
            virtual RegisterType& getValueRegister() noexcept = 0;
            virtual RegisterType& getMaskRegister() noexcept = 0;
            virtual RegisterType  getShiftRegister() noexcept = 0;
            virtual RegisterType  getFieldRegister() noexcept = 0;
            virtual bool& getConditionRegister() noexcept = 0;
            virtual void pushWord(Word value) {
				pushWord(value, getStackPointer());
			}
            virtual void pushWord(Word value, RegisterType& sp) {
				decrementAddress(sp);
				storeWord(sp, value);
			}
            virtual void pushRegisterValue(RegisterType value) {
				pushRegisterValue(value, getStackPointer());
			}
            virtual void pushRegisterValue(RegisterType value, RegisterType& sp) {
				pushWord(decodeUpperHalf(value), sp);
				pushWord(decodeLowerHalf(value), sp);
			}
            virtual Word popWord() {
				return popWord(getStackPointer());
			}
            virtual Word popWord(RegisterType& sp) {
				auto result = loadWord(sp);
				incrementAddress(sp);
				return result;
			}
            virtual RegisterType popRegisterValue(RegisterType& sp) {
				auto lower = popWord(sp);
				auto upper = popWord(sp);
				return encodeRegisterValue(upper, lower);
			}
            virtual RegisterType popRegisterValue() {
				return popRegisterValue(getStackPointer());
			}
            inline Word loadWord(RegisterType address, byte offset) {
				return loadWord(address + offset);
			}
            inline void storeWord(RegisterType address, byte offset, Word value) {
				storeWord(address + offset, value);
			}
            inline RegisterType loadRegisterValue(RegisterType address) {
                // just load both halves
                auto lower = RegisterType(loadWord(address));
                auto upper = RegisterType(loadWord(address + 1)) << 16;
                return upper | lower;
            }
            inline void storeRegisterValue(RegisterType address, RegisterType value) {
                // carve the pieces up and store the lower then upper
                storeWord(address, decodeLowerHalf(value));
                storeWord(address + 1, decodeUpperHalf(value));
            }
		protected:
			virtual void returnOperation() noexcept {
				// pop the top address off of the call stack and place it in the
				// instruction pointer!
				getInstructionPointer() = popRegisterValue(getCallStackPointer());
				advanceIp = false;
			}
            virtual bool isTerminateAddress(RegisterValue address) const noexcept {
				return address == ArchitectureConstants::TerminateAddress;
			}
            virtual void hex8ToRegister() {
				// 1) use the address contained in address to read the next 8 words
				// 2) Parse each word as an ascii character and convert it into a 4 bit quantity
				// 3) Place that 4bit quantity into the appropriate position in value
				auto addr = getAddressRegister();
				auto value = syn::encodeBits<RegisterValue, byte, 0x0000000F, 0>(0, convertTextToHex(loadWord(addr)));
				value = syn::encodeBits<RegisterValue, byte, 0x000000F0, 4>(value, convertTextToHex(loadWord(addr, 1)));
				value = syn::encodeBits<RegisterValue, byte, 0x00000F00, 8>(value, convertTextToHex(loadWord(addr, 2)));
				value = syn::encodeBits<RegisterValue, byte, 0x0000F000, 12>(value, convertTextToHex(loadWord(addr, 3)));
				value = syn::encodeBits<RegisterValue, byte, 0x000F0000, 16>(value, convertTextToHex(loadWord(addr, 4)));
				value = syn::encodeBits<RegisterValue, byte, 0x00F00000, 20>(value, convertTextToHex(loadWord(addr, 5)));
				value = syn::encodeBits<RegisterValue, byte, 0x0F000000, 24>(value, convertTextToHex(loadWord(addr, 6)));
				getValueRegister() = syn::encodeBits<RegisterValue, byte, 0xF0000000, 28>(value, convertTextToHex(loadWord(addr, 7)));
			}
            virtual void registerToHex8() {
				auto addr = getAddressRegister();
				auto value = getValueRegister();
				storeWord(addr, extractHexAndConvertToText<0x0000000F, 0>(value));
				storeWord(addr, 1, extractHexAndConvertToText<0x000000F0, 4>(value));
				storeWord(addr, 2, extractHexAndConvertToText<0x00000F00, 8>(value));
				storeWord(addr, 3, extractHexAndConvertToText<0x0000F000, 12>(value));
				storeWord(addr, 4, extractHexAndConvertToText<0x000F0000, 16>(value));
				storeWord(addr, 5, extractHexAndConvertToText<0x00F00000, 20>(value));
				storeWord(addr, 6, extractHexAndConvertToText<0x0F000000, 24>(value));
				storeWord(addr, 7, extractHexAndConvertToText<0xF0000000, 28>(value));
			}
            virtual void defaultEncodingOperation(EncodingOperation op) {
				switch(op) {
					case EncodingOperation::Decode:
						decodeBits();
						break;
					case EncodingOperation::Encode:
						encodeBits();
						break;
					case EncodingOperation::BitSet:
						setBit();
						break;
					case EncodingOperation::BitUnset:
						unsetBit();
						break;
					default:
						throw syn::Problem("Illegal complex encoding operation defined!");
				}
			}
            virtual void setBit() = 0;
            virtual void unsetBit() = 0;
            virtual void decodeBits() = 0;
            virtual void encodeBits() = 0;
		protected:
			bool advanceIp = true;
	};
    /**
     * A generic implementation of a condition register. It is built assuming
     * that the condition register is not part of the GPR set.
     */
	class ConditionRegisterImplementation {
		public:
            /**
             * retrieve the condition register value as a reference
             * @return a bool reference of the backing condition register
             */
			virtual bool& getConditionRegister() noexcept { return _conditionRegister; }
		protected:
			bool _conditionRegister = true;
	};

    /**
     * Standard two bank core implementation, all standard cisc0 configuration
     * uses this design.
     */
	class Core : public BankedCore<2, RegisterValue>, public ConditionRegisterImplementation {
        public:
            using RegisterFile = syn::FixedSizeLoadStoreUnit<RegisterValue, byte, ArchitectureConstants::RegisterCount>;
			using Parent = BankedCore<2, RegisterValue>;
            static constexpr RegisterValue busStart = 0x00000000;
            static constexpr RegisterValue busEnd = 0xFFFFFFFF;
		public:
			Core(const std::string& busMicrocodePath, RegisterValue ioStart = busStart, RegisterValue ioEnd = busEnd) noexcept;
			virtual ~Core() noexcept { }
			virtual bool handleOperation(void* env, CLIPSValue* ret) override;
            virtual void initialize() override;
			virtual void shutdown() override;
		protected:
			virtual RegisterValue& registerValue(byte bank, byte offset) override;
			virtual RegisterValue& registerValue(byte index);
			virtual bool& getConditionRegister() noexcept override;
            virtual void incrementAddress(RegisterValue& ptr) noexcept override;
            virtual void decrementAddress(RegisterValue& ptr) noexcept override;
            virtual RegisterValue& getInstructionPointer() noexcept override;
            virtual RegisterValue& getStackPointer() noexcept override;
            virtual RegisterValue& getCallStackPointer() noexcept override;
            virtual RegisterValue& getAddressRegister() noexcept override;
            virtual RegisterValue& getValueRegister() noexcept override;
            virtual RegisterValue& getMaskRegister() noexcept override;
            virtual RegisterValue  getShiftRegister() noexcept override;
            virtual RegisterValue  getFieldRegister() noexcept override;
        protected:
            template<syn::Comparator::StandardOperations op>
            inline void defaultSliceBitAndCheck() {
                getConditionRegister() = sliceBitAndCheck<op>(getAddressRegister(), getFieldRegister());
            }
            virtual void setBit() override;
            virtual void unsetBit() override;
            virtual void decodeBits() override;
            virtual void encodeBits() override;
		protected:
			RegisterFile _gpr;
	};
} // end namespace cisc0

#endif // end _TARGET_CISC0_IRIS_H
