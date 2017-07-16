/*
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

namespace cisc0 {

    void illegalInstruction(const DecodedInstruction& current, RegisterValue ip);
	constexpr Word lowerMask(byte bitmask) noexcept {
		return syn::encodeUint16LE(syn::expandBit(syn::getBit<byte, 0>(bitmask)),
									syn::expandBit(syn::getBit<byte, 1>(bitmask)));
	}
	constexpr Word upperMask(byte bitmask) noexcept {
		return syn::encodeUint16LE(syn::expandBit(syn::getBit<byte, 2>(bitmask)),
									syn::expandBit(syn::getBit<byte, 3>(bitmask)));
	}

	constexpr RegisterValue mask(byte bitmask) noexcept {
		return syn::encodeUint32LE(lowerMask(bitmask), upperMask(bitmask));
	}

	constexpr bool readLower(byte bitmask) noexcept {
		return lowerMask(bitmask) != 0;
	}

	constexpr bool readUpper(byte bitmask) noexcept {
		return upperMask(bitmask) != 0;
	}
    constexpr RegisterValue encodeRegisterValue(byte a, byte b, byte c, byte d) noexcept {
        return syn::encodeUint32LE(a, b, c, d);
    }
    constexpr Word encodeWord(byte a, byte b) noexcept {
        return syn::encodeUint16LE(a, b);
    }

    constexpr Word decodeUpperHalf(RegisterValue value) noexcept {
        return syn::decodeBits<RegisterValue, Word, mask(0b1100), 16>(value);
    }
    constexpr Word decodeLowerHalf(RegisterValue value) noexcept {
        return syn::decodeBits<RegisterValue, Word, mask(0b0011), 0>(value);
    }

    constexpr RegisterValue encodeUpperHalf(RegisterValue value, Word upperHalf) noexcept {
        return syn::encodeBits<RegisterValue, Word, mask(0b1100), 16>(value, upperHalf);
    }
    constexpr RegisterValue encodeLowerHalf(RegisterValue value, Word lowerHalf) noexcept {
        return syn::encodeBits<RegisterValue, Word, mask(0b0011), 0>(value, lowerHalf);
    }

    constexpr RegisterValue encodeRegisterValue(Word upper, Word lower) noexcept {
        if (upper == 0 && lower == 0) {
            return 0;
        }
        return encodeUpperHalf(encodeLowerHalf(0, lower), upper);
    }
    constexpr RegisterValue normalizeCondition(RegisterValue input) noexcept {
        return input != 0 ? 0xFFFFFFFF : 0x00000000;
    }

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
	template<byte bankCount, typename R>
	class BankedCore : public syn::ClipsCore {
		static_assert(bankCount <= ArchitectureConstants::MaxRegisterBanks, "Too many register banks specified!");
		public:
			using RegisterType = R;
			using IOBus = syn::CLIPSIOController<Word, CLIPSInteger>;
			using RegisterFile = syn::FixedSizeLoadStoreUnit<RegisterType, byte, ArchitectureConstants::RegistersPerBank * bankCount>;
			static constexpr auto targetBankCount = bankCount;
		public:
			BankedCore(const std::string& busMicrocodePath, RegisterType ioStart, RegisterType ioEnd) noexcept : _bus(ioStart, ioEnd, busMicrocodePath) { }
			virtual ~BankedCore() noexcept { }
            virtual void initialize() override {
				_bus.initialize();
			}
            virtual void shutdown() override {
				_bus.shutdown();
			}
		protected:
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
            virtual void storeWord(RegisterType address, Word value) {
				if (isTerminateAddress(address)) {
					execute = false;
					advanceIp = false;
				} else {
					_bus.write(address, value);
				}
			}
			virtual Word loadWord(RegisterType address) {
				if (isTerminateAddress(address)) {
					return 0;
				} else {
					return _bus.read(address);
				}
			}
            inline Word loadWord(RegisterType address, byte offset) {
				return loadWord(address + offset);
			}
            inline void storeWord(RegisterType address, byte offset, Word value) {
				storeWord(address + offset, value);
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
			IOBus _bus;
	};
	class ConditionRegisterImplementation {
		public:
			virtual bool& getConditionRegister() noexcept { return _conditionRegister; }
		protected:
			bool _conditionRegister = true;
	};
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
