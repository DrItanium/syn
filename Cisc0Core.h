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
	static constexpr Word extractHexAndConvertToText(RegisterValue value) noexcept {
		return hexToText(syn::decodeBits<RegisterValue, byte, mask, shift>(value));
	}
	class Core : public syn::ClipsCore {
        public:
			using IOBus = syn::CLIPSIOController<Word, CLIPSInteger>;
            using RegisterFile = syn::FixedSizeLoadStoreUnit<RegisterValue, byte, ArchitectureConstants::RegisterCount>;
            static constexpr RegisterValue busStart = 0x00000000;
            static constexpr RegisterValue busEnd = 0xFFFFFFFF;
		public:
			Core(const std::string& busMicrocodePath, RegisterValue ioStart = busStart, RegisterValue ioEnd = busEnd) noexcept;
			virtual ~Core() noexcept { }
			virtual bool handleOperation(void* env, CLIPSValue* ret) override;
            virtual void initialize() override;
            virtual void shutdown() override;
        protected:
            void incrementAddress(RegisterValue& ptr) noexcept;
            void decrementAddress(RegisterValue& ptr) noexcept;
            void incrementInstructionPointer() noexcept;
            virtual RegisterValue& getInstructionPointer() noexcept;
            virtual RegisterValue& getStackPointer() noexcept;
            virtual RegisterValue& getCallStackPointer() noexcept;
            virtual RegisterValue& getAddressRegister() noexcept;
            virtual RegisterValue& getValueRegister() noexcept;
            virtual RegisterValue& getMaskRegister() noexcept;
            virtual RegisterValue  getShiftRegister() noexcept;
            virtual RegisterValue  getFieldRegister() noexcept;
            virtual bool& getConditionRegister() noexcept = 0;
            void pushWord(Word value);
            void pushWord(Word value, RegisterValue& sp);
            void pushRegisterValue(RegisterValue value);
            void pushRegisterValue(RegisterValue value, RegisterValue& sp);
            Word popWord();
            Word popWord(RegisterValue& sp);
            RegisterValue popRegisterValue(RegisterValue& sp);
            RegisterValue popRegisterValue();

            virtual RegisterValue& registerValue(byte index) = 0;
            virtual void storeWord(RegisterValue address, Word value) = 0;
            virtual Word loadWord(RegisterValue address) = 0;

		protected:
			bool advanceIp = true;
            IOBus _bus;
	};


} // end namespace cisc0

#endif // end _TARGET_CISC0_IRIS_H
