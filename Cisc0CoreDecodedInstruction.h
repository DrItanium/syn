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


#ifndef _TARGET_CISC0_INSTRUCTION_DECODER_H
#define _TARGET_CISC0_INSTRUCTION_DECODER_H

#include <tuple>

#include "Cisc0CoreConstants.h"
#include "cisc0_defines.h"

namespace cisc0 {
	class DecodedInstruction {
		public:
            using BranchFlags = std::tuple<bool, bool, bool>;
        private:
            static constexpr bool hasBitmask(Operation op) noexcept {
                switch(op) {
                    case Operation::Set:
                    case Operation::Memory:
                    case Operation::Move:
                    case Operation::Logical:
                        return true;
                    default:
                        return false;
                }
            }

            static constexpr bool hasImmediateFlag(Operation op) noexcept {
                switch(op) {
                    case Operation::Shift:
                    case Operation::Logical:
                    case Operation::Branch:
                    case Operation::Compare:
                    case Operation::Arithmetic:
                        return true;
                    default:
                        return false;
                }
            }
            static constexpr bool hasImmediateValue(Operation op) noexcept {
                switch (op) {
                    case Operation::Shift:
                    case Operation::Arithmetic:
                        return true;
                    default:
                        return false;
                }
            }
            static constexpr bool hasSubtype(Operation op) noexcept {
                switch(op) {
                    case Operation::Compare:
                    case Operation::Memory:
                    case Operation::Arithmetic:
                    case Operation::Complex:
                    case Operation::Logical:
                        return true;
                    default:
                        return false;
                }
            }
        public:
			DecodedInstruction(RawInstruction input) noexcept : _rawValue(input) { }
			DecodedInstruction(const DecodedInstruction&) = delete;
            virtual ~DecodedInstruction() { }
			RawInstruction getRawValue() const noexcept { return _rawValue; }
            inline byte getUpper() const noexcept { return decodeUpper(_rawValue); }
            inline Operation getControl() const noexcept { return decodeControl(_rawValue); }
            inline byte getSetDestination() const noexcept { return decodeSetDestination(_rawValue); }
            inline byte getMemoryRegister() const noexcept { return decodeMemoryRegister(_rawValue); }
            inline byte getBranchIndirectDestination() const noexcept { return decodeBranchIndirectDestination(_rawValue); }
            inline bool shouldShiftLeft() const noexcept { return decodeShiftFlagLeft(_rawValue); }
            inline bool isIndirectOperation() const noexcept { return decodeMemoryFlagIndirect(_rawValue); }
            inline byte getMemoryOffset() const noexcept { return decodeMemoryOffset(_rawValue); }
            BranchFlags getOtherBranchFlags() const noexcept;
            inline EncodingOperation getEncodingOperation() const noexcept { return decodeComplexClassEncoding_Type(_rawValue); }
            inline ExtendedOperation getExtendedOperation() const noexcept { return decodeComplexClassExtended_Type(_rawValue); }

            template<int index>
            inline byte getShiftRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal shift register index!");
                if (index == 0) {
                    return decodeShiftRegister0(_rawValue);
                } else {
                    return decodeShiftRegister1(_rawValue);
                }
            }
            template<int index>
            inline byte getMoveRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal move register index!");
                if (index == 0) {
                    return decodeMoveRegister0(_rawValue);
                } else {
                    return decodeMoveRegister1(_rawValue);
                }
            }
            template<int index>
            inline byte getSwapRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal swap register index!");
                if (index == 0) {
                    return decodeSwapDestination(_rawValue);
                } else {
                    return decodeSwapSource(_rawValue);
                }
            }
            template<int index>
            inline byte getCompareRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal compare register index!");
                if (index == 0) {
                    return decodeCompareRegister0(_rawValue);
                } else {
                    return decodeCompareRegister1(_rawValue);
                }
            }
            template<int index>
            inline byte getArithmeticRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal arithmetic register index!");
                if (index == 0) {
                    return decodeArithmeticDestination(_rawValue);
                } else {
                    return decodeArithmeticSource(_rawValue);
                }
            }

            template<int index>
            inline byte getComplexExtendedArg() const noexcept {
                static_assert(index >= 0 && index < 1, "Illegal complex extended arg index!");
                return decodeComplexClassExtended_Arg0(_rawValue);
            }
            template<Operation op>
            inline byte getBitmask() const noexcept {
                static_assert(hasBitmask(op), "provided operation does not use a bitmask!");
                switch(op) {
                    case Operation::Set:
                        return decodeSetBitmask(_rawValue);
                    case Operation::Memory:
                        return decodeMemoryFlagBitmask(_rawValue);
                    case Operation::Move:
                        return decodeMoveBitmask(_rawValue);
                    case Operation::Logical:
                        return decodeLogicalFlagImmediateMask(_rawValue);
                    default:
                        return 0;
                }
            }
            template<Operation op>
            inline bool getImmediateFlag() const noexcept {
                static_assert(hasImmediateFlag(op), "provided operation does not have an immediate flag!");
                switch(op) {
                    case Operation::Shift:
                        return decodeShiftFlagImmediate(_rawValue);
                    case Operation::Arithmetic:
                        return decodeArithmeticFlagImmediate(_rawValue);
                    case Operation::Logical:
                        return decodeLogicalFlagImmediate(_rawValue);
                    case Operation::Branch:
                        return decodeBranchFlagIsImmediate(_rawValue);
                    case Operation::Compare:
                        return decodeCompareImmediateFlag(_rawValue);
                    default:
                        return false;
                }
            }
            template<Operation op>
            inline byte getImmediate() const noexcept {
                static_assert(hasImmediateValue(op), "provided operation cannot contain an immediate value!");
                switch(op) {
                    case Operation::Shift:
                        return decodeShiftImmediate(_rawValue);
                    case Operation::Arithmetic:
                        return decodeArithmeticImmediate(_rawValue);
                    default:
                        return 0;
                }
            }
            template<int index>
            inline byte getLogicalRegister() const noexcept {
                static_assert(index >= 0 && index < 2, "Illegal logical register index!");
                if (index == 0) {
                    if (getImmediateFlag<Operation::Logical>()) {
                        return decodeLogicalImmediateDestination(_rawValue);
                    } else {
                        return decodeLogicalRegister0(_rawValue);
                    }
                } else {
                    if (getImmediateFlag<Operation::Logical>()) {
                        throw syn::Problem("There is no second register argument for an immediate logical operation!");
                    } else {
                        return decodeLogicalRegister1(_rawValue);
                    }
                }
            }
            template<bool path>
            inline byte getBranchIfPathRegister() const noexcept {
                if (path) {
                    return decodeBranchIfOnTrue(_rawValue);
                } else {
                    return decodeBranchIfOnFalse(_rawValue);
                }
            }
            template<Operation op>
			inline typename DecodeSubType<op>::ReturnType getSubtype() const noexcept {
				return cisc0::decodeSubType<op>(_rawValue);
			}
		private:
			RawInstruction _rawValue;
	};
}

#endif // end _TARGET_CISC0_INSTRUCTION_DECODER_H
