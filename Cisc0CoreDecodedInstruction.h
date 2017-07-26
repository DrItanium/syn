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
            using BranchFlags = std::tuple<bool, bool>;
        private:

            static constexpr bool hasImmediateValue(Operation op) noexcept {
                switch (op) {
                    case Operation::Shift:
                    case Operation::Arithmetic:
                        return true;
                    default:
                        return false;
                }
            }
        public:
			DecodedInstruction(RawInstruction input = 0) noexcept : _rawValue(input) { }
			DecodedInstruction(const DecodedInstruction&) = delete;
            virtual ~DecodedInstruction() { }
			RawInstruction getRawValue() const noexcept { return _rawValue; }
            inline byte getUpper() const noexcept { return decodeUpper(_rawValue); }
            inline Operation getControl() const noexcept { return decodeControl(_rawValue); }
            inline byte getDestination() const noexcept { return decodeGenericDestination(_rawValue); }
            inline byte getMemoryOffset() const noexcept { return getDestination(); }
            inline byte getSubtypeControlBits() const noexcept { return decodeGenericCommonSubTypeField(_rawValue); }
            inline bool shouldShiftLeft() const noexcept { return decodeShiftFlagLeft(_rawValue); }
            inline bool isIndirectOperation() const noexcept { return decodeMemoryFlagIndirect(_rawValue); }
            BranchFlags getOtherBranchFlags() const noexcept;
			template<ComplexSubTypes op>
			inline typename DecodeComplexSubType<op>::ReturnType getComplexSubType() const noexcept {
				return decodeComplexSubType<op>(_rawValue);
			}
            inline EncodingOperation getEncodingOperation() const noexcept { return getComplexSubType<ComplexSubTypes::Encoding>(); }
            inline ExtendedOperation getExtendedOperation() const noexcept { return getComplexSubType<ComplexSubTypes::Extended>(); }
			inline ParsingOperation getParsingOperation() const noexcept { return getComplexSubType<ComplexSubTypes::Parsing>(); }
			inline FeatureCheckOperation getFeatureCheckOperation() const noexcept { return getComplexSubType<ComplexSubTypes::FeatureCheck>(); }

			template<Operation op>
			inline byte getSourceRegister() const noexcept {
                static_assert(usesSource<op>(), "Provided type does not use the source field!");
                return cisc0::decodeGenericSource(_rawValue);
			}

			template<Operation op>
			inline byte getDestinationRegister() const noexcept {
                static_assert(usesDestination<op>(), "Provided operation does not use the destination type!");
                return getDestination();
			}

            template<ComplexSubTypes op>
            inline byte getDestinationRegister() const noexcept {
				static_assert(op == cisc0::ComplexSubTypes::Extended, "Only extended instructions have arguments!");
                return getDestination();
            }
            template<Operation op>
            inline byte getBitmask() const noexcept {
				return cisc0::decodeBitmask<op>(_rawValue);
            }
            template<Operation op>
            inline bool getImmediateFlag() const noexcept {
                static_assert(hasImmediateFlag<op>(), "Provided operation does not have an immediate flag!");
                return cisc0::decodeGenericImmediateFlag(_rawValue);
            }
            template<Operation op>
            inline byte getImmediate() const noexcept {
                static_assert(hasImmediateValue(op), "provided operation cannot contain an immediate value!");
                switch(op) {
                    case Operation::Shift:
                        return decodeGenericImmediate5(_rawValue);
                    case Operation::Arithmetic:
                        return decodeGenericImmediate4(_rawValue);
                    default:
                        return 0;
                }
            }
            template<Operation op>
			inline typename cisc0::DecodeType<op>::ReturnType getSubtype() const noexcept {
				return cisc0::decodeType<op>(_rawValue);
			}
		private:
			RawInstruction _rawValue;
	};
}

#endif // end _TARGET_CISC0_INSTRUCTION_DECODER_H
