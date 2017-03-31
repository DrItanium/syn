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


#ifndef IRIS_CORE_ENCODING_OPERATIONS_H
#define IRIS_CORE_ENCODING_OPERATIONS_H
#include "Base.h"
#include "ExecutionUnits.h"
#include "Core.h"
#include "IOController.h"
#include <cstdint>
#include <memory>
#include "IrisCoreTypes.h"
#include "ClipsExtensions.h"
#include "iris_defines.h"

namespace iris {
    namespace InstructionDecoder {
        constexpr byte getDestinationIndex(raw_instruction value) noexcept { return decodeDestination(value); }
        constexpr byte getSource0Index(raw_instruction value) noexcept { return decodeSource0(value); }
        constexpr byte getSource1Index(raw_instruction value) noexcept { return decodeSource1(value); }
        constexpr byte getOperationByte(raw_instruction value) noexcept { return decodeOperation(value); }
        constexpr byte getGroupByte(raw_instruction value) noexcept { return decodeGroup(value); }
        constexpr InstructionGroup getGroup(raw_instruction value) noexcept { return static_cast<InstructionGroup>(getGroupByte(value)); }
        constexpr word getHalfImmediate(raw_instruction value) noexcept { return decodeHalfImmediate(value); }
        constexpr word getImmediate(raw_instruction value) noexcept { return decodeImmediate(value); }
        template<typename T>
        constexpr T getOperation(raw_instruction value) noexcept {
            return static_cast<T>(getOperationByte(value));
        }
        template<int index>
        constexpr byte getRegisterIndex(raw_instruction value) noexcept {
            static_assert(index >= 0 && index < 3, "Illegal register index!");
            if (index == 0) {
                return getDestinationIndex(value);
            } else if (index == 1) {
                return getSource0Index(value);
            } else {
                return getSource1Index(value);
            }
        }
        constexpr byte getPredicateResultIndex(raw_instruction value) noexcept { return decodePredicateResult(value); }
        constexpr byte getPredicateInverseResultIndex(raw_instruction value) noexcept { return decodePredicateInverseResult(value); }
        constexpr byte getPredicateSource0Index(raw_instruction value) noexcept { return decodePredicateSource0(value); }
        constexpr byte getPredicateSource1Index(raw_instruction value) noexcept { return decodePredicateSource1(value); }
        template<int index>
        constexpr byte getPredicateIndex(raw_instruction value) noexcept {
            static_assert(index >= 0 && index < 4, "Illegal predicate field index!");
            switch(index) {
                case 0: return getPredicateResultIndex(value);
                case 1: return getPredicateInverseResultIndex(value);
                case 2: return getPredicateSource0Index(value);
                case 3: return getPredicateSource1Index(value);
                default:
                    throw syn::Problem("Illegal index!!!!");
            }
        }
        constexpr bool samePredicateDestinations(raw_instruction value) noexcept {
            return getPredicateResultIndex(value) == getPredicateInverseResultIndex(value);
        }
        constexpr byte chooseRegister(raw_instruction value, bool cond) noexcept {
            return cond ? getSource0Index(value) : getSource1Index(value);
        }
    };
    constexpr raw_instruction encodeInstruction(byte group, byte operation, byte dest, byte src0, byte src1) noexcept {
        return encodeSource1( encodeSource0( encodeDestination(encodeOperation( encodeGroup(0, group), operation), dest), src0), src1);
    }
    constexpr raw_instruction encodeInstruction(byte group, byte operation, byte dest, word immediate) noexcept {
        return encodeInstruction(group, operation, dest, syn::getLowerHalf(immediate), syn::getUpperHalf(immediate));
    }
    template<bool upper>
    inline byte encode4Bits(byte dest, byte value) noexcept {
        if (upper) {
            return encodeUpper4Bits(dest, value);
        } else {
            return encodeLower4Bits(dest, value);
        }
    }

}
#endif
