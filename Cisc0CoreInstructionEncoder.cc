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


#include <functional>
#include <sstream>
#include <utility>
#include <map>

#include "Problem.h"
#include "Cisc0CoreInstructionEncoder.h"

namespace cisc0 {
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

    constexpr int instructionSizeFromImmediateMask(byte bitmask) noexcept {
        return 1 + (readLower(bitmask) ? 1 : 0) + (readUpper(bitmask) ? 1 : 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeArithmetic() const {
        auto first = encodeControl(0, type);
        first = encodeArithmeticFlagImmediate(first, immediate);
        first = encodeArithmeticFlagType(first, static_cast<ArithmeticOps>(subType));
        first = encodeArithmeticDestination(first, arg0);
        first = immediate ? encodeArithmeticImmediate(first, arg1) : encodeArithmeticSource(first, arg1);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeMove() const {
        auto first = encodeControl(0, type);
        first = encodeMoveBitmask(first, bitmask);
        first = encodeMoveRegister0(first, arg0);
        first = encodeMoveRegister1(first, arg1);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeSwap() const {
        return std::make_tuple(1, encodeSwapSource( encodeSwapDestination( encodeControl(0, type), arg0), arg1), 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeShift() const {
        auto first = encodeControl(0, type);
        first = encodeShiftFlagImmediate(first, immediate);
        first = encodeShiftFlagLeft(first, shiftLeft);
        first = encodeShiftRegister0(first, arg0);
        first = immediate ? encodeShiftImmediate(first, arg1) : encodeShiftRegister1(first, arg1);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeCompare() const {
        auto first = encodeControl(0, type);
        first = encodeCompareType(first, static_cast<CompareStyle>(subType));
        first = encodeCompareImmediateFlag(first, immediate);
        auto second = encodeCompareRegister0(0, arg0);
        second = immediate ? encodeCompareImmediate(second, arg1) : encodeCompareRegister1(second, arg1);
        return std::make_tuple(2, first, second, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeSet() const {
        int count = instructionSizeFromImmediateMask(bitmask);
        auto first = encodeControl(0, type);
        first = encodeSetBitmask(first, bitmask);
        first = encodeSetDestination(first, arg0);
        // use the mask during encoding since we know how many Words the
        // instruction is made up of
        auto maskedValue = mask(bitmask) & fullImmediate;
        auto second = static_cast<Word>(maskedValue);
        auto third = static_cast<Word>(maskedValue >> 16);
        return std::make_tuple(count, first, second, third);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeMemory() const {
        auto first = encodeControl(0, type);
        first = encodeMemoryFlagType(first, static_cast<MemoryOperation>(subType));
        first = encodeMemoryFlagBitmask(first, bitmask);
        first = encodeMemoryFlagIndirect(first, indirect);
        // the register and offset occupy the same space
        first = encodeMemoryOffset(first, arg0);
        // be lazy and set up the second word even if it isn't used. Reduces
        // the amount of branching and special cases :)
        auto second = encodeMemoryAddress(0, arg1);
        second = encodeMemoryValue(0, arg2);
        return std::make_tuple(readNextWord ? 2 : 1, first, second, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeLogical() const {
        auto first = encodeControl(0, type);
        first = encodeLogicalFlagImmediate(first, immediate);
        first = encodeLogicalFlagType(first, static_cast<LogicalOps>(subType));
        if (immediate) {
            first = encodeLogicalFlagImmediateMask(first, bitmask);
            first = encodeLogicalImmediateDestination(first, arg0);
            auto maskedImmediate = mask(bitmask) & fullImmediate;
            auto second = static_cast<Word>(maskedImmediate);
            auto third = static_cast<Word>(maskedImmediate >> 16);
            return std::make_tuple(instructionSizeFromImmediateMask(bitmask), first, second, third);
        } else {
            first = encodeLogicalRegister0(first, arg0);
            first = encodeLogicalRegister1(first, arg1);
            return std::make_tuple(1, first, 0, 0);
        }
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeBranch() const {
        auto first = encodeControl(0, type);
        first = encodeBranchFlagIsConditional(first, isConditional);
        first = encodeBranchFlagIsIfForm(first, isIf);
        first = encodeBranchFlagIsImmediate(first, immediate);
        first = encodeBranchFlagIsCallForm(first, isCall);
        if (isIf) {
            first = encodeBranchIfOnTrue(first, arg0);
            first = encodeBranchIfOnFalse(first, arg1);
            return std::make_tuple(1, first, 0, 0);
        } else {
            if (immediate) {
                // encode the 24-bit number
                auto second = static_cast<Word>(fullImmediate);
				auto third = static_cast<Word>(fullImmediate >> 16);
                return std::make_tuple(3, first, second, third);
            } else {
                first = encodeBranchIndirectDestination(first, arg0);
                return std::make_tuple(1, first, 0, 0);
            }
        }
    }
    InstructionEncoder::Encoding InstructionEncoder::encodeComplex() const {
        auto sType = static_cast<ComplexSubTypes>(subType);
        if (sType != ComplexSubTypes::Encoding) {
            throw syn::Problem("Attempted to encode an unsupported value as a complex type!");
        }
        auto first = encodeControl(0, type);
        first = encodeComplexSubClass(first, sType);
        // right now it is a single word
        first = encodeComplexClassEncoding_Type(first, static_cast<EncodingOperation>(bitmask));
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encode() const {
        // always encode the type
        static auto testMemFn = std::mem_fn(&InstructionEncoder::encode);
        static std::map<Operation, decltype(testMemFn)> dispatchTable = {
            { Operation::Memory, std::mem_fn(&InstructionEncoder::encodeMemory) },
            { Operation::Arithmetic, std::mem_fn(&InstructionEncoder::encodeArithmetic) },
            { Operation::Shift, std::mem_fn(&InstructionEncoder::encodeShift) },
            { Operation::Logical, std::mem_fn(&InstructionEncoder::encodeLogical) },
            { Operation::Compare, std::mem_fn(&InstructionEncoder::encodeCompare) },
            { Operation::Branch, std::mem_fn(&InstructionEncoder::encodeBranch) },
            { Operation::Move, std::mem_fn(&InstructionEncoder::encodeMove) },
            { Operation::Set, std::mem_fn(&InstructionEncoder::encodeSet) },
            { Operation::Swap, std::mem_fn(&InstructionEncoder::encodeSwap) },
            { Operation::Complex, std::mem_fn(&InstructionEncoder::encodeComplex) },
        };

        auto result = dispatchTable.find(type);
        if (result == dispatchTable.end()) {
            throw syn::Problem("Illegal type to encode!");
        } else {
            return result->second(this);
        }
    }

    int InstructionEncoder::numWords() const {
        return std::get<0>(encode());
    }
    void InstructionEncoder::clear() {
        currentLine = 0;
        address = 0;
        type = Operation::Memory;
        immediate = false;
        shiftLeft = false;
        isIf = false;
        isCall = false;
        isConditional = false;
        bitmask = 0b0000;
        arg0 = 0;
        arg1 = 0;
        arg2 = 0;
        isLabel = false;
        labelValue.clear();
        subType = 0;
        fullImmediate = 0;
        indirect = false;
        readNextWord = false;
    }

}
