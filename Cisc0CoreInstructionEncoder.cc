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

	template<bool v>
	using ConditionFulfillment = syn::ConditionFulfillment<v>;


	Word InstructionEncoder::commonEncoding() const {
		return encodeControl(0, _type);
	}

	template<Operation op>
	constexpr Word encodeDestination(Word value, byte index) noexcept {
		return cisc0::encodeDestination<op, byte>(value, index);
	}

	template<Operation op>
	constexpr Word setImmediateBit(Word input, bool immediate) noexcept {
		return cisc0::encodeFlagImmediate<op>(input, immediate);
	}


	template<Operation op>
	struct HasBitmaskField : ConditionFulfillment< false> { 
		static constexpr Word setBitmaskField(Word value, byte mask) noexcept {
			return value;
		}
	};

	template<Operation op>
	constexpr Word setBitmaskField(Word input, byte mask) noexcept {
		return cisc0::encodeBitmask<op, byte>(input, mask);
	}

	template<Operation op>
	struct HasArg1 : ConditionFulfillment<false> {
		static constexpr Word encodeArg1(Word input, byte index, bool immediate) noexcept {
			return input;
		}
	};

#define DefHasArg1WithImmediate(o, immAction , regAction ) \
	template<> \
	struct HasArg1 < Operation :: o > : ConditionFulfillment<true> { \
		static constexpr Word encodeArg1(Word input, byte index, bool immediate) noexcept { \
			return immediate ? immAction ( input, index ) : regAction ( input, index ); \
		} \
	}
#define DefHasArg1DoNothingOnImmediate(o, action) \
	template<> \
	struct HasArg1 < Operation :: o > : ConditionFulfillment<true> { \
		static constexpr Word encodeArg1(Word input, byte index, bool immediate) noexcept { \
			return immediate ? input : action ( input, index ) ; \
		} \
	}
#define DefHasArg1(o, action) \
	template<> \
	struct HasArg1 < Operation :: o > : ConditionFulfillment<true> { \
		static constexpr Word encodeArg1(Word input, byte index, bool immediate) noexcept { \
			return action ( input, index ) ; \
		} \
	}
	DefHasArg1WithImmediate(Arithmetic, encodeArithmeticImmediate, encodeArithmeticSource);
	DefHasArg1WithImmediate(Shift, encodeShiftImmediate, encodeShiftSource);
	DefHasArg1WithImmediate(Compare, encodeCompareImmediate, encodeCompareSource);
	DefHasArg1(Move, encodeMoveSource);
	DefHasArg1(Swap, encodeSwapSource);
	DefHasArg1DoNothingOnImmediate(Logical, encodeLogicalSource);
#undef DefHasArg1DoNothingOnImmediate
#undef DefHasArg1
#undef DefHasArg1WithImmediate


	template<Operation op>
	constexpr Word encodeArg1(Word input, byte index, bool immediate = false) noexcept {
		static_assert(HasArg1<op>::value, "Given operation type does not have a second argument!");
		return HasArg1<op>::encodeArg1(input, index, immediate);
	}


    InstructionEncoder::Encoding InstructionEncoder::encodeArithmetic() const {
		constexpr auto op = Operation::Arithmetic;
		auto first = encodeSubType<op>(commonEncoding() , _subType);
		first = setImmediateBit<op>(first, _immediate);
		first = encodeDestination<op>(first, _arg0);
		first = encodeArg1<op>(first, _arg1, _immediate);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeMove() const {
		constexpr auto op = Operation::Move;
		auto first = setBitmaskField<op>(commonEncoding(), _bitmask);
		first = encodeDestination<op>(first, _arg0);
		first = encodeArg1<op>(first, _arg1);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeSwap() const {
		constexpr auto op = Operation::Swap;
		auto first = encodeDestination<op>(commonEncoding(), _arg0);
		first = encodeArg1<op>(first, _arg1);
		return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeShift() const {
		constexpr auto op = Operation::Arithmetic;
		auto first = setImmediateBit<op>(commonEncoding(), _immediate);
        first = encodeShiftFlagLeft(first, _shiftLeft);
		first = encodeDestination<op>(first, _arg0);
		first = encodeArg1<op>(first, _arg1, _immediate);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeCompare() const {
		constexpr auto op = Operation::Compare;
		auto first = encodeSubType<op>(commonEncoding(), _subType);
		first = setImmediateBit<op>(first, _immediate);
		auto second = encodeDestination<op>(0, _arg0);
		second = encodeArg1<op>(second, _arg1, _immediate);
        return std::make_tuple(2, first, second, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeSet() const {
		constexpr auto op = Operation::Set;
		auto first = setBitmaskField<op>(commonEncoding(), _bitmask);
		first = encodeDestination<op>(first, _arg0);
        // use the mask during encoding since we know how many Words the
        // instruction is made up of
        auto maskedValue = mask(_bitmask) & _fullImmediate;
        auto second = static_cast<Word>(maskedValue);
        auto third = static_cast<Word>(maskedValue >> 16);
        return std::make_tuple(instructionSizeFromImmediateMask(_bitmask), first, second, third);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeMemory() const {
		constexpr auto op = Operation::Memory;
		auto first = encodeSubType<op>(commonEncoding(), _subType);
		first = setBitmaskField<op>(first, _bitmask);
        first = encodeMemoryFlagIndirect(first, _indirect);
        // the register and offset occupy the same space
		first = encodeDestination<Operation::Memory>(first, _arg0);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeLogical() const {
		constexpr auto op = Operation::Logical;
		auto first = encodeSubType<op>(commonEncoding(), _subType);
		auto second = 0u;
		auto third = 0u;
		auto width = _immediate ? instructionSizeFromImmediateMask(_bitmask) : 1;
		first = setImmediateBit<op>(first, _immediate);
		first = encodeDestination<op>(first, _arg0, _immediate);
		// if we are not looking at an immediate then this operation will
		// actually do something
		first = encodeArg1<op>(first, _arg1, _immediate);
        if (_immediate) {
			first = setBitmaskField<op>(first, _bitmask);
            auto maskedImmediate = mask(_bitmask) & _fullImmediate;
            second = static_cast<Word>(maskedImmediate);
            third = static_cast<Word>(maskedImmediate >> 16);
        } 
		return std::make_tuple(width, first, second, third);
    }

	template<>
	struct HasArg0 < Operation::Branch> : ConditionFulfillment<true> {
		static constexpr Word encodeDestination(Word input, byte index, bool immediate) noexcept {
			if (cisc0::decodeBranchFlagIsIfForm(input)) {
				return cisc0::encodeBranchIfOnTrue(input, index); 
			} else {
				return immediate ? input : cisc0::encodeBranchIndirectDestination(input, index);
			}
		}
	};

	template<>
	struct HasArg1 < Operation::Branch> : ConditionFulfillment<true> {
		static constexpr Word encodeArg1(Word input, byte index, bool immediate) noexcept {
			if (cisc0::decodeBranchFlagIsIfForm(input)) {
				return cisc0::encodeBranchIfOnFalse(input, index); 
			} else {
				return input;
			}
		}
	};

    InstructionEncoder::Encoding InstructionEncoder::encodeBranch() const {
		constexpr auto op = Operation::Branch;
		auto first = setImmediateBit<op>(commonEncoding(), _immediate);
		auto second = _immediate ? static_cast<Word>(_fullImmediate) : 0u;
		auto third = _immediate ? static_cast<Word>(_fullImmediate >> 16) : 0u;
		auto width = _immediate ? 3 : 1;
        first = encodeBranchFlagIsConditional(first, _isConditional);
        first = encodeBranchFlagIsIfForm(first, _isIf);
        first = encodeBranchFlagIsCallForm(first, _isCall);
		first = encodeDestination<op>(first, _arg0, _immediate);
		first = encodeArg1<op>(first, _arg1, _immediate);
        return std::make_tuple(width, first, second, third);
    }
	template<ComplexSubTypes t> struct ComplexSubTypeToNestedType { };

	template<> struct ComplexSubTypeToNestedType < ComplexSubTypes::Encoding > { 
		using type = EncodingOperation; 
		static Word encodeType(Word value, type t) noexcept {
			return encodeComplexClassEncodingType(value, t);
		}
	};
	template<> struct ComplexSubTypeToNestedType < ComplexSubTypes::Extended > { 
		using type = ExtendedOperation; 
		static Word encodeType(Word value, type t) noexcept {
			return encodeComplexClassExtendedType(value, t);
		}
	};

	template<ComplexSubTypes t>
	typename ComplexSubTypeToNestedType<t>::type convertBitmask(byte mask) noexcept {
		return static_cast<typename ComplexSubTypeToNestedType<t>::type>(mask);
	}

	template<ComplexSubTypes t>
	Word encodeSubType(Word value, byte mask) noexcept {
		return ComplexSubTypeToNestedType<t>::encodeType(value, convertBitmask<t>(mask));
	}

	template<ComplexSubTypes t>
	Word encodeDestination(Word value, byte index) noexcept {
		static_assert(t != ComplexSubTypes::Encoding, "Encoding operations take in no arguments!");
		switch(t) {
			case ComplexSubTypes::Extended:
				return cisc0::encodeComplexClassExtendedDestination(value, index);
			default:
				throw syn::Problem("Given complex sub type does not take in arguments!");
		}
	}
    InstructionEncoder::Encoding InstructionEncoder::encodeComplex() const {
        auto sType = static_cast<ComplexSubTypes>(_subType);
		auto first = encodeSubType<Operation::Complex>(commonEncoding(), _subType);
        if (sType == ComplexSubTypes::Encoding) {
			// right now it is a single word
			first = encodeSubType<ComplexSubTypes::Encoding>(first, _bitmask);
			return std::make_tuple(1, first, 0, 0);
        } else if (sType == ComplexSubTypes::Extended) {
			first = encodeSubType<ComplexSubTypes::Extended>(first, _bitmask);
			first = encodeDestination<ComplexSubTypes::Extended>(first, _arg0);
			return std::make_tuple(1, first, 0, 0);
		} else {
			throw syn::Problem("Illegal complex instruction group!");
		}
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

        auto result = dispatchTable.find(_type);
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
        _currentLine = 0;
        _address = 0;
        _type = Operation::Memory;
        _immediate = false;
        _shiftLeft = false;
        _isIf = false;
        _isCall = false;
        _isConditional = false;
        _bitmask = 0b0000;
        _arg0 = 0;
        _arg1 = 0;
        _arg2 = 0;
        _isLabel = false;
        _labelValue.clear();
        _subType = 0;
        _fullImmediate = 0;
        _indirect = false;
    }

}
