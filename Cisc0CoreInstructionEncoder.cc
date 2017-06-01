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
	using ConditionFulfillment = std::integral_constant<bool, v>;
	template<Operation op>
	struct HasArg0 : ConditionFulfillment<false> {
		static constexpr RegisterValue encodeArg0(RegisterValue input, byte index, bool immediate) noexcept {
			return input;
		}
	};

	template<>
	struct HasArg0 < Operation::Logical > : ConditionFulfillment<true> {
		static constexpr RegisterValue encodeArg0(RegisterValue input, byte index, bool immediate) noexcept {
			if (immediate) {
				return encodeLogicalImmediateDestination(input, index);
			} else {
				return encodeLogicalRegister0(input, index);
			}
		}
	};

#define DefHasArg0(o, action) \
	template<> \
	struct HasArg0 < Operation:: o > : ConditionFulfillment< true >  { \
		static constexpr RegisterValue encodeArg0(RegisterValue input, byte index, bool immediate) noexcept { \
			return action ( input , index ); \
		} \
	}
DefHasArg0(Arithmetic, encodeArithmeticDestination);
DefHasArg0(Swap, encodeSwapDestination);
DefHasArg0(Move, encodeMoveRegister0);
DefHasArg0(Shift, encodeShiftRegister0);
DefHasArg0(Compare, encodeCompareRegister0);
DefHasArg0(Set, encodeSetDestination);
DefHasArg0(Memory, encodeMemoryOffset); // the register and offset occupy the same space
#undef DefHasArg0


	template<Operation op>
	Word encodeArg0(Word value, byte index, bool immediate = false) noexcept {
		static_assert(HasArg0<op>::value, "Provided operation does not have arg0");
		return HasArg0<op>::encodeArg0(value, index, immediate);
	}

	RegisterValue InstructionEncoder::commonEncoding() const {
		return encodeControl(0, _type);
	}
	template<Operation op> 
	struct OperationToType : ConditionFulfillment<false> {
		using type = decltype(syn::defaultErrorState<Operation>); 
		static constexpr RegisterValue encodeType(RegisterValue input, byte value) noexcept {
			return input; 
		}
	};

#define DefOpToType( targetOperation , actualType , encoder ) \
	template<> \
	struct OperationToType< Operation :: targetOperation > : ConditionFulfillment< true> { \
		using type = actualType; \
		static constexpr RegisterValue encodeType(RegisterValue input, byte value) noexcept { \
			return encoder ( input , static_cast < actualType > ( value ) ) ; \
		} \
	}
	DefOpToType(Arithmetic , ArithmeticOps, encodeArithmeticFlagType);
	DefOpToType(Compare , CompareStyle , encodeCompareType );
	DefOpToType(Memory , MemoryOperation, encodeMemoryFlagType );
	DefOpToType(Logical, LogicalOps, encodeLogicalFlagType );
	DefOpToType(Complex , ComplexSubTypes , encodeComplexSubClass );
#undef DefOpToType

	template<Operation op>
	constexpr RegisterValue encodeType(RegisterValue input, byte t) noexcept {
		static_assert(OperationToType<op>::value, "Specialized implementation for encoding the instruction type has not been provided!");
		return OperationToType<op>::encodeType ( input, t) ; 
	}


	template<Operation op>
	struct HasImmediateFlag : ConditionFulfillment< false> { 
		static constexpr RegisterValue setImmediateBit(RegisterValue value, bool immediate) noexcept {
			return value;
		}
	};

#define DefImmediateFlag( o , action ) template<> \
	struct HasImmediateFlag < Operation :: o > : ConditionFulfillment< true > {  \
		static constexpr RegisterValue setImmediateBit(RegisterValue value, bool immediate) noexcept { \
			return action ( value , immediate ); \
		} \
	}
	DefImmediateFlag(Arithmetic, encodeArithmeticFlagImmediate);
	DefImmediateFlag(Shift, encodeShiftFlagImmediate);
	DefImmediateFlag(Compare, encodeCompareImmediate);
	DefImmediateFlag(Logical, encodeLogicalFlagImmediate);
	DefImmediateFlag(Branch, encodeBranchFlagIsImmediate);
#undef DefImmediateFlag


	template<Operation op>
	constexpr RegisterValue setImmediateBit(RegisterValue input, bool immediate) noexcept {
		static_assert(HasImmediateFlag<op>::value, "Given operation type does not have an immediate bit!");
		return HasImmediateFlag<op>::setImmediateBit(input, immediate);
	}


	template<Operation op>
	struct HasBitmaskField : ConditionFulfillment< false> { 
		static constexpr RegisterValue setBitmaskField(RegisterValue value, byte mask) noexcept {
			return value;
		}
	};

#define DefHasBitmask( o , action ) \
	template<> \
	struct HasBitmaskField < Operation :: o > : ConditionFulfillment< true> { \
		static constexpr RegisterValue setBitmaskField(RegisterValue value, byte mask) noexcept { \
			return action ( value, mask ); \
		} \
	}
	DefHasBitmask(Move , encodeMoveBitmask);
	DefHasBitmask(Set, encodeSetBitmask);
	DefHasBitmask(Memory, encodeMemoryFlagBitmask);
	DefHasBitmask(Logical, encodeLogicalFlagImmediateMask);
#undef DefHasBitmask

	template<Operation op>
	constexpr RegisterValue setBitmaskField(RegisterValue input, byte mask) noexcept {
		static_assert(HasBitmaskField<op>::value, "Given operation does not have a bitmask field!");
		return HasBitmaskField<op>::setBitmaskField(input, mask); 
	}


    InstructionEncoder::Encoding InstructionEncoder::encodeArithmetic() const {
		constexpr auto op = Operation::Arithmetic;
		auto first = encodeType<op>(commonEncoding() , _subType);
		first = setImmediateBit<op>(first, _immediate);
		first = encodeArg0<op>(first, _arg0);
        first = _immediate ? encodeArithmeticImmediate(first, _arg1) : encodeArithmeticSource(first, _arg1);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeMove() const {
		constexpr auto op = Operation::Move;
		auto first = setBitmaskField<op>(commonEncoding(), _bitmask);
		first = encodeArg0<op>(first, _arg0);
        first = encodeMoveRegister1(first, _arg1);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeSwap() const {
        return std::make_tuple(1, encodeSwapSource( encodeSwapDestination( commonEncoding(), _arg0), _arg1), 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeShift() const {
		constexpr auto op = Operation::Arithmetic;
		auto first = setImmediateBit<op>(commonEncoding(), _immediate);
        first = encodeShiftFlagLeft(first, _shiftLeft);
		first = encodeArg0<op>(first, _arg0);
        first = _immediate ? encodeShiftImmediate(first, _arg1) : encodeShiftRegister1(first, _arg1);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeCompare() const {
		constexpr auto op = Operation::Compare;
		auto first = encodeType<op>(commonEncoding(), _subType);
		first = setImmediateBit<op>(first, _immediate);
		auto second = encodeArg0<op>(0, _arg0);
        second = _immediate ? encodeCompareImmediate(second, _arg1) : encodeCompareRegister1(second, _arg1);
        return std::make_tuple(2, first, second, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeSet() const {
		constexpr auto op = Operation::Set;
		auto first = setBitmaskField<op>(commonEncoding(), _bitmask);
		first = encodeArg0<op>(first, _arg0);
        // use the mask during encoding since we know how many Words the
        // instruction is made up of
        auto maskedValue = mask(_bitmask) & _fullImmediate;
        auto second = static_cast<Word>(maskedValue);
        auto third = static_cast<Word>(maskedValue >> 16);
        return std::make_tuple(instructionSizeFromImmediateMask(_bitmask), first, second, third);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeMemory() const {
		constexpr auto op = Operation::Memory;
		auto first = encodeType<op>(commonEncoding(), _subType);
		first = setBitmaskField<op>(first, _bitmask);
        first = encodeMemoryFlagIndirect(first, _indirect);
        // the register and offset occupy the same space
		first = encodeArg0<Operation::Memory>(first, _arg0);
        return std::make_tuple(1, first, 0, 0);
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeLogical() const {
		constexpr auto op = Operation::Logical;
		auto first = encodeType<op>(commonEncoding(), _subType);
		first = setImmediateBit<op>(first, _immediate);
		first = encodeArg0<op>(first, _arg0, _immediate);
        if (_immediate) {
			first = setBitmaskField<op>(first, _bitmask);
            auto maskedImmediate = mask(_bitmask) & _fullImmediate;
            auto second = static_cast<Word>(maskedImmediate);
            auto third = static_cast<Word>(maskedImmediate >> 16);
            return std::make_tuple(instructionSizeFromImmediateMask(_bitmask), first, second, third);
        } else {
            first = encodeLogicalRegister1(first, _arg1);
            return std::make_tuple(1, first, 0, 0);
        }
    }

    InstructionEncoder::Encoding InstructionEncoder::encodeBranch() const {
		constexpr auto op = Operation::Branch;
		auto first = setImmediateBit<op>(commonEncoding(), _immediate);
        first = encodeBranchFlagIsConditional(first, _isConditional);
        first = encodeBranchFlagIsIfForm(first, _isIf);
        first = encodeBranchFlagIsCallForm(first, _isCall);
        if (_isIf) {
            first = encodeBranchIfOnTrue(first, _arg0);
            first = encodeBranchIfOnFalse(first, _arg1);
            return std::make_tuple(1, first, 0, 0);
        } else {
            if (_immediate) {
                // encode the 24-bit number
                auto second = static_cast<Word>(_fullImmediate);
				auto third = static_cast<Word>(_fullImmediate >> 16);
                return std::make_tuple(3, first, second, third);
            } else {
                first = encodeBranchIndirectDestination(first, _arg0);
                return std::make_tuple(1, first, 0, 0);
            }
        }
    }
	template<ComplexSubTypes t> struct ComplexSubTypeToNestedType { };

	template<> struct ComplexSubTypeToNestedType < ComplexSubTypes::Encoding > { 
		using type = EncodingOperation; 
		static Word encodeType(Word value, type t) noexcept {
			return encodeComplexClassEncoding_Type(value, t);
		}
	};
	template<> struct ComplexSubTypeToNestedType < ComplexSubTypes::Extended > { 
		using type = ExtendedOperation; 
		static Word encodeType(Word value, type t) noexcept {
			return encodeComplexClassExtended_Type(value, t);
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
	Word encodeArg0(Word value, byte index) noexcept {
		static_assert(t != ComplexSubTypes::Encoding, "Encoding operations take in no arguments!");
		switch(t) {
			case ComplexSubTypes::Extended:
				return encodeComplexClassExtended_Arg0(value, index);
			default:
				throw syn::Problem("Given complex sub type does not take in arguments!");
		}
	}
    InstructionEncoder::Encoding InstructionEncoder::encodeComplex() const {
        auto sType = static_cast<ComplexSubTypes>(_subType);
		auto first = encodeType<Operation::Complex>(commonEncoding(), _subType);
        if (sType == ComplexSubTypes::Encoding) {
			// right now it is a single word
			first = encodeSubType<ComplexSubTypes::Encoding>(first, _bitmask);
			return std::make_tuple(1, first, 0, 0);
        } else if (sType == ComplexSubTypes::Extended) {
			first = encodeSubType<ComplexSubTypes::Extended>(first, _bitmask);
			first = encodeArg0<ComplexSubTypes::Extended>(first, _arg0);
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
