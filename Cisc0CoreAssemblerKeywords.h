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


#ifndef CISC0_CORE_ASSEMBLER_KEYWORDS_H__
#define CISC0_CORE_ASSEMBLER_KEYWORDS_H__

#include "AssemblerBase.h"

namespace cisc0 {
    // modifiers
	DefSymbol(Immediate, immediate);
	DefSymbol(Indirect, indirect);
	DefSymbol(Direct, direct);
    // shift modifiers
	DefSymbol(Left, left);
	DefSymbol(Right, right);
    // register names
	DefSymbol(AddrRegister, addr);
	DefSymbol(StackPointer, sp);
	DefSymbol(InstructionPointer, ip);
	DefSymbol(ValueRegister, value);
	DefSymbol(MaskRegister, mask);
	DefSymbol(FieldRegister, field);
	DefSymbol(CallStackPointer, csp);

    // compare operations
	DefSymbol(Equals, ==);
	DefSymbol(NotEquals, !=);
	DefSymbol(LessThan, <);
	DefSymbol(LessThanOrEqualTo, <=);
	DefSymbol(GreaterThan, >);
	DefSymbol(GreaterThanOrEqualTo, >=);
    DefSymbol(MoveToCondition, MoveToCondition);
    DefSymbol(MoveFromCondition, MoveFromCondition);
    // arithmetic operations
    using SymbolAdd = syn::SymbolAddKeyword;
    using SymbolSub = syn::SymbolSubKeyword;
    using SymbolMul = syn::SymbolMulKeyword;
    using SymbolDiv = syn::SymbolDivKeyword;
    using SymbolRem = syn::SymbolRemKeyword;
    DefSymbol(Min, min);
    DefSymbol(Max, max);

    // memory operations
	DefSymbol(Load, load);
	DefSymbol(Store, store);
	DefSymbol(Push, push);
	DefSymbol(Pop, pop);

    // logical operations
    using SymbolAnd = syn::SymbolAndKeyword;
    using SymbolOr = syn::SymbolOrKeyword;
    using SymbolNot = syn::SymbolNotKeyword;
	DefSymbol(Xor, xor);
	DefSymbol(Nand, nand);

	DefSymbol(BitSet, bitset);
	DefSymbol(BitUnset, bitunset);
	DefSymbol(Encode, encode);
	DefSymbol(Decode, decode);

	DefSymbol(PushValueAddr, PushValueAddr);
	DefSymbol(PopValueAddr,  PopValueAddr);
	DefSymbol(IncrementValueAddr, IncrementValueAddr);
	DefSymbol(DecrementValueAddr, DecrementValueAddr);
	DefSymbol(WordsBeforeFirstZero, CountWordsBeforeFirstZero);

	DefSymbol(IsEven, evenp);
	DefSymbol(IsOdd, oddp);

	DefSymbol(Hex8ToRegister, Hex8ToRegister);
	DefSymbol(RegisterToHex8, RegisterToHex8);
	DefSymbol(MemCopy, MemCopy);

	DefSymbol(Call, call);
	DefSymbol(NoCall, nocall);
	DefSymbol(Conditional, conditional);
	DefSymbol(Unconditional, unconditional);
} // end namespace cisc0
#endif // end CISC0_CORE_ASSEMBLER_KEYWORDS_H__
