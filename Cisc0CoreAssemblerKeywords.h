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
	// groups
	DefSymbol(Shift, shift);
	DefSymbol(Compare, compare);
    using SymbolMove = syn::SymbolMoveKeyword;
    using SymbolSet = syn::SymbolSetKeyword;
    using SymbolSwap = syn::SymbolSwapKeyword;
	DefSymbol(Arithmetic, arithmetic);
	DefSymbol(Memory, memory);
	DefSymbol(Logical, logical);
	DefSymbol(Complex, complex);
    using SymbolBranch = syn::SymbolBranchKeyword;
	DefSymbol(Return, return);
    // modifiers
    using SymbolImmediate = syn::SymbolImmediateKeyword;
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

	DefSymbol(Call, call);
	DefSymbol(NoCall, nocall);
	DefSymbol(Conditional, conditional);
	DefSymbol(Unconditional, unconditional);

    // have one location where specific symbols are defined!
#define X(str, _, id) DefSymbol (id , str);
    // compare operations
#include "desc/cisc0/CompareStyle.desc"
    // arithmetic operations
#include "desc/cisc0/ArithmeticOps.desc"
    // memory operations
#include "desc/cisc0/MemoryOperation.desc"
    // logical operations
#include "desc/cisc0/LogicalOps.desc"

    // encoding operations
#include "desc/cisc0/EncodingOperation.desc"
    // extended operations
#include "desc/cisc0/ExtendedOperation.desc"
    // parsing operations
#include "desc/cisc0/ParsingOperation.desc"
    // complex related
#include "desc/cisc0/ComplexSubTypes.desc"
#undef X
} // end namespace cisc0
#endif // end CISC0_CORE_ASSEMBLER_KEYWORDS_H__
