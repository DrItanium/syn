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


// assembler keywords
#ifndef IRIS_CORE_ASSEMBLER_KEYWORDS_H__
#define IRIS_CORE_ASSEMBLER_KEYWORDS_H__

#include "AssemblerBase.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>
#include <pegtl/parse.hh>


namespace iris {

	// DIRECTIVES
    DefSymbol(DataDirective, .data);
    DefSymbol(CodeDirective, .code);
	// Arithmetic operations
    using SymbolAdd = syn::SymbolAddKeyword;
    using SymbolSub = syn::SymbolSubKeyword;
    using SymbolMul = syn::SymbolMulKeyword;
    using SymbolDiv = syn::SymbolDivKeyword;
    using SymbolRem = syn::SymbolRemKeyword;
    DefSymbol(ShiftLeft, shl);
    DefSymbol(ShiftRight, shr);
    DefSymbol(And, and);
    DefSymbol(Or, or);
    DefSymbol(Xor, xor);
    DefSymbol(Nand, nand);
    DefSymbol(Nor, nor);
    DefSymbol(Min, min);
    DefSymbol(Max, max);
	// two arg variants
    DefSymbol(Not, not);
	// immediate variants
    DefSymbol(AddImmediate, addi);
    DefSymbol(SubImmediate, subi);
    DefSymbol(MulImmediate, muli);
    DefSymbol(DivImmediate, divi);
    DefSymbol(RemImmediate, remi);
    DefSymbol(ShiftLeftImmediate, shli);
    DefSymbol(ShiftRightImmediate, shri);
	// MoveOperations
	// one arg operations
    DefSymbol(MoveToIP, mtip);
    DefSymbol(MoveFromIP, mfip);
    DefSymbol(MoveToLR, mtlr);
    DefSymbol(MoveFromLR, mflr);
    DefSymbol(RestoreAllRegisters, rregs);
    DefSymbol(SaveAllRegisters, sregs);

	// two arg operations
    DefSymbol(Move, move);
    DefSymbol(Swap, swap);
    DefSymbol(Load, ld);
    DefSymbol(Store, st);
    DefSymbol(LoadIO, iold);
    DefSymbol(StoreIO, iost);
    DefSymbol(Push, push);
    DefSymbol(Pop, pop);
	// immediate variants
    DefSymbol(LoadWithOffset, ldof);
    DefSymbol(StoreWithOffset, stof);
    DefSymbol(LoadIOWithOffset, ioldof);
    DefSymbol(StoreIOWithOffset, iostof);
	// three gpr variant
    DefSymbol(LoadCode, cld);
    DefSymbol(StoreCode, cst);
	// gpr immediate
    DefSymbol(PushImmediate, pushi);
    DefSymbol(Set, set);
    DefSymbol(LoadImmediate, ldi);
    DefSymbol(StoreImmediate, sti);

	// branch instructions
	// one unconditional gpr
    DefSymbol(BranchUnconditional, j);
    DefSymbol(BranchUnconditionalLink, jl);
	// branch unconditional immediate
    DefSymbol(BranchUnconditionalImmediate, ji);
    DefSymbol(BranchUnconditionalImmediateLink, jil);
	// conditional branch gpr
    DefSymbol(BranchConditional, bc);
    DefSymbol(BranchConditionalLink, bcl);
	// branch conditional immediate variants
    DefSymbol(BranchConditionalImmediate, bci);
    DefSymbol(BranchConditionalImmediateLink, bcil);
	// if then else branching
    DefSymbol(IfThenElse, if);
    DefSymbol(IfThenElseLink, ifl);
	// branch conditional to the link register
    DefSymbol(BranchConditionalLR, bclr);
    DefSymbol(BranchConditionalLRAndLink, bclrl);
	// no argument branching 
    DefSymbol(BranchUnconditionalLR, blr);
    DefSymbol(BranchUnconditionalLRAndLink, blrl);
    DefSymbol(BranchReturnFromError, rfe);
	// Compare operations
    DefSymbol(Eq, eq);
    DefSymbol(Neq, neq);
    DefSymbol(LessThan, lt);
    DefSymbol(GreaterThan, gt);
    DefSymbol(LessThanOrEqualTo, le);
    DefSymbol(GreaterThanOrEqualTo, ge);
    DefSymbol(EqImmediate, eqi);
    DefSymbol(NeqImmediate, neqi);
    DefSymbol(LessThanImmediate, lti);
    DefSymbol(GreaterThanImmediate, gti);
    DefSymbol(LessThanOrEqualToImmediate, lei);
    DefSymbol(GreaterThanOrEqualToImmediate, gei);
	// conditional register operations
    DefSymbol(SaveCRs, psave);
    DefSymbol(RestoreCRs, prestore);
    DefSymbol(CRXor, pxor);
    DefSymbol(CRNot, pnot);
    DefSymbol(CRAnd, pand);
    DefSymbol(CROr, por);
    DefSymbol(CRNand, pnand);
    DefSymbol(CRNor, pnor);
    DefSymbol(CRSwap, pswap);
    DefSymbol(CRMove, pmove);
} // end namespace iris


#endif // end IRIS_CORE_ASSEMBLER_KEYWORDS_H__

