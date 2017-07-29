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
    using SymbolAnd = syn::SymbolAndKeyword;
    using SymbolOr = syn::SymbolOrKeyword;
    using SymbolXor = syn::SymbolXorKeyword;
    using SymbolNand = syn::SymbolNandKeyword;
    using SymbolNor = syn::SymbolNorKeyword;
    using SymbolMin = syn::SymbolMinKeyword;
    using SymbolMax = syn::SymbolMaxKeyword;
	// two arg variants
    using SymbolNot = syn::SymbolNotKeyword;
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
    using SymbolMove = syn::SymbolMoveKeyword;
    using SymbolSwap = syn::SymbolSwapKeyword;
    using SymbolStore = syn::SymbolStoreKeyword;
    using SymbolLoad = syn::SymbolLoadKeyword;
    DefSymbol(LoadIO, io-load);
    DefSymbol(StoreIO, io-store);
    using SymbolPush = syn::SymbolPushKeyword;
    using SymbolPop = syn::SymbolPopKeyword;
	// immediate variants
    DefSymbol(LoadWithOffset, load-offset);
    DefSymbol(StoreWithOffset, store-offset);
    DefSymbol(LoadIOWithOffset, io-load-offset);
    DefSymbol(StoreIOWithOffset, io-store-offset);
	// three gpr variant
    DefSymbol(LoadCode, code-load);
    DefSymbol(StoreCode, code-store);
	// gpr immediate
    DefSymbol(PushImmediate, pushi);
    using SymbolSet = syn::SymbolSetKeyword;
    DefSymbol(LoadImmediate, loadi);
    DefSymbol(StoreImmediate, storei);

	// branch instructions
	// one unconditional gpr
    using SymbolBranchUnconditional = syn::SymbolBranchKeyword;
    DefSymbol(BranchUnconditionalLink, branch_l);
	// branch unconditional immediate
    DefSymbol(BranchUnconditionalImmediate, branch_i);
    DefSymbol(BranchUnconditionalImmediateLink, branch_il);
	// conditional branch gpr
    DefSymbol(BranchConditional, branch_c);
    DefSymbol(BranchConditionalLink, branch_cl);
	// branch conditional immediate variants
    DefSymbol(BranchConditionalImmediate, branch_ci);
    DefSymbol(BranchConditionalImmediateLink, branch_cil);
	// branch conditional to the link register
    DefSymbol(BranchConditionalLR, branch_clr);
    DefSymbol(BranchConditionalLRAndLink, branch_clrl);
	// no argument branching
    DefSymbol(BranchUnconditionalLR, branch_lr);
    DefSymbol(BranchUnconditionalLRAndLink, branch_lrl);
    DefSymbol(BranchReturnFromError, rfe);
	// Compare operations
    using SymbolEq = syn::SymbolEqualsKeyword;
    using SymbolNeq = syn::SymbolNotEqualsKeyword;
    using SymbolLessThan = syn::SymbolLessThanKeyword;
    using SymbolLessThanOrEqualTo = syn::SymbolLessThanOrEqualToKeyword;
    using SymbolGreaterThan = syn::SymbolGreaterThanKeyword;
    using SymbolGreaterThanOrEqualTo = syn::SymbolGreaterThanOrEqualToKeyword;
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

