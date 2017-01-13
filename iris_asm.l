%option prefix="iris"
%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <cstdint>
#include "iris.h"
#include "iris_asm.tab.h"
#include "syn_asm_base.h"
#define _POSIX_SOURCE 1

extern void iriserror(const char* c);
void reportIris16Error(const std::string& str) {
	iriserror(str.c_str());
}
%}
%option yylineno
%option noyywrap
%option nounput

digit [0-9]
xdigit [a-fA-F0-9]
bdigit [0-1]
letter [a-zA-Z]
omni [a-zA-Z0-9._]


%%
"min" { return ARITHMETIC_OP_MIN; }
"max" { return ARITHMETIC_OP_MAX; }
"add" { return ARITHMETIC_OP_ADD; }
"sub" { return ARITHMETIC_OP_SUB; }
"mul" { return ARITHMETIC_OP_MUL; }
"div" { return ARITHMETIC_OP_DIV; }
"rem" { return ARITHMETIC_OP_REM; }
"shl" { return ARITHMETIC_OP_SHIFTLEFT; }
"shr" { return ARITHMETIC_OP_SHIFTRIGHT; }
"and" { return ARITHMETIC_OP_BINARYAND; }
"or" { return ARITHMETIC_OP_BINARYOR; }
"not" { return ARITHMETIC_OP_BINARYNOT; }
"xor" { return ARITHMETIC_OP_BINARYXOR; }
"addi" { return ARITHMETIC_OP_ADD_IMM; }
"subi" { return ARITHMETIC_OP_SUB_IMM; }
"muli" { return ARITHMETIC_OP_MUL_IMM; }
"divi" { return ARITHMETIC_OP_DIV_IMM; }
"remi" { return ARITHMETIC_OP_REM_IMM; }
"shli" { return ARITHMETIC_OP_SHIFTLEFT_IMM; }
"shri" { return ARITHMETIC_OP_SHIFTRIGHT_IMM; }
"move" { return MOVE_OP_MOVE; }
"swap" { return MOVE_OP_SWAP; }
"set" { return MOVE_OP_SET; }
"ld" { return MOVE_OP_LOAD; }
"ldi" { return MOVE_OP_LOADMEM; }
"ldm" { return MOVE_OP_LOADMEM; }
"ldwo" { return MOVE_OP_LOAD_OFFSET; }
"ldc" { return MOVE_OP_LOAD_CODE; }
"st" { return MOVE_OP_STORE; }
"stc" { return MOVE_OP_STORE_CODE; }
"stwo" { return MOVE_OP_STORE_OFFSET; }
"ldio" { return MOVE_OP_LOAD_IO; }
"stio" { return MOVE_OP_STORE_IO; }
"ldiowo" { return MOVE_OP_LOAD_IO_OFFSET; }
"stiowo" { return MOVE_OP_STORE_IO_OFFSET; }
"memset" { return MOVE_OP_STOREIMM; }
"bi" 		{ return JUMP_OP_UNCONDITIONALIMMEDIATE; }
"bil" 		{ return JUMP_OP_UNCONDITIONALIMMEDIATELINK; }
"b" 		{ return JUMP_OP_UNCONDITIONALREGISTER; }
"bl" 		{ return JUMP_OP_UNCONDITIONALREGISTERLINK; }
"bit" 		{ return JUMP_OP_CONDITIONALTRUEIMMEDIATE; }
"bitl" 		{ return JUMP_OP_CONDITIONALTRUEIMMEDIATELINK; }
"bt" 		{ return JUMP_OP_CONDITIONALTRUEREGISTER; }
"btl" 		{ return JUMP_OP_CONDITIONALTRUEREGISTERLINK; }
"bif" 		{ return JUMP_OP_CONDITIONALFALSEIMMEDIATE; }
"bifl" 		{ return JUMP_OP_CONDITIONALFALSEIMMEDIATELINK; }
"bf" 		{ return JUMP_OP_CONDITIONALFALSEREGISTER; }
"bfl" 		{ return JUMP_OP_CONDITIONALFALSEREGISTERLINK; }
"ift" 		{ return JUMP_OP_IFTHENELSENORMALPREDTRUE; }
"iff" 		{ return JUMP_OP_IFTHENELSENORMALPREDFALSE; }
"iftl" { return JUMP_OP_IFTHENELSELINKPREDTRUE; }
"iffl" { return JUMP_OP_IFTHENELSELINKPREDFALSE; }
"eq" { return COMPARE_OP_EQ; }
"ne" { return COMPARE_OP_NEQ; }
"lt" { return COMPARE_OP_LESSTHAN; }
"gt" { return COMPARE_OP_GREATERTHAN; }
"le" { return COMPARE_OP_LESSTHANOREQUALTO; }
"ge" { return COMPARE_OP_GREATERTHANOREQUALTO; }
"eqi" { return COMPARE_OP_EQ_IMMEDIATE; }
"nei" { return COMPARE_OP_NEQ_IMMEDIATE; }
"lti" { return COMPARE_OP_LESSTHAN_IMMEDIATE; }
"gti" { return COMPARE_OP_GREATERTHAN_IMMEDIATE; }
"lei" { return COMPARE_OP_LESSTHANOREQUALTO_IMMEDIATE; }
"gei" { return COMPARE_OP_GREATERTHANOREQUALTO_IMMEDIATE; }
"push" { return MOVE_OP_PUSH; }
"pushi" { return MOVE_OP_PUSHIMMEDIATE; }
"pop" { return MOVE_OP_POP; }
"mtip" { return OP_MOVE_TO_IP; }
"mfip" { return OP_MOVE_FROM_IP; }
"mtlr" { return OP_MOVE_TO_LR; }
"mflr" { return OP_MOVE_FROM_LR; }
"svcr" { return OP_SAVE_CR; }
"recr" { return OP_RESTORE_CR; }
"crxor" { return OP_CR_XOR; }
"crnot" { return OP_CR_NOT; }
"crand" { return OP_CR_AND; }
"cror" { return OP_CR_OR; }
"crnand" { return OP_CR_NAND; }
"crnor" { return OP_CR_NOR; }
"crswap" { return OP_CR_SWAP; }
"crmove" { return OP_CR_MOVE; }
"blr" { return BRANCH_LR; }
"blrl" { return BRANCH_LR_LINK; }
"blrt" { return COND_TRUE_BRANCH_LR; }
"blrtl" { return COND_TRUE_BRANCH_LR_LINK; }
"blrf" { return COND_FALSE_BRANCH_LR; }
"blrfl" { return COND_FALSE_BRANCH_LR_LINK; }

"@label" { return LABEL; }
"@data"  { return DIRECTIVE_DATA; }
"@code"  { return DIRECTIVE_CODE; }
"@org"   { return DIRECTIVE_ORG; }
"@declare" { return DIRECTIVE_DECLARE; }

"@low"	{ return TAG_LOW; }
"@high" { return TAG_HI; }

"sp"	 { irislval.rval = 255; return GPR; }
[ \t\n]     ;
[;].*       ; // comments
r{digit}+ {
	auto value = (syn::getRegister<int, 256>(iristext, reportIris16Error));
	irislval.rval = static_cast<byte>(value);
	return GPR;
}
p{digit}+ {
	auto value = (syn::getRegister<int, 16>(iristext, reportIris16Error));
	irislval.rval = static_cast<byte>(value);
	return PREDICATE_REGISTER;
}
{digit}+ {
	irislval.ival = syn::getDecimalImmediate<iris::word>(iristext, reportIris16Error);
	return IMMEDIATE;
}
0x{xdigit}+ {
	irislval.ival = syn::getHexImmediate<iris::word>(iristext, reportIris16Error);
	return IMMEDIATE;
}
0b[01]+ {
	irislval.ival = syn::getBinaryImmediate<iris::word>(iristext, reportIris16Error);
	return IMMEDIATE;
}
{omni}+ {
	irislval.sval = iristext;
	return IRIS16_SYMBOL;
}
%%
