%option prefix="cisc0"
%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <cstdint>
#include "cisc0.h"
#include "cisc0_asm.tab.h"
#include "iris_asm_base.h"
#define _POSIX_SOURCE 1

extern void cisc0error(const char* c);
void reportIris18Error(const std::string& msg) {
	cisc0error(msg.c_str());
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
"nop"        { return OP_NOP; }
"arithmetic" { return OP_ARITHMETIC; }
"shift"      { return OP_SHIFT; }
"logical"    { return OP_LOGICAL; }
"compare"    { return OP_COMPARE; }
"branch"     { return OP_BRANCH; }
"return"     { return OP_RETURN; }
"system"     { return OP_SYSTEM; }
"move"       { return OP_MOVE; }
"set"        { return OP_SET; }
"swap"       { return OP_SWAP; }
"memory"     { return OP_MEMORY; }
"complex"    { return OP_COMPLEX; }

"increment"  { return MACRO_OP_INCREMENT; }
"decrement"  { return MACRO_OP_DECREMENT; }
"double"     { return MACRO_OP_DOUBLE; }
"halve"      { return MACRO_OP_HALVE; }
"zero"       { return MACRO_OP_ZERO; }
"copy"       { return MACRO_OP_COPY; }

"immediate" { return FLAG_IMMEDIATE; }
"add"       { return ARITHMETIC_OP_ADD; }
"sub"       { return ARITHMETIC_OP_SUB; }
"mul"       { return ARITHMETIC_OP_MUL; }
"div"       { return ARITHMETIC_OP_DIV; }
"rem"       { return ARITHMETIC_OP_REM; }
"left"      { return SHIFT_FLAG_LEFT; }
"right"     { return SHIFT_FLAG_RIGHT; }

"encoding"  { return COMPLEX_OP_ENCODING; }
"bitset"    { return COMPLEX_OP_ENCODING_BITSET; }
"bitunset"  { return COMPLEX_OP_ENCODING_BITUNSET; }
"encode"    { return COMPLEX_OP_ENCODING_ENCODE; }
"decode"    { return COMPLEX_OP_ENCODING_DECODE; }

"=="        { return COMPARE_OP_EQ; }
"!="        { return COMPARE_OP_NEQ; }
"<"         { return COMPARE_OP_LT; }
"<="        { return COMPARE_OP_LT_EQ; }
">"         { return COMPARE_OP_GT; }
">="        { return COMPARE_OP_GT_EQ; }

"none"      { return ACTION_NONE; }
"and"       { return ACTION_AND; }
"or"        { return ACTION_OR; }
"xor"       { return ACTION_XOR; }

"not"       { return LOGICAL_OP_NOT; }
"nand"      { return LOGICAL_OP_NAND; }

"load"      { return MEMORY_OP_LOAD; }
"store"     { return MEMORY_OP_STORE; }
"push"      { return MEMORY_OP_PUSH; }
"pop"       { return MEMORY_OP_POP; }

"call"      { return BRANCH_FLAG_CALL; }
"if"        { return BRANCH_FLAG_IF; }
"cond"      { return BRANCH_FLAG_COND; }
"indirect"  { return TAG_INDIRECT; }

"@label"    { return LABEL; }
"@org"      { return DIRECTIVE_ORG; }
"@word"     { return DIRECTIVE_WORD; }
"@dword"    { return DIRECTIVE_DWORD; }
"@startup"  { return DIRECTIVE_REGISTER_AT_START; }
"@constant" { return DIRECTIVE_CONSTANT; }

"ip"           { cisc0lval.rval = cisc0::ArchitectureConstants::InstructionPointer; return REGISTER; }
"sp"           { cisc0lval.rval = cisc0::ArchitectureConstants::StackPointer; return REGISTER; }
"addr"         { cisc0lval.rval = cisc0::ArchitectureConstants::AddressRegister; return REGISTER; }
"value"        { cisc0lval.rval = cisc0::ArchitectureConstants::ValueRegister; return REGISTER; }
"mask"         { cisc0lval.rval = cisc0::ArchitectureConstants::MaskRegister; return REGISTER; }
"shift_width"  { cisc0lval.rval = cisc0::ArchitectureConstants::ShiftRegister; return REGISTER; }
"field"        { cisc0lval.rval = cisc0::ArchitectureConstants::FieldRegister; return REGISTER; }
"cr"           { cisc0lval.rval = cisc0::ArchitectureConstants::ConditionRegister; return REGISTER; }


[ \t\n,]     ;
[;].*       ; // comments
r{digit}+   { 
	cisc0lval.rval = stdiris::getRegister<byte, cisc0::ArchitectureConstants::RegisterCount>(cisc0text, reportIris18Error);
	return REGISTER;
}
{digit}+ { 
	cisc0lval.ival = stdiris::getDecimalImmediate<cisc0::Word>(cisc0text, reportIris18Error);
	return IMMEDIATE; 
}
0x{xdigit}+ { 
	cisc0lval.ival = stdiris::getHexImmediate<cisc0::Word>(cisc0text, reportIris18Error);
	return IMMEDIATE;
}
0b[01]+ {
	cisc0lval.ival = stdiris::getBinaryImmediate<cisc0::Word>(cisc0text, reportIris18Error);
	return IMMEDIATE;
}
0m[01][01][01][01] {
	cisc0lval.ival = stdiris::decodeBits<cisc0::Word, byte, 0x0000000F, 0>(stdiris::getBinaryImmediate<cisc0::Word>(cisc0text, reportIris18Error));
	return BITMASK4;
}
[?]{omni}+    { cisc0lval.sval = cisc0text; return ALIAS; }
{omni}+     {
    cisc0lval.sval = cisc0text;
	return IRIS18_SYMBOL;
}
%%
