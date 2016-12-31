%option prefix="iris19"
%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <cstdint>
#include "iris19.h"
#include "iris19_asm.tab.h"
#include "iris_asm_base.h"
#define _POSIX_SOURCE 1

namespace iris19 {
	unsigned long getbitmask();
}
extern void iris19error(const char* c);
void reportIris19Error(const std::string& str) {
	iris19error(str.c_str());
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

"=="        { return COMPARE_OP_EQ; }
"!="        { return COMPARE_OP_NEQ; }
"<"         { return COMPARE_OP_LT; }
"<="        { return COMPARE_OP_LT_EQ; }
">"         { return COMPARE_OP_GT; }
">="        { return COMPARE_OP_GT_EQ; }

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

"stack"     { return TAG_STACK; }

"ip"        { iris19lval.rval = iris19::ArchitectureConstants::InstructionPointer; return REGISTER; }
"sp"        { iris19lval.rval = iris19::ArchitectureConstants::StackPointer; return REGISTER; }

[ \t\n,]     ;
[;].*       ; // comments
r{digit}+   { 
	iris19lval.rval = iris::getRegister<byte, iris19::ArchitectureConstants::RegisterCount>(iris19text, reportIris19Error);
	return REGISTER;
}
{digit}+ { 
	iris19lval.ival = iris::getDecimalImmediate<iris19::Word>(iris19text, reportIris19Error);
	return IMMEDIATE; 
}
0x{xdigit}+ { 
	iris19lval.ival = iris::getHexImmediate<iris19::Word>(iris19text, reportIris19Error);
	return IMMEDIATE;
}
0b[01]+ {
	iris19lval.ival = iris::getBinaryImmediate<iris19::Word>(iris19text, reportIris19Error);
	return IMMEDIATE;
}
0m[01][01][01][01][01][01][01][01] { 
	iris19lval.ival = iris19::getbitmask(); 
	return BITMASK; 
}
[?]{omni}+    { iris19lval.sval = iris19text; return ALIAS; }
{omni}+     {
    iris19lval.sval = iris19text;
	return Symbol;
}
"<-" { return ASSIGN; }
%%
namespace iris19 {

unsigned long getbitmask() {
	unsigned long tmp = 0;
	std::string mask(iris19text);
	// replace the code with the binary symbol
	mask.at(1) = '0';
	tmp = strtoul(mask.c_str(), NULL, 2);
	if(tmp == ULONG_MAX && errno == ERANGE) {
		iris19error("number is too large and overflowed!");
		return 0;
	} else {
		if(tmp > 0xFF || tmp < 0) {
			iris19error("provided number is larger than 8-bits!");
			return 0;
		} else {
			return tmp;
		}
	}
}

}
