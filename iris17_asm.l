%option prefix="iris17"
%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <cstdint>
#include "iris17.h"
#include "iris17_asm.tab.h"
#define _POSIX_SOURCE 1

namespace iris17 {
	static byte getregister(void);
	static unsigned long getdecimalimmediate(void);
	static unsigned long getheximmediate(void);
	static unsigned long getbinaryimmediate(void);
}
extern void iris17error(const char* c);
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

"none"      { return ACTION_NONE; }
"and"       { return ACTION_AND; }
"or"        { return ACTION_OR; }
"xor"       { return ACTION_XOR; }

"not"       { return LOGICAL_OP_NOT; }
"nand"      { return LOGICAL_OP_NAND; }

"ld"        { return MEMORY_OP_LOAD; }
"merge"     { return MEMORY_OP_MERGE; }
"st"        { return MEMORY_OP_STORE; }
"push"      { return MEMORY_OP_PUSH; }
"pop"       { return MEMORY_OP_POP; }

"call"      { return BRANCH_FLAG_CALL; }
"if"        { return BRANCH_FLAG_IF; }
"cond"      { return BRANCH_FLAG_COND; }

"@label"    { return LABEL; }
"@org"      { return DIRECTIVE_ORG; }
"@word"     { return DIRECTIVE_WORD; }
"@dword"    { return DIRECTIVE_DWORD; }
"@startup"  { return DIRECTIVE_REGISTER_AT_START; }

"ip"        { iris17lval.rval = iris17::ArchitectureConstants::InstructionPointer; return REGISTER; }
"lr"        { iris17lval.rval = iris17::ArchitectureConstants::LinkRegister; return REGISTER; }
"sp"        { iris17lval.rval = iris17::ArchitectureConstants::StackPointer; return REGISTER; }
"cr"        { iris17lval.rval = iris17::ArchitectureConstants::ConditionRegister; return REGISTER; }
"addr"      { iris17lval.rval = iris17::ArchitectureConstants::AddressRegister; return REGISTER; }
"value"     { iris17lval.rval = iris17::ArchitectureConstants::ValueRegister; return REGISTER; }

[ \t\n]     ;
[;].*       ; // comments
r{digit}+   { iris17lval.rval = iris17::getregister(); return REGISTER; }
{digit}+    { iris17lval.ival = iris17::getdecimalimmediate(); return IMMEDIATE; }
0x{xdigit}+ { iris17lval.ival = iris17::getheximmediate(); return IMMEDIATE; }
0b[01]+     { iris17lval.ival = iris17::getbinaryimmediate(); return IMMEDIATE; }
0m[01][01][01][01] { iris17lval.ival = iris17::getbinaryimmediate(); return BITMASK4; }
{omni}+     { 
	std::string contents(iris17text);
	iris17lval.sval = (char*)contents.c_str();
	return SYMBOL;
}
%%
namespace iris17 {
byte getregister(void) {
   int c;

   iris17text[0] = '0';
   c = atoi(iris17text);
   if(c > ArchitectureConstants::RegisterCount || c < 0) {
      iris17error("target register is too large");
      return 0;
   } else {
      return (byte)c;
   }
}

unsigned long getdecimalimmediate() {
   unsigned long tmp;
   tmp = strtoul(iris17text, NULL, 10);
   if(tmp == ULONG_MAX && errno == ERANGE) {
      iris17error("number is too large and overflowed!");
      return 0;
   } else {
      if(tmp > 0xFFFFFFFF || tmp < 0) {
         iris17error("provided number is larger than 32-bits!");
         return 0;
      } else {
         return tmp;
      }
   }
}

unsigned long getheximmediate() {
   unsigned long tmp;
   tmp = strtoul(iris17text, NULL, 16);
   if(tmp == ULONG_MAX && errno == ERANGE) {
      iris17error("number is too large and overflowed!");
      return 0;
   } else {
      if(tmp > 0xFFFFFFFF || tmp < 0) {
         iris17error("provided number is larger than 32-bits!");
         return 0;
      } else {
         return tmp;
      }
   }
}

unsigned long getbinaryimmediate() {
   unsigned long tmp;
   tmp = strtoul(iris17text, NULL, 2);
   if(tmp == ULONG_MAX && errno == ERANGE) {
      iris17error("number is too large and overflowed!");
      return 0;
   } else {
      if(tmp > 0xFFFFFFFF || tmp < 0) {
         iris17error("provided number is larger than 32-bits!");
         return 0;
      } else {
         return tmp;
      }
   }
}

}
