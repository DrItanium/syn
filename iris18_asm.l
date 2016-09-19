%option prefix="iris18"
%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <cstdint>
#include "iris18.h"
#include "iris18_asm.tab.h"
#define _POSIX_SOURCE 1

namespace iris18 {
	byte getregister();
	unsigned long getdecimalimmediate();
	unsigned long getheximmediate();
	unsigned long getbinaryimmediate();
	unsigned long getbitmask4();
}
extern void iris18error(const char* c);
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

"ip"        { iris18lval.rval = iris18::ArchitectureConstants::InstructionPointer; return REGISTER; }
"lr"        { iris18lval.rval = iris18::ArchitectureConstants::LinkRegister; return REGISTER; }
"sp"        { iris18lval.rval = iris18::ArchitectureConstants::StackPointer; return REGISTER; }
"cr"        { iris18lval.rval = iris18::ArchitectureConstants::ConditionRegister; return REGISTER; }
"addr"      { iris18lval.rval = iris18::ArchitectureConstants::AddressRegister; return REGISTER; }
"value"     { iris18lval.rval = iris18::ArchitectureConstants::ValueRegister; return REGISTER; }
"mask"      { iris18lval.rval = iris18::ArchitectureConstants::MaskRegister; return REGISTER; }
"shift_width"     { iris18lval.rval = iris18::ArchitectureConstants::ShiftRegister; return REGISTER; }
"field"     { iris18lval.rval = iris18::ArchitectureConstants::FieldRegister; return REGISTER; }
"count"     { iris18lval.rval = iris18::ArchitectureConstants::CountRegister; return REGISTER; }


[ \t\n]     ;
[;].*       ; // comments
r{digit}+   { iris18lval.rval = iris18::getregister(); return REGISTER; }
{digit}+    { iris18lval.ival = iris18::getdecimalimmediate(); return IMMEDIATE; }
0x{xdigit}+ { iris18lval.ival = iris18::getheximmediate(); return IMMEDIATE; }
0b[01]+     { iris18lval.ival = iris18::getbinaryimmediate(); return IMMEDIATE; }
0m[01][01][01][01] { iris18lval.ival = iris18::getbitmask4(); return BITMASK4; }
[?]{omni}+    { iris18lval.sval = iris18text; return ALIAS; }
{omni}+     {
    iris18lval.sval = iris18text;
	return SYMBOL;
}
%%
namespace iris18 {
byte getregister(void) {
   int c;

   iris18text[0] = '0';
   c = atoi(iris18text);
   if(c > ArchitectureConstants::RegisterCount || c < 0) {
      iris18error("target register is too large");
      return 0;
   } else {
      return (byte)c;
   }
}

unsigned long getdecimalimmediate() {
   unsigned long tmp;
   std::string str(iris18text);
   tmp = strtoul(str.c_str(), NULL, 10);
   if(tmp == ULONG_MAX && errno == ERANGE) {
      iris18error("number is too large and overflowed!");
      return 0;
   } else {
      if(tmp > 0xFFFFFFFF || tmp < 0) {
         iris18error("provided number is larger than 32-bits!");
         return 0;
      } else {
         return tmp;
      }
   }
}

unsigned long getheximmediate() {
   unsigned long tmp;
   std::string str(iris18text);
   str[1] = '0';
   tmp = strtoul(str.c_str(), NULL, 16);
   if(tmp == ULONG_MAX && errno == ERANGE) {
      iris18error("number is too large and overflowed!");
      return 0;
   } else {
      if(tmp > 0xFFFFFFFF || tmp < 0) {
         iris18error("provided number is larger than 32-bits!");
         return 0;
      } else {
         return tmp;
      }
   }
}

unsigned long getbinaryimmediate() {
   unsigned long tmp;
   std::string str(iris18text);
   str[1] = '0';
   tmp = strtoul(str.c_str(), NULL, 2);
   if(tmp == ULONG_MAX && errno == ERANGE) {
      iris18error("number is too large and overflowed!");
      return 0;
   } else {
      if(tmp > 0xFFFFFFFF || tmp < 0) {
         iris18error("provided number is larger than 32-bits!");
         return 0;
      } else {
         return tmp;
      }
   }
}

unsigned long getbitmask4() {
	unsigned long tmp = 0;
	std::string mask(iris18text);
	// replace the code with the binary symbol
	mask.at(1) = '0';
	tmp = strtoul(mask.c_str(), NULL, 2);
	if(tmp == ULONG_MAX && errno == ERANGE) {
		iris18error("number is too large and overflowed!");
		return 0;
	} else {
		if(tmp > 0xFFFFFFFF || tmp < 0) {
			iris18error("provided number is larger than 32-bits!");
			return 0;
		} else {
			return tmp;
		}
	}
}

}
