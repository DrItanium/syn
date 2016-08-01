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
"add"  { return OP_ADD; }
"sub"  { return OP_SUB; }
"mul"  { return OP_MUL; }
"div"  { return OP_DIV; }
"rem"  { return OP_REM; }
"shl"  { return OP_SHIFTLEFT; }
"shr"  { return OP_SHIFTRIGHT; }
"and"  { return OP_BINARYAND; }
"or"   { return OP_BINARYOR; }
"not"  { return OP_BINARYNOT; }
"xor"  { return OP_BINARYXOR; }
"addi" { return OP_ADD_IMM; }
"subi" { return OP_SUB_IMM; }
"muli" { return OP_MUL_IMM; }
"divi" { return OP_DIV_IMM; }
"remi" { return OP_REM_IMM; }
"shli" { return OP_SHIFTLEFT_IMM; }
"shri" { return OP_SHIFTRIGHT_IMM; }

"incr" { return OP_INCR; }
"decr" { return OP_DECR; }
"halve" { return OP_HALVE; }
"double" { return OP_DOUBLE; }
"square" { return OP_SQUARE; }
"cube"	 { return OP_CUBE; }

"move" { return OP_MOVE; }
"swap" { return OP_SWAP; }
"set" { return OP_SET; }
"ld" { return OP_LOAD; }
"ld.merge" { return OP_MERGE; }
"st" { return OP_STORE; }
"push" { return OP_PUSH; }
"pop" { return OP_POP; }

"branch" 		{ return OP_BRANCH; }
"call" 		{ return OP_CALL; }
"branch.indirect" 		{ return OP_BRANCH_INDIRECT; }
"call.indirect" 	{ return OP_CALL_INDIRECT; }
"branch.cond" 		{ return OP_BRANCH_CONDITIONAL; }
"branch.cond.indirect" 		{ return OP_BRANCH_CONDITIONAL_INDIRECT; }
"branch.if"		{ return OP_BRANCH_IF_THEN_ELSE; }
"call.if"		{ return OP_CALL_IF_THEN_ELSE; }
"return"	{ return OP_RETURN; }

"compare"   { return OP_COMPARE; }

"system" { return OP_SYSTEM; }


"@label" { return LABEL; }
"@data"  { return DIRECTIVE_DATA; }
"@code"  { return DIRECTIVE_CODE; }
"@org"   { return DIRECTIVE_ORG; }
"@declare" { return DIRECTIVE_DECLARE; }

"ip"	 	 { iris17lval.rval = iris17::ArchitectureConstants::InstructionPointer; return REGISTER; }
"lr"	 	 { iris17lval.rval = iris17::ArchitectureConstants::LinkRegister; return REGISTER; }
"sp"	 	 { iris17lval.rval = iris17::ArchitectureConstants::StackPointer; return REGISTER; }
"cond"	 	 { iris17lval.rval = iris17::ArchitectureConstants::ConditionRegister; return REGISTER; }
"addr"	 	 { iris17lval.rval = iris17::ArchitectureConstants::AddressRegister; return REGISTER; }
"value"	 	 { iris17lval.rval = iris17::ArchitectureConstants::ValueRegister; return REGISTER; }

[ \t\n]     ;
[;].*       ; // comments
r{digit}+ { iris17lval.rval = iris17::getregister(); return REGISTER; }
{digit}+ { iris17lval.ival = iris17::getdecimalimmediate(); return IMMEDIATE; }
0x{xdigit}+ { iris17lval.ival = iris17::getheximmediate(); return IMMEDIATE; }
0b[01]+ { iris17lval.ival = iris17::getbinaryimmediate(); return IMMEDIATE; }
{omni}+ { 
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
