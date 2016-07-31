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

"incr" { return ARITHMETIC_MACRO_OP_INCR; }
"decr" { return ARITHMETIC_MACRO_OP_DECR; }
"halve" { return ARITHMETIC_MACRO_OP_HALVE; }
"double" { return ARITHMETIC_MACRO_OP_DOUBLE; }

"move" { return MOVE_OP_MOVE; }
"swap" { return MOVE_OP_SWAP; }
"set" { return MOVE_OP_SET; }
"ld" { return MOVE_OP_LOAD; }
"st" { return MOVE_OP_STORE; }

"b" 		{ return JUMP_OP_UNCONDITIONALIMMEDIATE; }
"call" 		{ return JUMP_OP_UNCONDITIONALIMMEDIATELINK; }
"br" 		{ return JUMP_OP_UNCONDITIONALREGISTER; }
"callr" 		{ return JUMP_OP_UNCONDITIONALREGISTERLINK; }
"bc" 		{ return JUMP_OP_CONDITIONALTRUEIMMEDIATE; }
"bcr" 		{ return JUMP_OP_CONDITIONALTRUEIMMEDIATE; }
"bif"		{ return JUMP_OP_IFTHENELSE; }
"bifl"		{ return JUMP_OP_IFTHENELSELINK; }

"eq" { return COMPARE_OP_EQ; }
"neq" { return COMPARE_OP_NEQ; }
"lt" { return COMPARE_OP_LESSTHAN; }
"gt" { return COMPARE_OP_GREATERTHAN; }
"le" { return COMPARE_OP_LESSTHANOREQUALTO; }
"ge" { return COMPARE_OP_GREATERTHANOREQUALTO; }

"eqi" { return COMPARE_OP_EQ_IMMEDIATE; }
"neqi" { return COMPARE_OP_NEQ_IMMEDIATE; }
"lti" { return COMPARE_OP_LESSTHAN_IMMEDIATE; }
"gti" { return COMPARE_OP_GREATERTHAN_IMMEDIATE; }
"lei" { return COMPARE_OP_LESSTHANOREQUALTO_IMMEDIATE; }
"gei" { return COMPARE_OP_GREATERTHANOREQUALTO_IMMEDIATE; }

"eqi" { return COMPARE_OP_EQ_IMMEDIATE; }
"neqi" { return COMPARE_OP_NEQ_IMMEDIATE; }
"lti" { return COMPARE_OP_LESSTHAN_IMMEDIATE; }
"gti" { return COMPARE_OP_GREATERTHAN_IMMEDIATE; }
"lei" { return COMPARE_OP_LESSTHANOREQUALTO_IMMEDIATE; }
"gei" { return COMPARE_OP_GREATERTHANOREQUALTO_IMMEDIATE; }

"system" { return MISC_OP_SYSTEMCALL; }
"push" { return MOVE_OP_PUSH; }
"pop" { return MOVE_OP_POP; }


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
