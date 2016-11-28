%option prefix="iris20"
%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <cstdint>
#include "iris20.h"
#include "iris20_asm.tab.h"
#define _POSIX_SOURCE 1

namespace iris20 {
	static byte getregister(void);
	static unsigned long getdecimalimmediate(void);
	static unsigned long getheximmediate(void);
}
extern void iris20error(const char* c);
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
"move" { return MOVE_OP_MOVE; }
"swap" { return MOVE_OP_SWAP; }
"set" { return MOVE_OP_SET; }
"ld" { return MOVE_OP_LOAD; }
"ldi" { return MOVE_OP_LOADMEM; }
"ldm" { return MOVE_OP_LOADMEM; }
"ldc" { return MOVE_OP_LOAD_CODE; }
"st" { return MOVE_OP_STORE; }
"stc" { return MOVE_OP_STORE_CODE; }
"ldx" { return MOVE_OP_LOAD_EXTENDED; }
"stx" { return MOVE_OP_STORE_EXTENDED; }
"memset" { return MOVE_OP_STOREIMM; }
"j" 		{ return JUMP_OP_UNCONDITIONALIMMEDIATE; }
"jl" 		{ return JUMP_OP_UNCONDITIONALIMMEDIATELINK; }
"jr" 		{ return JUMP_OP_UNCONDITIONALREGISTER; }
"jrl" 		{ return JUMP_OP_UNCONDITIONALREGISTERLINK; }
"jt" 		{ return JUMP_OP_CONDITIONALTRUEIMMEDIATE; }
"jtl" 		{ return JUMP_OP_CONDITIONALTRUEIMMEDIATELINK; }
"jrt" 		{ return JUMP_OP_CONDITIONALTRUEREGISTER; }
"jrtl" 		{ return JUMP_OP_CONDITIONALTRUEREGISTERLINK; }
"jf" 		{ return JUMP_OP_CONDITIONALFALSEIMMEDIATE; }
"jfl" 		{ return JUMP_OP_CONDITIONALFALSEIMMEDIATELINK; }
"jrf" 		{ return JUMP_OP_CONDITIONALFALSEREGISTER; }
"jrfl" 		{ return JUMP_OP_CONDITIONALFALSEREGISTERLINK; }
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
"system" { return MISC_OP_SYSTEMCALL; }
"push" { return MOVE_OP_PUSH; }
"pushi" { return MOVE_OP_PUSHIMMEDIATE; }
"pop" { return MOVE_OP_POP; }

"incr" { return ARITHMETIC_MACRO_OP_INCR; }
"decr" { return ARITHMETIC_MACRO_OP_DECR; }
"halve" { return ARITHMETIC_MACRO_OP_HALVE; }
"double" { return ARITHMETIC_MACRO_OP_DOUBLE; }

"@label" { return LABEL; }
"@data"  { return DIRECTIVE_DATA; }
"@code"  { return DIRECTIVE_CODE; }
"@org"   { return DIRECTIVE_ORG; }
"@declare" { return DIRECTIVE_DECLARE; }
"sp"	 { iris20lval.rval = iris20::ArchitectureConstants::StackPointerIndex; return REGISTER; }
"lr"	 { iris20lval.rval = iris20::ArchitectureConstants::LinkRegisterIndex; return REGISTER; }
"ip"	 { iris20lval.rval = iris20::ArchitectureConstants::InstructionPointerIndex; return REGISTER; }
[ \t\n]     ;
[;].*       ; // comments
r{digit}+ { iris20lval.rval = iris20::getregister(); return REGISTER; }
{digit}+ { iris20lval.ival = iris20::getdecimalimmediate(); return IMMEDIATE; }
0x{xdigit}+ { iris20lval.ival = iris20::getheximmediate(); return IMMEDIATE; }
{omni}+ { 
	std::string contents(iris20text);
	iris20lval.sval = (char*)contents.c_str();
	return IRIS16_SYMBOL;
}
%%
namespace iris20 {
byte getregister(void) {
   int c;

   iris20text[0] = '0';
   c = atoi(iris20text);
   if(c > 255 || c < 0) {
      iris20error("target register is too large");
      return 0;
   } else {
      return (byte)c;
   }
}

unsigned long getdecimalimmediate() {
   unsigned long tmp;
   tmp = strtoul(iris20text, NULL, 10);
   if(tmp == ULONG_MAX && errno == ERANGE) {
      iris20error("number is too large and overflowed!");
      return 0;
   } else {
      if(tmp > 65535 || tmp < 0) {
         iris20error("provided number is larger than 16-bits!");
         return 0;
      } else {
         return tmp;
      }
   }
}

unsigned long getheximmediate() {
   unsigned long tmp;
   tmp = strtoul(iris20text, NULL, 16);
   if(tmp == ULONG_MAX && errno == ERANGE) {
      iris20error("number is too large and overflowed!");
      return 0;
   } else {
      if(tmp > 65535 || tmp < 0) {
         iris20error("provided number is larger than 16-bits!");
         return 0;
      } else {
         return tmp;
      }
   }
}

}
