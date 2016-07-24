%option prefix="IRIS17"
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

static byte getregister(void);
static unsigned long getdecimalimmediate(void);
static unsigned long getheximmediate(void);
extern void yyerror(const char* c);
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
"outptr" { yylval.rval = 227; return REGISTER; }
"inptr"  { yylval.rval = 228; return REGISTER; }
"fdcur"  { yylval.rval = 229; return REGISTER; }
"zero"   { yylval.rval = 230; return REGISTER; }
"stmp7"  { yylval.rval = 231; return REGISTER; }
"wlen"   { yylval.rval = 232; return REGISTER; }
"stmp6"  { yylval.rval = 233; return REGISTER; }
"stmp5"  { yylval.rval = 234; return REGISTER; }
"stmp4"  { yylval.rval = 235; return REGISTER; }
"space"  { yylval.rval = 236; return REGISTER; }
"stmp3"  { yylval.rval = 237; return REGISTER; }
"stmp2"  { yylval.rval = 238; return REGISTER; }
"stmp1"  { yylval.rval = 239; return REGISTER; }
"stmp0"  { yylval.rval = 240; return REGISTER; }
"sres3"  { yylval.rval = 241; return REGISTER; }
"sres2"  { yylval.rval = 242; return REGISTER; }
"sres1"  { yylval.rval = 243; return REGISTER; }
"sres0"  { yylval.rval = 244; return REGISTER; }
"spar3"  { yylval.rval = 245; return REGISTER; }
"spar2"  { yylval.rval = 246; return REGISTER; }
"spar1"  { yylval.rval = 247; return REGISTER; }
"spar0"  { yylval.rval = 248; return REGISTER; }
"rend"	 { yylval.rval = 249; return REGISTER; }
"rtop"	 { yylval.rval = 250; return REGISTER; }
"pend"	 { yylval.rval = 251; return REGISTER; }
"ptop"	 { yylval.rval = 252; return REGISTER; }
"sp"	 { yylval.rval = 253; return REGISTER; }
"lr"	 { yylval.rval = 254; return REGISTER; }
"ip"	 { yylval.rval = 255; return REGISTER; }
[ \t\n]     ;
[;].*       ; // comments
r{digit}+ { yylval.rval = getregister(); return REGISTER; }
{digit}+ { yylval.ival = getdecimalimmediate(); return IMMEDIATE; }
0x{xdigit}+ { yylval.ival = getheximmediate(); return IMMEDIATE; }
{omni}+ { 
	std::string contents(yytext);
	yylval.sval = (char*)contents.c_str();
	return SYMBOL;
}
%%

byte getregister(void) {
   int c;

   yytext[0] = '0';
   c = atoi(yytext);
   if(c > 255 || c < 0) {
      yyerror("target register is too large");
      return 0;
   } else {
      return (byte)c;
   }
}

unsigned long getdecimalimmediate() {
   unsigned long tmp;
   tmp = strtoul(yytext, NULL, 10);
   if(tmp == ULONG_MAX && errno == ERANGE) {
      yyerror("number is too large and overflowed!");
      return 0;
   } else {
      if(tmp > 65535 || tmp < 0) {
         yyerror("provided number is larger than 16-bits!");
         return 0;
      } else {
         return tmp;
      }
   }
}

unsigned long getheximmediate() {
   unsigned long tmp;
   tmp = strtoul(yytext, NULL, 16);
   if(tmp == ULONG_MAX && errno == ERANGE) {
      yyerror("number is too large and overflowed!");
      return 0;
   } else {
      if(tmp > 65535 || tmp < 0) {
         yyerror("provided number is larger than 16-bits!");
         return 0;
      } else {
         return tmp;
      }
   }
}

