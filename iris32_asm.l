%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <cstdint>
#include "iris32.h"
#include "iris32_asm.tab.h"
#define _POSIX_SOURCE 1

static byte getregister(void);
static word getdecimalimmediate(void);
static word getheximmediate(void);
static word getbinaryimmediate(void);
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
"setl" { return MOVE_OP_SET_LOWER; }
"setu" { return MOVE_OP_SET_UPPER; }
"ld" { return MOVE_OP_LOAD; }
"st" { return MOVE_OP_STORE; }
"j" 		{ return JUMP_OP_UNCONDITIONALREGISTER; }
"jl" 		{ return JUMP_OP_UNCONDITIONALREGISTERLINK; }
"jt" 		{ return JUMP_OP_CONDITIONALTRUEREGISTER; }
"jtl" 		{ return JUMP_OP_CONDITIONALTRUEREGISTERLINK; }
"jf" 		{ return JUMP_OP_CONDITIONALFALSEREGISTER; }
"jfl" 		{ return JUMP_OP_CONDITIONALFALSEREGISTERLINK; }
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
"pop" { return MOVE_OP_POP; }

"incr" { return ARITHMETIC_MACRO_OP_INCR; }
"decr" { return ARITHMETIC_MACRO_OP_DECR; }
"halve" { return ARITHMETIC_MACRO_OP_HALVE; }
"double" { return ARITHMETIC_MACRO_OP_DOUBLE; }

"@label" { return LABEL; }
"@data"  { return DIRECTIVE_DATA; }
"@code"  { return DIRECTIVE_CODE; }
"@org"   { return DIRECTIVE_ORG; }
"@word" { return DIRECTIVE_DECLARE; }
"tid"	 { yylval.rval = 251; return REGISTER; }
"cp"	 { yylval.rval = 252; return REGISTER; }
"sp"	 { yylval.rval = 253; return REGISTER; }
"lr"	 { yylval.rval = 254; return REGISTER; }
"ip"	 { yylval.rval = 255; return REGISTER; }
[ \t\n]     ;
[;].*       ; // comments
r{digit}+ { yylval.rval = getregister(); return REGISTER; }
{digit}+ { yylval.ival = getdecimalimmediate(); return IMMEDIATE; }
0x{xdigit}+ { yylval.ival = getheximmediate(); return IMMEDIATE; }
0b[01]+ { yylval.ival = getbinaryimmediate(); return IMMEDIATE; }

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
word getbinaryimmediate() {
	uint32_t tmp;
	tmp = strtoul(yytext, NULL, 2);
	if(tmp == ULONG_MAX && errno == ERANGE) {
		yyerror("number is too large and overflowed!");
		return 0;
	} else {
		return word(tmp);
	}
}
word getdecimalimmediate() {
	uint32_t tmp;
	tmp = strtoul(yytext, NULL, 10);
	if(tmp == ULONG_MAX && errno == ERANGE) {
		yyerror("number is too large and overflowed!");
		return 0;
	} else {
		return word(tmp);
	}
}

word getheximmediate() {
	uint32_t tmp;
	tmp = strtoul(yytext, NULL, 16);
	if(tmp == ULONG_MAX && errno == ERANGE) {
		yyerror("number is too large and overflowed!");
		return 0;
	} else {
		return word(tmp);
	}
}

