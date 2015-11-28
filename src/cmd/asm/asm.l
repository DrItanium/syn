%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include "iris.h"
#include "asm.tab.h"
#define _POSIX_SOURCE 1

static byte getregister(void);
static byte getinputregister(void);
static byte getoutputregister(void);
static byte getlocalregister(void);
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
"mov"  { return MOVE_OP_MOVE; }
"swap" { return MOVE_OP_SWAP; }
"swap.reg.addr" { return MOVE_OP_SWAPREGADDR; }
"swap.addr.addr" { return MOVE_OP_SWAPADDRADDR; }
"swap.reg.mem" { return MOVE_OP_SWAPREGMEM; }
"swap.addr.mem" { return MOVE_OP_SWAPADDRMEM; }
"set" { return MOVE_OP_SET; }
"load" { return MOVE_OP_LOAD; }
"load.mem" { return MOVE_OP_LOADMEM; }
"store" { return MOVE_OP_STORE; }
"store.addr" { return MOVE_OP_STOREADDR; }
"memcopy" { return MOVE_OP_STOREMEM; }
"memset" { return MOVE_OP_STOREIMM; }
"goto" { return JUMP_OP_UNCONDITIONALIMMEDIATE; }
"goto.link" { return JUMP_OP_UNCONDITIONALIMMEDIATELINK; }
"jump" { return JUMP_OP_UNCONDITIONALREGISTER; }
"jump.link" { return JUMP_OP_UNCONDITIONALREGISTERLINK; }
"goto.if1" { return JUMP_OP_CONDITIONALTRUEIMMEDIATE; }
"goto.if1.link" { return JUMP_OP_CONDITIONALTRUEIMMEDIATELINK; }
"jump.if1" { return JUMP_OP_CONDITIONALTRUEREGISTER; }
"jump.if1.link" { return JUMP_OP_CONDITIONALTRUEREGISTERLINK; }
"goto.if0" { return JUMP_OP_CONDITIONALFALSEIMMEDIATE; }
"goto.if0.link" { return JUMP_OP_CONDITIONALFALSEIMMEDIATELINK; }
"jump.if0" { return JUMP_OP_CONDITIONALFALSEREGISTER; }
"jump.if0.link" { return JUMP_OP_CONDITIONALFALSEREGISTERLINK; }
"if1" { return JUMP_OP_IFTHENELSENORMALPREDTRUE; }
"if0" { return JUMP_OP_IFTHENELSENORMALPREDFALSE; }
"if1.link" { return JUMP_OP_IFTHENELSELINKPREDTRUE; }
"if0.link" { return JUMP_OP_IFTHENELSELINKPREDFALSE; }
"eq" { return COMPARE_OP_EQ; }
"and.eq" { return COMPARE_OP_EQAND; }
"or.eq" { return COMPARE_OP_EQOR; }
"xor.eq" { return COMPARE_OP_EQXOR; }
"ne" { return COMPARE_OP_NEQ; }
"and.ne" { return COMPARE_OP_NEQAND; }
"or.ne" { return COMPARE_OP_NEQOR; }
"xor.ne" { return COMPARE_OP_NEQXOR; }
"lt" { return COMPARE_OP_LESSTHAN; }
"and.lt" { return COMPARE_OP_LESSTHANAND; }
"or.lt" { return COMPARE_OP_LESSTHANOR; }
"xor.lt" { return COMPARE_OP_LESSTHANXOR; }
"gt" { return COMPARE_OP_GREATERTHAN; }
"and.gt" { return COMPARE_OP_GREATERTHANAND; }
"or.gt" { return COMPARE_OP_GREATERTHANOR; }
"xor.gt" { return COMPARE_OP_GREATERTHANXOR; }
"le" { return COMPARE_OP_LESSTHANOREQUALTO; }
"and.le" { return COMPARE_OP_LESSTHANOREQUALTOAND; }
"or.le" { return COMPARE_OP_LESSTHANOREQUALTOOR; }
"xor.le" { return COMPARE_OP_LESSTHANOREQUALTOXOR; }
"ge" { return COMPARE_OP_GREATERTHANOREQUALTO; }
"and.ge" { return COMPARE_OP_GREATERTHANOREQUALTOAND; }
"or.ge" { return COMPARE_OP_GREATERTHANOREQUALTOOR; }
"xor.ge" { return COMPARE_OP_GREATERTHANOREQUALTOXOR; }
"system" { return MISC_OP_SYSTEMCALL; }
"push" { return MOVE_OP_PUSH; }
"push.imm" { return MOVE_OP_PUSHIMMEDIATE; }
"pop" { return MOVE_OP_POP; }
"@label" { return LABEL; }
"@data"  { return DIRECTIVE_DATA; }
"@code"  { return DIRECTIVE_CODE; }
"@org"   { return DIRECTIVE_ORG; }
"@declare" { return DIRECTIVE_DECLARE; }
[ \t\n]     ;
[;].*       ; // comments
r{digit}+ { yylval.rval = getregister(); return REGISTER; }
; // microcode extensions
"?zero"   { yylval.rval = 225; return REGISTER; } ; // literal zero
"?one" 	  { yylval.rval = 226; return REGISTER; } ; // literal one
"?two"	  { yylval.rval = 229; return REGISTER; } ; // literal two
"?three"  { yylval.rval = 230; return REGISTER; } ; // literal three
"?min"    { yylval.rval = 227; return REGISTER; } ; // smallest number
"?max"    { yylval.rval = 228; return REGISTER; } ; // largest number
"?pred"   { yylval.rval = 255; return REGISTER; } ; // predicate register
"?sp"	  { yylval.rval = 254; return REGISTER; } ; // stack pointer
"?opvec"  { yylval.rval = 0; return REGISTER; }   ; // opcode vector
"?ps0"    { yylval.rval = 1; return REGISTER; }   ; // parameter stack position 0 / top
"?ps1"    { yylval.rval = 2; return REGISTER; }   ; // parameter stack position 1 / second
"?ps2"	  { yylval.rval = 3; return REGISTER; }   ; // parameter stack position 2 / third
"?psaddr" { yylval.rval = 4; return REGISTER; }   ; // parameter stack memory "top"
"?pslen"  { yylval.rval = 5; return REGISTER; }   ; // parameter stack count
"?ip"	  { yylval.rval = 16; return REGISTER; }  ; // instruction pointer
"?link"   { yylval.rval = 253; return REGISTER; }
"?iaddr"  { yylval.rval = 252; return REGISTER; }
"?temp0"  { yylval.rval = 251; return REGISTER; }
"?ret"	  { yylval.rval = 32; return REGISTER; }
"?memstart" { yylval.rval = 231; return REGISTER; } ; // start of exposed memory
"?memend"   { yylval.rval = 232; return REGISTER; } ; // end of exposed memory
"?vecret" { yylval.rval = 127; return REGISTER; } ; // The register containing the return address from an internal opcode vector call
return{digit}+ { yylval.rval = getoutputregister(); return REGISTER; }
output{digit}+ { yylval.rval = getoutputregister(); return REGISTER; }
input{digit}+ { yylval.rval = getinputregister(); return REGISTER; }
arg{digit}+ { yylval.rval = getinputregister(); return REGISTER; }
local{digit}+ { yylval.rval = getlocalregister(); return REGISTER; }
l{digit}+	{ yylval.rval = getlocalregister(); return REGISTER; }

{digit}+ { yylval.ival = getdecimalimmediate(); return IMMEDIATE; }
0x{xdigit}+ { yylval.ival = getheximmediate(); return IMMEDIATE; }
{omni}+ { 
   int j;
   char* contents;
   j = strlen(yytext);
   contents = calloc(j + 1, sizeof(char));
   /* strdup isn't c-standard only POSIX...... */
   if(strncpy(contents, yytext, j) == contents) {
      yylval.sval = contents;
      return SYMBOL;
   } else {
      yyerror("couldn't copy yytext!");
   }
}
%%

enum {
	RegisterMax = RegisterCount - 1,
	NumOutputRegisters = 32,
	OutputRegistersStart = 32,
	OutputRegistersMax = NumOutputRegisters - 1,
	NumInputRegisters = 64,
	InputRegistersStart = 64,
	InputRegistersMax = NumInputRegisters - 1,
	NumLocalRegisters = 64,
	LocalRegistersStart = 128,
	LocalRegistersMax = NumLocalRegisters - 1,
};

byte getregister(void) {
   int c;

   yytext[0] = '0';
   c = atoi(yytext);
   if(c > RegisterMax || c < 0) {
      yyerror("target register is too large");
      return 0;
   } else {
      return (byte)c;
   }
}

byte getoutputregister(void) {
   int c;

   yytext[0] = '0';
   c = atoi(yytext);
   if(c > OutputRegistersMax || c < 0) {
      yyerror("target output register is too large");
      return 0;
   } else {
      return (byte)(c + OutputRegistersStart);
   }
}

byte getinputregister(void) {
   int c;

   yytext[0] = '0';
   c = atoi(yytext);
   if(c > InputRegistersMax || c < 0) {
      yyerror("target input register is too large");
      return 0;
   } else {
      return (byte)(c + InputRegistersStart);
   }
}

byte getlocalregister(void) {
   int c;

   yytext[0] = '0';
   c = atoi(yytext);
   if(c > LocalRegistersMax || c < 0) {
      yyerror("target local register is too large");
      return 0;
   } else {
      return (byte)(c + LocalRegistersStart);
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

