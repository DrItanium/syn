%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include "iris.h"
#include "asm.tab.h"

static byte getregister(void);
static ulong getdecimalimmediate(void);
static ulong getheximmediate(void);
extern void yyerror(const char* c);
%}
%option yylineno
%option noyywrap
%option nounput
digit [0-9]
xdigit [a-fA-F0-9]
bdigit [0-1]
letter [a-zA-Z]
omni [a-zA-Z0-9.]


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
"move" { return MOVE_OP_MOVE; }
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
"implicit.register.set" { return MISC_OP_SETIMPLICITREGISTERIMMEDIATE; }
"implicit.register.indirect.set" { return MISC_OP_SETIMPLICITREGISTERINDIRECT; }
"implicit.register.get" { return MISC_OP_GETIMPLICITREGISTERIMMEDIATE; }
"implicit.register.indirect.get" { return MISC_OP_GETIMPLICITREGISTERINDIRECT; }
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
{digit}+ { yylval.ival = getdecimalimmediate(); return IMMEDIATE; }
0x{xdigit}+ { yylval.ival = getheximmediate(); return IMMEDIATE; }
{omni}+ { 
   int j;
   char* contents;
   j = strlen(yytext);
   contents = calloc(j + 1, sizeof(char));
   /* strdup isn't c-standard only POSIX...... */
   if(strcpy(contents, yytext, j) == contents) {
      yylval.sval = contents;
      return SYMBOL;
   } else {
      yyerror("couldn't copy yytext!");
   }
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

ulong getdecimalimmediate() {
   ulong tmp;
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

ulong getheximmediate() {
   ulong tmp;
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

