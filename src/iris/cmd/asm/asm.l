%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "iris.h"
%}

digit [0-9]
xdigit [a-fA-F0-9]
bdigit [0-1]
letter [a-zA-Z]
omni [a-zA-Z0-9]


%%
[ \t\n]     ;
r{digit}+     { printf("Found register %s\n", yytext); }
${digit}+     { printf("Found decimal number %s\n", yytext); }
${xdigit}+ { printf("Found hex number %s\n", yytext); }
{letter}({omni}*): { printf("Found a label %s\n", yytext); }
@{letter}({omni}*) { printf("Found a label replace %s\n", yytext); }
.data      { printf("Found a data directive\n"); }
.code      { printf("Found a code directive\n"); }
.org       { printf("Found an org directive\n"); }
{omni}+ { printf("Found a symbol\n"); }
%%

int main() {

   yylex();
}
