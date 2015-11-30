%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <stdbool.h>
#include "iris.h"
#include "util.h"

#include "asm.tab.h"

extern int yylex();
extern int yyparse();
extern FILE* yyin;
extern int yylineno;

void yyerror(const char* s);
enum {
   FileWrapperInput = 0,
   FileWrapperOutput,

   /* always last */
   FileWrapperCount,
};

FileWrapper files[] = {
   /* input */
   { 0, 0, "r", 0 },
   /* output */
   { 0, 0, "w", 0 },
};

#define outfile (files[FileWrapperOutput])
#define infile (files[FileWrapperInput])

#define outfile_ptr (&outfile)
#define infile_ptr (&infile)
/* segment */
enum  {
   CodeSegment = 0,
   DataSegment,
};
typedef struct labelentry {
   char* value;
   byte loweraddr;
   byte upperaddr;
} labelentry;
/* used to store ops which require a second pass */
typedef struct dynamicop {
   int segment;
   word address;
   byte group;
   byte op;
   union {
	struct {
		bool immediate;
		bool link;
		bool condcheck;
	} jump;
	struct {
		byte position;
	} move;
	struct {
		byte combine;
	} compare;
   } fields;
   byte reg0;
   byte reg1;
   byte reg2;
   int hassymbol;
   char* symbol;
} dynamicop;
struct {
   int segment;
   word code_address;
   word data_address;
   int entry_count;
   int entry_storage_length;
   labelentry* entries;
   int dynop_count;
   int dynop_storage_length;
   dynamicop* dynops;
   FILE* output;
} asmstate;

dynamicop curri;

void add_label_entry(char* name, word address);
void persist_dynamic_op(void);
void save_encoding(void);
void write_dynamic_op(dynamicop* op);
void initialize(FILE* output, FILE* input);
void cleanup(void);
void resolve_labels(void);
int resolve_op(dynamicop* dop);
void usage(char* arg0);
%}

%union {
      char* sval;
      byte rval;
      unsigned long ival;
}


%token DIRECTIVE_ORG DIRECTIVE_CODE DIRECTIVE_DATA LABEL DIRECTIVE_DECLARE
%token ARITHMETIC_OP_ADD
%token ARITHMETIC_OP_SUB
%token ARITHMETIC_OP_MUL
%token ARITHMETIC_OP_DIV
%token ARITHMETIC_OP_REM
%token ARITHMETIC_OP_SHIFTLEFT
%token ARITHMETIC_OP_SHIFTRIGHT
%token ARITHMETIC_OP_BINARYAND
%token ARITHMETIC_OP_BINARYOR
%token ARITHMETIC_OP_BINARYNOT
%token ARITHMETIC_OP_BINARYXOR
%token ARITHMETIC_OP_ADD_IMM
%token ARITHMETIC_OP_SUB_IMM
%token ARITHMETIC_OP_MUL_IMM
%token ARITHMETIC_OP_DIV_IMM
%token ARITHMETIC_OP_REM_IMM
%token ARITHMETIC_OP_SHIFTLEFT_IMM
%token ARITHMETIC_OP_SHIFTRIGHT_IMM
%token MOVE_OP_MOVE
%token MOVE_OP_SWAP
%token MOVE_OP_SET
%token MOVE_OP_SLICE
%token JUMP_OP_UNCONDITIONAL
%token JUMP_OP_CONDITIONAL
%token JUMP_OP_IFTHENELSE
%token COMPARE_OP_EQ
%token COMPARE_OP_NEQ
%token COMPARE_OP_LESSTHAN
%token COMPARE_OP_GREATERTHAN
%token COMPARE_OP_LESSTHANOREQUALTO
%token COMPARE_OP_GREATERTHANOREQUALTO
%token MISC_OP_SYSTEMCALL
%token COMBINE_OP_AND
%token COMBINE_OP_OR
%token COMBINE_OP_XOR
%token MACRO_OP_EXIT
%token MACRO_OP_PUTCHAR
%token MACRO_OP_READCHAR
%token MACRO_OP_INCREMENT
%token MACRO_OP_DECREMENT
%token MACRO_OP_DOUBLE
%token MACRO_OP_HALVE
%token IF_TOK
%token IS_TOK
%token THEN_TOK
%token TRUE_TOK
%token FALSE_TOK
%token ELSE_TOK
%token GOTO_TOK
%token JUMP_TOK
%token LINK_TOK


%token <rval> REGISTER
%token <ival> IMMEDIATE
%token <sval> SYMBOL

%%
Q: /* empty */ |
   F 
;
F:
   F asm {
      curri.segment = 0;
      curri.address = 0;
      curri.group = 0;
      curri.op = 0;
      curri.reg0 = 0;
      curri.reg1 = 0;
      curri.reg2 = 0;
      curri.hassymbol = 0;
      curri.symbol = 0;
   } | 
   asm {
      curri.segment = 0;
      curri.address = 0;
      curri.group = 0;
      curri.op = 0;
      curri.reg0 = 0;
      curri.reg1 = 0;
      curri.reg2 = 0;
      curri.hassymbol = 0;
      curri.symbol = 0;
   }
   ;
asm:
   directive |
   statement
   ;
directive:
         DIRECTIVE_ORG IMMEDIATE {
            if(asmstate.segment == CodeSegment) {
               asmstate.code_address = $2;
            } else if(asmstate.segment == DataSegment) {
               asmstate.data_address = $2;
            } else {
               yyerror("Invalid segment!");
            }
            } | 
      DIRECTIVE_CODE { asmstate.segment = CodeSegment; } |
      DIRECTIVE_DATA { asmstate.segment = DataSegment; } |
      DIRECTIVE_DECLARE lexeme { 
            if(asmstate.segment == DataSegment) {
               curri.segment = DataSegment;
               curri.address = asmstate.data_address;
               save_encoding();
               asmstate.data_address++;
            } else {
               yyerror("Declaration in non-data segment!");
            }
      }
      ;
statement:
         label { curri.segment = asmstate.segment; }|
         operation {
            if(asmstate.segment == CodeSegment) {
               curri.segment = CodeSegment;
               curri.address = asmstate.code_address;
               save_encoding();
               asmstate.code_address++;
            } else {
               yyerror("operation in an invalid segment!");
            }
         }
         ;
label:
     LABEL SYMBOL { 
      if(asmstate.segment == CodeSegment) {
          add_label_entry($2, asmstate.code_address);
      } else if (asmstate.segment == DataSegment) {
          add_label_entry($2, asmstate.data_address);
      } else {
          yyerror("label in invalid segment!");
      }
     }
   ;
operation:
         arithmetic_op { curri.group = InstructionGroupArithmetic; } |
         move_op { curri.group = InstructionGroupMove; } |
         jump_op { curri.group = InstructionGroupJump; } |
         compare_op { curri.group = InstructionGroupCompare; } |
         misc_op { curri.group = InstructionGroupMisc; } |
		 macro_op
         ;
macro_op:
		MACRO_OP_EXIT REGISTER {
			curri.group = InstructionGroupMisc;
			curri.op= MiscOpSystemCall;
			curri.reg0 = SystemCommandTerminate;
			curri.reg1 = $2;
			curri.reg2 = $2;
		} |
		MACRO_OP_PUTCHAR REGISTER {
			curri.group = InstructionGroupMisc;
			curri.op= MiscOpSystemCall;
			curri.reg0 = SystemCommandPutC;
			curri.reg1 = $2;
			curri.reg2 = $2;
		} |
		MACRO_OP_READCHAR REGISTER {
			curri.group = InstructionGroupMisc;
			curri.op= MiscOpSystemCall;
			curri.reg0 = SystemCommandGetC;
			curri.reg1 = $2;
			curri.reg2 = $2;
		} |
		MACRO_OP_INCREMENT REGISTER {
				curri.group = InstructionGroupArithmetic;
				curri.op = ArithmeticOpAddImmediate;
				curri.reg0 = $2;
				curri.reg1 = $2;
				curri.reg2 = 1;
		} |
		MACRO_OP_DECREMENT REGISTER {
				curri.group = InstructionGroupArithmetic;
				curri.op = ArithmeticOpSubImmediate;
				curri.reg0 = $2;
				curri.reg1 = $2;
				curri.reg2 = 1;
		} |
		MACRO_OP_DOUBLE REGISTER {
				curri.group = InstructionGroupArithmetic;
				curri.op = ArithmeticOpMulImmediate;
				curri.reg0 = $2;
				curri.reg1 = $2;
				curri.reg2 = 2;
		} |
		MACRO_OP_HALVE REGISTER {
				curri.group = InstructionGroupArithmetic;
				curri.op = ArithmeticOpDivImmediate;
				curri.reg0 = $2;
				curri.reg1 = $2;
				curri.reg2 = 2;
		} 
		;

arithmetic_op:
             aop REGISTER REGISTER REGISTER {
                  curri.reg0 = $2;
                  curri.reg1 = $3;
                  curri.reg2 = $4;
             }|
             ARITHMETIC_OP_BINARYNOT REGISTER REGISTER {
			 	  curri.op = ArithmeticOpBinaryNot;
                  curri.reg0 = $2;
                  curri.reg1 = $3;
             } |
             aop_imm REGISTER REGISTER IMMEDIATE {
               if($4 > 255) {
                  yyerror("immediate value offset out of range!");
               }
               curri.reg0 = $2;
               curri.reg1 = $3;
               curri.reg2 = $4;
             }
      ;
move_op:
	   MOVE_OP_MOVE IMMEDIATE REGISTER REGISTER IMMEDIATE {
	   //TODO: add checks for position ranges ($2)
	   //TODO: add checks for bit mask ranges ($5)
	   		curri.op = MoveOpMove;
			curri.fields.move.position = $2;
			curri.reg0 = $3;
			curri.reg1 = $4;
			curri.reg2 = $5;
	   } |
	   MOVE_OP_SWAP IMMEDIATE REGISTER REGISTER IMMEDIATE {
	   //TODO: add checks for position ranges ($2)
	   //TODO: add checks for bit mask ranges ($5)
	   		curri.op = MoveOpSwap;
			curri.fields.move.position = $2;
			curri.reg0 = $3;
			curri.reg1 = $4;
			curri.reg2 = $5;
	   } |
	   MOVE_OP_SLICE IMMEDIATE REGISTER REGISTER IMMEDIATE {
	   //TODO: add checks for position ranges ($2)
	   //TODO: add checks for bit mask ranges ($5)
	   		curri.op = MoveOpSlice;
			curri.fields.move.position = $2;
			curri.reg0 = $3;
			curri.reg1 = $4;
			curri.reg2 = $5;
	   } |
	   MOVE_OP_SET IMMEDIATE REGISTER lexeme {
	   //TODO: add checks for position ranges ($2)
			curri.op = MoveOpSet;
			curri.fields.move.position = $2;
			curri.reg0 = $3;
	   }
       ;

jump_op:
	   unconditional_jump_op { curri.op = JumpOpUnconditional; } |
	   conditional_jump_op { curri.op = JumpOpConditional; } |
	   ifthenelse_jump_op { curri.op = JumpOpIfThenElse; } ;
unconditional_jump_op:
	   JUMP_TOK lexeme {
			curri.fields.jump.immediate = true;
			curri.fields.jump.link = false;
	   } |
	   JUMP_TOK REGISTER {
			curri.fields.jump.immediate = false;
			curri.fields.jump.link = false;
			curri.reg0 = $2;
	   } |
	   JUMP_TOK lexeme LINK_TOK REGISTER {
			curri.fields.jump.immediate = true;
			curri.fields.jump.link = true;
			curri.reg0 = $4;
	   } |
	   JUMP_TOK REGISTER LINK_TOK REGISTER {
	   		curri.fields.jump.immediate = false;
			curri.fields.jump.link = true;
			curri.reg1 = $2;
			curri.reg0 = $4;
	   };

conditional_jump_op:
	   IF_TOK REGISTER IS_TOK TRUE_TOK THEN_TOK JUMP_TOK lexeme LINK_TOK REGISTER {
			curri.fields.jump.immediate = true;
			curri.fields.jump.link = true;
			curri.fields.jump.condcheck = true;
			curri.reg0 = $9;
			if ($2 != PredicateRegisterIndex) {
				yyerror("The implied predicate register ?pred is the only allowed register in conditional immediate link instructions!");
			}
	   } |
	   IF_TOK REGISTER IS_TOK FALSE_TOK THEN_TOK JUMP_TOK lexeme LINK_TOK REGISTER {
			curri.fields.jump.immediate = true;
			curri.fields.jump.link = true;
			curri.fields.jump.condcheck = false;
			curri.reg0 = $9;
			if ($2 != PredicateRegisterIndex) {
				yyerror("The implied predicate register ?pred is the only allowed register in conditional immediate link instructions!");
			}
	   } |
	   IF_TOK REGISTER IS_TOK TRUE_TOK THEN_TOK then_body {
		   curri.fields.jump.condcheck = true;
		curri.reg0 = $2;
	   } |
	   IF_TOK REGISTER IS_TOK FALSE_TOK THEN_TOK then_body {
		   curri.fields.jump.condcheck = false;
			curri.reg0 = $2;
	   };
then_body:
	   JUMP_TOK lexeme {
	   		curri.fields.jump.immediate = true;
			curri.fields.jump.link = false;
	   }|
	   JUMP_TOK REGISTER {
	   		curri.fields.jump.immediate = false;
			curri.fields.jump.link = false;
			curri.reg1 = $2;
	   }|
	   JUMP_TOK REGISTER LINK_TOK REGISTER {
			curri.fields.jump.immediate = false;
			curri.fields.jump.link = true;
			curri.reg1 = $2;
			curri.reg2 = $4;
	   };

ifthenelse_jump_op:
	   IF_TOK REGISTER IS_TOK TRUE_TOK THEN_TOK REGISTER ELSE_TOK REGISTER {
	   		curri.fields.jump.link = false;
	   		curri.fields.jump.condcheck = true;
			curri.reg0 = $2;
			curri.reg1 = $6;
			curri.reg2 = $8;
	   } |
	   IF_TOK REGISTER IS_TOK FALSE_TOK THEN_TOK REGISTER ELSE_TOK REGISTER {
	   		curri.fields.jump.link = false;
	   		curri.fields.jump.condcheck = false;
			curri.reg0 = $2;
			curri.reg1 = $6;
			curri.reg2 = $8;
	   } |
	   IF_TOK REGISTER IS_TOK TRUE_TOK THEN_TOK REGISTER ELSE_TOK REGISTER LINK_TOK REGISTER {
			if ($2 != PredicateRegisterIndex) {
				yyerror("The implied predicate register ?pred is the only allowed register in if then else with link instructions!");
			}
			curri.fields.jump.link = true;
		    curri.fields.jump.condcheck = true;
			curri.reg0 = $10;
			curri.reg1 = $6;
			curri.reg2 = $8;
	   } |
	   IF_TOK REGISTER IS_TOK FALSE_TOK THEN_TOK REGISTER ELSE_TOK REGISTER LINK_TOK REGISTER {
			if ($2 != PredicateRegisterIndex) {
				yyerror("The implied predicate register ?pred is the only allowed register in if then else with link instructions!");
			}
			curri.fields.jump.link = true;
		    curri.fields.jump.condcheck = false;
			curri.reg0 = $10;
			curri.reg1 = $6;
			curri.reg2 = $8;
	   };

compare_op:
          cop REGISTER REGISTER REGISTER {
		  	   curri.fields.compare.combine = CombineOpSet;
               curri.reg0 = $2;
               curri.reg1 = $3;
               curri.reg2 = $4;
          } |
		  cop combineop REGISTER REGISTER REGISTER {
               curri.reg0 = $3;
               curri.reg1 = $4;
               curri.reg2 = $5;
		  }
          ;
misc_op:
       MISC_OP_SYSTEMCALL IMMEDIATE REGISTER REGISTER 
       { 
         curri.op = MiscOpSystemCall; 
         if($2 > 255) {
            yyerror("system call offset out of range!");
         }
         curri.reg0 = $2;
         curri.reg1 = $3;
         curri.reg2 = $4;
       } 
       ;
aop:
   ARITHMETIC_OP_ADD { curri.op = ArithmeticOpAdd; } |
   ARITHMETIC_OP_SUB { curri.op = ArithmeticOpSub; } |
   ARITHMETIC_OP_MUL { curri.op = ArithmeticOpMul; } |
   ARITHMETIC_OP_DIV { curri.op = ArithmeticOpDiv; } |
   ARITHMETIC_OP_REM { curri.op = ArithmeticOpRem; } |
   ARITHMETIC_OP_SHIFTLEFT { curri.op = ArithmeticOpShiftLeft; } |
   ARITHMETIC_OP_SHIFTRIGHT { curri.op = ArithmeticOpShiftRight; } |
   ARITHMETIC_OP_BINARYAND { curri.op = ArithmeticOpBinaryAnd; } |
   ARITHMETIC_OP_BINARYOR { curri.op = ArithmeticOpBinaryOr; } |
   ARITHMETIC_OP_BINARYXOR { curri.op = ArithmeticOpBinaryXor; } 
   ;

aop_imm:
   ARITHMETIC_OP_ADD_IMM { curri.op = ArithmeticOpAddImmediate; } |
   ARITHMETIC_OP_SUB_IMM { curri.op = ArithmeticOpSubImmediate; } |
   ARITHMETIC_OP_MUL_IMM { curri.op = ArithmeticOpMulImmediate; } | 
   ARITHMETIC_OP_DIV_IMM { curri.op = ArithmeticOpDivImmediate; } |
   ARITHMETIC_OP_REM_IMM { curri.op = ArithmeticOpRemImmediate; } |
   ARITHMETIC_OP_SHIFTLEFT_IMM { curri.op = ArithmeticOpShiftLeftImmediate; } |
   ARITHMETIC_OP_SHIFTRIGHT_IMM { curri.op = ArithmeticOpShiftRightImmediate; } 
   ;



/*
jop_reg_imm:
   JUMP_OP_UNCONDITIONALIMMEDIATELINK { curri.op = JumpOpUnconditionalImmediateLink; } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATE { curri.op = JumpOpConditionalTrueImmediate; } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATELINK { curri.op = JumpOpConditionalTrueImmediateLink; } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATE { curri.op = JumpOpConditionalFalseImmediate; } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATELINK { curri.op = JumpOpConditionalFalseImmediateLink; } 
   ;


jop_reg_reg:
   JUMP_OP_UNCONDITIONALREGISTERLINK { curri.op = JumpOpUnconditionalRegisterLink; } |
   JUMP_OP_CONDITIONALTRUEREGISTER { curri.op = JumpOpConditionalTrueRegister; } |
   JUMP_OP_CONDITIONALFALSEREGISTER { curri.op = JumpOpConditionalFalseRegister; }
   ;

jop_reg_reg_reg:
   JUMP_OP_CONDITIONALTRUEREGISTERLINK { curri.op = JumpOpConditionalTrueRegisterLink; } |
   JUMP_OP_CONDITIONALFALSEREGISTERLINK { curri.op = JumpOpConditionalFalseRegisterLink; } |
   JUMP_OP_IFTHENELSENORMALPREDTRUE { curri.op = JumpOpIfThenElseNormalPredTrue; } |
   JUMP_OP_IFTHENELSENORMALPREDFALSE { curri.op = JumpOpIfThenElseNormalPredFalse; } |
   JUMP_OP_IFTHENELSELINKPREDTRUE { curri.op = JumpOpIfThenElseLinkPredTrue; } |
   JUMP_OP_IFTHENELSELINKPREDFALSE { curri.op = JumpOpIfThenElseLinkPredFalse; } |
;
*/

cop:
   COMPARE_OP_EQ { curri.op = CompareOpEq; } |
   COMPARE_OP_NEQ { curri.op = CompareOpNeq; } |
   COMPARE_OP_LESSTHAN { curri.op = CompareOpLessThan; } |
   COMPARE_OP_GREATERTHAN { curri.op = CompareOpGreaterThan; } |
   COMPARE_OP_LESSTHANOREQUALTO { curri.op = CompareOpLessThanOrEqualTo; } |
   COMPARE_OP_GREATERTHANOREQUALTO { curri.op = CompareOpGreaterThanOrEqualTo; } 
;
combineop:
		  COMBINE_OP_AND { curri.fields.compare.combine = CombineOpAnd; } |
		  COMBINE_OP_OR { curri.fields.compare.combine = CombineOpOr; } |
		  COMBINE_OP_XOR { curri.fields.compare.combine = CombineOpXor; }
;
lexeme:
      SYMBOL { curri.hassymbol = 1; 
               curri.symbol = $1; } | 
      IMMEDIATE { 
            curri.reg1 = (byte)(($1 & 0x00FF));
            curri.reg2 = (byte)(($1 & 0xFF00) >> 8);
      }
;
%%
int main(int argc, char* argv[]) {
   char* tmpline;
   int last, i, errorfree;
   /* make sure these are properly initialized */
   last = argc - 1;
   tmpline = 0;
   errorfree = 1;
   i = 0;
   if(argc > 1) {
      for(i = 1; errorfree && (i < last); ++i) {
         tmpline = argv[i];
         if(strlen(tmpline) == 2 && tmpline[0] == '-') {
            switch(tmpline[1]) {
               case 'o':
                  i++;
                  outfile.line = argv[i];
                  break;
               case 'h':
               default:
                  errorfree = 0;
                  break;
            }
         } else {
            errorfree = 0;
            break;
         }
      }
      if(errorfree) {
         if(i == last) {
            /* open the input file */
            tmpline = argv[i];
            if(strlen(tmpline) == 1 && tmpline[0] == '-') {
               infile.fptr = stdin;
            } else if(strlen(tmpline) >= 1 && tmpline[0] != '-') {
               infile.line = tmpline;
               openfw(infile_ptr);
            }

            /* open the output */
            if(!(outfile.line)) {
               outfile.line = "v.obj";
            }
            if(strlen(outfile.line) == 1 && (outfile.line)[0] == '-') {
               outfile.fptr = stdout; 
            } else {
               openfw(outfile_ptr);
            }
         } else {
            fprintf(stderr, "no file provided\n");
         }
      }
   }
   if(outfile.fptr && infile.fptr) {
      initialize(outfile.fptr, infile.fptr);
      do {
         yyparse();
      } while(!feof(yyin));
      resolve_labels();
      cleanup();
   } else {
      usage(argv[0]);
   }
   return 0;
}

void usage(char* arg0) {
   fprintf(stderr, "usage: %s [-o <file>] <file>\n", arg0);
}
void add_label_entry(char* c, word addr) {
   labelentry* le;
   int i;
   if(asmstate.entry_count == asmstate.entry_storage_length) {
     le = realloc(asmstate.entries, asmstate.entry_storage_length + 80);
     if(le == NULL) {
         fprintf(stderr, "panic: couldn't allocate more label memory!\n");
         free(asmstate.entries);
         exit(1);
     } else {
         asmstate.entries = le;
         asmstate.entry_storage_length += 80;
     }
   }
   for(i = 0; i < asmstate.entry_count; i++) {
      if(strcmp(asmstate.entries[i].value, c) == 0) {
         yyerror("Found a duplicate label!");
         exit(1);
      }
   }
   asmstate.entries[asmstate.entry_count].value = c;
   asmstate.entries[asmstate.entry_count].loweraddr = (byte)((addr & 0x00FF));
   asmstate.entries[asmstate.entry_count].upperaddr = (byte)((addr & 0xFF00) >> 8);
   asmstate.entry_count++;
}

void persist_dynamic_op(void) {
   dynamicop* d;
   if(asmstate.dynop_count == asmstate.dynop_storage_length) {
      d = realloc(asmstate.dynops, asmstate.dynop_storage_length + 80);
      if(d == NULL) {
         fprintf(stderr, "panic: couldn't allocate more dynamic operation memory!\n");
         free(asmstate.dynops);
         exit(1);
      } else {
         asmstate.dynops = d;
         asmstate.dynop_storage_length += 80;
      }
   }
   asmstate.dynops[asmstate.dynop_count] = curri;
   asmstate.dynop_count++;
}

void save_encoding(void) {
   if(curri.hassymbol) {
      persist_dynamic_op();
   } else {
      write_dynamic_op(&curri); 
   }
}
void write_dynamic_op(dynamicop* dop) {
   /* ((instruction & ~mask) | (value << shiftcount)) */
   /* little endian build up */
   byte tmp;
   tmp = 0;
   fputc(dop->segment, asmstate.output);
   fputc(((dop->address & 0x00FF)), asmstate.output);
   fputc(((dop->address & 0xFF00) >> 8), asmstate.output);
   if(dop->segment == CodeSegment) {
      tmp = ((tmp & ~0x7) | (dop->group));
      tmp = ((tmp & ~0xF8) | (dop->op << 3));
      fputc(tmp, asmstate.output);
      fputc(dop->reg0, asmstate.output);
      fputc(dop->reg1, asmstate.output);
      fputc(dop->reg2, asmstate.output);
   } else if(dop->segment == DataSegment) {
      fputc(dop->reg1, asmstate.output);
      fputc(dop->reg2, asmstate.output);
   } else {
      fprintf(stderr, "panic: unknown segment %d\n", dop->segment);
      exit(1);
   }
}

void yyerror(const char* s) {
   printf("%d: %s\n", yylineno, s);
   exit(-1);
}
void resolve_labels() {
   /* we need to go through the list of dynamic operations and replace
      the label with the corresponding address */
   int i;
   for(i = 0; i < asmstate.dynop_count; i++) {
      if(!resolve_op(&(asmstate.dynops[i]))) {
         fprintf(stderr, "panic: couldn't find label %s\n", asmstate.dynops[i].symbol);
         exit(1);
      } else {
         write_dynamic_op(&(asmstate.dynops[i]));
      }
   }
}
int resolve_op(dynamicop* dop) {
   int i;
   for(i = 0; i < asmstate.entry_count; i++) {
      if(strcmp(asmstate.entries[i].value, dop->symbol) == 0) {
         /* we found the corresponding label so save the address to the
          * encoding */
         dop->reg1 = asmstate.entries[i].loweraddr;
         dop->reg2 = asmstate.entries[i].upperaddr;
         return 1;
      }
   }
   return 0;
}
void cleanup() {
   int i;
   /* clean up */
   for(i = 0; i < FileWrapperCount; i++) {
         closefw(&(files[i]));
   }
   free(asmstate.dynops);
   free(asmstate.entries);
   asmstate.entries = 0;
   asmstate.dynops = 0;
}
void initialize(FILE* output, FILE* input) {
   labelentry* le;
   dynamicop* dops;
   yyin = input;
   le = calloc(80, sizeof(labelentry));
   dops = calloc(80, sizeof(dynamicop));
   asmstate.segment = CodeSegment;
   asmstate.data_address = 0;
   asmstate.code_address = 0;
   asmstate.entries = le;
   asmstate.entry_count = 0;
   asmstate.entry_storage_length = 80;
   asmstate.dynops = dops;
   asmstate.dynop_count = 0;
   asmstate.dynop_storage_length = 80;
   asmstate.output = output;
   curri.segment = 0;
   curri.address = 0;
   curri.group = 0;
   curri.op = 0;
   curri.reg0 = 0;
   curri.reg1 = 0;
   curri.reg2 = 0;
   curri.hassymbol = 0;
   curri.symbol = 0;
}
