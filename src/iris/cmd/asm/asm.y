%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
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
   ushort address;
   byte group;
   byte op;
   byte reg0;
   byte reg1;
   byte reg2;
   int hassymbol;
   char* symbol;
} dynamicop;
struct {
   int segment;
   ushort code_address;
   ushort data_address;
   int entry_count;
   int entry_storage_length;
   labelentry* entries;
   int dynop_count;
   int dynop_storage_length;
   dynamicop* dynops;
   FILE* output;
} asmstate;

dynamicop curri;

void add_label_entry(char* name, ushort address);
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
      ulong ival;
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
%token MOVE_OP_MOVE
%token MOVE_OP_SWAP
%token MOVE_OP_SWAPREGADDR
%token MOVE_OP_SWAPADDRADDR
%token MOVE_OP_SWAPREGMEM
%token MOVE_OP_SWAPADDRMEM
%token MOVE_OP_SET
%token MOVE_OP_LOAD
%token MOVE_OP_LOADMEM
%token MOVE_OP_STORE
%token MOVE_OP_STOREADDR
%token MOVE_OP_STOREMEM
%token MOVE_OP_STOREIMM
%token JUMP_OP_UNCONDITIONALIMMEDIATE
%token JUMP_OP_UNCONDITIONALIMMEDIATELINK
%token JUMP_OP_UNCONDITIONALREGISTER
%token JUMP_OP_UNCONDITIONALREGISTERLINK
%token JUMP_OP_CONDITIONALTRUEIMMEDIATE
%token JUMP_OP_CONDITIONALTRUEIMMEDIATELINK
%token JUMP_OP_CONDITIONALTRUEREGISTER
%token JUMP_OP_CONDITIONALTRUEREGISTERLINK
%token JUMP_OP_CONDITIONALFALSEIMMEDIATE
%token JUMP_OP_CONDITIONALFALSEIMMEDIATELINK
%token JUMP_OP_CONDITIONALFALSEREGISTER
%token JUMP_OP_CONDITIONALFALSEREGISTERLINK
%token JUMP_OP_IFTHENELSENORMALPREDTRUE
%token JUMP_OP_IFTHENELSENORMALPREDFALSE
%token JUMP_OP_IFTHENELSELINKPREDTRUE
%token JUMP_OP_IFTHENELSELINKPREDFALSE
%token COMPARE_OP_EQ
%token COMPARE_OP_EQAND
%token COMPARE_OP_EQOR
%token COMPARE_OP_EQXOR
%token COMPARE_OP_NEQ
%token COMPARE_OP_NEQAND
%token COMPARE_OP_NEQOR
%token COMPARE_OP_NEQXOR
%token COMPARE_OP_LESSTHAN
%token COMPARE_OP_LESSTHANAND
%token COMPARE_OP_LESSTHANOR
%token COMPARE_OP_LESSTHANXOR
%token COMPARE_OP_GREATERTHAN
%token COMPARE_OP_GREATERTHANAND
%token COMPARE_OP_GREATERTHANOR
%token COMPARE_OP_GREATERTHANXOR
%token COMPARE_OP_LESSTHANOREQUALTO
%token COMPARE_OP_LESSTHANOREQUALTOAND
%token COMPARE_OP_LESSTHANOREQUALTOOR
%token COMPARE_OP_LESSTHANOREQUALTOXOR
%token COMPARE_OP_GREATERTHANOREQUALTO
%token COMPARE_OP_GREATERTHANOREQUALTOAND
%token COMPARE_OP_GREATERTHANOREQUALTOOR
%token COMPARE_OP_GREATERTHANOREQUALTOXOR
%token MISC_OP_SYSTEMCALL
%token MISC_OP_SETIMPLICITREGISTERIMMEDIATE
%token MISC_OP_SETIMPLICITREGISTERINDIRECT
%token MISC_OP_GETIMPLICITREGISTERIMMEDIATE
%token MISC_OP_GETIMPLICITREGISTERINDIRECT


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
         misc_op { curri.group = InstructionGroupMisc; }
         ;
arithmetic_op:
             aop REGISTER REGISTER REGISTER {
                  curri.reg0 = $2;
                  curri.reg1 = $3;
                  curri.reg2 = $4;
             }|
             ARITHMETIC_OP_BINARYNOT REGISTER REGISTER {
                  curri.reg0 = $2;
                  curri.reg1 = $3;
             }
      ;
move_op:
       mop_reg REGISTER REGISTER {
            curri.reg0 = $2;
            curri.reg1 = $3;
       } |
       mop_mixed REGISTER lexeme { curri.reg0 = $2; }
       ;

jump_op:
       JUMP_OP_UNCONDITIONALIMMEDIATE lexeme { 
         curri.op = JumpOpUnconditionalImmediate; 
         } | 
       JUMP_OP_UNCONDITIONALREGISTER REGISTER { 
         curri.op = JumpOpUnconditionalRegister; 
         curri.reg0 = $2;
       } |
       jop_reg_reg REGISTER REGISTER {
            curri.reg0 = $2;
            curri.reg1 = $3;
       } |
       jop_reg_imm REGISTER lexeme { curri.reg0 = $2; } |
       jop_reg_reg_reg REGISTER REGISTER REGISTER {
            curri.reg0 = $2;
            curri.reg1 = $3;
            curri.reg2 = $4;
       }
       ;

compare_op:
          cop REGISTER REGISTER REGISTER {
               curri.reg0 = $2;
               curri.reg1 = $3;
               curri.reg2 = $4;
          }
          ;
misc_op:
       MISC_OP_SYSTEMCALL IMMEDIATE REGISTER REGISTER 
       { 
         curri.op = MiscOpSystemCall; 
         if($2 > 255 || $2 < 0) {
            yyerror("system call offset out of range!");
         }
         curri.reg0 = $2;
         curri.reg1 = $3;
         curri.reg2 = $4;
       } |
       MISC_OP_SETIMPLICITREGISTERIMMEDIATE IMMEDIATE REGISTER {
       curri.op = MiscOpSetImplicitRegisterImmediate; 
       if($2 > 255 || $2 < 0) {
            yyerror("implicit register offset out of range!");
       }
       curri.reg0 = $2;
       curri.reg1 = $3;
       } |
       MISC_OP_GETIMPLICITREGISTERIMMEDIATE REGISTER IMMEDIATE { 
       curri.op = MiscOpGetImplicitRegisterImmediate; 
       if($3 > 255 || $3 < 0) {
            yyerror("implicit register offset out of range!");
       }
       curri.reg0 = $2;
       curri.reg1 = $3;
       } |
       miop REGISTER REGISTER  { 
         curri.reg0 = $2;
         curri.reg1 = $3;
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

mop_reg:
   MOVE_OP_MOVE { curri.op = MoveOpMove; } |
   MOVE_OP_SWAP { curri.op = MoveOpSwap; } |
   MOVE_OP_SWAPREGADDR { curri.op = MoveOpSwapRegAddr; } |
   MOVE_OP_SWAPADDRADDR { curri.op = MoveOpSwapAddrAddr; } |
   MOVE_OP_LOAD { curri.op = MoveOpLoad; } |
   MOVE_OP_STORE { curri.op = MoveOpStore; } |
   MOVE_OP_STOREADDR { curri.op = MoveOpStoreAddr; } 
   ;

mop_mixed:
   MOVE_OP_SWAPREGMEM { curri.op = MoveOpSwapRegMem; } |
   MOVE_OP_SWAPADDRMEM { curri.op = MoveOpSwapAddrMem; } |
   MOVE_OP_SET { curri.op = MoveOpSet; } |
   MOVE_OP_LOADMEM { curri.op = MoveOpLoadMem; } |
   MOVE_OP_STOREMEM { curri.op = MoveOpStoreMem; } |
   MOVE_OP_STOREIMM { curri.op = MoveOpStoreImm; }
   ;

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

cop:
   COMPARE_OP_EQ { curri.op = CompareOpEq; } |
   COMPARE_OP_EQAND { curri.op = CompareOpEqAnd; } |
   COMPARE_OP_EQOR { curri.op = CompareOpEqOr; } |
   COMPARE_OP_EQXOR { curri.op = CompareOpEqXor; } |
   COMPARE_OP_NEQ { curri.op = CompareOpNeq; } |
   COMPARE_OP_NEQAND { curri.op = CompareOpNeqAnd; } |
   COMPARE_OP_NEQOR { curri.op = CompareOpNeqOr; } |
   COMPARE_OP_NEQXOR { curri.op = CompareOpNeqXor; } |
   COMPARE_OP_LESSTHAN { curri.op = CompareOpLessThan; } |
   COMPARE_OP_LESSTHANAND { curri.op = CompareOpLessThanAnd; } |
   COMPARE_OP_LESSTHANOR { curri.op = CompareOpLessThanOr; } |
   COMPARE_OP_LESSTHANXOR { curri.op = CompareOpLessThanXor; } |
   COMPARE_OP_GREATERTHAN { curri.op = CompareOpGreaterThan; } |
   COMPARE_OP_GREATERTHANAND { curri.op = CompareOpGreaterThanAnd; } |
   COMPARE_OP_GREATERTHANOR { curri.op = CompareOpGreaterThanOr; } |
   COMPARE_OP_GREATERTHANXOR { curri.op = CompareOpGreaterThanXor; } |
   COMPARE_OP_LESSTHANOREQUALTO { curri.op = CompareOpLessThanOrEqualTo; } |
   COMPARE_OP_LESSTHANOREQUALTOAND { curri.op = CompareOpLessThanOrEqualToAnd; } |
   COMPARE_OP_LESSTHANOREQUALTOOR { curri.op = CompareOpLessThanOrEqualToOr; } |
   COMPARE_OP_LESSTHANOREQUALTOXOR { curri.op = CompareOpLessThanOrEqualToXor; } |
   COMPARE_OP_GREATERTHANOREQUALTO { curri.op = CompareOpGreaterThanOrEqualTo; } |
   COMPARE_OP_GREATERTHANOREQUALTOAND { curri.op = CompareOpGreaterThanOrEqualToAnd; } |
   COMPARE_OP_GREATERTHANOREQUALTOOR { curri.op = CompareOpGreaterThanOrEqualToOr; } |
   COMPARE_OP_GREATERTHANOREQUALTOXOR { curri.op = CompareOpGreaterThanOrEqualToXor; }
;
miop: 
   MISC_OP_SETIMPLICITREGISTERINDIRECT { curri.op = MiscOpSetImplicitRegisterIndirect; } |
   MISC_OP_GETIMPLICITREGISTERINDIRECT { curri.op = MiscOpGetImplicitRegisterIndirect; } 
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
void add_label_entry(char* c, ushort addr) {
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
   int i;
   labelentry* le;
   dynamicop* dops;
   i = 0;
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
