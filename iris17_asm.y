%define api.prefix {iris17}
%{
#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include "asm_interact.h"
#include "iris17.h"
#include "iris17_asm.tab.h"

extern int iris17lex();
extern int iris17parse();
extern FILE* iris17in;
extern int iris17lineno;

void iris17error(const char* s);
/* used to store ops which require a second pass */
namespace iris17 {
struct dynamicop {
   word address;
   byte group;
   byte op;
   byte reg0;
   byte reg1;
   byte reg2;
   byte reg3;
   byte reg4;
   word dataWord;
   int hassymbol;
   std::string symbol;
   bool useUpper = false;
   bool isOperation = false;
};

struct asmstate {
	
   ~asmstate() { }
   word address;
   std::map<std::string, word> labels;
   std::vector<dynamicop> dynops;
   std::ostream* output;
};

asmstate state;
dynamicop curri;

void add_label_entry(const std::string& name, word address);
void persist_dynamic_op(void);
void save_encoding(void);
void write_dynamic_op(iris17::dynamicop* op);
void initialize(std::ostream* output, FILE* input);
void resolve_labels(void);
bool resolve_op(iris17::dynamicop* dop);
}
%}

%union {
      char* sval;
      byte rval;
      unsigned long ival;
}


%token DIRECTIVE_ORG LABEL DIRECTIVE_DECLARE
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
%token MOVE_OP_SET_LOWER MOVE_OP_SET_UPPER
%token MOVE_OP_LOAD
%token MOVE_OP_STORE
%token MOVE_OP_POP
%token MOVE_OP_PUSH
%token JUMP_OP_UNCONDITIONALREGISTER JUMP_OP_UNCONDITIONALREGISTERLINK
%token JUMP_OP_CONDITIONALTRUEREGISTER JUMP_OP_CONDITIONALTRUEREGISTERLINK
%token JUMP_OP_CONDITIONALFALSEREGISTER JUMP_OP_CONDITIONALFALSEREGISTERLINK
%token JUMP_OP_IFTHENELSENORMALPREDTRUE JUMP_OP_IFTHENELSENORMALPREDFALSE
%token JUMP_OP_IFTHENELSELINKPREDTRUE JUMP_OP_IFTHENELSELINKPREDFALSE
%token COMPARE_OP_EQ
%token COMPARE_OP_NEQ
%token COMPARE_OP_LESSTHAN
%token COMPARE_OP_GREATERTHAN
%token COMPARE_OP_LESSTHANOREQUALTO
%token COMPARE_OP_GREATERTHANOREQUALTO
%token COMPARE_OP_EQ_IMMEDIATE
%token COMPARE_OP_NEQ_IMMEDIATE
%token COMPARE_OP_LESSTHAN_IMMEDIATE
%token COMPARE_OP_GREATERTHAN_IMMEDIATE
%token COMPARE_OP_LESSTHANOREQUALTO_IMMEDIATE
%token COMPARE_OP_GREATERTHANOREQUALTO_IMMEDIATE
%token MISC_OP_SYSTEMCALL
%token ARITHMETIC_MACRO_OP_INCR
%token ARITHMETIC_MACRO_OP_DECR
%token ARITHMETIC_MACRO_OP_HALVE
%token ARITHMETIC_MACRO_OP_DOUBLE
%token JUMP_MACRO_OP_RETURN

%token <rval> REGISTER
%token <ival> IMMEDIATE
%token <sval> IRIS17_SYMBOL

%%
Q: /* empty */ |
   F 
;
F:
   F asm {
      iris17::curri.address = 0;
      iris17::curri.group = 0;
      iris17::curri.op = 0;
      iris17::curri.reg0 = 0;
      iris17::curri.reg1 = 0;
      iris17::curri.reg2 = 0;
      iris17::curri.hassymbol = 0;
      iris17::curri.symbol = "";
	  iris17::curri.useUpper = false;
	  iris17::curri.reg3 = 0;
	  iris17::curri.reg4 = 0;
	  iris17::curri.dataWord = 0;
	  iris17::curri.isOperation = false;
   } | 
   asm {
      iris17::curri.address = 0;
      iris17::curri.group = 0;
      iris17::curri.op = 0;
      iris17::curri.reg0 = 0;
      iris17::curri.reg1 = 0;
      iris17::curri.reg2 = 0;
      iris17::curri.hassymbol = 0;
      iris17::curri.symbol = "";
	  iris17::curri.useUpper = false;
	  iris17::curri.reg3 = 0;
	  iris17::curri.reg4 = 0;
	  iris17::curri.dataWord = 0;
	  iris17::curri.isOperation = false;
   }
   ;
asm:
   directive |
   statement
   ;
directive:
		DIRECTIVE_ORG IMMEDIATE {
			   iris17::state.address = $2;
		} | 
        DIRECTIVE_DECLARE lexeme { 
			   iris17::curri.address = iris17::state.address;
			   iris17::save_encoding();
			   iris17::state.address++;
      }
      ;
statement:
         label |
         operation {
			   iris17::curri.address = iris17::state.address;
			   iris17::curri.isOperation = true;
			   iris17::save_encoding();
			   iris17::state.address++;
         }
         ;
label:
     LABEL IRIS17_SYMBOL { 
	 iris17::add_label_entry($2, iris17::state.address); 
	 } ;
operation:
         arithmetic_op { iris17::curri.group = (byte)iris17::InstructionGroup::Arithmetic; } |
         move_op { iris17::curri.group = (byte)iris17::InstructionGroup::Move; } |
         jump_op { iris17::curri.group = (byte)iris17::InstructionGroup::Jump; } |
         compare_op { iris17::curri.group = (byte)iris17::InstructionGroup::Compare; } |
         misc_op { iris17::curri.group = (byte)iris17::InstructionGroup::Misc; }
         ;
arithmetic_op:
             aop REGISTER REGISTER REGISTER {
                  iris17::curri.reg0 = $2;
                  iris17::curri.reg1 = $3;
                  iris17::curri.reg2 = $4;
             }|
             ARITHMETIC_OP_BINARYNOT REGISTER REGISTER {
                  iris17::curri.reg0 = $2;
                  iris17::curri.reg1 = $3;
             } |
             aop_imm REGISTER REGISTER IMMEDIATE {
               if($4 > 255) {
                  iris17error("immediate value offset out of range!");
               }
               iris17::curri.reg0 = $2;
               iris17::curri.reg1 = $3;
               iris17::curri.reg2 = $4;
             } |
			 aop_single_macro REGISTER {
			 	iris17::curri.reg0 = $2;
				iris17::curri.reg1 = $2;
			 }
      ;
aop_single_macro:
   ARITHMETIC_MACRO_OP_INCR { 
     iris17::curri.op = (byte)iris17::ArithmeticOp::AddImmediate;
     iris17::curri.reg2 = 1;
   } |
   ARITHMETIC_MACRO_OP_DECR { 
     iris17::curri.op = (byte)iris17::ArithmeticOp::SubImmediate;  
	 iris17::curri.reg2 = 1;
   } |
   ARITHMETIC_MACRO_OP_HALVE { 
     iris17::curri.op = (byte)iris17::ArithmeticOp::DivImmediate; 
     iris17::curri.reg2 = 2;
   } |
   ARITHMETIC_MACRO_OP_DOUBLE {
     iris17::curri.op = (byte)iris17::ArithmeticOp::MulImmediate; 
     iris17::curri.reg2 = 2;
   }
   ;
move_op:
       mop_reg REGISTER REGISTER {
            iris17::curri.reg0 = $2;
            iris17::curri.reg1 = $3;
       } |

       MOVE_OP_SET_UPPER REGISTER lexeme { 
	        iris17::curri.op = (byte)iris17::MoveOp::SetUpper; 
	        iris17::curri.reg0 = $2;
	        iris17::curri.reg1 = iris17::curri.reg3;
	        iris17::curri.reg2 = iris17::curri.reg4;
			iris17::curri.useUpper = true;
	   } |
       MOVE_OP_SET_LOWER REGISTER lexeme { 
         iris17::curri.op = (byte)iris17::MoveOp::SetLower; 
		 iris17::curri.reg0 = $2;
       }  |
       mop_single REGISTER {
         iris17::curri.reg0 = $2;
       } 
       ;

jump_op:
	   JUMP_MACRO_OP_RETURN {
			iris17::curri.op = (byte)iris17::JumpOp::UnconditionalRegister;
			iris17::curri.reg0 = iris17::ArchitectureConstants::LinkRegisterIndex;
	   } |
	   jop_reg REGISTER {
	   	iris17::curri.reg0 = $2;
	   } |
       jop_reg_reg REGISTER REGISTER {
            iris17::curri.reg0 = $2;
            iris17::curri.reg1 = $3;
       } |
       jop_reg_reg_reg REGISTER REGISTER REGISTER {
            iris17::curri.reg0 = $2;
            iris17::curri.reg1 = $3;
            iris17::curri.reg2 = $4;
       }
       ;

compare_op:
          cop REGISTER REGISTER REGISTER {
               iris17::curri.reg0 = $2;
               iris17::curri.reg1 = $3;
               iris17::curri.reg2 = $4;
          } |
		  icop REGISTER REGISTER IMMEDIATE {
		  	if ($4 > 255) {
                  iris17error("immediate value offset out of range!");
			}
			iris17::curri.reg0 = $2;
			iris17::curri.reg1 = $3;
			iris17::curri.reg2 = $4;
		  }
          ;
misc_op:
       MISC_OP_SYSTEMCALL IMMEDIATE REGISTER REGISTER 
       { 
         iris17::curri.op = (byte)iris17::MiscOp::SystemCall; 
         if($2 > 255) {
            iris17error("system call offset out of range!");
         }
         iris17::curri.reg0 = $2;
         iris17::curri.reg1 = $3;
         iris17::curri.reg2 = $4;
       } 
       ;
aop:
   ARITHMETIC_OP_ADD { iris17::curri.op = (byte)iris17::ArithmeticOp::Add; } |
   ARITHMETIC_OP_SUB { iris17::curri.op = (byte)iris17::ArithmeticOp::Sub; } |
   ARITHMETIC_OP_MUL { iris17::curri.op = (byte)iris17::ArithmeticOp::Mul; } |
   ARITHMETIC_OP_DIV { iris17::curri.op = (byte)iris17::ArithmeticOp::Div; } |
   ARITHMETIC_OP_REM { iris17::curri.op = (byte)iris17::ArithmeticOp::Rem; } |
   ARITHMETIC_OP_SHIFTLEFT { iris17::curri.op = (byte)iris17::ArithmeticOp::ShiftLeft; } |
   ARITHMETIC_OP_SHIFTRIGHT { iris17::curri.op = (byte)iris17::ArithmeticOp::ShiftRight; } |
   ARITHMETIC_OP_BINARYAND { iris17::curri.op = (byte)iris17::ArithmeticOp::BinaryAnd; } |
   ARITHMETIC_OP_BINARYOR { iris17::curri.op = (byte)iris17::ArithmeticOp::BinaryOr; } |
   ARITHMETIC_OP_BINARYXOR { iris17::curri.op = (byte)iris17::ArithmeticOp::BinaryXor; } 
   ;

aop_imm:
   ARITHMETIC_OP_ADD_IMM { iris17::curri.op = (byte)iris17::ArithmeticOp::AddImmediate; } |
   ARITHMETIC_OP_SUB_IMM { iris17::curri.op = (byte)iris17::ArithmeticOp::SubImmediate; } |
   ARITHMETIC_OP_MUL_IMM { iris17::curri.op = (byte)iris17::ArithmeticOp::MulImmediate; } | 
   ARITHMETIC_OP_DIV_IMM { iris17::curri.op = (byte)iris17::ArithmeticOp::DivImmediate; } |
   ARITHMETIC_OP_REM_IMM { iris17::curri.op = (byte)iris17::ArithmeticOp::RemImmediate; } |
   ARITHMETIC_OP_SHIFTLEFT_IMM { iris17::curri.op = (byte)iris17::ArithmeticOp::ShiftLeftImmediate; } |
   ARITHMETIC_OP_SHIFTRIGHT_IMM { iris17::curri.op = (byte)iris17::ArithmeticOp::ShiftRightImmediate; } 
   ;

mop_reg:
   MOVE_OP_MOVE { iris17::curri.op = (byte)iris17::MoveOp::Move; } |
   MOVE_OP_SWAP { iris17::curri.op = (byte)iris17::MoveOp::Swap; } |
   MOVE_OP_LOAD { iris17::curri.op = (byte)iris17::MoveOp::Load; } |
   MOVE_OP_STORE { iris17::curri.op = (byte)iris17::MoveOp::Store; } |
   ;


mop_single:
   MOVE_OP_PUSH { iris17::curri.op = (byte)iris17::MoveOp::Push; } |
   MOVE_OP_POP { iris17::curri.op = (byte)iris17::MoveOp::Pop; } 
   ;

jop_reg:
   JUMP_OP_UNCONDITIONALREGISTERLINK { iris17::curri.op = (byte)iris17::JumpOp::UnconditionalRegisterLink; } |
	   JUMP_OP_UNCONDITIONALREGISTER { iris17::curri.op = (byte)iris17::JumpOp::UnconditionalRegister; }
	   ;


jop_reg_reg:
   JUMP_OP_CONDITIONALTRUEREGISTER { iris17::curri.op = (byte)iris17::JumpOp::ConditionalTrueRegister; } |
   JUMP_OP_CONDITIONALFALSEREGISTER { iris17::curri.op = (byte)iris17::JumpOp::ConditionalFalseRegister; }
   ;

jop_reg_reg_reg:
   JUMP_OP_CONDITIONALTRUEREGISTERLINK { iris17::curri.op = (byte)iris17::JumpOp::ConditionalTrueRegisterLink; } |
   JUMP_OP_CONDITIONALFALSEREGISTERLINK { iris17::curri.op = (byte)iris17::JumpOp::ConditionalFalseRegisterLink; } |
   JUMP_OP_IFTHENELSENORMALPREDTRUE { iris17::curri.op = (byte)iris17::JumpOp::IfThenElseNormalPredTrue; } |
   JUMP_OP_IFTHENELSENORMALPREDFALSE { iris17::curri.op = (byte)iris17::JumpOp::IfThenElseNormalPredFalse; } |
   JUMP_OP_IFTHENELSELINKPREDTRUE { iris17::curri.op = (byte)iris17::JumpOp::IfThenElseLinkPredTrue; } |
   JUMP_OP_IFTHENELSELINKPREDFALSE { iris17::curri.op = (byte)iris17::JumpOp::IfThenElseLinkPredFalse; }
;

cop:
   COMPARE_OP_EQ { iris17::curri.op = (byte)iris17::CompareOp::Eq; } |
   COMPARE_OP_NEQ { iris17::curri.op = (byte)iris17::CompareOp::Neq; } |
   COMPARE_OP_LESSTHAN { iris17::curri.op = (byte)iris17::CompareOp::LessThan; } |
   COMPARE_OP_GREATERTHAN { iris17::curri.op = (byte)iris17::CompareOp::GreaterThan; } |
   COMPARE_OP_LESSTHANOREQUALTO { iris17::curri.op = (byte)iris17::CompareOp::LessThanOrEqualTo; } |
   COMPARE_OP_GREATERTHANOREQUALTO { iris17::curri.op = (byte)iris17::CompareOp::GreaterThanOrEqualTo; } 
;
icop:
   COMPARE_OP_EQ_IMMEDIATE { iris17::curri.op = (byte)iris17::CompareOp::EqImm; } |
   COMPARE_OP_NEQ_IMMEDIATE { iris17::curri.op = (byte)iris17::CompareOp::NeqImm; } |
   COMPARE_OP_LESSTHAN_IMMEDIATE { iris17::curri.op = (byte)iris17::CompareOp::LessThanImm; } |
   COMPARE_OP_GREATERTHAN_IMMEDIATE { iris17::curri.op = (byte)iris17::CompareOp::GreaterThanImm; } |
   COMPARE_OP_LESSTHANOREQUALTO_IMMEDIATE { iris17::curri.op = (byte)iris17::CompareOp::LessThanOrEqualToImm; } |
   COMPARE_OP_GREATERTHANOREQUALTO_IMMEDIATE { iris17::curri.op = (byte)iris17::CompareOp::GreaterThanOrEqualToImm; }
;
lexeme:
      IRIS17_SYMBOL { iris17::curri.hassymbol = 1; 
               iris17::curri.symbol = $1; } | 
      IMMEDIATE { 
	  //std::cerr << "IMMEDIATE value = " << std::hex << word($1) << std::endl;
            iris17::curri.reg1 = (byte)(($1 & 0x000000FF));
            iris17::curri.reg2 = (byte)(($1 & 0x0000FF00) >> 8);
			iris17::curri.reg3 = (byte)(($1 & 0x00FF0000) >> 16);
			iris17::curri.reg4 = (byte)(($1 & 0xFF000000) >> 24);
			iris17::curri.dataWord = $1;
      } 
;
%%
void iris17error(const char* s) {
   printf("%d: %s\n", iris17lineno, s);
   exit(-1);
}

namespace iris17 {
	void assemble(FILE* input, std::ostream* output) {
      initialize(output, input);
      do {
         yyparse();
      } while(!feof(iris17in));
      resolve_labels();
	}
}

namespace iris17 {
void add_label_entry(const std::string& c, word addr) {
   if (iris17::state.labels.count(c) != 0) {
		iris17error("Found a duplicate label!");
		exit(1);
   } else {
	 iris17::state.labels[c] = addr;
   }
}

void persist_dynamic_op(void) {
   iris17::state.dynops.push_back(curri);
}

void save_encoding(void) {
   if(iris17::curri.hassymbol) {
      persist_dynamic_op();
   } else {
      write_dynamic_op(&curri); 
   }
}

void write_dynamic_op(dynamicop* dop) {
   /* ((instruction & ~mask) | (value << shiftcount)) */
   /* little endian build up */
   char buf[8] = { 0 };
   buf[0] = char(dop->address & 0x000000FF);
   buf[1] = char((dop->address & 0x0000FF00) >> 8);
   buf[2] = char((dop->address & 0x00FF0000) >> 16);
   buf[3] = char((dop->address & 0xFF000000) >> 24);
   if (dop->isOperation) {
		buf[4] = char(iris::encodeBits<byte, byte, 0b11111000, 3>(
							iris::encodeBits<byte, byte, 0b00000111, 0>((byte)0, dop->group),
							dop->op));
		buf[5] = char(dop->reg0);
		if (dop->useUpper) {
			buf[6] = char(dop->reg3);
			buf[7] = char(dop->reg4);
		} else {
			buf[6] = char(dop->reg1);
			buf[7] = char(dop->reg2);
		}
	} else {
		buf[4] = char(dop->reg1);
		buf[5] = char(dop->reg2);
		buf[6] = char(dop->reg3);
		buf[7] = char(dop->reg4);
	}
   iris17::state.output->write(buf, 8);
}

void resolve_labels() {
   /* we need to go through the list of dynamic operations and replace
      the label with the corresponding address */
   for (auto &op : iris17::state.dynops) {
   		if (!resolve_op(&op)) {
			std::cerr << "panic: couldn't find label " << op.symbol << std::endl;
			exit(1);
		} else {
			write_dynamic_op(&op);
		}
   }
}
bool resolve_op(dynamicop* dop) {
   if(iris17::state.labels.count(dop->symbol) == 1) {
		word addr = iris17::state.labels[dop->symbol];
		dop->reg1 = iris::decodeField<word, byte, 0>(addr);
		dop->reg2 = iris::decodeField<word, byte, 1>(addr);
		dop->reg3 = iris::decodeField<word, byte, 2>(addr);
		dop->reg4 = iris::decodeField<word, byte, 3>(addr);
		return true;
   }
   return false;
}

void initialize(std::ostream* output, FILE* input) {
   iris17in = input;
   iris17::state.address = 0;
   iris17::state.output = output;
   iris17::curri.address = 0;
   iris17::curri.group = 0;
   iris17::curri.op = 0;
   iris17::curri.reg0 = 0;
   iris17::curri.reg1 = 0;
   iris17::curri.reg2 = 0;
   iris17::curri.hassymbol = 0;
}
}
