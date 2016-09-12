%define api.prefix {iris32}
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
#include "iris32.h"
#include "iris32_asm.tab.h"

extern int iris32lex();
extern int iris32parse();
extern FILE* iris32in;
extern int iris32lineno;

void iris32error(const char* s);
/* used to store ops which require a second pass */
namespace iris32 {
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
void write_dynamic_op(iris32::dynamicop* op);
void initialize(std::ostream* output, FILE* input);
void resolve_labels(void);
bool resolve_op(iris32::dynamicop* dop);
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
%token <sval> SYMBOL

%%
Q: /* empty */ |
   F 
;
F:
   F asm {
      iris32::curri.address = 0;
      iris32::curri.group = 0;
      iris32::curri.op = 0;
      iris32::curri.reg0 = 0;
      iris32::curri.reg1 = 0;
      iris32::curri.reg2 = 0;
      iris32::curri.hassymbol = 0;
      iris32::curri.symbol = "";
	  iris32::curri.useUpper = false;
	  iris32::curri.reg3 = 0;
	  iris32::curri.reg4 = 0;
	  iris32::curri.dataWord = 0;
	  iris32::curri.isOperation = false;
   } | 
   asm {
      iris32::curri.address = 0;
      iris32::curri.group = 0;
      iris32::curri.op = 0;
      iris32::curri.reg0 = 0;
      iris32::curri.reg1 = 0;
      iris32::curri.reg2 = 0;
      iris32::curri.hassymbol = 0;
      iris32::curri.symbol = "";
	  iris32::curri.useUpper = false;
	  iris32::curri.reg3 = 0;
	  iris32::curri.reg4 = 0;
	  iris32::curri.dataWord = 0;
	  iris32::curri.isOperation = false;
   }
   ;
asm:
   directive |
   statement
   ;
directive:
		DIRECTIVE_ORG IMMEDIATE {
			   iris32::state.address = $2;
		} | 
        DIRECTIVE_DECLARE lexeme { 
			   iris32::curri.address = iris32::state.address;
			   iris32::save_encoding();
			   iris32::state.address++;
      }
      ;
statement:
         label |
         operation {
			   iris32::curri.address = iris32::state.address;
			   iris32::curri.isOperation = true;
			   iris32::save_encoding();
			   iris32::state.address++;
         }
         ;
label:
     LABEL SYMBOL { 
	 iris32::add_label_entry($2, iris32::state.address); 
	 } ;
operation:
         arithmetic_op { iris32::curri.group = (byte)iris32::InstructionGroup::Arithmetic; } |
         move_op { iris32::curri.group = (byte)iris32::InstructionGroup::Move; } |
         jump_op { iris32::curri.group = (byte)iris32::InstructionGroup::Jump; } |
         compare_op { iris32::curri.group = (byte)iris32::InstructionGroup::Compare; } |
         misc_op { iris32::curri.group = (byte)iris32::InstructionGroup::Misc; }
         ;
arithmetic_op:
             aop REGISTER REGISTER REGISTER {
                  iris32::curri.reg0 = $2;
                  iris32::curri.reg1 = $3;
                  iris32::curri.reg2 = $4;
             }|
             ARITHMETIC_OP_BINARYNOT REGISTER REGISTER {
                  iris32::curri.reg0 = $2;
                  iris32::curri.reg1 = $3;
             } |
             aop_imm REGISTER REGISTER IMMEDIATE {
               if($4 > 255) {
                  iris32error("immediate value offset out of range!");
               }
               iris32::curri.reg0 = $2;
               iris32::curri.reg1 = $3;
               iris32::curri.reg2 = $4;
             } |
			 aop_single_macro REGISTER {
			 	iris32::curri.reg0 = $2;
				iris32::curri.reg1 = $2;
			 }
      ;
aop_single_macro:
   ARITHMETIC_MACRO_OP_INCR { 
     iris32::curri.op = (byte)iris32::ArithmeticOp::AddImmediate;
     iris32::curri.reg2 = 1;
   } |
   ARITHMETIC_MACRO_OP_DECR { 
     iris32::curri.op = (byte)iris32::ArithmeticOp::SubImmediate;  
	 iris32::curri.reg2 = 1;
   } |
   ARITHMETIC_MACRO_OP_HALVE { 
     iris32::curri.op = (byte)iris32::ArithmeticOp::DivImmediate; 
     iris32::curri.reg2 = 2;
   } |
   ARITHMETIC_MACRO_OP_DOUBLE {
     iris32::curri.op = (byte)iris32::ArithmeticOp::MulImmediate; 
     iris32::curri.reg2 = 2;
   }
   ;
move_op:
       mop_reg REGISTER REGISTER {
            iris32::curri.reg0 = $2;
            iris32::curri.reg1 = $3;
       } |

       MOVE_OP_SET_UPPER REGISTER lexeme { 
	        iris32::curri.op = (byte)iris32::MoveOp::SetUpper; 
	        iris32::curri.reg0 = $2;
	        iris32::curri.reg1 = iris32::curri.reg3;
	        iris32::curri.reg2 = iris32::curri.reg4;
			iris32::curri.useUpper = true;
	   } |
       MOVE_OP_SET_LOWER REGISTER lexeme { 
         iris32::curri.op = (byte)iris32::MoveOp::SetLower; 
		 iris32::curri.reg0 = $2;
       }  |
       mop_single REGISTER {
         iris32::curri.reg0 = $2;
       } 
       ;

jump_op:
	   JUMP_MACRO_OP_RETURN {
			iris32::curri.op = (byte)iris32::JumpOp::UnconditionalRegister;
			iris32::curri.reg0 = iris32::ArchitectureConstants::LinkRegisterIndex;
	   } |
	   jop_reg REGISTER {
	   	iris32::curri.reg0 = $2;
	   } |
       jop_reg_reg REGISTER REGISTER {
            iris32::curri.reg0 = $2;
            iris32::curri.reg1 = $3;
       } |
       jop_reg_reg_reg REGISTER REGISTER REGISTER {
            iris32::curri.reg0 = $2;
            iris32::curri.reg1 = $3;
            iris32::curri.reg2 = $4;
       }
       ;

compare_op:
          cop REGISTER REGISTER REGISTER {
               iris32::curri.reg0 = $2;
               iris32::curri.reg1 = $3;
               iris32::curri.reg2 = $4;
          } |
		  icop REGISTER REGISTER IMMEDIATE {
		  	if ($4 > 255) {
                  iris32error("immediate value offset out of range!");
			}
			iris32::curri.reg0 = $2;
			iris32::curri.reg1 = $3;
			iris32::curri.reg2 = $4;
		  }
          ;
misc_op:
       MISC_OP_SYSTEMCALL IMMEDIATE REGISTER REGISTER 
       { 
         iris32::curri.op = (byte)iris32::MiscOp::SystemCall; 
         if($2 > 255) {
            iris32error("system call offset out of range!");
         }
         iris32::curri.reg0 = $2;
         iris32::curri.reg1 = $3;
         iris32::curri.reg2 = $4;
       } 
       ;
aop:
   ARITHMETIC_OP_ADD { iris32::curri.op = (byte)iris32::ArithmeticOp::Add; } |
   ARITHMETIC_OP_SUB { iris32::curri.op = (byte)iris32::ArithmeticOp::Sub; } |
   ARITHMETIC_OP_MUL { iris32::curri.op = (byte)iris32::ArithmeticOp::Mul; } |
   ARITHMETIC_OP_DIV { iris32::curri.op = (byte)iris32::ArithmeticOp::Div; } |
   ARITHMETIC_OP_REM { iris32::curri.op = (byte)iris32::ArithmeticOp::Rem; } |
   ARITHMETIC_OP_SHIFTLEFT { iris32::curri.op = (byte)iris32::ArithmeticOp::ShiftLeft; } |
   ARITHMETIC_OP_SHIFTRIGHT { iris32::curri.op = (byte)iris32::ArithmeticOp::ShiftRight; } |
   ARITHMETIC_OP_BINARYAND { iris32::curri.op = (byte)iris32::ArithmeticOp::BinaryAnd; } |
   ARITHMETIC_OP_BINARYOR { iris32::curri.op = (byte)iris32::ArithmeticOp::BinaryOr; } |
   ARITHMETIC_OP_BINARYXOR { iris32::curri.op = (byte)iris32::ArithmeticOp::BinaryXor; } 
   ;

aop_imm:
   ARITHMETIC_OP_ADD_IMM { iris32::curri.op = (byte)iris32::ArithmeticOp::AddImmediate; } |
   ARITHMETIC_OP_SUB_IMM { iris32::curri.op = (byte)iris32::ArithmeticOp::SubImmediate; } |
   ARITHMETIC_OP_MUL_IMM { iris32::curri.op = (byte)iris32::ArithmeticOp::MulImmediate; } | 
   ARITHMETIC_OP_DIV_IMM { iris32::curri.op = (byte)iris32::ArithmeticOp::DivImmediate; } |
   ARITHMETIC_OP_REM_IMM { iris32::curri.op = (byte)iris32::ArithmeticOp::RemImmediate; } |
   ARITHMETIC_OP_SHIFTLEFT_IMM { iris32::curri.op = (byte)iris32::ArithmeticOp::ShiftLeftImmediate; } |
   ARITHMETIC_OP_SHIFTRIGHT_IMM { iris32::curri.op = (byte)iris32::ArithmeticOp::ShiftRightImmediate; } 
   ;

mop_reg:
   MOVE_OP_MOVE { iris32::curri.op = (byte)iris32::MoveOp::Move; } |
   MOVE_OP_SWAP { iris32::curri.op = (byte)iris32::MoveOp::Swap; } |
   MOVE_OP_LOAD { iris32::curri.op = (byte)iris32::MoveOp::Load; } |
   MOVE_OP_STORE { iris32::curri.op = (byte)iris32::MoveOp::Store; } |
   ;


mop_single:
   MOVE_OP_PUSH { iris32::curri.op = (byte)iris32::MoveOp::Push; } |
   MOVE_OP_POP { iris32::curri.op = (byte)iris32::MoveOp::Pop; } 
   ;

jop_reg:
   JUMP_OP_UNCONDITIONALREGISTERLINK { iris32::curri.op = (byte)iris32::JumpOp::UnconditionalRegisterLink; } |
	   JUMP_OP_UNCONDITIONALREGISTER { iris32::curri.op = (byte)iris32::JumpOp::UnconditionalRegister; }
	   ;


jop_reg_reg:
   JUMP_OP_CONDITIONALTRUEREGISTER { iris32::curri.op = (byte)iris32::JumpOp::ConditionalTrueRegister; } |
   JUMP_OP_CONDITIONALFALSEREGISTER { iris32::curri.op = (byte)iris32::JumpOp::ConditionalFalseRegister; }
   ;

jop_reg_reg_reg:
   JUMP_OP_CONDITIONALTRUEREGISTERLINK { iris32::curri.op = (byte)iris32::JumpOp::ConditionalTrueRegisterLink; } |
   JUMP_OP_CONDITIONALFALSEREGISTERLINK { iris32::curri.op = (byte)iris32::JumpOp::ConditionalFalseRegisterLink; } |
   JUMP_OP_IFTHENELSENORMALPREDTRUE { iris32::curri.op = (byte)iris32::JumpOp::IfThenElseNormalPredTrue; } |
   JUMP_OP_IFTHENELSENORMALPREDFALSE { iris32::curri.op = (byte)iris32::JumpOp::IfThenElseNormalPredFalse; } |
   JUMP_OP_IFTHENELSELINKPREDTRUE { iris32::curri.op = (byte)iris32::JumpOp::IfThenElseLinkPredTrue; } |
   JUMP_OP_IFTHENELSELINKPREDFALSE { iris32::curri.op = (byte)iris32::JumpOp::IfThenElseLinkPredFalse; }
;

cop:
   COMPARE_OP_EQ { iris32::curri.op = (byte)iris32::CompareOp::Eq; } |
   COMPARE_OP_NEQ { iris32::curri.op = (byte)iris32::CompareOp::Neq; } |
   COMPARE_OP_LESSTHAN { iris32::curri.op = (byte)iris32::CompareOp::LessThan; } |
   COMPARE_OP_GREATERTHAN { iris32::curri.op = (byte)iris32::CompareOp::GreaterThan; } |
   COMPARE_OP_LESSTHANOREQUALTO { iris32::curri.op = (byte)iris32::CompareOp::LessThanOrEqualTo; } |
   COMPARE_OP_GREATERTHANOREQUALTO { iris32::curri.op = (byte)iris32::CompareOp::GreaterThanOrEqualTo; } 
;
icop:
   COMPARE_OP_EQ_IMMEDIATE { iris32::curri.op = (byte)iris32::CompareOp::EqImm; } |
   COMPARE_OP_NEQ_IMMEDIATE { iris32::curri.op = (byte)iris32::CompareOp::NeqImm; } |
   COMPARE_OP_LESSTHAN_IMMEDIATE { iris32::curri.op = (byte)iris32::CompareOp::LessThanImm; } |
   COMPARE_OP_GREATERTHAN_IMMEDIATE { iris32::curri.op = (byte)iris32::CompareOp::GreaterThanImm; } |
   COMPARE_OP_LESSTHANOREQUALTO_IMMEDIATE { iris32::curri.op = (byte)iris32::CompareOp::LessThanOrEqualToImm; } |
   COMPARE_OP_GREATERTHANOREQUALTO_IMMEDIATE { iris32::curri.op = (byte)iris32::CompareOp::GreaterThanOrEqualToImm; }
;
lexeme:
      SYMBOL { iris32::curri.hassymbol = 1; 
               iris32::curri.symbol = $1; } | 
      IMMEDIATE { 
	  //std::cerr << "IMMEDIATE value = " << std::hex << word($1) << std::endl;
            iris32::curri.reg1 = (byte)(($1 & 0x000000FF));
            iris32::curri.reg2 = (byte)(($1 & 0x0000FF00) >> 8);
			iris32::curri.reg3 = (byte)(($1 & 0x00FF0000) >> 16);
			iris32::curri.reg4 = (byte)(($1 & 0xFF000000) >> 24);
			iris32::curri.dataWord = $1;
      } 
;
%%
void iris32error(const char* s) {
   printf("%d: %s\n", iris32lineno, s);
   exit(-1);
}

namespace iris32 {
	void assemble(FILE* input, std::ostream* output) {
      initialize(output, input);
      do {
         yyparse();
      } while(!feof(iris32in));
      resolve_labels();
	}
}

namespace iris32 {
void add_label_entry(const std::string& c, word addr) {
   if (iris32::state.labels.count(c) != 0) {
		iris32error("Found a duplicate label!");
		exit(1);
   } else {
	 iris32::state.labels[c] = addr;
   }
}

void persist_dynamic_op(void) {
   iris32::state.dynops.push_back(curri);
}

void save_encoding(void) {
   if(iris32::curri.hassymbol) {
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
   iris32::state.output->write(buf, 8);
}

void resolve_labels() {
   /* we need to go through the list of dynamic operations and replace
      the label with the corresponding address */
   for (auto &op : iris32::state.dynops) {
   		if (!resolve_op(&op)) {
			std::cerr << "panic: couldn't find label " << op.symbol << std::endl;
			exit(1);
		} else {
			write_dynamic_op(&op);
		}
   }
}
bool resolve_op(dynamicop* dop) {
   if(iris32::state.labels.count(dop->symbol) == 1) {
		word addr = iris32::state.labels[dop->symbol];
		dop->reg1 = iris::decodeField<word, byte, 0>(addr);
		dop->reg2 = iris::decodeField<word, byte, 1>(addr);
		dop->reg3 = iris::decodeField<word, byte, 2>(addr);
		dop->reg4 = iris::decodeField<word, byte, 3>(addr);
		return true;
   }
   return false;
}

void initialize(std::ostream* output, FILE* input) {
   iris32in = input;
   iris32::state.address = 0;
   iris32::state.output = output;
   iris32::curri.address = 0;
   iris32::curri.group = 0;
   iris32::curri.op = 0;
   iris32::curri.reg0 = 0;
   iris32::curri.reg1 = 0;
   iris32::curri.reg2 = 0;
   iris32::curri.hassymbol = 0;
}
}
