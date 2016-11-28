%define api.prefix {iris20}
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
#include "iris20.h"

#include "iris20_asm.tab.h"

extern int yylex();
extern int yyparse();
extern FILE* iris20in;
extern int iris20lineno;

void iris20error(const char* s);

namespace iris20 {
/* segment */
enum class Segment : byte {
   Code,
   Data,
};
/* used to store ops which require a second pass */
struct dynamicop {
   Segment segment;
   word address;
   byte group;
   byte op;
   byte reg0;
   byte reg1;
   byte reg2;
   int hassymbol;
   std::string symbol;
};
struct asmstate {
	
   ~asmstate() {
   }
   Segment segment;
   word code_address;
   word data_address;
   std::map<std::string, word> labels;
   std::vector<dynamicop> dynops;
   std::ostream* output;
};

asmstate state;
dynamicop curri;

void add_label_entry(const std::string& name, word address);
void persist_dynamic_op(void);
void save_encoding(void);
void write_dynamic_op(dynamicop* op);
void initialize(std::ostream* output, FILE* input);
void resolve_labels(void);
bool resolve_op(dynamicop* dop);
}
%}

%union {
      char* sval;
      byte rval;
      unsigned long ival;
}


%token DIRECTIVE_ORG DIRECTIVE_CODE DIRECTIVE_DATA LABEL DIRECTIVE_DECLARE
%token MOVE_OP_STORE_EXTENDED MOVE_OP_LOAD_EXTENDED
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
%token MOVE_OP_POP
%token MOVE_OP_PUSH
%token MOVE_OP_PUSHIMMEDIATE
%token MOVE_OP_LOAD_CODE
%token MOVE_OP_STORE_CODE
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

/*
%token COMPARE_OP_EQAND
%token COMPARE_OP_EQOR
%token COMPARE_OP_EQXOR
%token COMPARE_OP_NEQAND
%token COMPARE_OP_NEQOR
%token COMPARE_OP_NEQXOR
%token COMPARE_OP_LESSTHANAND
%token COMPARE_OP_LESSTHANOR
%token COMPARE_OP_LESSTHANXOR
%token COMPARE_OP_GREATERTHANOREQUALTOAND
%token COMPARE_OP_GREATERTHANOREQUALTOOR
%token COMPARE_OP_GREATERTHANOREQUALTOXOR
%token COMPARE_OP_LESSTHANOREQUALTOAND
%token COMPARE_OP_LESSTHANOREQUALTOOR
%token COMPARE_OP_LESSTHANOREQUALTOXOR
%token COMPARE_OP_GREATERTHANAND
%token COMPARE_OP_GREATERTHANOR
%token COMPARE_OP_GREATERTHANXOR
// from cop
   COMPARE_OP_EQAND { iris20::curri.op = (byte)iris20::CompareOp::EqAnd; } |
   COMPARE_OP_EQOR { iris20::curri.op = (byte)iris20::CompareOp::EqOr; } |
   COMPARE_OP_EQXOR { iris20::curri.op = (byte)iris20::CompareOp::EqXor; } |
   COMPARE_OP_NEQAND { iris20::curri.op = (byte)iris20::CompareOp::NeqAnd; } |
   COMPARE_OP_NEQOR { iris20::curri.op = (byte)iris20::CompareOp::NeqOr; } |
   COMPARE_OP_NEQXOR { iris20::curri.op = (byte)iris20::CompareOp::NeqXor; } |
   COMPARE_OP_LESSTHANAND { iris20::curri.op = (byte)iris20::CompareOp::LessThanAnd; } |
   COMPARE_OP_LESSTHANOR { iris20::curri.op = (byte)iris20::CompareOp::LessThanOr; } |
   COMPARE_OP_LESSTHANXOR { iris20::curri.op = (byte)iris20::CompareOp::LessThanXor; } |
   COMPARE_OP_GREATERTHANAND { iris20::curri.op = (byte)iris20::CompareOp::GreaterThanAnd; } |
   COMPARE_OP_GREATERTHANOR { iris20::curri.op = (byte)iris20::CompareOp::GreaterThanOr; } |
   COMPARE_OP_GREATERTHANXOR { iris20::curri.op = (byte)iris20::CompareOp::GreaterThanXor; } |
   COMPARE_OP_LESSTHANOREQUALTOAND { iris20::curri.op = (byte)iris20::CompareOp::LessThanOrEqualToAnd; } |
   COMPARE_OP_LESSTHANOREQUALTOOR { iris20::curri.op = (byte)iris20::CompareOp::LessThanOrEqualToOr; } |
   COMPARE_OP_LESSTHANOREQUALTOXOR { iris20::curri.op = (byte)iris20::CompareOp::LessThanOrEqualToXor; } |
   COMPARE_OP_GREATERTHANOREQUALTOAND { iris20::curri.op = (byte)iris20::CompareOp::GreaterThanOrEqualToAnd; } |
   COMPARE_OP_GREATERTHANOREQUALTOOR { iris20::curri.op = (byte)iris20::CompareOp::GreaterThanOrEqualToOr; } |
   COMPARE_OP_GREATERTHANOREQUALTOXOR { iris20::curri.op = (byte)iris20::CompareOp::GreaterThanOrEqualToXor; } |
*/

%token <rval> REGISTER
%token <ival> IMMEDIATE
%token <sval> IRIS16_SYMBOL

%%
Q: /* empty */ |
   F 
;
F:
   F asm {
      iris20::curri.segment = iris20::Segment::Code;
      iris20::curri.address = 0;
      iris20::curri.group = 0;
      iris20::curri.op = 0;
      iris20::curri.reg0 = 0;
      iris20::curri.reg1 = 0;
      iris20::curri.reg2 = 0;
      iris20::curri.hassymbol = 0;
      iris20::curri.symbol = "";
   } | 
   asm {
      iris20::curri.segment = iris20::Segment::Code;
      iris20::curri.address = 0;
      iris20::curri.group = 0;
      iris20::curri.op = 0;
      iris20::curri.reg0 = 0;
      iris20::curri.reg1 = 0;
      iris20::curri.reg2 = 0;
      iris20::curri.hassymbol = 0;
      iris20::curri.symbol = "";
   }
   ;
asm:
   directive |
   statement
   ;
directive:
         DIRECTIVE_ORG IMMEDIATE {
            if(iris20::state.segment == iris20::Segment::Code) {
               iris20::state.code_address = $2;
            } else if(iris20::state.segment == iris20::Segment::Data) {
               iris20::state.data_address = $2;
            } else {
               iris20error("Invalid segment!");
            }
            } | 
      DIRECTIVE_CODE { iris20::state.segment = iris20::Segment::Code; } |
      DIRECTIVE_DATA { iris20::state.segment = iris20::Segment::Data; } |
      DIRECTIVE_DECLARE lexeme { 
            if(iris20::state.segment == iris20::Segment::Data) {
               iris20::curri.segment = iris20::Segment::Data;
               iris20::curri.address = iris20::state.data_address;
               iris20::save_encoding();
               iris20::state.data_address++;
            } else {
               iris20error("Declaration in non-data segment!");
            }
      }
      ;
statement:
         label { iris20::curri.segment = iris20::state.segment; }|
         operation {
            if(iris20::state.segment == iris20::Segment::Code) {
               iris20::curri.segment = iris20::Segment::Code;
               iris20::curri.address = iris20::state.code_address;
               iris20::save_encoding();
               iris20::state.code_address++;
            } else {
               iris20error("operation in an invalid segment!");
            }
         }
         ;
label:
     LABEL IRIS16_SYMBOL { 
      if(iris20::state.segment == iris20::Segment::Code) {
          iris20::add_label_entry($2, iris20::state.code_address);
      } else if (iris20::state.segment == iris20::Segment::Data) {
          iris20::add_label_entry($2, iris20::state.data_address);
      } else {
          iris20error("label in invalid segment!");
      }
     }
   ;
operation:
         arithmetic_op { iris20::curri.group = (byte)iris20::InstructionGroup::Arithmetic; } |
         move_op { iris20::curri.group = (byte)iris20::InstructionGroup::Move; } |
         jump_op { iris20::curri.group = (byte)iris20::InstructionGroup::Jump; } |
         compare_op { iris20::curri.group = (byte)iris20::InstructionGroup::Compare; } |
         misc_op { iris20::curri.group = (byte)iris20::InstructionGroup::Misc; }
         ;
arithmetic_op:
             aop REGISTER REGISTER REGISTER {
                  iris20::curri.reg0 = $2;
                  iris20::curri.reg1 = $3;
                  iris20::curri.reg2 = $4;
             }|
             ARITHMETIC_OP_BINARYNOT REGISTER REGISTER {
                  iris20::curri.reg0 = $2;
                  iris20::curri.reg1 = $3;
             } |
             aop_imm REGISTER REGISTER IMMEDIATE {
               if($4 > 255) {
                  iris20error("immediate value offset out of range!");
               }
               iris20::curri.reg0 = $2;
               iris20::curri.reg1 = $3;
               iris20::curri.reg2 = $4;
             } |
			 aop_single_macro REGISTER {
			 	iris20::curri.reg0 = $2;
				iris20::curri.reg1 = $2;
			 }
      ;
aop_single_macro:
   ARITHMETIC_MACRO_OP_INCR { 
     iris20::curri.op = (byte)iris20::ArithmeticOp::AddImmediate;
     iris20::curri.reg2 = 1;
   } |
   ARITHMETIC_MACRO_OP_DECR { 
     iris20::curri.op = (byte)iris20::ArithmeticOp::SubImmediate;  
	 iris20::curri.reg2 = 1;
   } |
   ARITHMETIC_MACRO_OP_HALVE { 
     iris20::curri.op = (byte)iris20::ArithmeticOp::DivImmediate; 
     iris20::curri.reg2 = 2;
   } |
   ARITHMETIC_MACRO_OP_DOUBLE {
     iris20::curri.op = (byte)iris20::ArithmeticOp::MulImmediate; 
     iris20::curri.reg2 = 2;
   }
   ;
move_op:
       mop_reg REGISTER REGISTER {
            iris20::curri.reg0 = $2;
            iris20::curri.reg1 = $3;
       } |
       mop_mixed REGISTER lexeme { iris20::curri.reg0 = $2; } |
       mop_single REGISTER {
         iris20::curri.reg0 = $2;
       } |
       MOVE_OP_PUSHIMMEDIATE lexeme { 
         iris20::curri.op = (byte)iris20::MoveOp::PushImmediate;
       } |
	   MOVE_OP_STORE_CODE REGISTER REGISTER REGISTER {
	    iris20::curri.op = static_cast<byte>(iris20::MoveOp::StoreCode);
		iris20::curri.reg0 = $2;
		iris20::curri.reg1 = $3;
		iris20::curri.reg2 = $4;
	   } |
	   MOVE_OP_LOAD_CODE REGISTER REGISTER REGISTER {
	    iris20::curri.op = static_cast<byte>(iris20::MoveOp::LoadCode);
	   	iris20::curri.reg0 = $2;
		iris20::curri.reg1 = $3;
		iris20::curri.reg2 = $4;
	   } |
	   MOVE_OP_LOAD_EXTENDED REGISTER REGISTER REGISTER {
	    iris20::curri.op = static_cast<byte>(iris20::MoveOp::ExtendedMemoryRead);
	   	iris20::curri.reg0 = $2;
		iris20::curri.reg1 = $3;
		iris20::curri.reg2 = $4;
	   } |
	   MOVE_OP_STORE_EXTENDED REGISTER REGISTER REGISTER {
	    iris20::curri.op = static_cast<byte>(iris20::MoveOp::ExtendedMemoryWrite);
	   	iris20::curri.reg0 = $2;
		iris20::curri.reg1 = $3;
		iris20::curri.reg2 = $4;
	   } 
       ;

jump_op:
       JUMP_OP_UNCONDITIONALIMMEDIATE lexeme { 
         iris20::curri.op = (byte)iris20::JumpOp::UnconditionalImmediate; 
         } | 
       JUMP_OP_UNCONDITIONALREGISTER REGISTER { 
         iris20::curri.op = (byte)iris20::JumpOp::UnconditionalRegister; 
         iris20::curri.reg0 = $2;
       } |
       jop_reg_reg REGISTER REGISTER {
            iris20::curri.reg0 = $2;
            iris20::curri.reg1 = $3;
       } |
       jop_reg_imm REGISTER lexeme { iris20::curri.reg0 = $2; } |
       jop_reg_reg_reg REGISTER REGISTER REGISTER {
            iris20::curri.reg0 = $2;
            iris20::curri.reg1 = $3;
            iris20::curri.reg2 = $4;
       }
       ;

compare_op:
          cop REGISTER REGISTER REGISTER {
               iris20::curri.reg0 = $2;
               iris20::curri.reg1 = $3;
               iris20::curri.reg2 = $4;
          } |
		  icop REGISTER REGISTER IMMEDIATE {
		  	if ($4 > 255) {
                  iris20error("immediate value offset out of range!");
			}
			iris20::curri.reg0 = $2;
			iris20::curri.reg1 = $3;
			iris20::curri.reg2 = $4;
		  }
          ;
misc_op:
       MISC_OP_SYSTEMCALL IMMEDIATE REGISTER REGISTER 
       { 
         iris20::curri.op = (byte)iris20::MiscOp::SystemCall; 
         if($2 > 255) {
            iris20error("system call offset out of range!");
         }
         iris20::curri.reg0 = $2;
         iris20::curri.reg1 = $3;
         iris20::curri.reg2 = $4;
       } 
       ;
aop:
   ARITHMETIC_OP_ADD { iris20::curri.op = (byte)iris20::ArithmeticOp::Add; } |
   ARITHMETIC_OP_SUB { iris20::curri.op = (byte)iris20::ArithmeticOp::Sub; } |
   ARITHMETIC_OP_MUL { iris20::curri.op = (byte)iris20::ArithmeticOp::Mul; } |
   ARITHMETIC_OP_DIV { iris20::curri.op = (byte)iris20::ArithmeticOp::Div; } |
   ARITHMETIC_OP_REM { iris20::curri.op = (byte)iris20::ArithmeticOp::Rem; } |
   ARITHMETIC_OP_SHIFTLEFT { iris20::curri.op = (byte)iris20::ArithmeticOp::ShiftLeft; } |
   ARITHMETIC_OP_SHIFTRIGHT { iris20::curri.op = (byte)iris20::ArithmeticOp::ShiftRight; } |
   ARITHMETIC_OP_BINARYAND { iris20::curri.op = (byte)iris20::ArithmeticOp::BinaryAnd; } |
   ARITHMETIC_OP_BINARYOR { iris20::curri.op = (byte)iris20::ArithmeticOp::BinaryOr; } |
   ARITHMETIC_OP_BINARYXOR { iris20::curri.op = (byte)iris20::ArithmeticOp::BinaryXor; } 
   ;

aop_imm:
   ARITHMETIC_OP_ADD_IMM { iris20::curri.op = (byte)iris20::ArithmeticOp::AddImmediate; } |
   ARITHMETIC_OP_SUB_IMM { iris20::curri.op = (byte)iris20::ArithmeticOp::SubImmediate; } |
   ARITHMETIC_OP_MUL_IMM { iris20::curri.op = (byte)iris20::ArithmeticOp::MulImmediate; } | 
   ARITHMETIC_OP_DIV_IMM { iris20::curri.op = (byte)iris20::ArithmeticOp::DivImmediate; } |
   ARITHMETIC_OP_REM_IMM { iris20::curri.op = (byte)iris20::ArithmeticOp::RemImmediate; } |
   ARITHMETIC_OP_SHIFTLEFT_IMM { iris20::curri.op = (byte)iris20::ArithmeticOp::ShiftLeftImmediate; } |
   ARITHMETIC_OP_SHIFTRIGHT_IMM { iris20::curri.op = (byte)iris20::ArithmeticOp::ShiftRightImmediate; } 
   ;

mop_reg:
   MOVE_OP_MOVE { iris20::curri.op = (byte)iris20::MoveOp::Move; } |
   MOVE_OP_SWAP { iris20::curri.op = (byte)iris20::MoveOp::Swap; } |
   MOVE_OP_LOAD { iris20::curri.op = (byte)iris20::MoveOp::Load; } |
   MOVE_OP_STORE { iris20::curri.op = (byte)iris20::MoveOp::Store; } |
   ;

mop_mixed:
   MOVE_OP_SET { iris20::curri.op = (byte)iris20::MoveOp::Set; } |
   MOVE_OP_STOREIMM { iris20::curri.op = (byte)iris20::MoveOp::Memset; } |
   MOVE_OP_LOADMEM { iris20::curri.op = (byte)iris20::MoveOp::LoadImmediate; } 
   ;

mop_single:
   MOVE_OP_PUSH { iris20::curri.op = (byte)iris20::MoveOp::Push; } |
   MOVE_OP_POP { iris20::curri.op = (byte)iris20::MoveOp::Pop; } 
   ;

jop_reg_imm:
   JUMP_OP_UNCONDITIONALIMMEDIATELINK { iris20::curri.op = (byte)iris20::JumpOp::UnconditionalImmediateLink; } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATE { iris20::curri.op = (byte)iris20::JumpOp::ConditionalTrueImmediate; } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATELINK { iris20::curri.op = (byte)iris20::JumpOp::ConditionalTrueImmediateLink; } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATE { iris20::curri.op = (byte)iris20::JumpOp::ConditionalFalseImmediate; } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATELINK { iris20::curri.op = (byte)iris20::JumpOp::ConditionalFalseImmediateLink; } 
   ;


jop_reg_reg:
   JUMP_OP_UNCONDITIONALREGISTERLINK { iris20::curri.op = (byte)iris20::JumpOp::UnconditionalRegisterLink; } |
   JUMP_OP_CONDITIONALTRUEREGISTER { iris20::curri.op = (byte)iris20::JumpOp::ConditionalTrueRegister; } |
   JUMP_OP_CONDITIONALFALSEREGISTER { iris20::curri.op = (byte)iris20::JumpOp::ConditionalFalseRegister; }
   ;

jop_reg_reg_reg:
   JUMP_OP_CONDITIONALTRUEREGISTERLINK { iris20::curri.op = (byte)iris20::JumpOp::ConditionalTrueRegisterLink; } |
   JUMP_OP_CONDITIONALFALSEREGISTERLINK { iris20::curri.op = (byte)iris20::JumpOp::ConditionalFalseRegisterLink; } |
   JUMP_OP_IFTHENELSENORMALPREDTRUE { iris20::curri.op = (byte)iris20::JumpOp::IfThenElseNormalPredTrue; } |
   JUMP_OP_IFTHENELSENORMALPREDFALSE { iris20::curri.op = (byte)iris20::JumpOp::IfThenElseNormalPredFalse; } |
   JUMP_OP_IFTHENELSELINKPREDTRUE { iris20::curri.op = (byte)iris20::JumpOp::IfThenElseLinkPredTrue; } |
   JUMP_OP_IFTHENELSELINKPREDFALSE { iris20::curri.op = (byte)iris20::JumpOp::IfThenElseLinkPredFalse; }
;

cop:
   COMPARE_OP_EQ { iris20::curri.op = (byte)iris20::CompareOp::Eq; } |
   COMPARE_OP_NEQ { iris20::curri.op = (byte)iris20::CompareOp::Neq; } |
   COMPARE_OP_LESSTHAN { iris20::curri.op = (byte)iris20::CompareOp::LessThan; } |
   COMPARE_OP_GREATERTHAN { iris20::curri.op = (byte)iris20::CompareOp::GreaterThan; } |
   COMPARE_OP_LESSTHANOREQUALTO { iris20::curri.op = (byte)iris20::CompareOp::LessThanOrEqualTo; } |
   COMPARE_OP_GREATERTHANOREQUALTO { iris20::curri.op = (byte)iris20::CompareOp::GreaterThanOrEqualTo; } 
;
icop:
   COMPARE_OP_EQ_IMMEDIATE { iris20::curri.op = (byte)iris20::CompareOp::EqImm; } |
   COMPARE_OP_NEQ_IMMEDIATE { iris20::curri.op = (byte)iris20::CompareOp::NeqImm; } |
   COMPARE_OP_LESSTHAN_IMMEDIATE { iris20::curri.op = (byte)iris20::CompareOp::LessThanImm; } |
   COMPARE_OP_GREATERTHAN_IMMEDIATE { iris20::curri.op = (byte)iris20::CompareOp::GreaterThanImm; } |
   COMPARE_OP_LESSTHANOREQUALTO_IMMEDIATE { iris20::curri.op = (byte)iris20::CompareOp::LessThanOrEqualToImm; } |
   COMPARE_OP_GREATERTHANOREQUALTO_IMMEDIATE { iris20::curri.op = (byte)iris20::CompareOp::GreaterThanOrEqualToImm; }
;
lexeme:
      IRIS16_SYMBOL { iris20::curri.hassymbol = 1; 
               iris20::curri.symbol = $1; } | 
      IMMEDIATE { 
            iris20::curri.reg1 = (byte)(($1 & 0x00FF));
            iris20::curri.reg2 = (byte)(($1 & 0xFF00) >> 8);
      }
;
%%
void iris20error(const char* s) {
   printf("%d: %s\n", iris20lineno, s);
   exit(-1);
}

namespace iris20 {
	void assemble(FILE* input, std::ostream* output) {
	  initialize(output, input);
	  do {
		 yyparse();
	  } while(!feof(iris20in));
	  resolve_labels();
	}

void add_label_entry(const std::string& c, word addr) {
   if (iris20::state.labels.count(c) != 0) {
		iris20error("Found a duplicate label!");
		exit(1);
   } else {
	 iris20::state.labels[c] = addr;
   }
}

void persist_dynamic_op(void) {
   iris20::state.dynops.push_back(curri);
}

void save_encoding(void) {
   if(iris20::curri.hassymbol) {
	  persist_dynamic_op();
   } else {
	  write_dynamic_op(&curri); 
   }
}
void write_dynamic_op(dynamicop* dop) {
   /* ((instruction & ~mask) | (value << shiftcount)) */
   /* little endian build up */
   char* buf = new char[8];
   buf[0] = 0;
   buf[1] = (char)dop->segment;
   buf[2] = (char)(dop->address & 0x00FF);
   buf[3] = (char)((dop->address & 0xFF00) >> 8);
   switch(dop->segment) {
		case iris20::Segment::Code:
			buf[4] = (char)iris::encodeBits<byte, byte, 0b11111000, 3>(
								iris::encodeBits<byte, byte, 0b00000111, 0>((byte)0, dop->group),
								dop->op);
			buf[5] = (char)dop->reg0;
			buf[6] = (char)dop->reg1;
			buf[7] = (char)dop->reg2;
			break;
		case iris20::Segment::Data:
			buf[4] = (char)dop->reg1;
			buf[5] = (char)dop->reg2;
			buf[6] = 0;
			buf[7] = 0;
			break;
		default:
			std::cerr << "panic: unknown segment " << (byte)dop->segment << std::endl;
			exit(1);
   }
   iris20::state.output->write(buf, 8);
   delete[] buf;
}

void resolve_labels() {
   /* we need to go through the list of dynamic operations and replace
	  the label with the corresponding address */
   for(std::vector<dynamicop>::iterator it = iris20::state.dynops.begin(); it != iris20::state.dynops.end(); ++it) {
		if (!resolve_op(&(*it))) {
			std::cerr << "panic: couldn't find label " << it->symbol << std::endl;
			exit(1);
		} else {
			write_dynamic_op(&(*it));
		}
   }
}
bool resolve_op(dynamicop* dop) {
   if(iris20::state.labels.count(dop->symbol) == 1) {
		word addr = iris20::state.labels[dop->symbol];
		dop->reg1 = iris::decodeField<word, byte, 0>(addr);
		dop->reg2 = iris::decodeField<word, byte, 1>(addr);
		return true;
   }
   return false;
}

void initialize(std::ostream* output, FILE* input) {
   iris20in = input;
   iris20::state.segment = iris20::Segment::Code;
   iris20::state.data_address = 0;
   iris20::state.code_address = 0;
   iris20::state.output = output;
   iris20::curri.segment = iris20::Segment::Code;
   iris20::curri.address = 0;
   iris20::curri.group = 0;
   iris20::curri.op = 0;
   iris20::curri.reg0 = 0;
   iris20::curri.reg1 = 0;
   iris20::curri.reg2 = 0;
   iris20::curri.hassymbol = 0;
}
}
