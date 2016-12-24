%define api.prefix {iris16}
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
#include "iris16.h"

#include "iris16_asm.tab.h"

extern int yylex();
extern int yyparse();
extern FILE* iris16in;
extern int iris16lineno;

void iris16error(const char* s);

namespace iris16 {
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
%token MOVE_OP_STORE_IO MOVE_OP_LOAD_IO
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
   COMPARE_OP_EQAND { iris16::curri.op = (byte)iris16::CompareOp::EqAnd; } |
   COMPARE_OP_EQOR { iris16::curri.op = (byte)iris16::CompareOp::EqOr; } |
   COMPARE_OP_EQXOR { iris16::curri.op = (byte)iris16::CompareOp::EqXor; } |
   COMPARE_OP_NEQAND { iris16::curri.op = (byte)iris16::CompareOp::NeqAnd; } |
   COMPARE_OP_NEQOR { iris16::curri.op = (byte)iris16::CompareOp::NeqOr; } |
   COMPARE_OP_NEQXOR { iris16::curri.op = (byte)iris16::CompareOp::NeqXor; } |
   COMPARE_OP_LESSTHANAND { iris16::curri.op = (byte)iris16::CompareOp::LessThanAnd; } |
   COMPARE_OP_LESSTHANOR { iris16::curri.op = (byte)iris16::CompareOp::LessThanOr; } |
   COMPARE_OP_LESSTHANXOR { iris16::curri.op = (byte)iris16::CompareOp::LessThanXor; } |
   COMPARE_OP_GREATERTHANAND { iris16::curri.op = (byte)iris16::CompareOp::GreaterThanAnd; } |
   COMPARE_OP_GREATERTHANOR { iris16::curri.op = (byte)iris16::CompareOp::GreaterThanOr; } |
   COMPARE_OP_GREATERTHANXOR { iris16::curri.op = (byte)iris16::CompareOp::GreaterThanXor; } |
   COMPARE_OP_LESSTHANOREQUALTOAND { iris16::curri.op = (byte)iris16::CompareOp::LessThanOrEqualToAnd; } |
   COMPARE_OP_LESSTHANOREQUALTOOR { iris16::curri.op = (byte)iris16::CompareOp::LessThanOrEqualToOr; } |
   COMPARE_OP_LESSTHANOREQUALTOXOR { iris16::curri.op = (byte)iris16::CompareOp::LessThanOrEqualToXor; } |
   COMPARE_OP_GREATERTHANOREQUALTOAND { iris16::curri.op = (byte)iris16::CompareOp::GreaterThanOrEqualToAnd; } |
   COMPARE_OP_GREATERTHANOREQUALTOOR { iris16::curri.op = (byte)iris16::CompareOp::GreaterThanOrEqualToOr; } |
   COMPARE_OP_GREATERTHANOREQUALTOXOR { iris16::curri.op = (byte)iris16::CompareOp::GreaterThanOrEqualToXor; } |
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
      iris16::curri.segment = iris16::Segment::Code;
      iris16::curri.address = 0;
      iris16::curri.group = 0;
      iris16::curri.op = 0;
      iris16::curri.reg0 = 0;
      iris16::curri.reg1 = 0;
      iris16::curri.reg2 = 0;
      iris16::curri.hassymbol = 0;
      iris16::curri.symbol = "";
   } | 
   asm {
      iris16::curri.segment = iris16::Segment::Code;
      iris16::curri.address = 0;
      iris16::curri.group = 0;
      iris16::curri.op = 0;
      iris16::curri.reg0 = 0;
      iris16::curri.reg1 = 0;
      iris16::curri.reg2 = 0;
      iris16::curri.hassymbol = 0;
      iris16::curri.symbol = "";
   }
   ;
asm:
   directive |
   statement
   ;
directive:
         DIRECTIVE_ORG IMMEDIATE {
            if(iris16::state.segment == iris16::Segment::Code) {
               iris16::state.code_address = $2;
            } else if(iris16::state.segment == iris16::Segment::Data) {
               iris16::state.data_address = $2;
            } else {
               iris16error("Invalid segment!");
            }
            } | 
      DIRECTIVE_CODE { iris16::state.segment = iris16::Segment::Code; } |
      DIRECTIVE_DATA { iris16::state.segment = iris16::Segment::Data; } |
      DIRECTIVE_DECLARE lexeme { 
            if(iris16::state.segment == iris16::Segment::Data) {
               iris16::curri.segment = iris16::Segment::Data;
               iris16::curri.address = iris16::state.data_address;
               iris16::save_encoding();
               iris16::state.data_address++;
            } else {
               iris16error("Declaration in non-data segment!");
            }
      }
      ;
statement:
         label { iris16::curri.segment = iris16::state.segment; }|
         operation {
            if(iris16::state.segment == iris16::Segment::Code) {
               iris16::curri.segment = iris16::Segment::Code;
               iris16::curri.address = iris16::state.code_address;
               iris16::save_encoding();
               iris16::state.code_address++;
            } else {
               iris16error("operation in an invalid segment!");
            }
         }
         ;
label:
     LABEL IRIS16_SYMBOL { 
      if(iris16::state.segment == iris16::Segment::Code) {
          iris16::add_label_entry($2, iris16::state.code_address);
      } else if (iris16::state.segment == iris16::Segment::Data) {
          iris16::add_label_entry($2, iris16::state.data_address);
      } else {
          iris16error("label in invalid segment!");
      }
     }
   ;
operation:
         arithmetic_op { iris16::curri.group = static_cast<byte>(iris16::InstructionGroup::Arithmetic); } |
         move_op { iris16::curri.group = static_cast<byte>(iris16::InstructionGroup::Move); } |
         jump_op { iris16::curri.group = static_cast<byte>(iris16::InstructionGroup::Jump); } |
         compare_op { iris16::curri.group = static_cast<byte>(iris16::InstructionGroup::Compare); };
arithmetic_op:
             aop REGISTER REGISTER REGISTER {
                  iris16::curri.reg0 = $2;
                  iris16::curri.reg1 = $3;
                  iris16::curri.reg2 = $4;
             }|
             ARITHMETIC_OP_BINARYNOT REGISTER REGISTER {
                  iris16::curri.reg0 = $2;
                  iris16::curri.reg1 = $3;
             } |
             aop_imm REGISTER REGISTER IMMEDIATE {
               if($4 > 255) {
                  iris16error("immediate value offset out of range!");
               }
               iris16::curri.reg0 = $2;
               iris16::curri.reg1 = $3;
               iris16::curri.reg2 = $4;
             } |
			 aop_single_macro REGISTER {
			 	iris16::curri.reg0 = $2;
				iris16::curri.reg1 = $2;
			 }
      ;
aop_single_macro:
   ARITHMETIC_MACRO_OP_INCR { 
     iris16::curri.op = (byte)iris16::ArithmeticOp::AddImmediate;
     iris16::curri.reg2 = 1;
   } |
   ARITHMETIC_MACRO_OP_DECR { 
     iris16::curri.op = (byte)iris16::ArithmeticOp::SubImmediate;  
	 iris16::curri.reg2 = 1;
   } |
   ARITHMETIC_MACRO_OP_HALVE { 
     iris16::curri.op = (byte)iris16::ArithmeticOp::DivImmediate; 
     iris16::curri.reg2 = 2;
   } |
   ARITHMETIC_MACRO_OP_DOUBLE {
     iris16::curri.op = (byte)iris16::ArithmeticOp::MulImmediate; 
     iris16::curri.reg2 = 2;
   }
   ;
move_op:
       mop_reg REGISTER REGISTER {
            iris16::curri.reg0 = $2;
            iris16::curri.reg1 = $3;
       } |
       mop_mixed REGISTER lexeme { iris16::curri.reg0 = $2; } |
       mop_single REGISTER {
         iris16::curri.reg0 = $2;
       } |
       MOVE_OP_PUSHIMMEDIATE lexeme { 
         iris16::curri.op = (byte)iris16::MoveOp::PushImmediate;
       } |
	   MOVE_OP_STORE_CODE REGISTER REGISTER REGISTER {
	    iris16::curri.op = static_cast<byte>(iris16::MoveOp::StoreCode);
		iris16::curri.reg0 = $2;
		iris16::curri.reg1 = $3;
		iris16::curri.reg2 = $4;
	   } |
	   MOVE_OP_LOAD_CODE REGISTER REGISTER REGISTER {
	    iris16::curri.op = static_cast<byte>(iris16::MoveOp::LoadCode);
	   	iris16::curri.reg0 = $2;
		iris16::curri.reg1 = $3;
		iris16::curri.reg2 = $4;
	   } |
	   MOVE_OP_LOAD_IO REGISTER REGISTER {
	    iris16::curri.op = static_cast<byte>(iris16::MoveOp::IORead);
	   	iris16::curri.reg0 = $2;
		iris16::curri.reg1 = $3;
	   } |
	   MOVE_OP_STORE_IO REGISTER REGISTER {
	    iris16::curri.op = static_cast<byte>(iris16::MoveOp::IOWrite);
	   	iris16::curri.reg0 = $2;
		iris16::curri.reg1 = $3;
	   } 
       ;

jump_op:
       JUMP_OP_UNCONDITIONALIMMEDIATE lexeme { 
         iris16::curri.op = (byte)iris16::JumpOp::UnconditionalImmediate; 
         } | 
       JUMP_OP_UNCONDITIONALREGISTER REGISTER { 
         iris16::curri.op = (byte)iris16::JumpOp::UnconditionalRegister; 
         iris16::curri.reg0 = $2;
       } |
       jop_reg_reg REGISTER REGISTER {
            iris16::curri.reg0 = $2;
            iris16::curri.reg1 = $3;
       } |
       jop_reg_imm REGISTER lexeme { iris16::curri.reg0 = $2; } |
       jop_reg_reg_reg REGISTER REGISTER REGISTER {
            iris16::curri.reg0 = $2;
            iris16::curri.reg1 = $3;
            iris16::curri.reg2 = $4;
       }
       ;

compare_op:
          cop REGISTER REGISTER REGISTER {
               iris16::curri.reg0 = $2;
               iris16::curri.reg1 = $3;
               iris16::curri.reg2 = $4;
          } |
		  icop REGISTER REGISTER IMMEDIATE {
		  	if ($4 > 255) {
                  iris16error("immediate value offset out of range!");
			}
			iris16::curri.reg0 = $2;
			iris16::curri.reg1 = $3;
			iris16::curri.reg2 = $4;
		  }
          ;
aop:
   ARITHMETIC_OP_ADD { iris16::curri.op = (byte)iris16::ArithmeticOp::Add; } |
   ARITHMETIC_OP_SUB { iris16::curri.op = (byte)iris16::ArithmeticOp::Sub; } |
   ARITHMETIC_OP_MUL { iris16::curri.op = (byte)iris16::ArithmeticOp::Mul; } |
   ARITHMETIC_OP_DIV { iris16::curri.op = (byte)iris16::ArithmeticOp::Div; } |
   ARITHMETIC_OP_REM { iris16::curri.op = (byte)iris16::ArithmeticOp::Rem; } |
   ARITHMETIC_OP_SHIFTLEFT { iris16::curri.op = (byte)iris16::ArithmeticOp::ShiftLeft; } |
   ARITHMETIC_OP_SHIFTRIGHT { iris16::curri.op = (byte)iris16::ArithmeticOp::ShiftRight; } |
   ARITHMETIC_OP_BINARYAND { iris16::curri.op = (byte)iris16::ArithmeticOp::BinaryAnd; } |
   ARITHMETIC_OP_BINARYOR { iris16::curri.op = (byte)iris16::ArithmeticOp::BinaryOr; } |
   ARITHMETIC_OP_BINARYXOR { iris16::curri.op = (byte)iris16::ArithmeticOp::BinaryXor; } 
   ;

aop_imm:
   ARITHMETIC_OP_ADD_IMM { iris16::curri.op = (byte)iris16::ArithmeticOp::AddImmediate; } |
   ARITHMETIC_OP_SUB_IMM { iris16::curri.op = (byte)iris16::ArithmeticOp::SubImmediate; } |
   ARITHMETIC_OP_MUL_IMM { iris16::curri.op = (byte)iris16::ArithmeticOp::MulImmediate; } | 
   ARITHMETIC_OP_DIV_IMM { iris16::curri.op = (byte)iris16::ArithmeticOp::DivImmediate; } |
   ARITHMETIC_OP_REM_IMM { iris16::curri.op = (byte)iris16::ArithmeticOp::RemImmediate; } |
   ARITHMETIC_OP_SHIFTLEFT_IMM { iris16::curri.op = (byte)iris16::ArithmeticOp::ShiftLeftImmediate; } |
   ARITHMETIC_OP_SHIFTRIGHT_IMM { iris16::curri.op = (byte)iris16::ArithmeticOp::ShiftRightImmediate; } 
   ;

mop_reg:
   MOVE_OP_MOVE { iris16::curri.op = (byte)iris16::MoveOp::Move; } |
   MOVE_OP_SWAP { iris16::curri.op = (byte)iris16::MoveOp::Swap; } |
   MOVE_OP_LOAD { iris16::curri.op = (byte)iris16::MoveOp::Load; } |
   MOVE_OP_STORE { iris16::curri.op = (byte)iris16::MoveOp::Store; } |
   ;

mop_mixed:
   MOVE_OP_SET { iris16::curri.op = (byte)iris16::MoveOp::Set; } |
   MOVE_OP_STOREIMM { iris16::curri.op = (byte)iris16::MoveOp::Memset; } |
   MOVE_OP_LOADMEM { iris16::curri.op = (byte)iris16::MoveOp::LoadImmediate; } 
   ;

mop_single:
   MOVE_OP_PUSH { iris16::curri.op = (byte)iris16::MoveOp::Push; } |
   MOVE_OP_POP { iris16::curri.op = (byte)iris16::MoveOp::Pop; } 
   ;

jop_reg_imm:
   JUMP_OP_UNCONDITIONALIMMEDIATELINK { iris16::curri.op = (byte)iris16::JumpOp::UnconditionalImmediateLink; } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATE { iris16::curri.op = (byte)iris16::JumpOp::ConditionalTrueImmediate; } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATELINK { iris16::curri.op = (byte)iris16::JumpOp::ConditionalTrueImmediateLink; } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATE { iris16::curri.op = (byte)iris16::JumpOp::ConditionalFalseImmediate; } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATELINK { iris16::curri.op = (byte)iris16::JumpOp::ConditionalFalseImmediateLink; } 
   ;


jop_reg_reg:
   JUMP_OP_UNCONDITIONALREGISTERLINK { iris16::curri.op = (byte)iris16::JumpOp::UnconditionalRegisterLink; } |
   JUMP_OP_CONDITIONALTRUEREGISTER { iris16::curri.op = (byte)iris16::JumpOp::ConditionalTrueRegister; } |
   JUMP_OP_CONDITIONALFALSEREGISTER { iris16::curri.op = (byte)iris16::JumpOp::ConditionalFalseRegister; }
   ;

jop_reg_reg_reg:
   JUMP_OP_CONDITIONALTRUEREGISTERLINK { iris16::curri.op = (byte)iris16::JumpOp::ConditionalTrueRegisterLink; } |
   JUMP_OP_CONDITIONALFALSEREGISTERLINK { iris16::curri.op = (byte)iris16::JumpOp::ConditionalFalseRegisterLink; } |
   JUMP_OP_IFTHENELSENORMALPREDTRUE { iris16::curri.op = (byte)iris16::JumpOp::IfThenElseNormalPredTrue; } |
   JUMP_OP_IFTHENELSENORMALPREDFALSE { iris16::curri.op = (byte)iris16::JumpOp::IfThenElseNormalPredFalse; } |
   JUMP_OP_IFTHENELSELINKPREDTRUE { iris16::curri.op = (byte)iris16::JumpOp::IfThenElseLinkPredTrue; } |
   JUMP_OP_IFTHENELSELINKPREDFALSE { iris16::curri.op = (byte)iris16::JumpOp::IfThenElseLinkPredFalse; }
;

cop:
   COMPARE_OP_EQ { iris16::curri.op = (byte)iris16::CompareOp::Eq; } |
   COMPARE_OP_NEQ { iris16::curri.op = (byte)iris16::CompareOp::Neq; } |
   COMPARE_OP_LESSTHAN { iris16::curri.op = (byte)iris16::CompareOp::LessThan; } |
   COMPARE_OP_GREATERTHAN { iris16::curri.op = (byte)iris16::CompareOp::GreaterThan; } |
   COMPARE_OP_LESSTHANOREQUALTO { iris16::curri.op = (byte)iris16::CompareOp::LessThanOrEqualTo; } |
   COMPARE_OP_GREATERTHANOREQUALTO { iris16::curri.op = (byte)iris16::CompareOp::GreaterThanOrEqualTo; } 
;
icop:
   COMPARE_OP_EQ_IMMEDIATE { iris16::curri.op = (byte)iris16::CompareOp::EqImm; } |
   COMPARE_OP_NEQ_IMMEDIATE { iris16::curri.op = (byte)iris16::CompareOp::NeqImm; } |
   COMPARE_OP_LESSTHAN_IMMEDIATE { iris16::curri.op = (byte)iris16::CompareOp::LessThanImm; } |
   COMPARE_OP_GREATERTHAN_IMMEDIATE { iris16::curri.op = (byte)iris16::CompareOp::GreaterThanImm; } |
   COMPARE_OP_LESSTHANOREQUALTO_IMMEDIATE { iris16::curri.op = (byte)iris16::CompareOp::LessThanOrEqualToImm; } |
   COMPARE_OP_GREATERTHANOREQUALTO_IMMEDIATE { iris16::curri.op = (byte)iris16::CompareOp::GreaterThanOrEqualToImm; }
;
lexeme:
      IRIS16_SYMBOL { iris16::curri.hassymbol = 1; 
               iris16::curri.symbol = $1; } | 
      IMMEDIATE { 
            iris16::curri.reg1 = (byte)(($1 & 0x00FF));
            iris16::curri.reg2 = (byte)(($1 & 0xFF00) >> 8);
      }
;
%%
void iris16error(const char* s) {
   printf("%d: %s\n", iris16lineno, s);
   exit(-1);
}

namespace iris16 {
	void assemble(FILE* input, std::ostream* output) {
	  initialize(output, input);
	  do {
		 yyparse();
	  } while(!feof(iris16in));
	  resolve_labels();
	}

void add_label_entry(const std::string& c, word addr) {
   if (iris16::state.labels.count(c) != 0) {
		iris16error("Found a duplicate label!");
		exit(1);
   } else {
	 iris16::state.labels[c] = addr;
   }
}

void persist_dynamic_op(void) {
   iris16::state.dynops.push_back(curri);
}

void save_encoding(void) {
   if(iris16::curri.hassymbol) {
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
		case iris16::Segment::Code:
			buf[4] = (char)iris::encodeBits<byte, byte, 0b11111000, 3>(
								iris::encodeBits<byte, byte, 0b00000111, 0>((byte)0, dop->group),
								dop->op);
			buf[5] = (char)dop->reg0;
			buf[6] = (char)dop->reg1;
			buf[7] = (char)dop->reg2;
			break;
		case iris16::Segment::Data:
			buf[4] = (char)dop->reg1;
			buf[5] = (char)dop->reg2;
			buf[6] = 0;
			buf[7] = 0;
			break;
		default:
			std::cerr << "panic: unknown segment " << (byte)dop->segment << std::endl;
			exit(1);
   }
   iris16::state.output->write(buf, 8);
   delete[] buf;
}

void resolve_labels() {
   /* we need to go through the list of dynamic operations and replace
	  the label with the corresponding address */
   for(std::vector<dynamicop>::iterator it = iris16::state.dynops.begin(); it != iris16::state.dynops.end(); ++it) {
		if (!resolve_op(&(*it))) {
			std::cerr << "panic: couldn't find label " << it->symbol << std::endl;
			exit(1);
		} else {
			write_dynamic_op(&(*it));
		}
   }
}
bool resolve_op(dynamicop* dop) {
   if(iris16::state.labels.count(dop->symbol) == 1) {
		word addr = iris16::state.labels[dop->symbol];
		dop->reg1 = iris::decodeField<word, byte, 0>(addr);
		dop->reg2 = iris::decodeField<word, byte, 1>(addr);
		return true;
   }
   return false;
}

void initialize(std::ostream* output, FILE* input) {
   iris16in = input;
   iris16::state.segment = iris16::Segment::Code;
   iris16::state.data_address = 0;
   iris16::state.code_address = 0;
   iris16::state.output = output;
   iris16::curri.segment = iris16::Segment::Code;
   iris16::curri.address = 0;
   iris16::curri.group = 0;
   iris16::curri.op = 0;
   iris16::curri.reg0 = 0;
   iris16::curri.reg1 = 0;
   iris16::curri.reg2 = 0;
   iris16::curri.hassymbol = 0;
}
}
