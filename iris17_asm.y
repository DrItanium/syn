%define api.prefix {iris17}
%{
#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include "iris17.h"
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include "asm_interact.h"

#include "iris17_asm.tab.h"

extern int yylex();
extern int yyparse();
extern FILE* iris17in;
extern int iris17lineno;

void iris17error(const char* s);
namespace iris17 {
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
   COMPARE_OP_EQAND { iris17::curri.op = (byte)iris17::CompareOp::EqAnd; } |
   COMPARE_OP_EQOR { iris17::curri.op = (byte)iris17::CompareOp::EqOr; } |
   COMPARE_OP_EQXOR { iris17::curri.op = (byte)iris17::CompareOp::EqXor; } |
   COMPARE_OP_NEQAND { iris17::curri.op = (byte)iris17::CompareOp::NeqAnd; } |
   COMPARE_OP_NEQOR { iris17::curri.op = (byte)iris17::CompareOp::NeqOr; } |
   COMPARE_OP_NEQXOR { iris17::curri.op = (byte)iris17::CompareOp::NeqXor; } |
   COMPARE_OP_LESSTHANAND { iris17::curri.op = (byte)iris17::CompareOp::LessThanAnd; } |
   COMPARE_OP_LESSTHANOR { iris17::curri.op = (byte)iris17::CompareOp::LessThanOr; } |
   COMPARE_OP_LESSTHANXOR { iris17::curri.op = (byte)iris17::CompareOp::LessThanXor; } |
   COMPARE_OP_GREATERTHANAND { iris17::curri.op = (byte)iris17::CompareOp::GreaterThanAnd; } |
   COMPARE_OP_GREATERTHANOR { iris17::curri.op = (byte)iris17::CompareOp::GreaterThanOr; } |
   COMPARE_OP_GREATERTHANXOR { iris17::curri.op = (byte)iris17::CompareOp::GreaterThanXor; } |
   COMPARE_OP_LESSTHANOREQUALTOAND { iris17::curri.op = (byte)iris17::CompareOp::LessThanOrEqualToAnd; } |
   COMPARE_OP_LESSTHANOREQUALTOOR { iris17::curri.op = (byte)iris17::CompareOp::LessThanOrEqualToOr; } |
   COMPARE_OP_LESSTHANOREQUALTOXOR { iris17::curri.op = (byte)iris17::CompareOp::LessThanOrEqualToXor; } |
   COMPARE_OP_GREATERTHANOREQUALTOAND { iris17::curri.op = (byte)iris17::CompareOp::GreaterThanOrEqualToAnd; } |
   COMPARE_OP_GREATERTHANOREQUALTOOR { iris17::curri.op = (byte)iris17::CompareOp::GreaterThanOrEqualToOr; } |
   COMPARE_OP_GREATERTHANOREQUALTOXOR { iris17::curri.op = (byte)iris17::CompareOp::GreaterThanOrEqualToXor; } |
*/

%token <rval> REGISTER
%token <ival> IMMEDIATE
%token <sval> SYMBOL

%%
Q: /* empty */ |
   F 
;
F:
   F asm {
      iris17::curri.segment = iris17::Segment::Code;
      iris17::curri.address = 0;
      iris17::curri.group = 0;
      iris17::curri.op = 0;
      iris17::curri.reg0 = 0;
      iris17::curri.reg1 = 0;
      iris17::curri.reg2 = 0;
      iris17::curri.hassymbol = 0;
      iris17::curri.symbol = "";
   } | 
   asm {
      iris17::curri.segment = iris17::Segment::Code;
      iris17::curri.address = 0;
      iris17::curri.group = 0;
      iris17::curri.op = 0;
      iris17::curri.reg0 = 0;
      iris17::curri.reg1 = 0;
      iris17::curri.reg2 = 0;
      iris17::curri.hassymbol = 0;
      iris17::curri.symbol = "";
   }
   ;
asm:
   directive |
   statement
   ;
directive:
         DIRECTIVE_ORG IMMEDIATE {
            if(iris17::state.segment == iris17::Segment::Code) {
               iris17::state.code_address = $2;
            } else if(iris17::state.segment == iris17::Segment::Data) {
               iris17::state.data_address = $2;
            } else {
               yyerror("Invalid segment!");
            }
            } | 
      DIRECTIVE_CODE { iris17::state.segment = iris17::Segment::Code; } |
      DIRECTIVE_DATA { iris17::state.segment = iris17::Segment::Data; } |
      DIRECTIVE_DECLARE lexeme { 
            if(iris17::state.segment == iris17::Segment::Data) {
               iris17::curri.segment = iris17::Segment::Data;
               iris17::curri.address = iris17::state.data_address;
               iris17::save_encoding();
               iris17::state.data_address++;
            } else {
               yyerror("Declaration in non-data segment!");
            }
      }
      ;
statement:
         label { iris17::curri.segment = iris17::state.segment; }|
         operation {
            if(iris17::state.segment == iris17::Segment::Code) {
               iris17::curri.segment = iris17::Segment::Code;
               iris17::curri.address = iris17::state.code_address;
               iris17::save_encoding();
               iris17::state.code_address++;
            } else {
               yyerror("operation in an invalid segment!");
            }
         }
         ;
label:
     LABEL SYMBOL { 
      if(iris17::state.segment == iris17::Segment::Code) {
          iris17::add_label_entry($2, iris17::state.code_address);
      } else if (iris17::state.segment == iris17::Segment::Data) {
          iris17::add_label_entry($2, iris17::state.data_address);
      } else {
          yyerror("label in invalid segment!");
      }
     }
   ;
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
                  yyerror("immediate value offset out of range!");
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
       mop_mixed REGISTER lexeme { iris17::curri.reg0 = $2; } |
       mop_single REGISTER {
         iris17::curri.reg0 = $2;
       } |
       MOVE_OP_PUSHIMMEDIATE lexeme { 
         iris17::curri.op = (byte)iris17::MoveOp::PushImmediate;
       } |
	   MOVE_OP_STORE_CODE REGISTER REGISTER REGISTER {
		iris17::curri.reg0 = $2;
		iris17::curri.reg1 = $3;
		iris17::curri.reg2 = $4;
	   } |
	   MOVE_OP_LOAD_CODE REGISTER REGISTER REGISTER {
	   	iris17::curri.reg0 = $2;
		iris17::curri.reg1 = $3;
		iris17::curri.reg2 = $4;
	   }

       ;

jump_op:
       JUMP_OP_UNCONDITIONALIMMEDIATE lexeme { 
         iris17::curri.op = (byte)iris17::JumpOp::UnconditionalImmediate; 
         } | 
       JUMP_OP_UNCONDITIONALREGISTER REGISTER { 
         iris17::curri.op = (byte)iris17::JumpOp::UnconditionalRegister; 
         iris17::curri.reg0 = $2;
       } |
       jop_reg_reg REGISTER REGISTER {
            iris17::curri.reg0 = $2;
            iris17::curri.reg1 = $3;
       } |
       jop_reg_imm REGISTER lexeme { iris17::curri.reg0 = $2; } |
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
                  yyerror("immediate value offset out of range!");
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
            yyerror("system call offset out of range!");
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

mop_mixed:
   MOVE_OP_SET { iris17::curri.op = (byte)iris17::MoveOp::Set; } |
   MOVE_OP_STOREIMM { iris17::curri.op = (byte)iris17::MoveOp::Memset; } |
   MOVE_OP_LOADMEM { iris17::curri.op = (byte)iris17::MoveOp::LoadImmediate; } 
   ;

mop_single:
   MOVE_OP_PUSH { iris17::curri.op = (byte)iris17::MoveOp::Push; } |
   MOVE_OP_POP { iris17::curri.op = (byte)iris17::MoveOp::Pop; } 
   ;

jop_reg_imm:
   JUMP_OP_UNCONDITIONALIMMEDIATELINK { iris17::curri.op = (byte)iris17::JumpOp::UnconditionalImmediateLink; } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATE { iris17::curri.op = (byte)iris17::JumpOp::ConditionalTrueImmediate; } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATELINK { iris17::curri.op = (byte)iris17::JumpOp::ConditionalTrueImmediateLink; } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATE { iris17::curri.op = (byte)iris17::JumpOp::ConditionalFalseImmediate; } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATELINK { iris17::curri.op = (byte)iris17::JumpOp::ConditionalFalseImmediateLink; } 
   ;


jop_reg_reg:
   JUMP_OP_UNCONDITIONALREGISTERLINK { iris17::curri.op = (byte)iris17::JumpOp::UnconditionalRegisterLink; } |
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
      SYMBOL { iris17::curri.hassymbol = 1; 
               iris17::curri.symbol = $1; } | 
      IMMEDIATE { 
            iris17::curri.reg1 = (byte)(($1 & 0x00FF));
            iris17::curri.reg2 = (byte)(($1 & 0xFF00) >> 8);
      }
;
%%
namespace iris {

	template<>
	void assemble<Architecture::iris17>(FILE* input, std::ostream* output) {
      iris17::initialize(output, input);
      do {
         yyparse();
      } while(!feof(iris17in));
      iris17::resolve_labels();
	}
}
void iris17error(const char* s) {
   printf("%d: %s\n", iris17lineno, s);
   exit(-1);
}
namespace iris17 {
void add_label_entry(const std::string& c, word addr) {
   if (iris17::state.labels.count(c) != 0) {
		yyerror("Found a duplicate label!");
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
   char* buf = new char[8];
   buf[0] = 0;
   buf[1] = (char)dop->segment;
   buf[2] = (char)(dop->address & 0x00FF);
   buf[3] = (char)((dop->address & 0xFF00) >> 8);
   switch(dop->segment) {
   		case iris17::Segment::Code:
			buf[4] = (char)iris::encodeBits<byte, byte, 0b11111000, 3>(
								iris::encodeBits<byte, byte, 0b00000111, 0>((byte)0, dop->group),
								dop->op);
			buf[5] = (char)dop->reg0;
			buf[6] = (char)dop->reg1;
			buf[7] = (char)dop->reg2;
			break;
		case iris17::Segment::Data:
			buf[4] = (char)dop->reg1;
			buf[5] = (char)dop->reg2;
			buf[6] = 0;
			buf[7] = 0;
			break;
		default:
			std::cerr << "panic: unknown segment " << (byte)dop->segment << std::endl;
			exit(1);
   }
   iris17::state.output->write(buf, 8);
   delete[] buf;
}

void resolve_labels() {
   /* we need to go through the list of dynamic operations and replace
      the label with the corresponding address */
   for(std::vector<dynamicop>::iterator it = iris17::state.dynops.begin(); it != iris17::state.dynops.end(); ++it) {
   		if (!resolve_op(&(*it))) {
			std::cerr << "panic: couldn't find label " << it->symbol << std::endl;
			exit(1);
		} else {
			write_dynamic_op(&(*it));
		}
   }
}
bool resolve_op(dynamicop* dop) {
   if(iris17::state.labels.count(dop->symbol) == 1) {
		word addr = iris17::state.labels[dop->symbol];
		dop->reg1 = iris::decodeBits<word, byte, 0x00FF>(addr);
		dop->reg2 = iris::decodeBits<word, byte, 0xFF00, 8>(addr);
		return true;
   }
   return false;
}

void initialize(std::ostream* output, FILE* input) {
   iris17in = input;
   iris17::state.segment = iris17::Segment::Code;
   iris17::state.data_address = 0;
   iris17::state.code_address = 0;
   iris17::state.output = output;
   iris17::curri.segment = iris17::Segment::Code;
   iris17::curri.address = 0;
   iris17::curri.group = 0;
   iris17::curri.op = 0;
   iris17::curri.reg0 = 0;
   iris17::curri.reg1 = 0;
   iris17::curri.reg2 = 0;
   iris17::curri.hassymbol = 0;
}
}
