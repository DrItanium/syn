%{
#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include "iris.h"
#include <fstream>
#include <iostream>
#include <vector>
#include <map>

#include "asm.tab.h"

extern int yylex();
extern int yyparse();
extern FILE* yyin;
extern int yylineno;

void yyerror(const char* s);
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
   bool closeOutput;
};

asmstate state;
dynamicop curri;

void add_label_entry(const std::string& name, word address);
void persist_dynamic_op(void);
void save_encoding(void);
void write_dynamic_op(dynamicop* op);
void initialize(std::ostream* output, bool close, FILE* input);
void resolve_labels(void);
bool resolve_op(dynamicop* dop);
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
   COMPARE_OP_EQAND { curri.op = (byte)iris16::CompareOp::EqAnd; } |
   COMPARE_OP_EQOR { curri.op = (byte)iris16::CompareOp::EqOr; } |
   COMPARE_OP_EQXOR { curri.op = (byte)iris16::CompareOp::EqXor; } |
   COMPARE_OP_NEQAND { curri.op = (byte)iris16::CompareOp::NeqAnd; } |
   COMPARE_OP_NEQOR { curri.op = (byte)iris16::CompareOp::NeqOr; } |
   COMPARE_OP_NEQXOR { curri.op = (byte)iris16::CompareOp::NeqXor; } |
   COMPARE_OP_LESSTHANAND { curri.op = (byte)iris16::CompareOp::LessThanAnd; } |
   COMPARE_OP_LESSTHANOR { curri.op = (byte)iris16::CompareOp::LessThanOr; } |
   COMPARE_OP_LESSTHANXOR { curri.op = (byte)iris16::CompareOp::LessThanXor; } |
   COMPARE_OP_GREATERTHANAND { curri.op = (byte)iris16::CompareOp::GreaterThanAnd; } |
   COMPARE_OP_GREATERTHANOR { curri.op = (byte)iris16::CompareOp::GreaterThanOr; } |
   COMPARE_OP_GREATERTHANXOR { curri.op = (byte)iris16::CompareOp::GreaterThanXor; } |
   COMPARE_OP_LESSTHANOREQUALTOAND { curri.op = (byte)iris16::CompareOp::LessThanOrEqualToAnd; } |
   COMPARE_OP_LESSTHANOREQUALTOOR { curri.op = (byte)iris16::CompareOp::LessThanOrEqualToOr; } |
   COMPARE_OP_LESSTHANOREQUALTOXOR { curri.op = (byte)iris16::CompareOp::LessThanOrEqualToXor; } |
   COMPARE_OP_GREATERTHANOREQUALTOAND { curri.op = (byte)iris16::CompareOp::GreaterThanOrEqualToAnd; } |
   COMPARE_OP_GREATERTHANOREQUALTOOR { curri.op = (byte)iris16::CompareOp::GreaterThanOrEqualToOr; } |
   COMPARE_OP_GREATERTHANOREQUALTOXOR { curri.op = (byte)iris16::CompareOp::GreaterThanOrEqualToXor; } |
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
      curri.segment = Segment::Code;
      curri.address = 0;
      curri.group = 0;
      curri.op = 0;
      curri.reg0 = 0;
      curri.reg1 = 0;
      curri.reg2 = 0;
      curri.hassymbol = 0;
      curri.symbol = "";
   } | 
   asm {
      curri.segment = Segment::Code;
      curri.address = 0;
      curri.group = 0;
      curri.op = 0;
      curri.reg0 = 0;
      curri.reg1 = 0;
      curri.reg2 = 0;
      curri.hassymbol = 0;
      curri.symbol = "";
   }
   ;
asm:
   directive |
   statement
   ;
directive:
         DIRECTIVE_ORG IMMEDIATE {
            if(state.segment == Segment::Code) {
               state.code_address = $2;
            } else if(state.segment == Segment::Data) {
               state.data_address = $2;
            } else {
               yyerror("Invalid segment!");
            }
            } | 
      DIRECTIVE_CODE { state.segment = Segment::Code; } |
      DIRECTIVE_DATA { state.segment = Segment::Data; } |
      DIRECTIVE_DECLARE lexeme { 
            if(state.segment == Segment::Data) {
               curri.segment = Segment::Data;
               curri.address = state.data_address;
               save_encoding();
               state.data_address++;
            } else {
               yyerror("Declaration in non-data segment!");
            }
      }
      ;
statement:
         label { curri.segment = state.segment; }|
         operation {
            if(state.segment == Segment::Code) {
               curri.segment = Segment::Code;
               curri.address = state.code_address;
               save_encoding();
               state.code_address++;
            } else {
               yyerror("operation in an invalid segment!");
            }
         }
         ;
label:
     LABEL SYMBOL { 
      if(state.segment == Segment::Code) {
          add_label_entry($2, state.code_address);
      } else if (state.segment == Segment::Data) {
          add_label_entry($2, state.data_address);
      } else {
          yyerror("label in invalid segment!");
      }
     }
   ;
operation:
         arithmetic_op { curri.group = (byte)iris16::InstructionGroup::Arithmetic; } |
         move_op { curri.group = (byte)iris16::InstructionGroup::Move; } |
         jump_op { curri.group = (byte)iris16::InstructionGroup::Jump; } |
         compare_op { curri.group = (byte)iris16::InstructionGroup::Compare; } |
         misc_op { curri.group = (byte)iris16::InstructionGroup::Misc; }
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
             } |
             aop_imm REGISTER REGISTER IMMEDIATE {
               if($4 > 255) {
                  yyerror("immediate value offset out of range!");
               }
               curri.reg0 = $2;
               curri.reg1 = $3;
               curri.reg2 = $4;
             } |
			 aop_single_macro REGISTER {
			 	curri.reg0 = $2;
				curri.reg1 = $2;
				curri.reg2 = 1;
			 }
      ;
aop_single_macro:
   ARITHMETIC_MACRO_OP_INCR { curri.op = (byte)iris16::ArithmeticOp::AddImmediate; } |
   ARITHMETIC_MACRO_OP_DECR { curri.op = (byte)iris16::ArithmeticOp::SubImmediate; } 
   ;
move_op:
       mop_reg REGISTER REGISTER {
            curri.reg0 = $2;
            curri.reg1 = $3;
       } |
       mop_mixed REGISTER lexeme { curri.reg0 = $2; } |
       mop_single REGISTER {
         curri.reg0 = $2;
       } |
       MOVE_OP_PUSHIMMEDIATE lexeme { 
         curri.op = (byte)iris16::MoveOp::PushImmediate;
       } |
	   MOVE_OP_STORE_CODE REGISTER REGISTER REGISTER {
		curri.reg0 = $2;
		curri.reg1 = $3;
		curri.reg2 = $4;
	   } |
	   MOVE_OP_LOAD_CODE REGISTER REGISTER REGISTER {
	   	curri.reg0 = $2;
		curri.reg1 = $3;
		curri.reg2 = $4;
	   }

       ;

jump_op:
       JUMP_OP_UNCONDITIONALIMMEDIATE lexeme { 
         curri.op = (byte)iris16::JumpOp::UnconditionalImmediate; 
         } | 
       JUMP_OP_UNCONDITIONALREGISTER REGISTER { 
         curri.op = (byte)iris16::JumpOp::UnconditionalRegister; 
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
          } |
		  icop REGISTER REGISTER IMMEDIATE {
		  	if ($4 > 255) {
                  yyerror("immediate value offset out of range!");
			}
			curri.reg0 = $2;
			curri.reg1 = $3;
			curri.reg2 = $4;
		  }
          ;
misc_op:
       MISC_OP_SYSTEMCALL IMMEDIATE REGISTER REGISTER 
       { 
         curri.op = (byte)iris16::MiscOp::SystemCall; 
         if($2 > 255) {
            yyerror("system call offset out of range!");
         }
         curri.reg0 = $2;
         curri.reg1 = $3;
         curri.reg2 = $4;
       } 
       ;
aop:
   ARITHMETIC_OP_ADD { curri.op = (byte)iris16::ArithmeticOp::Add; } |
   ARITHMETIC_OP_SUB { curri.op = (byte)iris16::ArithmeticOp::Sub; } |
   ARITHMETIC_OP_MUL { curri.op = (byte)iris16::ArithmeticOp::Mul; } |
   ARITHMETIC_OP_DIV { curri.op = (byte)iris16::ArithmeticOp::Div; } |
   ARITHMETIC_OP_REM { curri.op = (byte)iris16::ArithmeticOp::Rem; } |
   ARITHMETIC_OP_SHIFTLEFT { curri.op = (byte)iris16::ArithmeticOp::ShiftLeft; } |
   ARITHMETIC_OP_SHIFTRIGHT { curri.op = (byte)iris16::ArithmeticOp::ShiftRight; } |
   ARITHMETIC_OP_BINARYAND { curri.op = (byte)iris16::ArithmeticOp::BinaryAnd; } |
   ARITHMETIC_OP_BINARYOR { curri.op = (byte)iris16::ArithmeticOp::BinaryOr; } |
   ARITHMETIC_OP_BINARYXOR { curri.op = (byte)iris16::ArithmeticOp::BinaryXor; } 
   ;

aop_imm:
   ARITHMETIC_OP_ADD_IMM { curri.op = (byte)iris16::ArithmeticOp::AddImmediate; } |
   ARITHMETIC_OP_SUB_IMM { curri.op = (byte)iris16::ArithmeticOp::SubImmediate; } |
   ARITHMETIC_OP_MUL_IMM { curri.op = (byte)iris16::ArithmeticOp::MulImmediate; } | 
   ARITHMETIC_OP_DIV_IMM { curri.op = (byte)iris16::ArithmeticOp::DivImmediate; } |
   ARITHMETIC_OP_REM_IMM { curri.op = (byte)iris16::ArithmeticOp::RemImmediate; } |
   ARITHMETIC_OP_SHIFTLEFT_IMM { curri.op = (byte)iris16::ArithmeticOp::ShiftLeftImmediate; } |
   ARITHMETIC_OP_SHIFTRIGHT_IMM { curri.op = (byte)iris16::ArithmeticOp::ShiftRightImmediate; } 
   ;

mop_reg:
   MOVE_OP_MOVE { curri.op = (byte)iris16::MoveOp::Move; } |
   MOVE_OP_SWAP { curri.op = (byte)iris16::MoveOp::Swap; } |
   MOVE_OP_LOAD { curri.op = (byte)iris16::MoveOp::Load; } |
   MOVE_OP_STORE { curri.op = (byte)iris16::MoveOp::Store; } |
   ;

mop_mixed:
   MOVE_OP_SET { curri.op = (byte)iris16::MoveOp::Set; } |
   MOVE_OP_STOREIMM { curri.op = (byte)iris16::MoveOp::Memset; } |
   MOVE_OP_LOADMEM { curri.op = (byte)iris16::MoveOp::LoadImmediate; } 
   ;

mop_single:
   MOVE_OP_PUSH { curri.op = (byte)iris16::MoveOp::Push; } |
   MOVE_OP_POP { curri.op = (byte)iris16::MoveOp::Pop; } 
   ;

jop_reg_imm:
   JUMP_OP_UNCONDITIONALIMMEDIATELINK { curri.op = (byte)iris16::JumpOp::UnconditionalImmediateLink; } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATE { curri.op = (byte)iris16::JumpOp::ConditionalTrueImmediate; } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATELINK { curri.op = (byte)iris16::JumpOp::ConditionalTrueImmediateLink; } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATE { curri.op = (byte)iris16::JumpOp::ConditionalFalseImmediate; } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATELINK { curri.op = (byte)iris16::JumpOp::ConditionalFalseImmediateLink; } 
   ;


jop_reg_reg:
   JUMP_OP_UNCONDITIONALREGISTERLINK { curri.op = (byte)iris16::JumpOp::UnconditionalRegisterLink; } |
   JUMP_OP_CONDITIONALTRUEREGISTER { curri.op = (byte)iris16::JumpOp::ConditionalTrueRegister; } |
   JUMP_OP_CONDITIONALFALSEREGISTER { curri.op = (byte)iris16::JumpOp::ConditionalFalseRegister; }
   ;

jop_reg_reg_reg:
   JUMP_OP_CONDITIONALTRUEREGISTERLINK { curri.op = (byte)iris16::JumpOp::ConditionalTrueRegisterLink; } |
   JUMP_OP_CONDITIONALFALSEREGISTERLINK { curri.op = (byte)iris16::JumpOp::ConditionalFalseRegisterLink; } |
   JUMP_OP_IFTHENELSENORMALPREDTRUE { curri.op = (byte)iris16::JumpOp::IfThenElseNormalPredTrue; } |
   JUMP_OP_IFTHENELSENORMALPREDFALSE { curri.op = (byte)iris16::JumpOp::IfThenElseNormalPredFalse; } |
   JUMP_OP_IFTHENELSELINKPREDTRUE { curri.op = (byte)iris16::JumpOp::IfThenElseLinkPredTrue; } |
   JUMP_OP_IFTHENELSELINKPREDFALSE { curri.op = (byte)iris16::JumpOp::IfThenElseLinkPredFalse; }
;

cop:
   COMPARE_OP_EQ { curri.op = (byte)iris16::CompareOp::Eq; } |
   COMPARE_OP_NEQ { curri.op = (byte)iris16::CompareOp::Neq; } |
   COMPARE_OP_LESSTHAN { curri.op = (byte)iris16::CompareOp::LessThan; } |
   COMPARE_OP_GREATERTHAN { curri.op = (byte)iris16::CompareOp::GreaterThan; } |
   COMPARE_OP_LESSTHANOREQUALTO { curri.op = (byte)iris16::CompareOp::LessThanOrEqualTo; } |
   COMPARE_OP_GREATERTHANOREQUALTO { curri.op = (byte)iris16::CompareOp::GreaterThanOrEqualTo; } 
;
icop:
   COMPARE_OP_EQ_IMMEDIATE { curri.op = (byte)iris16::CompareOp::EqImm; } |
   COMPARE_OP_NEQ_IMMEDIATE { curri.op = (byte)iris16::CompareOp::NeqImm; } |
   COMPARE_OP_LESSTHAN_IMMEDIATE { curri.op = (byte)iris16::CompareOp::LessThanImm; } |
   COMPARE_OP_GREATERTHAN_IMMEDIATE { curri.op = (byte)iris16::CompareOp::GreaterThanImm; } |
   COMPARE_OP_LESSTHANOREQUALTO_IMMEDIATE { curri.op = (byte)iris16::CompareOp::LessThanOrEqualToImm; } |
   COMPARE_OP_GREATERTHANOREQUALTO_IMMEDIATE { curri.op = (byte)iris16::CompareOp::GreaterThanOrEqualToImm; }
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
	FILE* input = 0;
	std::string line("v.obj");
	std::ostream* output = 0;
	bool closeOutput = false,
		 closeInput = false,
		 errorfree = true;
   int last = argc - 1,
   	   i = 0;
   /* make sure these are properly initialized */
   last = argc - 1;
   errorfree = 1;
   i = 0;
   if(argc > 1) {
      for(i = 1; errorfree && (i < last); ++i) {
		 std::string tmpline(argv[i]);
         if(tmpline.size() == 2 && tmpline[0] == '-') {
            switch(tmpline[1]) {
			   case 'o':
			   		++i;
			   		line = argv[i];
			   		break;
               case 'h':
               default:
                  errorfree = false;
                  break;
            }
         } else {
            errorfree = false;
            break;
         }
      }
      if(errorfree) {
         if(i == last) {
			std::string tline(argv[last]);
			if(tline.size() == 1 && tline[0] == '-') {
				input = stdin;
				closeInput = false;
			} else if (tline.size() >= 1) {
				if ((input = fopen(tline.c_str(), "r")) != NULL) {
					closeInput = true;
				} else {
					std::cerr << "Couldn't open " << tline << " for reading!" << std::endl;
					exit(1);
				}
			}
            /* open the output */
            if(line.size() == 1 && line[0] == '-') {
			   output = &std::cout; 
			   closeOutput = false;
            } else {
			   output = new std::ofstream(line.c_str(), std::ofstream::out | std::ofstream::binary);
			   closeOutput = true;
            }
         } else {
			 std::cerr << "no file provided" << std::endl;
         }
      } else {
	  	
	  }
   }
   if(output && input) {
      initialize(output, closeOutput, input);
      do {
         yyparse();
      } while(!feof(yyin));
      resolve_labels();
	  if (closeOutput) {
	  	static_cast<std::ofstream*>(output)->close();
	  	delete output;
	  	output = 0;
	  	state.output = 0;
	  }
	  if(closeInput) {
	  	fclose(input);
		input = 0;
	  }
   } else {
      usage(argv[0]);
   }
}

void usage(char* arg0) {
	std::cerr << "usage: " << arg0 << " [-o <file>] <file>" << std::endl;
}
void add_label_entry(const std::string& c, word addr) {
   if (state.labels.count(c) != 0) {
		yyerror("Found a duplicate label!");
		exit(1);
   } else {
	 state.labels[c] = addr;
   }
}

void persist_dynamic_op(void) {
   state.dynops.push_back(curri);
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
   char* buf = new char[8];
   buf[0] = 0;
   buf[1] = (char)dop->segment;
   buf[2] = (char)(dop->address & 0x00FF);
   buf[3] = (char)((dop->address & 0xFF00) >> 8);
   switch(dop->segment) {
   		case Segment::Code:
			buf[4] = (char)iris::encodeBits<byte, byte, 0b11111000, 3>(
								iris::encodeBits<byte, byte, 0b00000111, 0>((byte)0, dop->group),
								dop->op);
			buf[5] = (char)dop->reg0;
			buf[6] = (char)dop->reg1;
			buf[7] = (char)dop->reg2;
			break;
		case Segment::Data:
			buf[4] = (char)dop->reg1;
			buf[5] = (char)dop->reg2;
			buf[6] = 0;
			buf[7] = 0;
			break;
		default:
			std::cerr << "panic: unknown segment " << (byte)dop->segment << std::endl;
			exit(1);
   }
   state.output->write(buf, 8);
   delete[] buf;
}

void yyerror(const char* s) {
   printf("%d: %s\n", yylineno, s);
   exit(-1);
}
void resolve_labels() {
   /* we need to go through the list of dynamic operations and replace
      the label with the corresponding address */
   for(std::vector<dynamicop>::iterator it = state.dynops.begin(); it != state.dynops.end(); ++it) {
   		if (!resolve_op(&(*it))) {
			std::cerr << "panic: couldn't find label " << it->symbol << std::endl;
			exit(1);
		} else {
			write_dynamic_op(&(*it));
		}
   }
}
bool resolve_op(dynamicop* dop) {
   if(state.labels.count(dop->symbol) == 1) {
		word addr = state.labels[dop->symbol];
		dop->reg1 = iris::decodeBits<word, byte, 0x00FF>(addr);
		dop->reg2 = iris::decodeBits<word, byte, 0xFF00, 8>(addr);
		return true;
   }
   return false;
}

void initialize(std::ostream* output, bool close, FILE* input) {
   yyin = input;
   state.segment = Segment::Code;
   state.data_address = 0;
   state.code_address = 0;
   state.output = output;
   state.closeOutput = close;
   curri.segment = Segment::Code;
   curri.address = 0;
   curri.group = 0;
   curri.op = 0;
   curri.reg0 = 0;
   curri.reg1 = 0;
   curri.reg2 = 0;
   curri.hassymbol = 0;
}
