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
#include <sstream>
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
   bool hasSymbol;
   std::string symbol;
   template<typename T>
   void setOperation(T value) noexcept {
		op = static_cast<byte>(value);
   }
   template<typename T>
   void setGroup(T value) noexcept {
   		group = static_cast<byte>(value);
   }
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
template<typename T>
void setOperation(T value) noexcept {
	iris16::curri.setOperation<T>(value);
}
template<typename T>
void setGroup(T value) noexcept {
	iris16::curri.setGroup<T>(value);
}

void setStateSegment(iris16::Segment value) noexcept {
	iris16::state.segment = value;
}
void setCurrentInstructionSegment(iris16::Segment value) noexcept {
	iris16::curri.segment = value; 
}



void addLabelEntry(const std::string& name, iris16::word address) {
	iris16::add_label_entry(name, address);
}
void clearCurrentInstruction() noexcept {
	  iris16::curri.segment = iris16::Segment::Code;
	  iris16::curri.address = 0;
	  iris16::curri.group = 0;
	  iris16::curri.op = 0;
	  iris16::curri.reg0 = 0;
	  iris16::curri.reg1 = 0;
	  iris16::curri.reg2 = 0;
	  iris16::curri.hasSymbol = false;
	  iris16::curri.symbol = "";
}
iris16::Segment getStateSegment() noexcept {
	  return iris16::state.segment;
}
template<typename T, T value>
bool isEqualTo(T input) noexcept {
	return input == value;
}
bool stateInDataSegment() noexcept {
	return isEqualTo<iris16::Segment, iris16::Segment::Data>(getStateSegment());
}
bool stateInCodeSegment() noexcept {
	return isEqualTo<iris16::Segment, iris16::Segment::Code>(getStateSegment());
}
using word = iris16::word;
word performActionOnStateObject(std::function<word()> data, std::function<word()> code, std::function<word(const std::string&)> unknownSegment) {
	if (stateInDataSegment()) {
		return data();
	} else if (stateInCodeSegment()) {
		return code();
	} else {
		return unknownSegment("State current in unknown segment!");
	}
}
word getAppropriateStateAddress() noexcept {
	return performActionOnStateObject([]() { return iris16::state.data_address; }, []() { return iris16::state.code_address; }, [](auto msg) { iris16error(msg.c_str()); return 0; });
}
void incrementAppropriateStateAddress() noexcept {
	performActionOnStateObject([]() { ++iris16::state.data_address; return 0; }, []() { ++iris16::state.code_address; return 0; }, [](auto msg) { iris16error(msg.c_str()); return 0; });
}
void assignAppropriateAddressToCurrentInstruction() noexcept {
	iris16::curri.segment = iris16::state.segment;
	iris16::curri.address = getAppropriateStateAddress();
}
void setRegister2(byte value) noexcept {
	iris16::curri.reg2 = value;
}
void setRegister1(byte value) noexcept {
	iris16::curri.reg1 = value;
}
void setRegister0(byte value) noexcept {
	iris16::curri.reg0 = value;
}

void setFullImmediate(word value) noexcept {
	setRegister1(iris::getLowerHalf<word>(value));
	setRegister2(iris::getUpperHalf<word>(value));
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
%token MOVE_OP_LOAD_OFFSET MOVE_OP_STORE_OFFSET
%token MOVE_OP_LOAD_IO_OFFSET MOVE_OP_STORE_IO_OFFSET
%token TAG_LOW TAG_HI
%token OP_MOVE_TO_IP OP_MOVE_FROM_IP OP_MOVE_TO_LR OP_MOVE_FROM_LR
%token OP_SAVE_CR OP_RESTORE_CR
%token OP_CR_XOR OP_CR_NOT OP_CR_AND OP_CR_OR OP_CR_NAND OP_CR_NOR OP_CR_SWAP
%token OP_CR_MOVE
%token BRANCH_LR BRANCH_LR_LINK
%token COND_TRUE_BRANCH_LR COND_TRUE_BRANCH_LR_LINK
%token COND_FALSE_BRANCH_LR COND_FALSE_BRANCH_LR_LINK


%token <rval> GPR
%token <rval> PREDICATE_REGISTER
%token <ival> IMMEDIATE
%token <sval> IRIS16_SYMBOL

%%
Q: /* empty */ |
   F 
;
F:
   F asm {
   	  clearCurrentInstruction(); 
   } | 
   asm {
	  clearCurrentInstruction();
   }
   ;
asm:
   directive |
   statement
   ;
directive:
         DIRECTIVE_ORG IMMEDIATE {
		 	if (stateInCodeSegment()) {
               iris16::state.code_address = $2;
            } else if(stateInDataSegment()) {
               iris16::state.data_address = $2;
            } else {
               iris16error("Invalid segment!");
            }
            } | 
      DIRECTIVE_CODE { iris16::state.segment = iris16::Segment::Code; } |
      DIRECTIVE_DATA { iris16::state.segment = iris16::Segment::Data; } |
      DIRECTIVE_DECLARE lexeme { 
	  		if (stateInDataSegment()) {
			   assignAppropriateAddressToCurrentInstruction();
               iris16::save_encoding();
			   incrementAppropriateStateAddress();
            } else {
               iris16error("Declaration in non-data segment!");
            }
      }
      ;
statement:
         label { iris16::curri.segment = iris16::state.segment; }|
         operation {
		 	if (stateInCodeSegment()) {
			   assignAppropriateAddressToCurrentInstruction();
               iris16::save_encoding();
			   incrementAppropriateStateAddress();
            } else {
               iris16error("operation in an invalid segment!");
            }
         }
         ;
label:
     LABEL IRIS16_SYMBOL { 
	 	addLabelEntry($2, getAppropriateStateAddress());
     }
   ;
operation:
         arithmetic_op { setGroup(iris16::InstructionGroup::Arithmetic); } |
         move_op { setGroup(iris16::InstructionGroup::Move); } |
         jump_op { setGroup(iris16::InstructionGroup::Jump); } |
         compare_op { setGroup(iris16::InstructionGroup::Compare); } |
		 cond_reg_op { setGroup(iris16::InstructionGroup::ConditionalRegister); }
		 ;

arithmetic_op:
             aop THREE_GPRS |
             ARITHMETIC_OP_BINARYNOT TWO_GPRS |
			 aop_imm TWO_GPRS_WITH_OFFSET |
			 aop_single_macro GPR {
			 	setRegister0($2);
				setRegister1($2);
			 }
      ;
aop_single_macro:
   ARITHMETIC_MACRO_OP_INCR { 
   	 setOperation(iris16::ArithmeticOp::AddImmediate);
	 setRegister2(1);
   } |
   ARITHMETIC_MACRO_OP_DECR { 
	 setOperation(iris16::ArithmeticOp::SubImmediate);
	 setRegister2(1);
   } |
   ARITHMETIC_MACRO_OP_HALVE { 
	 setOperation(iris16::ArithmeticOp::DivImmediate);
	 setRegister2(2);
   } |
   ARITHMETIC_MACRO_OP_DOUBLE {
   	 setOperation(iris16::ArithmeticOp::MulImmediate);
	 setRegister2(2);
   }
   ;
move_op:
	   mop_reg TWO_GPRS |
       mop_mixed DESTINATION_GPR lexeme |
	   mop_offset TWO_GPRS_WITH_OFFSET |
       MOVE_OP_PUSHIMMEDIATE DESTINATION_GPR lexeme { setOperation(iris16::MoveOp::PushImmediate); } |
	   MOVE_OP_STORE_CODE THREE_GPRS { setOperation(iris16::MoveOp::StoreCode); } |
	   MOVE_OP_LOAD_CODE THREE_GPRS { setOperation(iris16::MoveOp::LoadCode); } |
	   MOVE_OP_LOAD_IO TWO_GPRS { setOperation(iris16::MoveOp::IORead); } |
	   MOVE_OP_STORE_IO TWO_GPRS { setOperation(iris16::MoveOp::IOWrite); } 
	   OP_MOVE_TO_IP DESTINATION_GPR { setOperation(iris16::MoveOp::MoveToIP); } |
	   OP_MOVE_FROM_IP DESTINATION_GPR { setOperation(iris16::MoveOp::MoveFromIP); } |
	   OP_MOVE_TO_LR DESTINATION_GPR { setOperation(iris16::MoveOp::MoveToLinkRegister); } |
	   OP_MOVE_FROM_LR DESTINATION_GPR { setOperation(iris16::MoveOp::MoveFromLinkRegister); }
       ;


aop:
   ARITHMETIC_OP_ADD { setOperation(iris16::ArithmeticOp::Add); } |
   ARITHMETIC_OP_SUB { setOperation(iris16::ArithmeticOp::Sub); } |
   ARITHMETIC_OP_MUL { setOperation(iris16::ArithmeticOp::Mul); } |
   ARITHMETIC_OP_DIV { setOperation(iris16::ArithmeticOp::Div); } |
   ARITHMETIC_OP_REM { setOperation(iris16::ArithmeticOp::Rem); } |
   ARITHMETIC_OP_SHIFTLEFT { setOperation(iris16::ArithmeticOp::ShiftLeft); } |
   ARITHMETIC_OP_SHIFTRIGHT { setOperation(iris16::ArithmeticOp::ShiftRight); } |
   ARITHMETIC_OP_BINARYAND { setOperation(iris16::ArithmeticOp::BinaryAnd); } |
   ARITHMETIC_OP_BINARYOR { setOperation(iris16::ArithmeticOp::BinaryOr); } |
   ARITHMETIC_OP_BINARYXOR { setOperation(iris16::ArithmeticOp::BinaryXor); } 
   ;

aop_imm:
   ARITHMETIC_OP_ADD_IMM { setOperation(iris16::ArithmeticOp::AddImmediate); } |
   ARITHMETIC_OP_SUB_IMM { setOperation(iris16::ArithmeticOp::SubImmediate); } |
   ARITHMETIC_OP_MUL_IMM { setOperation(iris16::ArithmeticOp::MulImmediate); } | 
   ARITHMETIC_OP_DIV_IMM { setOperation(iris16::ArithmeticOp::DivImmediate); } |
   ARITHMETIC_OP_REM_IMM { setOperation(iris16::ArithmeticOp::RemImmediate); } |
   ARITHMETIC_OP_SHIFTLEFT_IMM { setOperation(iris16::ArithmeticOp::ShiftLeftImmediate); } |
   ARITHMETIC_OP_SHIFTRIGHT_IMM { setOperation(iris16::ArithmeticOp::ShiftRightImmediate); } 
   ;

mop_reg:
   MOVE_OP_MOVE { setOperation(iris16::MoveOp::Move); } |
   MOVE_OP_SWAP { setOperation(iris16::MoveOp::Swap); } |
   MOVE_OP_LOAD { setOperation(iris16::MoveOp::Load); } |
   MOVE_OP_STORE { setOperation(iris16::MoveOp::Store); } |
   MOVE_OP_PUSH { setOperation(iris16::MoveOp::Push); } |
   MOVE_OP_POP { setOperation(iris16::MoveOp::Pop); }
   ;

mop_mixed:
   MOVE_OP_SET { setOperation(iris16::MoveOp::Set); } |
   MOVE_OP_STOREIMM { setOperation(iris16::MoveOp::Memset); } |
   MOVE_OP_LOADMEM { setOperation(iris16::MoveOp::LoadImmediate); } 
   ;

mop_offset:
	MOVE_OP_STORE_IO_OFFSET { setOperation(iris16::MoveOp::IOWriteWithOffset); } |
	MOVE_OP_LOAD_IO_OFFSET { setOperation(iris16::MoveOp::IOReadWithOffset); } |
	MOVE_OP_STORE_OFFSET { setOperation(iris16::MoveOp::StoreWithOffset); } |
	MOVE_OP_LOAD_OFFSET { setOperation(iris16::MoveOp::LoadWithOffset); }
	;

jump_op:
	   jop_imm lexeme |
	   jop_reg DESTINATION_GPR |
	   jop_cond_reg PREDICATE_REGISTER_RESULT SOURCE0_GPR |
	   jop_cond_imm PREDICATE_REGISTER_RESULT lexeme |
	   jop_cond_reg_reg PREDICATE_REGISTER_RESULT SOURCE0_GPR SOURCE1_GPR |
	   jop_no_args |
	   jop_only_cond PREDICATE_REGISTER_RESULT
	   ;
jop_only_cond:
		COND_TRUE_BRANCH_LR { setOperation(iris16::JumpOp::ConditionalTrueJumpLinkRegister); } |
		COND_TRUE_BRANCH_LR_LINK { setOperation(iris16::JumpOp::ConditionalTrueJumpLinkRegisterLink); } |
		COND_FALSE_BRANCH_LR { setOperation(iris16::JumpOp::ConditionalFalseJumpLinkRegister); } |
		COND_FALSE_BRANCH_LR_LINK { setOperation(iris16::JumpOp::ConditionalFalseJumpLinkRegisterLink); };
jop_no_args:
	   BRANCH_LR_LINK { setOperation(iris16::JumpOp::UnconditionalJumpLinkRegisterLink); } |
	   BRANCH_LR { setOperation(iris16::JumpOp::UnconditionalJumpLinkRegister); } 
	   ;

jop_imm:
       JUMP_OP_UNCONDITIONALIMMEDIATE { setOperation(iris16::JumpOp::UnconditionalImmediate); } | 
       JUMP_OP_UNCONDITIONALIMMEDIATELINK { setOperation(iris16::JumpOp::UnconditionalImmediateLink); };
jop_reg:
       JUMP_OP_UNCONDITIONALREGISTER { setOperation(iris16::JumpOp::UnconditionalRegister); } |
       JUMP_OP_UNCONDITIONALREGISTERLINK { setOperation(iris16::JumpOp::UnconditionalRegisterLink); }
	   ;
jop_cond_imm:
   JUMP_OP_CONDITIONALTRUEIMMEDIATE { setOperation(iris16::JumpOp::ConditionalTrueImmediate); } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATELINK { setOperation(iris16::JumpOp::ConditionalTrueImmediateLink); } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATE { setOperation(iris16::JumpOp::ConditionalFalseImmediate); } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATELINK { setOperation(iris16::JumpOp::ConditionalFalseImmediateLink); } 
   ;


jop_cond_reg:
   JUMP_OP_CONDITIONALTRUEREGISTERLINK { setOperation(iris16::JumpOp::ConditionalTrueRegisterLink); } |
   JUMP_OP_CONDITIONALFALSEREGISTERLINK { setOperation(iris16::JumpOp::ConditionalFalseRegisterLink); } |
   JUMP_OP_CONDITIONALTRUEREGISTER { setOperation(iris16::JumpOp::ConditionalTrueRegister); } |
   JUMP_OP_CONDITIONALFALSEREGISTER { setOperation(iris16::JumpOp::ConditionalFalseRegister); }
   ;

jop_cond_reg_reg:
   JUMP_OP_IFTHENELSENORMALPREDTRUE { setOperation(iris16::JumpOp::IfThenElseNormalPredTrue); } |
   JUMP_OP_IFTHENELSENORMALPREDFALSE { setOperation(iris16::JumpOp::IfThenElseNormalPredFalse); } |
   JUMP_OP_IFTHENELSELINKPREDTRUE { setOperation(iris16::JumpOp::IfThenElseLinkPredTrue); } |
   JUMP_OP_IFTHENELSELINKPREDFALSE { setOperation(iris16::JumpOp::IfThenElseLinkPredFalse); }
;

compare_op:
		  cop DESTINATION_PREDICATE_REGISTERS SOURCE0_GPR SOURCE1_GPR |
		  icop DESTINATION_PREDICATE_REGISTERS SOURCE0_GPR half_immediate;
cop:
   COMPARE_OP_EQ { setOperation(iris16::CompareOp::Eq); } |
   COMPARE_OP_NEQ { setOperation(iris16::CompareOp::Neq); } |
   COMPARE_OP_LESSTHAN { setOperation(iris16::CompareOp::LessThan); } |
   COMPARE_OP_GREATERTHAN { setOperation(iris16::CompareOp::GreaterThan); } |
   COMPARE_OP_LESSTHANOREQUALTO { setOperation(iris16::CompareOp::LessThanOrEqualTo); } |
   COMPARE_OP_GREATERTHANOREQUALTO { setOperation(iris16::CompareOp::GreaterThanOrEqualTo); } 
;
icop:
   COMPARE_OP_EQ_IMMEDIATE { setOperation(iris16::CompareOp::EqImm); } |
   COMPARE_OP_NEQ_IMMEDIATE { setOperation(iris16::CompareOp::NeqImm); } |
   COMPARE_OP_LESSTHAN_IMMEDIATE { setOperation(iris16::CompareOp::LessThanImm); } |
   COMPARE_OP_GREATERTHAN_IMMEDIATE { setOperation(iris16::CompareOp::GreaterThanImm); } |
   COMPARE_OP_LESSTHANOREQUALTO_IMMEDIATE { setOperation(iris16::CompareOp::LessThanOrEqualToImm); } |
   COMPARE_OP_GREATERTHANOREQUALTO_IMMEDIATE { setOperation(iris16::CompareOp::GreaterThanOrEqualToImm); }
;

cond_reg_op: 
	cond_save_restore_op DESTINATION_GPR lexeme |
	cond_two_arg DESTINATION_PREDICATE_REGISTERS |
	cond_four_arg DESTINATION_PREDICATE_REGISTERS TWO_ARG_PREDICATE_REGISTER |
	cond_three_arg DESTINATION_PREDICATE_REGISTERS SOURCE0_PREDICATE_REGISTER ;
cond_two_arg:
			OP_CR_SWAP { setOperation(iris16::ConditionRegisterOp::CRSwap); } |
			OP_CR_MOVE { setOperation(iris16::ConditionRegisterOp::CRMove); } ;
cond_save_restore_op:
		OP_SAVE_CR { setOperation(iris16::ConditionRegisterOp::SaveCRs); } |
		OP_RESTORE_CR { setOperation(iris16::ConditionRegisterOp::RestoreCRs); } ;
cond_three_arg:
		OP_CR_NOT { setOperation(iris16::ConditionRegisterOp::CRNot); } ;
cond_four_arg:
			 OP_CR_XOR { setOperation(iris16::ConditionRegisterOp::CRXor); } |
			 OP_CR_AND { setOperation(iris16::ConditionRegisterOp::CRAnd); } |
			 OP_CR_OR { setOperation(iris16::ConditionRegisterOp::CROr); } |
			 OP_CR_NAND  { setOperation(iris16::ConditionRegisterOp::CRNand); } |
			 OP_CR_NOR { setOperation(iris16::ConditionRegisterOp::CRNor); }
			 ;
lexeme:
      IRIS16_SYMBOL { iris16::curri.hasSymbol = true; 
               iris16::curri.symbol = $1; } | 
      IMMEDIATE { 
	  		setFullImmediate($1);
      }
;
PREDICATE_REGISTER_RESULT:
						 PREDICATE_REGISTER { setRegister0(iris16::encodePredicateResult(iris16::curri.reg0, $1)); };
PREDICATE_REGISTER_INVERSE:
						  PREDICATE_REGISTER { setRegister0(iris16::encodePredicateInverseResult(iris16::curri.reg0, $1)); };
DESTINATION_PREDICATE_REGISTERS:
							   PREDICATE_REGISTER_RESULT PREDICATE_REGISTER_INVERSE;

SOURCE0_PREDICATE_REGISTER:
						 PREDICATE_REGISTER { iris16::curri.reg1 = iris16::encodePredicateSource0(iris16::curri.reg1, $1); };
SOURCE1_PREDICATE_REGISTER:
						  PREDICATE_REGISTER { iris16::curri.reg1 = iris16::encodePredicateSource1(iris16::curri.reg1, $1); };
TWO_ARG_PREDICATE_REGISTER:
						  SOURCE0_PREDICATE_REGISTER SOURCE1_PREDICATE_REGISTER;


half_immediate:
				IMMEDIATE {
	   				if ($1 > 255) {
						iris16error("immediate value offset out of range!");
					}
					setRegister2($1);
				} | 
				TAG_LOW IMMEDIATE {
					setRegister2(iris::getLowerHalf<word>($2));
				} |
				TAG_HI IMMEDIATE {
					setRegister2(iris::getUpperHalf<word>($2));
				} ;

DESTINATION_GPR:
					GPR { setRegister0($1); };

SOURCE0_GPR:
				GPR { setRegister1($1); };

SOURCE1_GPR:
				GPR { setRegister2($1); };

TWO_GPRS:
			 DESTINATION_GPR SOURCE0_GPR;

TWO_GPRS_WITH_OFFSET:
				TWO_GPRS half_immediate;

THREE_GPRS:
			   TWO_GPRS SOURCE1_GPR;

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
   if(iris16::curri.hasSymbol) {
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
   buf[1] = static_cast<char>(dop->segment);
   buf[2] = static_cast<char>((dop->address & 0x00FF));
   buf[3] = static_cast<char>(((dop->address & 0xFF00) >> 8));
   switch(dop->segment) {
		case iris16::Segment::Code:
			buf[4] = static_cast<char>(iris::encodeBits<byte, byte, 0b11111000, 3>(
								iris::encodeBits<byte, byte, 0b00000111, 0>((byte)0, dop->group),
								dop->op));
			buf[5] = static_cast<char>(dop->reg0);
			buf[6] = static_cast<char>(dop->reg1);
			buf[7] = static_cast<char>(dop->reg2);
			break;
		case iris16::Segment::Data:
			buf[4] = static_cast<char>(dop->reg1);
			buf[5] = static_cast<char>(dop->reg2);
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
			std::stringstream ss;
			ss << "panic: couldn't find label " << it->symbol;
			auto str = ss.str();
			iris16error(str.c_str());
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
   iris16::curri.hasSymbol = false;
}
}
