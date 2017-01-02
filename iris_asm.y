%define api.prefix {iris}
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
#include "AssemblerRegistrar.h"
#include "iris.h"

#include "iris_asm.tab.h"

extern int yylex();
extern int yyparse();
extern FILE* irisin;
extern int irislineno;

void iriserror(const char* s);

namespace iris {
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
	iris::curri.setOperation<T>(value);
}
template<typename T>
void setGroup(T value) noexcept {
	iris::curri.setGroup<T>(value);
}

void setStateSegment(iris::Segment value) noexcept {
	iris::state.segment = value;
}
void setCurrentInstructionSegment(iris::Segment value) noexcept {
	iris::curri.segment = value; 
}



void addLabelEntry(const std::string& name, iris::word address) {
	iris::add_label_entry(name, address);
}
void clearCurrentInstruction() noexcept {
	  iris::curri.segment = iris::Segment::Code;
	  iris::curri.address = 0;
	  iris::curri.group = 0;
	  iris::curri.op = 0;
	  iris::curri.reg0 = 0;
	  iris::curri.reg1 = 0;
	  iris::curri.reg2 = 0;
	  iris::curri.hasSymbol = false;
	  iris::curri.symbol = "";
}
iris::Segment getStateSegment() noexcept {
	  return iris::state.segment;
}
template<typename T, T value>
bool isEqualTo(T input) noexcept {
	return input == value;
}
bool stateInDataSegment() noexcept {
	return isEqualTo<iris::Segment, iris::Segment::Data>(getStateSegment());
}
bool stateInCodeSegment() noexcept {
	return isEqualTo<iris::Segment, iris::Segment::Code>(getStateSegment());
}
using word = iris::word;
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
	return performActionOnStateObject([]() { return iris::state.data_address; }, []() { return iris::state.code_address; }, [](auto msg) { iriserror(msg.c_str()); return 0; });
}
void incrementAppropriateStateAddress() noexcept {
	performActionOnStateObject([]() { ++iris::state.data_address; return 0; }, []() { ++iris::state.code_address; return 0; }, [](auto msg) { iriserror(msg.c_str()); return 0; });
}
void assignAppropriateAddressToCurrentInstruction() noexcept {
	iris::curri.segment = iris::state.segment;
	iris::curri.address = getAppropriateStateAddress();
}
void setRegister2(byte value) noexcept {
	iris::curri.reg2 = value;
}
void setRegister1(byte value) noexcept {
	iris::curri.reg1 = value;
}
void setRegister0(byte value) noexcept {
	iris::curri.reg0 = value;
}

void setFullImmediate(word value) noexcept {
	setRegister1(syn::getLowerHalf<word>(value));
	setRegister2(syn::getUpperHalf<word>(value));
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
               iris::state.code_address = $2;
            } else if(stateInDataSegment()) {
               iris::state.data_address = $2;
            } else {
               iriserror("Invalid segment!");
            }
            } | 
      DIRECTIVE_CODE { iris::state.segment = iris::Segment::Code; } |
      DIRECTIVE_DATA { iris::state.segment = iris::Segment::Data; } |
      DIRECTIVE_DECLARE lexeme { 
	  		if (stateInDataSegment()) {
			   assignAppropriateAddressToCurrentInstruction();
               iris::save_encoding();
			   incrementAppropriateStateAddress();
            } else {
               iriserror("Declaration in non-data segment!");
            }
      }
      ;
statement:
         label { iris::curri.segment = iris::state.segment; }|
         operation {
		 	if (stateInCodeSegment()) {
			   assignAppropriateAddressToCurrentInstruction();
               iris::save_encoding();
			   incrementAppropriateStateAddress();
            } else {
               iriserror("operation in an invalid segment!");
            }
         }
         ;
label:
     LABEL IRIS16_SYMBOL { 
	 	addLabelEntry($2, getAppropriateStateAddress());
     }
   ;
operation:
         arithmetic_op { setGroup(iris::InstructionGroup::Arithmetic); } |
         move_op { setGroup(iris::InstructionGroup::Move); } |
         jump_op { setGroup(iris::InstructionGroup::Jump); } |
         compare_op { setGroup(iris::InstructionGroup::Compare); } |
		 cond_reg_op { setGroup(iris::InstructionGroup::ConditionalRegister); }
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
   	 setOperation(iris::ArithmeticOp::AddImmediate);
	 setRegister2(1);
   } |
   ARITHMETIC_MACRO_OP_DECR { 
	 setOperation(iris::ArithmeticOp::SubImmediate);
	 setRegister2(1);
   } |
   ARITHMETIC_MACRO_OP_HALVE { 
	 setOperation(iris::ArithmeticOp::DivImmediate);
	 setRegister2(2);
   } |
   ARITHMETIC_MACRO_OP_DOUBLE {
   	 setOperation(iris::ArithmeticOp::MulImmediate);
	 setRegister2(2);
   }
   ;
move_op:
	   mop_reg TWO_GPRS |
       mop_mixed DESTINATION_GPR lexeme |
	   mop_offset TWO_GPRS_WITH_OFFSET |
       MOVE_OP_PUSHIMMEDIATE DESTINATION_GPR lexeme { setOperation(iris::MoveOp::PushImmediate); } |
	   MOVE_OP_STORE_CODE THREE_GPRS { setOperation(iris::MoveOp::StoreCode); } |
	   MOVE_OP_LOAD_CODE THREE_GPRS { setOperation(iris::MoveOp::LoadCode); } |
	   MOVE_OP_LOAD_IO TWO_GPRS { setOperation(iris::MoveOp::IORead); } |
	   MOVE_OP_STORE_IO TWO_GPRS { setOperation(iris::MoveOp::IOWrite); } 
	   OP_MOVE_TO_IP DESTINATION_GPR { setOperation(iris::MoveOp::MoveToIP); } |
	   OP_MOVE_FROM_IP DESTINATION_GPR { setOperation(iris::MoveOp::MoveFromIP); } |
	   OP_MOVE_TO_LR DESTINATION_GPR { setOperation(iris::MoveOp::MoveToLinkRegister); } |
	   OP_MOVE_FROM_LR DESTINATION_GPR { setOperation(iris::MoveOp::MoveFromLinkRegister); }
       ;


aop:
   ARITHMETIC_OP_ADD { setOperation(iris::ArithmeticOp::Add); } |
   ARITHMETIC_OP_SUB { setOperation(iris::ArithmeticOp::Sub); } |
   ARITHMETIC_OP_MUL { setOperation(iris::ArithmeticOp::Mul); } |
   ARITHMETIC_OP_DIV { setOperation(iris::ArithmeticOp::Div); } |
   ARITHMETIC_OP_REM { setOperation(iris::ArithmeticOp::Rem); } |
   ARITHMETIC_OP_SHIFTLEFT { setOperation(iris::ArithmeticOp::ShiftLeft); } |
   ARITHMETIC_OP_SHIFTRIGHT { setOperation(iris::ArithmeticOp::ShiftRight); } |
   ARITHMETIC_OP_BINARYAND { setOperation(iris::ArithmeticOp::BinaryAnd); } |
   ARITHMETIC_OP_BINARYOR { setOperation(iris::ArithmeticOp::BinaryOr); } |
   ARITHMETIC_OP_BINARYXOR { setOperation(iris::ArithmeticOp::BinaryXor); } 
   ;

aop_imm:
   ARITHMETIC_OP_ADD_IMM { setOperation(iris::ArithmeticOp::AddImmediate); } |
   ARITHMETIC_OP_SUB_IMM { setOperation(iris::ArithmeticOp::SubImmediate); } |
   ARITHMETIC_OP_MUL_IMM { setOperation(iris::ArithmeticOp::MulImmediate); } | 
   ARITHMETIC_OP_DIV_IMM { setOperation(iris::ArithmeticOp::DivImmediate); } |
   ARITHMETIC_OP_REM_IMM { setOperation(iris::ArithmeticOp::RemImmediate); } |
   ARITHMETIC_OP_SHIFTLEFT_IMM { setOperation(iris::ArithmeticOp::ShiftLeftImmediate); } |
   ARITHMETIC_OP_SHIFTRIGHT_IMM { setOperation(iris::ArithmeticOp::ShiftRightImmediate); } 
   ;

mop_reg:
   MOVE_OP_MOVE { setOperation(iris::MoveOp::Move); } |
   MOVE_OP_SWAP { setOperation(iris::MoveOp::Swap); } |
   MOVE_OP_LOAD { setOperation(iris::MoveOp::Load); } |
   MOVE_OP_STORE { setOperation(iris::MoveOp::Store); } |
   MOVE_OP_PUSH { setOperation(iris::MoveOp::Push); } |
   MOVE_OP_POP { setOperation(iris::MoveOp::Pop); }
   ;

mop_mixed:
   MOVE_OP_SET { setOperation(iris::MoveOp::Set); } |
   MOVE_OP_STOREIMM { setOperation(iris::MoveOp::Memset); } |
   MOVE_OP_LOADMEM { setOperation(iris::MoveOp::LoadImmediate); } 
   ;

mop_offset:
	MOVE_OP_STORE_IO_OFFSET { setOperation(iris::MoveOp::IOWriteWithOffset); } |
	MOVE_OP_LOAD_IO_OFFSET { setOperation(iris::MoveOp::IOReadWithOffset); } |
	MOVE_OP_STORE_OFFSET { setOperation(iris::MoveOp::StoreWithOffset); } |
	MOVE_OP_LOAD_OFFSET { setOperation(iris::MoveOp::LoadWithOffset); }
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
		COND_TRUE_BRANCH_LR { setOperation(iris::JumpOp::ConditionalTrueJumpLinkRegister); } |
		COND_TRUE_BRANCH_LR_LINK { setOperation(iris::JumpOp::ConditionalTrueJumpLinkRegisterLink); } |
		COND_FALSE_BRANCH_LR { setOperation(iris::JumpOp::ConditionalFalseJumpLinkRegister); } |
		COND_FALSE_BRANCH_LR_LINK { setOperation(iris::JumpOp::ConditionalFalseJumpLinkRegisterLink); };
jop_no_args:
	   BRANCH_LR_LINK { setOperation(iris::JumpOp::UnconditionalJumpLinkRegisterLink); } |
	   BRANCH_LR { setOperation(iris::JumpOp::UnconditionalJumpLinkRegister); } 
	   ;

jop_imm:
       JUMP_OP_UNCONDITIONALIMMEDIATE { setOperation(iris::JumpOp::UnconditionalImmediate); } | 
       JUMP_OP_UNCONDITIONALIMMEDIATELINK { setOperation(iris::JumpOp::UnconditionalImmediateLink); };
jop_reg:
       JUMP_OP_UNCONDITIONALREGISTER { setOperation(iris::JumpOp::UnconditionalRegister); } |
       JUMP_OP_UNCONDITIONALREGISTERLINK { setOperation(iris::JumpOp::UnconditionalRegisterLink); }
	   ;
jop_cond_imm:
   JUMP_OP_CONDITIONALTRUEIMMEDIATE { setOperation(iris::JumpOp::ConditionalTrueImmediate); } |
   JUMP_OP_CONDITIONALTRUEIMMEDIATELINK { setOperation(iris::JumpOp::ConditionalTrueImmediateLink); } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATE { setOperation(iris::JumpOp::ConditionalFalseImmediate); } |
   JUMP_OP_CONDITIONALFALSEIMMEDIATELINK { setOperation(iris::JumpOp::ConditionalFalseImmediateLink); } 
   ;


jop_cond_reg:
   JUMP_OP_CONDITIONALTRUEREGISTERLINK { setOperation(iris::JumpOp::ConditionalTrueRegisterLink); } |
   JUMP_OP_CONDITIONALFALSEREGISTERLINK { setOperation(iris::JumpOp::ConditionalFalseRegisterLink); } |
   JUMP_OP_CONDITIONALTRUEREGISTER { setOperation(iris::JumpOp::ConditionalTrueRegister); } |
   JUMP_OP_CONDITIONALFALSEREGISTER { setOperation(iris::JumpOp::ConditionalFalseRegister); }
   ;

jop_cond_reg_reg:
   JUMP_OP_IFTHENELSENORMALPREDTRUE { setOperation(iris::JumpOp::IfThenElseNormalPredTrue); } |
   JUMP_OP_IFTHENELSENORMALPREDFALSE { setOperation(iris::JumpOp::IfThenElseNormalPredFalse); } |
   JUMP_OP_IFTHENELSELINKPREDTRUE { setOperation(iris::JumpOp::IfThenElseLinkPredTrue); } |
   JUMP_OP_IFTHENELSELINKPREDFALSE { setOperation(iris::JumpOp::IfThenElseLinkPredFalse); }
;

compare_op:
		  cop DESTINATION_PREDICATE_REGISTERS SOURCE0_GPR SOURCE1_GPR |
		  icop DESTINATION_PREDICATE_REGISTERS SOURCE0_GPR half_immediate;
cop:
   COMPARE_OP_EQ { setOperation(iris::CompareOp::Eq); } |
   COMPARE_OP_NEQ { setOperation(iris::CompareOp::Neq); } |
   COMPARE_OP_LESSTHAN { setOperation(iris::CompareOp::LessThan); } |
   COMPARE_OP_GREATERTHAN { setOperation(iris::CompareOp::GreaterThan); } |
   COMPARE_OP_LESSTHANOREQUALTO { setOperation(iris::CompareOp::LessThanOrEqualTo); } |
   COMPARE_OP_GREATERTHANOREQUALTO { setOperation(iris::CompareOp::GreaterThanOrEqualTo); } 
;
icop:
   COMPARE_OP_EQ_IMMEDIATE { setOperation(iris::CompareOp::EqImm); } |
   COMPARE_OP_NEQ_IMMEDIATE { setOperation(iris::CompareOp::NeqImm); } |
   COMPARE_OP_LESSTHAN_IMMEDIATE { setOperation(iris::CompareOp::LessThanImm); } |
   COMPARE_OP_GREATERTHAN_IMMEDIATE { setOperation(iris::CompareOp::GreaterThanImm); } |
   COMPARE_OP_LESSTHANOREQUALTO_IMMEDIATE { setOperation(iris::CompareOp::LessThanOrEqualToImm); } |
   COMPARE_OP_GREATERTHANOREQUALTO_IMMEDIATE { setOperation(iris::CompareOp::GreaterThanOrEqualToImm); }
;

cond_reg_op: 
	cond_save_restore_op DESTINATION_GPR lexeme |
	cond_two_arg DESTINATION_PREDICATE_REGISTERS |
	cond_four_arg DESTINATION_PREDICATE_REGISTERS TWO_ARG_PREDICATE_REGISTER |
	cond_three_arg DESTINATION_PREDICATE_REGISTERS SOURCE0_PREDICATE_REGISTER ;
cond_two_arg:
			OP_CR_SWAP { setOperation(iris::ConditionRegisterOp::CRSwap); } |
			OP_CR_MOVE { setOperation(iris::ConditionRegisterOp::CRMove); } ;
cond_save_restore_op:
		OP_SAVE_CR { setOperation(iris::ConditionRegisterOp::SaveCRs); } |
		OP_RESTORE_CR { setOperation(iris::ConditionRegisterOp::RestoreCRs); } ;
cond_three_arg:
		OP_CR_NOT { setOperation(iris::ConditionRegisterOp::CRNot); } ;
cond_four_arg:
			 OP_CR_XOR { setOperation(iris::ConditionRegisterOp::CRXor); } |
			 OP_CR_AND { setOperation(iris::ConditionRegisterOp::CRAnd); } |
			 OP_CR_OR { setOperation(iris::ConditionRegisterOp::CROr); } |
			 OP_CR_NAND  { setOperation(iris::ConditionRegisterOp::CRNand); } |
			 OP_CR_NOR { setOperation(iris::ConditionRegisterOp::CRNor); }
			 ;
lexeme:
      IRIS16_SYMBOL { iris::curri.hasSymbol = true; 
               iris::curri.symbol = $1; } | 
      IMMEDIATE { 
	  		setFullImmediate($1);
      }
;
PREDICATE_REGISTER_RESULT:
						 PREDICATE_REGISTER { setRegister0(iris::encodePredicateResult(iris::curri.reg0, $1)); };
PREDICATE_REGISTER_INVERSE:
						  PREDICATE_REGISTER { setRegister0(iris::encodePredicateInverseResult(iris::curri.reg0, $1)); };
DESTINATION_PREDICATE_REGISTERS:
							   PREDICATE_REGISTER_RESULT PREDICATE_REGISTER_INVERSE;

SOURCE0_PREDICATE_REGISTER:
						 PREDICATE_REGISTER { iris::curri.reg1 = iris::encodePredicateSource0(iris::curri.reg1, $1); };
SOURCE1_PREDICATE_REGISTER:
						  PREDICATE_REGISTER { iris::curri.reg1 = iris::encodePredicateSource1(iris::curri.reg1, $1); };
TWO_ARG_PREDICATE_REGISTER:
						  SOURCE0_PREDICATE_REGISTER SOURCE1_PREDICATE_REGISTER;


half_immediate:
				IMMEDIATE {
	   				if ($1 > 255) {
						iriserror("immediate value offset out of range!");
					}
					setRegister2($1);
				} | 
				TAG_LOW IMMEDIATE {
					setRegister2(syn::getLowerHalf<word>($2));
				} |
				TAG_HI IMMEDIATE {
					setRegister2(syn::getUpperHalf<word>($2));
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
void iriserror(const char* s) {
   printf("%d: %s\n", irislineno, s);
   exit(-1);
}

namespace iris {
	void assemble(FILE* input, std::ostream* output) {
	  initialize(output, input);
	  do {
		 yyparse();
	  } while(!feof(irisin));
	  resolve_labels();
	}

void add_label_entry(const std::string& c, word addr) {
   if (iris::state.labels.count(c) != 0) {
		iriserror("Found a duplicate label!");
		exit(1);
   } else {
	 iris::state.labels[c] = addr;
   }
}

void persist_dynamic_op(void) {
   iris::state.dynops.push_back(curri);
}

void save_encoding(void) {
   if(iris::curri.hasSymbol) {
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
		case iris::Segment::Code:
			buf[4] = static_cast<char>(syn::encodeBits<byte, byte, 0b11111000, 3>(
								syn::encodeBits<byte, byte, 0b00000111, 0>((byte)0, dop->group),
								dop->op));
			buf[5] = static_cast<char>(dop->reg0);
			buf[6] = static_cast<char>(dop->reg1);
			buf[7] = static_cast<char>(dop->reg2);
			break;
		case iris::Segment::Data:
			buf[4] = static_cast<char>(dop->reg1);
			buf[5] = static_cast<char>(dop->reg2);
			buf[6] = 0;
			buf[7] = 0;
			break;
		default:
			std::cerr << "panic: unknown segment " << (byte)dop->segment << std::endl;
			exit(1);
   }
   iris::state.output->write(buf, 8);
   delete[] buf;
}

void resolve_labels() {
   /* we need to go through the list of dynamic operations and replace
	  the label with the corresponding address */
   for(std::vector<dynamicop>::iterator it = iris::state.dynops.begin(); it != iris::state.dynops.end(); ++it) {
		if (!resolve_op(&(*it))) {
			std::stringstream ss;
			ss << "panic: couldn't find label " << it->symbol;
			auto str = ss.str();
			iriserror(str.c_str());
			exit(1);
		} else {
			write_dynamic_op(&(*it));
		}
   }
}
bool resolve_op(dynamicop* dop) {
   if(iris::state.labels.count(dop->symbol) == 1) {
		word addr = iris::state.labels[dop->symbol];
		dop->reg1 = syn::decodeField<word, byte, 0>(addr);
		dop->reg2 = syn::decodeField<word, byte, 1>(addr);
		return true;
   }
   return false;
}

void initialize(std::ostream* output, FILE* input) {
   irisin = input;
   iris::state.segment = iris::Segment::Code;
   iris::state.data_address = 0;
   iris::state.code_address = 0;
   iris::state.output = output;
   iris::curri.segment = iris::Segment::Code;
   iris::curri.address = 0;
   iris::curri.group = 0;
   iris::curri.op = 0;
   iris::curri.reg0 = 0;
   iris::curri.reg1 = 0;
   iris::curri.reg2 = 0;
   iris::curri.hasSymbol = false;
}
}
