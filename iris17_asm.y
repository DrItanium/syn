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
#define YYERROR_VERBOSE 1
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
enum class InstructionFields : byte {
#define X(title, mask, shift, type, post) title,
#include "def/iris17/instruction.def"
#undef X
};
template<InstructionFields field>
struct InstructionFieldInformation { };

#define X(_title, _mask, _shift, _type, _post) \
template<> \
struct InstructionFieldInformation<InstructionFields :: _title> { \
	static constexpr word mask = _mask; \
	static constexpr byte shiftCount = _shift; \
	typedef _type AssociatedType; \
}; 
#include "def/iris17/instruction.def"
#undef X

template<InstructionFields field>
typename InstructionFieldInformation<field>::AssociatedType encodeInstruction(raw_instruction base, typename InstructionFieldInformation<field>::AssociatedType value) {
	return iris::encodeBits<raw_instruction, InstructionFieldInformation<field>::AssociatedType, InstructionFieldInformation<field>::mask, InstructionFieldInformation<field>::shiftCount>(base, value);
}
/* used to store ops which require a second pass */
typedef std::string label;
struct dynamicop {
	RegisterValue address;
	int numWords;
	Operation type;
	union {
		uint64_t storage[16]; // make sure this is the largest entry
		struct {
			bool immediate;
			CompareStyle subType;
			CompareCombine combineType;
			byte register0;
			union {
				byte register1;
				byte immediateValue;
			};
		} Compare;
		struct {
			bool immediate;
			ArithmeticOps subType;
			byte immediateValue;
			byte destination;
			byte source;
		} Arithmetic;
		struct {
			bool immediate;
			// no union so that we can be lazy when determining the subtype
			struct {
				ImmediateLogicalOps subType;
				byte bitmask;
				byte destination;
				RegisterValue source;
			} Immediate;
			struct {
				LogicalOps subType;
				byte register0;
				byte register1;
			} Indirect;
		} Logical;
		struct {
			bool shiftLeft;
			bool immediate;
			byte register0;
			union {
				byte storage;
				byte immediateValue : 5;
				byte register1 : 4;
			};
		} Shift;
		struct {
			bool isIf;
			bool isCall;
			bool isImmediate;
			bool isConditional;
			union {
				struct {
					byte onTrue;
					byte onFalse;
				} If;
				struct {
					byte destination;
				} Indirect;
				struct {
					RegisterValue immediateValue;
				} Immediate;
			} forms;
		} Branch;
		struct {
			MemoryOperation subType;
			byte bitmask;
			union {
				byte storage;
				byte offset : 4;
				byte reg : 4;
			} forms;
		} Memory;
		struct {
			byte bitmask;
			byte register0;
			byte register1;
		} Move;
		struct {
			byte bitmask;
			byte destination;
			RegisterValue immediate;
		} Set;
		struct {
			byte dest;
			byte source;
		} Swap;
		struct {
			byte arg0;
		} System;
	};
};

struct asmstate {
   ~asmstate() { }
   void nextAddress();
   void registerLabel(const std::string& text);
   void registerDynamicOperation(dynamicop op);
   RegisterValue address;
   std::map<std::string, RegisterValue> labels;
   std::vector<dynamicop> dynops;
   std::ostream* output;
};

void asmstate::nextAddress() {
	++address;
}
void asmstate::registerLabel(const std::string& text) {
	labels.emplace(text, address);
}
void asmstate::registerDynamicOperation(dynamicop op) {
	op.address = address;
	dynops.emplace_back(op);
	//TODO: fix address computation by making a class internal to iris17 to
	//handle this
}


void add_label_entry(const std::string& name, word address);
void persist_dynamic_op(void);
void save_encoding(void);
void write_dynamic_op(dynamicop* op);
void initialize(std::ostream* output, FILE* input);
void resolve_labels(void);
bool resolve_op(dynamicop* dop);
}
iris17::asmstate state;
iris17::dynamicop op;
%}

%union {
      char* sval;
      byte rval;
      unsigned long ival;
}


%token DIRECTIVE_ORG LABEL DIRECTIVE_DECLARE
%token OP_NOP OP_ARITHMETIC OP_SHIFT OP_LOGICAL OP_COMPARE OP_BRANCH OP_RETURN
%token OP_SYSTEM OP_MOVE OP_SET OP_SWAP OP_MEMORY

%token ARITHMETIC_OP_ADD     ARITHMETIC_OP_SUB     ARITHMETIC_OP_MUL ARITHMETIC_OP_DIV 
%token ARITHMETIC_OP_REM     ARITHMETIC_OP_ADD_IMM ARITHMETIC_OP_SUB_IMM 
%token ARITHMETIC_OP_MUL_IMM ARITHMETIC_OP_DIV_IMM ARITHMETIC_OP_REM_IMM 
%token FLAG_IMMEDIATE
%token SHIFT_FLAG_LEFT SHIFT_FLAG_RIGHT
%token ACTION_NONE ACTION_AND ACTION_OR ACTION_XOR
%token LOGICAL_OP_NOT LOGICAL_OP_NAND
%token MEMORY_OP_LOAD MEMORY_OP_MERGE MEMORY_OP_STORE MEMORY_OP_POP MEMORY_OP_PUSH
%token BRANCH_FLAG_JUMP BRANCH_FLAG_CALL BRANCH_FLAG_IF BRANCH_FLAG_COND
%token COMPARE_OP_EQ COMPARE_OP_NEQ COMPARE_OP_GT COMPARE_OP_GT_EQ 
%token COMPARE_OP_LT COMPARE_OP_LT_EQ


%token <rval> REGISTER
%token <ival> IMMEDIATE
%token <sval> SYMBOL
%token <ival> BITMASK4


%%
Q: /* empty */ |
   F 
;
F:
   F asm {
   		op.numWords = 1;
		for (int i = 0; i < 16; ++i) {
			op.storage[i] = 0;
		}
   } | 
   asm {
   		op.numWords = 1;
		for (int i = 0; i < 16; ++i) {
			op.storage[i] = 0;
		}
   }
   ;
asm:
   directive |
   statement
   ;
directive:
	DIRECTIVE_ORG IMMEDIATE { 

	} | 
	DIRECTIVE_DECLARE lexeme { 

	} ;
statement:
         label { }|
         operation {
         }
         ;
label:
     LABEL SYMBOL { 
     }
   ;
operation:
		OP_NOP {
			op.type = iris17::Operation::Nop;
		} |
		OP_SHIFT shift_op {
			op.type = iris17::Operation::Shift;
		}|
		OP_LOGICAL logical_op {
			op.type = iris17::Operation::Logical;
		} |
		OP_COMPARE compare_op {
			op.type = iris17::Operation::Compare;
		} |
		OP_ARITHMETIC arithmetic_op {
			op.type = iris17::Operation::Arithmetic;
		} |
		OP_BRANCH branch_op {
			op.type = iris17::Operation::Branch;
		} |
		OP_SYSTEM system_op {
			op.type = iris17::Operation::SystemCall;
		}|
		OP_MOVE move_op {
			op.type = iris17::Operation::Move;
		} |
		OP_SET set_op {
			op.type = iris17::Operation::Set; 
		} |
		OP_SWAP swap_op {
			op.type = iris17::Operation::Swap; 
		} |
		OP_MEMORY memory_op {
			op.type = iris17::Operation::Memory;
		};
compare_op:
		  compare_type combine_type compare_args;
compare_args:
		 FLAG_IMMEDIATE REGISTER IMMEDIATE {
			// IMMEDIATE8
			op.Compare.immediate = true;
			op.Compare.immediateValue = static_cast<byte>($3);
			op.Compare.register0 = $2;
		 } |
		 REGISTER REGISTER {
			op.Compare.immediate = false;
			op.Compare.register0 = $1;
			op.Compare.register1 = $2;
		 };

compare_type:
		COMPARE_OP_EQ {
			op.Compare.subType = iris17::CompareStyle::Equals;
		} |
		COMPARE_OP_NEQ {
			op.Compare.subType = iris17::CompareStyle::NotEquals;
		} |
		COMPARE_OP_LT {
			op.Compare.subType = iris17::CompareStyle::LessThan;
		} |
		COMPARE_OP_LT_EQ {
			op.Compare.subType = iris17::CompareStyle::LessThanOrEqualTo;
		} |
		COMPARE_OP_GT {
			op.Compare.subType = iris17::CompareStyle::GreaterThanOrEqualTo;
		} |
		COMPARE_OP_GT_EQ {
			op.Compare.subType = iris17::CompareStyle::GreaterThan;
		};
combine_type: 
		ACTION_NONE {
			op.Compare.combineType = iris17::CompareCombine::None;
		} |
		ACTION_AND {
			op.Compare.combineType = iris17::CompareCombine::And;
		} |
		ACTION_OR {
			op.Compare.combineType = iris17::CompareCombine::Or;
		} |
		ACTION_XOR {
			op.Compare.combineType = iris17::CompareCombine::Xor;
		};

logical_op:
		logical_op logical_args |
		LOGICAL_OP_NOT REGISTER {
			op.Logical.immediate = false;
			op.Logical.Indirect.subType = iris17::LogicalOps::Not;
			op.Logical.Indirect.register0 = $2;
		};
logical_args: 
		FLAG_IMMEDIATE BITMASK4 REGISTER IMMEDIATE {
			op.Logical.immediate = true;
			op.Logical.Immediate.bitmask = $2;
			op.Logical.Immediate.destination = $3;
			op.Logical.Immediate.source = $4; 
		} |
		REGISTER REGISTER {
			op.Logical.immediate = false;
			op.Logical.Indirect.register0 = $1;
			op.Logical.Indirect.register1 = $2;
		};
logical_op: 
		ACTION_AND {
			op.Logical.Immediate.subType = iris17::ImmediateLogicalOps::And;
			op.Logical.Indirect.subType = iris17::LogicalOps::And;
		} |
		ACTION_OR {
			op.Logical.Immediate.subType = iris17::ImmediateLogicalOps::Or;
			op.Logical.Indirect.subType = iris17::LogicalOps::Or;
		} |
		ACTION_XOR {
			op.Logical.Immediate.subType = iris17::ImmediateLogicalOps::Xor;
			op.Logical.Indirect.subType = iris17::LogicalOps::Xor;
		} |
		LOGICAL_OP_NAND {
			op.Logical.Immediate.subType = iris17::ImmediateLogicalOps::Nand;
			op.Logical.Indirect.subType = iris17::LogicalOps::Nand;
		};
shift_op:
		shift_left_or_right shift_args;
shift_args:
		FLAG_IMMEDIATE REGISTER IMMEDIATE {
			op.Shift.immediate = true;
			// IMMEDIATE4
			op.Shift.register0 = $2;
			op.Shift.immediateValue = $3 & 0b11111;
		} |
		REGISTER REGISTER {
			op.Shift.immediate = false;
			op.Shift.register0 = $1;
			op.Shift.register1 = $2;
		};

shift_left_or_right:
		SHIFT_FLAG_LEFT {
			op.Shift.shiftLeft = true;
		} |
		SHIFT_FLAG_RIGHT {
			op.Shift.shiftLeft = false;
		};
system_op:
		IMMEDIATE {
		//IMMEDIATE8
			op.System.arg0 = $1;
		};
move_op: 
	   OP_MOVE BITMASK4 REGISTER REGISTER {
	
	   };
set_op: 
	  OP_SET BITMASK4 REGISTER lexeme {

	  };
swap_op:
		OP_SWAP REGISTER REGISTER {

		};
branch_op: 
		 OP_BRANCH branch {

		 };
branch:
	  	if_op |
		jump_op |
		call_op;
if_op: 
	 BRANCH_FLAG_IF REGISTER REGISTER {

	 } |
	 BRANCH_FLAG_IF BRANCH_FLAG_CALL REGISTER REGISTER {

	 };
jump_op:
	FLAG_IMMEDIATE lexeme {

	} |
	BRANCH_FLAG_COND FLAG_IMMEDIATE lexeme {

	} |
	REGISTER {
		
	} |
	BRANCH_FLAG_COND REGISTER {

	};
call_op:
	   BRANCH_FLAG_CALL FLAG_IMMEDIATE lexeme {

	   } |
	   BRANCH_FLAG_CALL REGISTER {

	   };
memory_op:
		OP_MEMORY memory {

		};
memory:
	  	load_store_op BITMASK4 IMMEDIATE {
			// IMMEDIATE4
		} |
		stack_operation BITMASK4 REGISTER {

		};

load_store_op:
			 MEMORY_OP_LOAD {

			 } |
			 MEMORY_OP_MERGE {

			 } |
			 MEMORY_OP_STORE {

			 };
stack_operation:
			   MEMORY_OP_PUSH {
					
			   } |
			   MEMORY_OP_POP {
					
			   };
arithmetic_op:
		OP_ARITHMETIC arithmetic_subop FLAG_IMMEDIATE REGISTER IMMEDIATE {

		} |
		OP_ARITHMETIC arithmetic_subop REGISTER REGISTER {

		};
arithmetic_subop: 
				ARITHMETIC_OP_ADD {

				} |
				ARITHMETIC_OP_SUB {

				} |
				ARITHMETIC_OP_MUL {

				} |
				ARITHMETIC_OP_DIV {

				} |
				ARITHMETIC_OP_REM {

				};
lexeme: 
	  IMMEDIATE {

	  } |
	  SYMBOL {

	  };

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
}

void persist_dynamic_op(void) {
}

void save_encoding(void) {
}
void write_dynamic_op(dynamicop* dop) {
}

void resolve_labels() {
}
bool resolve_op(dynamicop* dop) {
return false;
}

void initialize(std::ostream* output, FILE* input) {
	iris17in = input;
}
}
