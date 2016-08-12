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
			byte destination;
			union {
				byte source;
				byte immediateValue;
			};
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
				byte immediateValue;
				byte register1;
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
					bool isLabel;
					char* labelValue;
				} Immediate;
			};
		} Branch;
		struct {
			MemoryOperation subType;
			byte bitmask;
			union {
				byte offset;
				byte reg;
			};
		} Memory;
		struct {
			byte bitmask;
			byte register0;
			byte register1;
		} Move;
		struct {
			byte bitmask;
			byte destination;
			bool isSymbol;
			RegisterValue immediate;
			char* label;
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
struct data_registration
{
	data_registration(RegisterValue addr, int wd, RegisterValue imm) :
		address(addr),
		width(wd),
		immediate(imm),
		setImmediate(false) {

		}
	data_registration(RegisterValue addr, int wd, const std::string& l) :
	address(addr),
	width(wd),
	immediate(0),
	setImmediate(true),
	label(l) {

	}
		
	RegisterValue address = 0;
	int width = 0;
	RegisterValue immediate = 0;
	bool setImmediate = false;
	std::string label;

};
struct asmstate {
   ~asmstate() { }
   void nextAddress();
   void registerLabel(const std::string& text);
   void registerDynamicOperation(dynamicop op);
   RegisterValue address;
   std::map<std::string, RegisterValue> labels;
   std::vector<data_registration> declarations;
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


%token LABEL DIRECTIVE_ORG DIRECTIVE_WORD DIRECTIVE_DWORD
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
		state.address = ($2 & iris17::bitmask24);
	} | 
	directive_word |
	directive_dword;

directive_word:
	DIRECTIVE_WORD SYMBOL {

		state.declarations.emplace_back(state.address, 1, $2);
		++state.address;
	} |
	DIRECTIVE_WORD IMMEDIATE {
		state.declarations.emplace_back(state.address, 1, static_cast<iris17::word>($2 & iris17::lower16Mask));
		++state.address;
	};

directive_dword:
	DIRECTIVE_DWORD SYMBOL {
		state.declarations.emplace_back(state.address, 2, $2);
		state.address += 2;
	} |
	DIRECTIVE_DWORD IMMEDIATE {
		state.declarations.emplace_back(state.address, 2, static_cast<iris17::RegisterValue>($2 & iris17::bitmask24));
		state.address += 2;
	};
statement:
         label |
         operation; 
label:
     LABEL SYMBOL {
	 	state.registerLabel($2);
     };
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
		} |
		OP_RETURN {
			op.type = iris17::Operation::Return;
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
			op.System.arg0 = $1;
		};
move_op: 
	   BITMASK4 REGISTER REGISTER {
		 op.Move.bitmask = $1;
		 op.Move.register0 = $2;
		 op.Move.register1 = $3;
	   };

set_op: 
	  BITMASK4 REGISTER set_lexeme {
		op.Set.bitmask = $1;
		op.Set.destination = $2;
	  };
set_lexeme:
		  SYMBOL {
			op.Set.isSymbol = true;
			op.Set.label = $1;
		  } |
		  IMMEDIATE {
			op.Set.isSymbol = false;
			op.Set.immediate = $1;
		  };
swap_op:
		REGISTER REGISTER {
			op.Swap.dest = $1;
			op.Swap.source = $2;
		};
branch_op: 
		 branch {

		 };
branch:
	  	BRANCH_FLAG_IF if_op {
			op.Branch.isIf = true;
			op.Branch.isImmediate = false;
			op.Branch.isConditional = false;
		} |
		jump_op {
			op.Branch.isIf = false;
			op.Branch.isCall = false;
		} |
		BRANCH_FLAG_CALL call_op {
			op.Branch.isIf = false;
			op.Branch.isCall = true;
			op.Branch.isConditional = false;
		}; 
if_op: 
	 REGISTER REGISTER {
		op.Branch.isCall = false;
		op.Branch.If.onTrue = $1;
		op.Branch.If.onFalse = $2;
	 } |
	 BRANCH_FLAG_CALL REGISTER REGISTER {
		op.Branch.isCall = true;
		op.Branch.If.onTrue = $2;
		op.Branch.If.onFalse = $3;
	 };
call_op:
	   FLAG_IMMEDIATE branch_lexeme {
			op.Branch.isImmediate = true;
	   } |
	   REGISTER {
			op.Branch.isImmediate = false;
			op.Branch.Indirect.destination = $1;
	   };
branch_lexeme: 
		   SYMBOL {
		   		op.Branch.Immediate.isLabel = true;
				op.Branch.Immediate.labelValue = $1;
			} |
			IMMEDIATE {
				op.Branch.Immediate.isLabel = false;
				op.Branch.Immediate.immediateValue = $1;
			};
jump_op:
	FLAG_IMMEDIATE branch_lexeme {
		op.Branch.isImmediate = true;
		op.Branch.isConditional = false;
	} |
	BRANCH_FLAG_COND FLAG_IMMEDIATE branch_lexeme {
		op.Branch.isImmediate = true;
		op.Branch.isConditional = true;
	} |
	REGISTER {
		op.Branch.isImmediate = false;
		op.Branch.isConditional = false;
		op.Branch.Indirect.destination = $1;
	} |
	BRANCH_FLAG_COND REGISTER {
		op.Branch.isImmediate = false;
		op.Branch.isConditional = true;
		op.Branch.Indirect.destination = $2;
	};
memory_op:
	  	load_store_op BITMASK4 IMMEDIATE {
			// IMMEDIATE4
			op.Memory.bitmask = $2;
			op.Memory.offset = ($3 & 0b1111);
		} |
		stack_operation BITMASK4 REGISTER {
			op.Memory.bitmask = $2;
			op.Memory.reg = $3;
		};

load_store_op:
			 MEMORY_OP_LOAD {
				op.Memory.subType = iris17::MemoryOperation::Load;
			 } |
			 MEMORY_OP_MERGE {
				op.Memory.subType = iris17::MemoryOperation::LoadMerge;
			 } |
			 MEMORY_OP_STORE {
				op.Memory.subType = iris17::MemoryOperation::Store;
			 };
stack_operation:
			   MEMORY_OP_PUSH {
					op.Memory.subType = iris17::MemoryOperation::Push;
			   } |
			   MEMORY_OP_POP {
					op.Memory.subType = iris17::MemoryOperation::Pop;
			   };
arithmetic_op:
		arithmetic_subop FLAG_IMMEDIATE REGISTER IMMEDIATE {
			op.Arithmetic.immediate = true;
			op.Arithmetic.destination = $3;
			op.Arithmetic.immediateValue = $4;
		} |
		arithmetic_subop REGISTER REGISTER {
			op.Arithmetic.immediate = false;
			op.Arithmetic.destination = $2;
			op.Arithmetic.source = $3;
		};
arithmetic_subop: 
				ARITHMETIC_OP_ADD {
					op.Arithmetic.subType = iris17::ArithmeticOps::Add;
				} |
				ARITHMETIC_OP_SUB {
					op.Arithmetic.subType = iris17::ArithmeticOps::Sub;
				} |
				ARITHMETIC_OP_MUL {
					op.Arithmetic.subType = iris17::ArithmeticOps::Mul;
				} |
				ARITHMETIC_OP_DIV {
					op.Arithmetic.subType = iris17::ArithmeticOps::Div;
				} |
				ARITHMETIC_OP_REM {
					op.Arithmetic.subType = iris17::ArithmeticOps::Rem;
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
