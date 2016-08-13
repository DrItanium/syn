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
	using AssociatedType = _type ; \
}; 
#include "def/iris17/instruction.def"
#undef X

template<InstructionFields field>
typename InstructionFieldInformation<field>::AssociatedType encodeInstruction(raw_instruction base, typename InstructionFieldInformation<field>::AssociatedType value) {
	return iris::encodeBits<raw_instruction, InstructionFieldInformation<field>::AssociatedType, InstructionFieldInformation<field>::mask, InstructionFieldInformation<field>::shiftCount>(base, value);
}
struct data_registration
{
	data_registration(int lineno, RegisterValue addr, int wd, RegisterValue imm) :
		address(addr),
		width(wd),
		immediate(imm),
		setImmediate(false),
		currentLine(lineno) {

		}
	data_registration(int lineno, RegisterValue addr, int wd, const std::string& l) :
	address(addr),
	width(wd),
	immediate(0),
	setImmediate(true),
	label(l),
	currentLine(lineno) {

	}
		
	RegisterValue address = 0;
	int width = 0;
	RegisterValue immediate = 0;
	bool setImmediate = false;
	std::string label;
	int currentLine;

};
/* used to store ops which require a second pass */
struct asmstate {
   ~asmstate() { }
   void nextAddress();
   void registerLabel(const std::string& text);
   void registerDynamicOperation(InstructionEncoder op);
   RegisterValue address;
   std::map<std::string, RegisterValue> labels;
   std::vector<data_registration> declarations;
   std::vector<InstructionEncoder> dynops;
   std::ostream* output;
};

void asmstate::nextAddress() {
	++address;
}
void asmstate::registerLabel(const std::string& text) {
	labels.emplace(text, address);
}
void asmstate::registerDynamicOperation(InstructionEncoder op) {
	op.address = address;
	dynops.emplace_back(op);
	address += op.numWords();
}


void initialize(std::ostream* output, FILE* input);
void resolveLabels();
void saveEncoding();
}

iris17::asmstate state;
iris17::InstructionEncoder op;

namespace iris17 {
	void initialize(std::ostream* output, FILE* input) {
		iris17in = input;
		state.output = output;
	}
	void resolveLabels() {
		// need to go through and replace all symbols with corresponding immediates
		for (auto &op : state.dynops) {
			if (op.hasLabel()) {
				auto label = op.getLabel();
				if (!state.labels.count(label)) {
					std::stringstream stream;
					stream << op.currentLine << ": label " << label << " does not exist!\n";
					throw iris::Problem(stream.str());
				}
				op.imbueImmediate(state.labels[label]);
			}
		}
		for (auto &reg : state.declarations) {
			if (reg.setImmediate) {
				if (!state.labels.count(reg.label)) {
					std::stringstream stream;
					stream << reg.currentLine << ": label " << reg.label << " does not exist!\n";
					throw iris::Problem(stream.str());
				} else {
					reg.immediate = state.labels[reg.label];
				}
			}
		}
	}
	void writeEntry(RegisterValue address, word value) {
		char buf[6] = { 0 };
		buf[0] = static_cast<char>(address);
		buf[1] = static_cast<char>(address >> 8);
		buf[2] = static_cast<char>(address >> 16);
		buf[3] = static_cast<char>(address >> 24);
		buf[4] = static_cast<char>(value);
		buf[5] = static_cast<char>(value >> 8);
		state.output->write(buf, 6);
	}
	void saveEncoding() {
		// go through and generate our list of dynamic operations and corresponding declarations
		// start with the declarations because they are easier :)
		for (auto &reg : state.declarations) {
			switch(reg.width) {
				case 2: 
					writeEntry(reg.address + 1, static_cast<word>(reg.immediate >> 16));
				case 1: 
					writeEntry(reg.address, static_cast<word>(reg.immediate));
					break;
				default: {
					std::stringstream stream;
					stream << reg.currentLine << ": given data declaration has an unexpected width of " << reg.width << " words!\n";
					auto str = stream.str();
					throw iris::Problem(str);
				}
			}
		}
		for (auto &op : state.dynops) {
			auto encoding = op.encode();
			auto address = op.address;
			switch (std::get<0>(encoding)) {
				case 3:
					writeEntry(address + 2, std::get<3>(encoding));
				case 2:
					writeEntry(address + 1, std::get<2>(encoding));
				case 1:
					writeEntry(address, std::get<1>(encoding));
					break;
				default: {
					std::stringstream stream;
					stream << op.currentLine << ": given operation has an unexpected width of " << std::get<0>(encoding) << " words!\n";
					auto str = stream.str();
					throw iris::Problem(str);
				}
			}
		}
	}
}
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
		for (int i = 0; i < 16; ++i) {
			op.storage[i] = 0;
		}
   } | 
   asm {
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

		state.declarations.emplace_back(iris17lineno, state.address, 1, $2);
		++state.address;
	} |
	DIRECTIVE_WORD IMMEDIATE {
		state.declarations.emplace_back(iris17lineno, state.address, 1, static_cast<iris17::word>($2 & iris17::lower16Mask));
		++state.address;
	};

directive_dword:
	DIRECTIVE_DWORD SYMBOL {
		state.declarations.emplace_back(iris17lineno, state.address, 2, $2);
		state.address += 2;
	} |
	DIRECTIVE_DWORD IMMEDIATE {
		state.declarations.emplace_back(iris17lineno, state.address, 2, static_cast<iris17::RegisterValue>($2 & iris17::bitmask24));
		state.address += 2;
	};
statement:
         label |
         operation { 
		 	op.currentLine = iris17lineno;
		 	state.registerDynamicOperation(op);
		 };

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
		FLAG_IMMEDIATE BITMASK4 REGISTER logical_lexeme {
			op.Logical.immediate = true;
			op.Logical.Immediate.bitmask = $2;
			op.Logical.Immediate.destination = $3;
		} |
		REGISTER REGISTER {
			op.Logical.immediate = false;
			op.Logical.Indirect.register0 = $1;
			op.Logical.Indirect.register1 = $2;
		};
logical_lexeme:
			  IMMEDIATE {
			  		op.Logical.Immediate.isLabel = false;
					op.Logical.Immediate.source = $1;
			  } |
			  SYMBOL {
			  		op.Logical.Immediate.isLabel = true;
					op.Logical.Immediate.source = 0;
					op.Logical.Immediate.labelValue = $1;
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
      iris17::resolveLabels();
	  iris17::saveEncoding();
	}
}
void iris17error(const char* s) {
   printf("%d: %s\n", iris17lineno, s);
   exit(-1);
}
namespace iris17 {



}
