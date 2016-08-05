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
enum class InstructionFields : byte {
#define X(title, mask, shift, type, is_register, post) title,
#include "def/iris17/instruction.def"
#undef X
};
template<InstructionFields field>
struct InstructionFieldInformation { };

#define X(_title, _mask, _shift, _type, _is_register, _post) \
template<> \
struct InstructionFieldInformation<InstructionFields :: _title> { \
	static constexpr word mask = _mask; \
	static constexpr byte shiftCount = _shift; \
	typedef _type AssociatedType; \
	static constexpr bool isRegister = _is_register; \
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
   word first;
   word second;
   word third;
   int hassymbol;
   std::string symbol;
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
   } | 
   asm {
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

		} |
		shift_op |
		logical_op |
		compare_op |
		arithmetic_op |
		branch_op |
		system_op |
		move_op |
		set_op |
		swap_op |
		memory_op;
compare_op:
		  OP_COMPARE compare_type combine_type compare_args {

		  };
compare_args:
		 FLAG_IMMEDIATE REGISTER IMMEDIATE {
			// IMMEDIATE8
		 } |
		 REGISTER REGISTER {

		 };

compare_type:
		COMPARE_OP_EQ {

		} |
		COMPARE_OP_NEQ {

		} |
		COMPARE_OP_LT {

		} |
		COMPARE_OP_LT_EQ {

		} |
		COMPARE_OP_GT {

		} |
		COMPARE_OP_GT_EQ {

		};
combine_type: 
		ACTION_NONE {

		} |
		ACTION_AND {

		} |
		ACTION_OR {

		} |
		ACTION_XOR {

		};

logical_op:
		OP_LOGICAL logical_op logical_args {

		} |
		OP_LOGICAL LOGICAL_OP_NOT REGISTER {

		};
logical_args: 
		FLAG_IMMEDIATE REGISTER IMMEDIATE {
			//IMMEDIATE5
		} |
		REGISTER REGISTER {

		};
logical_op: 
		ACTION_AND {

		} |
		ACTION_OR {

		} |
		ACTION_XOR {

		} |
		LOGICAL_OP_NAND {

		};
shift_op:
		OP_SHIFT shift_left_or_right shift_args {

		};
shift_args:
		FLAG_IMMEDIATE REGISTER IMMEDIATE {
			// IMMEDIATE4
		} |
		REGISTER REGISTER {

		};

shift_left_or_right:
		SHIFT_FLAG_LEFT {

		} |
		SHIFT_FLAG_RIGHT {

		};
system_op:
		OP_SYSTEM IMMEDIATE REGISTER {
		//IMMEDIATE8

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
//   if (iris17::state.labels.count(c) != 0) {
//		yyerror("Found a duplicate label!");
//		exit(1);
//   } else {
//	 iris17::state.labels[c] = addr;
//   }
}

void persist_dynamic_op(void) {
//   iris17::state.dynops.push_back(curri);
}

void save_encoding(void) {
//   if(iris17::curri.hassymbol) {
//      persist_dynamic_op();
//   } else {
//      write_dynamic_op(&curri); 
//   }
}
void write_dynamic_op(dynamicop* dop) {
   /* ((instruction & ~mask) | (value << shiftcount)) */
   /* little endian build up */
//   char* buf = new char[8];
//   buf[0] = 0;
//   buf[1] = (char)dop->segment;
//   buf[2] = (char)(dop->address & 0x00FF);
//   buf[3] = (char)((dop->address & 0xFF00) >> 8);
//   switch(dop->segment) {
//   		case iris17::Segment::Code:
//			buf[4] = (char)iris::encodeBits<byte, byte, 0b11111000, 3>(
//								iris::encodeBits<byte, byte, 0b00000111, 0>((byte)0, dop->group),
//								dop->op);
//			buf[5] = (char)dop->reg0;
//			buf[6] = (char)dop->reg1;
//			buf[7] = (char)dop->reg2;
//			break;
//		case iris17::Segment::Data:
//			buf[4] = (char)dop->reg1;
//			buf[5] = (char)dop->reg2;
//			buf[6] = 0;
//			buf[7] = 0;
//			break;
//		default:
//			std::cerr << "panic: unknown segment " << (byte)dop->segment << std::endl;
//			exit(1);
//   }
//   iris17::state.output->write(buf, 8);
//   delete[] buf;
}

void resolve_labels() {
   /* we need to go through the list of dynamic operations and replace
      the label with the corresponding address */
//   for(std::vector<dynamicop>::iterator it = iris17::state.dynops.begin(); it != iris17::state.dynops.end(); ++it) {
//   		if (!resolve_op(&(*it))) {
//			std::cerr << "panic: couldn't find label " << it->symbol << std::endl;
//			exit(1);
//		} else {
//			write_dynamic_op(&(*it));
//		}
//   }
}
bool resolve_op(dynamicop* dop) {
//   if(iris17::state.labels.count(dop->symbol) == 1) {
//		word addr = iris17::state.labels[dop->symbol];
//		dop->reg1 = iris::decodeBits<word, byte, 0x00FF>(addr);
//		dop->reg2 = iris::decodeBits<word, byte, 0xFF00, 8>(addr);
//		return true;
//   }
//   return false;
return false;
}

void initialize(std::ostream* output, FILE* input) {
//   iris17in = input;
//   iris17::state.segment = iris17::Segment::Code;
//   iris17::state.data_address = 0;
//   iris17::state.code_address = 0;
//   iris17::state.output = output;
//   iris17::curri.segment = iris17::Segment::Code;
//   iris17::curri.address = 0;
//   iris17::curri.group = 0;
//   iris17::curri.op = 0;
//   iris17::curri.reg0 = 0;
//   iris17::curri.reg1 = 0;
//   iris17::curri.reg2 = 0;
//   iris17::curri.hassymbol = 0;
}
}
