%define api.prefix {iris19}
%{
#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include "iris19.h"
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include "asm_interact.h"

#include "iris19_asm.tab.h"
#define YYERROR_VERBOSE 1
extern int yylex();
extern int yyparse();
extern FILE* iris19in;
extern int iris19lineno;

void iris19error(const char* s);
namespace iris19 {
enum class InstructionFields : byte {
#define X(title, mask, shift, type, post) title,
#include "def/iris19/instruction.def"
#undef X
};
template<InstructionFields field>
struct InstructionFieldInformation { };

#define X(_title, _mask, _shift, _type, _post) \
template<> \
struct InstructionFieldInformation<InstructionFields :: _title> { \
	static constexpr Word mask = _mask; \
	static constexpr byte shiftCount = _shift; \
	using AssociatedType = _type ; \
};
#include "def/iris19/instruction.def"
#undef X

template<InstructionFields field>
typename InstructionFieldInformation<field>::AssociatedType encodeInstruction(RawInstruction base, typename InstructionFieldInformation<field>::AssociatedType value) {
	return iris::encodeBits<RawInstruction, InstructionFieldInformation<field>::AssociatedType, InstructionFieldInformation<field>::mask, InstructionFieldInformation<field>::shiftCount>(base, value);
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
   void registerLabel(const std::string& text);
   void registerDynamicOperation(InstructionEncoder op);
   void setRegisterAtStartup(byte index, RegisterValue value);
   RegisterValue getConstantValue(const std::string& text);
   void registerConstant(const std::string& title, RegisterValue value);
   RegisterValue address;
   std::map<std::string, RegisterValue> labels;
   std::vector<data_registration> declarations;
   std::vector<InstructionEncoder> dynops;
   std::ostream* output;
   std::map<std::string, RegisterValue> constants;
   RegisterValue registerStartupValues[ArchitectureConstants::RegisterCount] = { 0 };
};

void asmstate::registerLabel(const std::string& text) {
#ifdef DEBUG
	std::cout << "Registered label " << text << " for address: " <<  std::hex << address << std::endl;
#endif
	labels.emplace(text, address);
}
void asmstate::registerDynamicOperation(InstructionEncoder op) {
	op.address = address;
	dynops.emplace_back(op);
#ifdef DEBUG
	std::cout << "address: " << std::hex << op.address << " numWords: " << op.numWords() << std::endl;
#endif
	address += op.numWords();
}
void asmstate::setRegisterAtStartup(byte index, RegisterValue value) {
	if (index >= ArchitectureConstants::RegisterCount)  {
		throw iris::Problem("Out of range register set!");
	} else {
		registerStartupValues[index] = value;
	}
}

void asmstate::registerConstant(const std::string& text, RegisterValue value) {
	auto result = constants.find(text);
	if (result == constants.end()) {
		constants.emplace(text, value);
	} else {
		std::stringstream stream;
		stream << "Redefining constant: " << text << " to " << std::hex << value << "!";
		throw iris::Problem(stream.str());
	}

}
RegisterValue asmstate::getConstantValue(const std::string& text) {
	auto result = constants.find(text);
	if (result != constants.end()) {
		return result->second;
	} else {
		std::stringstream stream;
		stream << "Undefined constant " << text << "!";
		throw iris::Problem(stream.str());
	}
}


void initialize(std::ostream* output, FILE* input);
void resolveLabels();
void saveEncoding();
asmstate state;
InstructionEncoder op;
auto ifImmediate = static_cast<byte>(0);
auto ifNotImmediate = static_cast<byte>(0);
}


namespace iris19 {
	void initialize(std::ostream* output, FILE* input) {
		iris19in = input;
		state.output = output;
		state.address = 0;
	}
	void resolveLabels() {
		// need to go through and replace all symbols with corresponding immediates
		for (auto &op : state.dynops) {
			if (op.isLabel) {
				auto label = op.labelValue;
				if (!state.labels.count(label)) {
					std::stringstream stream;
					stream << op.currentLine << ": label " << label << " does not exist!\n";
					throw iris::Problem(stream.str());
				}
				op.fullImmediate = state.labels[label];
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
	void writeRegisterEntry(byte index, RegisterValue value) {
		constexpr auto bufSize = 8;
		char buf[bufSize] = { 0 };
		buf[0] = 1;
		buf[1] = index;
		buf[2] = static_cast<char>(value);
		buf[3] = static_cast<char>(value >> 8);
		buf[4] = static_cast<char>(value >> 16);
		buf[5] = static_cast<char>(value >> 24);
		state.output->write(buf, bufSize);
	}
	void writeEntry(RegisterValue address, Word value) {
		constexpr auto bufSize = 8;
		char buf[bufSize] = { 0 };
		buf[2] = static_cast<char>(address);
		buf[3] = static_cast<char>(address >> 8);
		buf[4] = static_cast<char>(address >> 16);
		buf[5] = static_cast<char>(address >> 24);
		buf[6] = static_cast<char>(value);
		buf[7] = static_cast<char>(value >> 8);
		state.output->write(buf, bufSize);
	}
	void saveEncoding() {
		for (byte i = 0; i < ArchitectureConstants::RegisterCount; ++i) {
			writeRegisterEntry(i, state.registerStartupValues[i]);
		}
		// go through and generate our list of dynamic operations and corresponding declarations
		// start with the declarations because they are easier :)
		for (auto &reg : state.declarations) {
			switch(reg.width) {
				case 2:
					writeEntry(reg.address + 1, static_cast<Word>(reg.immediate >> 16));
				case 1:
					writeEntry(reg.address, static_cast<Word>(reg.immediate));
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
%token DIRECTIVE_REGISTER_AT_START

%token ARITHMETIC_OP_ADD     ARITHMETIC_OP_SUB     ARITHMETIC_OP_MUL ARITHMETIC_OP_DIV
%token ARITHMETIC_OP_REM     ARITHMETIC_OP_ADD_IMM ARITHMETIC_OP_SUB_IMM
%token ARITHMETIC_OP_MUL_IMM ARITHMETIC_OP_DIV_IMM ARITHMETIC_OP_REM_IMM
%token FLAG_IMMEDIATE 
%token SHIFT_FLAG_LEFT SHIFT_FLAG_RIGHT
%token ACTION_AND ACTION_OR ACTION_XOR
%token LOGICAL_OP_NOT LOGICAL_OP_NAND
%token MEMORY_OP_LOAD MEMORY_OP_STORE MEMORY_OP_POP MEMORY_OP_PUSH
%token BRANCH_FLAG_JUMP BRANCH_FLAG_CALL BRANCH_FLAG_IF BRANCH_FLAG_COND
%token COMPARE_OP_EQ COMPARE_OP_NEQ COMPARE_OP_GT COMPARE_OP_GT_EQ
%token COMPARE_OP_LT COMPARE_OP_LT_EQ
%token TAG_STACK TAG_INDIRECT

%token MACRO_OP_INCREMENT MACRO_OP_DECREMENT MACRO_OP_DOUBLE MACRO_OP_HALVE
%token MACRO_OP_ZERO MACRO_OP_COPY

%token DIRECTIVE_CONSTANT DIRECTIVE_SPACE

%token <rval> REGISTER
%token <ival> IMMEDIATE
%token <sval> SYMBOL ALIAS
%token <ival> BITMASK


%%
Q: /* empty */ |
   F
;
F: F asm | asm;
asm:
   directive | statement ;

directive:
	DIRECTIVE_ORG IMMEDIATE { iris19::state.address = ($2 & iris19::memoryMaxBitmask); } |
	directive_word |
	directive_dword |
	DIRECTIVE_REGISTER_AT_START REGISTER IMMEDIATE { iris19::state.setRegisterAtStartup($2, $3); } |
	DIRECTIVE_CONSTANT IMMEDIATE ALIAS {
		try {
			iris19::state.registerConstant($3, $2);
		} catch(iris::Problem err) {
			iris19error(err.what().c_str());
		}
	};



directive_word:
	DIRECTIVE_WORD SYMBOL {
		iris19::state.declarations.emplace_back(iris19lineno, iris19::state.address, 1, $2);
		++iris19::state.address;
	} |
	DIRECTIVE_WORD IMMEDIATE {
		iris19::state.declarations.emplace_back(iris19lineno, iris19::state.address, 1, static_cast<iris19::Word>($2 & iris19::lower32Mask));
		++iris19::state.address;
	};

directive_dword:
	DIRECTIVE_DWORD SYMBOL {
		iris19::state.declarations.emplace_back(iris19lineno, iris19::state.address, 2, $2);
		iris19::state.address += 2;
	} |
	DIRECTIVE_DWORD IMMEDIATE {
		iris19::state.declarations.emplace_back(iris19lineno, iris19::state.address, 2, static_cast<iris19::RegisterValue>($2));
		iris19::state.address += 2;
	};
statement:
         label |
         operation {
		 	iris19::op.currentLine = iris19lineno;
		 	iris19::state.registerDynamicOperation(iris19::op);
			iris19::op.clear();
			iris19::op.currentLine = iris19lineno;
		 };

label:
     LABEL SYMBOL {
        auto str = std::string($2);
        iris19::state.registerLabel(str);
     };
operation:
		macro_op |
		OP_SHIFT shift_op { iris19::op.type = iris19::Operation::Shift; }|
		OP_LOGICAL logical_op { iris19::op.type = iris19::Operation::Logical; } |
		OP_COMPARE compare_op { iris19::op.type = iris19::Operation::Compare; } |
		OP_ARITHMETIC arithmetic_op { iris19::op.type = iris19::Operation::Arithmetic; } |
		OP_BRANCH branch_op { iris19::op.type = iris19::Operation::Branch; } |
		OP_SYSTEM system_op { iris19::op.type = iris19::Operation::SystemCall; }|
		move_group { iris19::op.type = iris19::Operation::Move; };
compare_op:
		  compare_type compare_args;

compare_args:
		 uses_immediate destination_register IMMEDIATE { iris19::op.arg1= static_cast<byte>($3); } |
		 uses_immediate destination_register ALIAS {
				try {
					iris19::op.arg1 = static_cast<byte>(iris19::state.getConstantValue($3));
				} catch(iris::Problem err) {
					iris19error(err.what().c_str());
				}
		 } |
		 destination_register source_register { iris19::op.immediate = false; };

compare_type:
		COMPARE_OP_EQ { iris19::op.subType = static_cast<byte>(iris19::CompareStyle::Equals); } |
		COMPARE_OP_NEQ { iris19::op.subType = static_cast<byte>(iris19::CompareStyle::NotEquals); } |
		COMPARE_OP_LT { iris19::op.subType = static_cast<byte>(iris19::CompareStyle::LessThan); } |
		COMPARE_OP_LT_EQ { iris19::op.subType = static_cast<byte>(iris19::CompareStyle::LessThanOrEqualTo); } |
		COMPARE_OP_GT { iris19::op.subType = static_cast<byte>(iris19::CompareStyle::GreaterThanOrEqualTo); } |
		COMPARE_OP_GT_EQ { iris19::op.subType = static_cast<byte>(iris19::CompareStyle::GreaterThan); };


logical_op:
		logical_subop logical_args {
			iris19::op.subType = iris19::op.immediate ? iris19::ifImmediate : iris19::ifNotImmediate;
		} |
		LOGICAL_OP_NOT destination_register {
			iris19::op.immediate = false;
			iris19::op.subType = static_cast<byte>(iris19::LogicalOps::Not);
		};

logical_args:
		uses_immediate bitmask destination_register lexeme |
		destination_register source_register { iris19::op.immediate = false; };

logical_subop:
		ACTION_AND {
			iris19::ifImmediate = static_cast<byte>(iris19::ImmediateLogicalOps::And);
			iris19::ifNotImmediate = static_cast<byte>(iris19::LogicalOps::And);
		} |
		ACTION_OR {
			iris19::ifImmediate = static_cast<byte>(iris19::ImmediateLogicalOps::Or);
			iris19::ifNotImmediate = static_cast<byte>(iris19::LogicalOps::Or);
		} |
		ACTION_XOR {
			iris19::ifImmediate = static_cast<byte>(iris19::ImmediateLogicalOps::Xor);
			iris19::ifNotImmediate = static_cast<byte>(iris19::LogicalOps::Xor);
		} |
		LOGICAL_OP_NAND {
			iris19::ifImmediate = static_cast<byte>(iris19::ImmediateLogicalOps::Nand);
			iris19::ifNotImmediate = static_cast<byte>(iris19::LogicalOps::Nand);
		};
shift_op:
		shift_left_or_right shift_args;


shift_args:
		uses_immediate destination_register IMMEDIATE { iris19::op.arg1 = $3 & 0b11111; } |
		destination_register source_register { iris19::op.immediate = false; };

shift_left_or_right:
		SHIFT_FLAG_LEFT { iris19::op.shiftLeft = true; } |
		SHIFT_FLAG_RIGHT { iris19::op.shiftLeft = false; };

move_group: 
		  move_op { iris19::op.subType = static_cast<byte>(iris19::MoveOperation::Move); } | 
		  set_op { iris19::op.subType = static_cast<byte>(iris19::MoveOperation::Set); } |
		  swap_op { iris19::op.subType = static_cast<byte>(iris19::MoveOperation::Swap); };

move_op: 
	   OP_MOVE bitmask destination_register source_register |
	   OP_RETURN {
	   		// equivalent to move 0m11111111 ip stack sp
			iris19::op.bitmask = 0b11111111;
			iris19::op.arg0 = iris19::encodeRegisterValue(iris19::ArchitectureConstants::InstructionPointer, false, false);
			iris19::op.arg1 = iris19::encodeRegisterValue(iris19::ArchitectureConstants::StackPointer, false, true);
	   };

set_op:
	  OP_SET bitmask destination_register lexeme;

swap_op:
		OP_SWAP destination_register source_register |
		OP_NOP {
			iris19::op.arg0 = 0;
			iris19::op.arg1 = 0;
		};


branch_op:
		 branch;

branch:
	  	BRANCH_FLAG_IF if_op {
			iris19::op.isIf = true;
			iris19::op.immediate = false;
			iris19::op.isConditional = false;
		} |
		jump_op {
			iris19::op.isIf = false;
			iris19::op.isCall = false;
		} |
		BRANCH_FLAG_CALL call_op {
			iris19::op.isIf = false;
			iris19::op.isCall = true;
			iris19::op.isConditional = false;
		};
if_op:
	 if_uses_call destination_register source_register;
if_uses_call:
	BRANCH_FLAG_CALL { iris19::op.isCall = true; } |
	{ iris19::op.isCall = false; };
call_op:
	   uses_immediate lexeme |
	   destination_register {
			iris19::op.immediate = false;
	   };
jump_op:
	cond_decl uses_immediate lexeme |
	cond_decl destination_register {
		iris19::op.immediate = false;
	};
cond_decl:
		 BRANCH_FLAG_COND {
			iris19::op.isConditional = true;
		 } | {
		 	iris19::op.isConditional = false;
		 };


system_op:
		IMMEDIATE source_register {
            iris19::op.arg0 = ($1 & 0b1111);
        };
//immediate_or_alias:
//		IMMEDIATE { iris19::op.arg0 = ($1 & 0b1111); } |
//		ALIAS {
//				try {
//					iris19::op.arg0 = static_cast<byte>(iris19::state.getConstantValue($1));
//				} catch(iris::Problem err) {
//					iris19error(err.what().c_str());
//				}
//		};


arithmetic_op:
		arithmetic_subop uses_immediate REGISTER IMMEDIATE {
			iris19::op.arg0 = $3;
			iris19::op.arg1 = $4;
		} |
		arithmetic_subop REGISTER REGISTER {
			iris19::op.immediate = false;
			iris19::op.arg0 = $2;
			iris19::op.arg1 = $3;
		};
arithmetic_subop:
				ARITHMETIC_OP_ADD {
					iris19::op.subType = static_cast<byte>(iris19::ArithmeticOps::Add);
				} |
				ARITHMETIC_OP_SUB {
					iris19::op.subType = static_cast<byte>(iris19::ArithmeticOps::Sub);
				} |
				ARITHMETIC_OP_MUL {
					iris19::op.subType = static_cast<byte>(iris19::ArithmeticOps::Mul);
				} |
				ARITHMETIC_OP_DIV {
					iris19::op.subType = static_cast<byte>(iris19::ArithmeticOps::Div);
				} |
				ARITHMETIC_OP_REM {
					iris19::op.subType = static_cast<byte>(iris19::ArithmeticOps::Rem);
				};
macro_op:
		MACRO_OP_COPY destination_register source_register {
			iris19::op.type = iris19::Operation::Move;
			iris19::op.bitmask = 0b1111;
		} |
		MACRO_OP_ZERO destination_register {
			iris19::op.type = iris19::Operation::Move;
			iris19::op.bitmask = 0x0;
			iris19::op.arg1 = iris19::op.arg0;
		} |
		MACRO_OP_INCREMENT destination_register {
			iris19::op.type = iris19::Operation::Arithmetic;
			iris19::op.immediate = true;
			iris19::op.subType = static_cast<byte>(iris19::ArithmeticOps::Add);
			iris19::op.arg1 = 0x1;
		} |
		MACRO_OP_DECREMENT destination_register {
			iris19::op.type = iris19::Operation::Arithmetic;
			iris19::op.immediate = true;
			iris19::op.subType = static_cast<byte>(iris19::ArithmeticOps::Sub);
			iris19::op.arg1 = 0x1;
		} |
		MACRO_OP_DOUBLE destination_register {
			iris19::op.type = iris19::Operation::Arithmetic;
			iris19::op.immediate = true;
			iris19::op.subType = static_cast<byte>(iris19::ArithmeticOps::Mul);
			iris19::op.arg1 = 0x2;
		} |
		MACRO_OP_HALVE destination_register {
			iris19::op.type = iris19::Operation::Arithmetic;
			iris19::op.immediate = true;
			iris19::op.subType = static_cast<byte>(iris19::ArithmeticOps::Div);
			iris19::op.arg1 = 0x2;
		} |
		COMPARE_OP_EQ destination_register source_register { 
			iris19::op.type = iris19::Operation::Compare;
			iris19::op.subType = static_cast<byte>(iris19::CompareStyle::Equals); 
			iris19::op.immediate = false;
		} |
		COMPARE_OP_NEQ destination_register source_register {
			iris19::op.type = iris19::Operation::Compare;
			iris19::op.subType = static_cast<byte>(iris19::CompareStyle::NotEquals); 
			iris19::op.immediate = false;
		};
bitmask:
	   BITMASK {
			iris19::op.bitmask = $1;
	   };
lexeme:
	SYMBOL {
		iris19::op.isLabel = true;
		iris19::op.labelValue = $1;
		iris19::op.fullImmediate = 0;
	} |
	ALIAS {
		iris19::op.isLabel = false;
		try {
			iris19::op.fullImmediate = iris19::state.getConstantValue($1);
		} catch(iris::Problem err) {
			iris19error(err.what().c_str());
		}
	} |
	IMMEDIATE {
		iris19::op.isLabel = false;
		iris19::op.fullImmediate = $1;
	};
uses_immediate: 
			  FLAG_IMMEDIATE { 
				  iris19::op.immediate = true; 
			  };
destination_register: 
					TAG_STACK REGISTER {
						iris19::op.arg0 = iris19::encodeRegisterValue($2, false, true);
					} |
					TAG_INDIRECT REGISTER {
						iris19::op.arg0 = iris19::encodeRegisterValue($2, true, false);
					} |
					REGISTER { 
						iris19::op.arg0 = iris19::encodeRegisterValue($1, false, false);
					};
source_register: 
			   TAG_STACK REGISTER {
					iris19::op.arg1 = iris19::encodeRegisterValue($2, false, true);
				} |
				TAG_INDIRECT REGISTER {
					iris19::op.arg1 = iris19::encodeRegisterValue($2, true, false);
				} |
				REGISTER { 
					iris19::op.arg1 = iris19::encodeRegisterValue($1, false, false);
				};
source1_register: 
				TAG_STACK REGISTER {
					iris19::op.arg2 = iris19::encodeRegisterValue($2, false, true);
				} |
				TAG_INDIRECT REGISTER {
					iris19::op.arg2 = iris19::encodeRegisterValue($2, true, false);
				} |
				REGISTER { 
					iris19::op.arg2 = iris19::encodeRegisterValue($1, false, false);
				};
%%
namespace iris19 {
	void assemble(FILE* input, std::ostream* output) {
      initialize(output, input);
      do {
         yyparse();
      } while(!feof(iris19in));
      resolveLabels();
	  saveEncoding();
	}
}
void iris19error(const char* s) {
   printf("%d: %s\n", iris19lineno, s);
   exit(-1);
}
