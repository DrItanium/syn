%define api.prefix {iris18}
%{
#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include "iris18.h"
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include "asm_interact.h"

#include "iris18_asm.tab.h"
#define YYERROR_VERBOSE 1
extern int yylex();
extern int yyparse();
extern FILE* iris18in;
extern int iris18lineno;

void iris18error(const char* s);
namespace iris18 {
enum class InstructionFields : byte {
#define X(title, mask, shift, type, post) title,
#include "def/iris18/instruction.def"
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
#include "def/iris18/instruction.def"
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
	labels.emplace(text, address);
}
void asmstate::registerDynamicOperation(InstructionEncoder op) {
	op.address = address;
	dynops.emplace_back(op);
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
}

iris18::asmstate state;
iris18::InstructionEncoder op;
auto ifImmediate = static_cast<byte>(0);
auto ifNotImmediate = static_cast<byte>(0);

namespace iris18 {
	void initialize(std::ostream* output, FILE* input) {
		iris18in = input;
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
%token OP_COMPLEX
%token DIRECTIVE_REGISTER_AT_START

%token COMPLEX_OP_ENCODING
%token COMPLEX_OP_ENCODING_BITSET 
%token COMPLEX_OP_ENCODING_BITUNSET
%token COMPLEX_OP_ENCODING_ENCODE 
%token COMPLEX_OP_ENCODING_DECODE

%token ARITHMETIC_OP_ADD     ARITHMETIC_OP_SUB     ARITHMETIC_OP_MUL ARITHMETIC_OP_DIV
%token ARITHMETIC_OP_REM     ARITHMETIC_OP_ADD_IMM ARITHMETIC_OP_SUB_IMM
%token ARITHMETIC_OP_MUL_IMM ARITHMETIC_OP_DIV_IMM ARITHMETIC_OP_REM_IMM
%token FLAG_IMMEDIATE TAG_INDIRECT
%token SHIFT_FLAG_LEFT SHIFT_FLAG_RIGHT
%token ACTION_NONE ACTION_AND ACTION_OR ACTION_XOR
%token LOGICAL_OP_NOT LOGICAL_OP_NAND
%token MEMORY_OP_LOAD MEMORY_OP_STORE MEMORY_OP_POP MEMORY_OP_PUSH
%token BRANCH_FLAG_JUMP BRANCH_FLAG_CALL BRANCH_FLAG_IF BRANCH_FLAG_COND
%token COMPARE_OP_EQ COMPARE_OP_NEQ COMPARE_OP_GT COMPARE_OP_GT_EQ
%token COMPARE_OP_LT COMPARE_OP_LT_EQ

%token MACRO_OP_INCREMENT MACRO_OP_DECREMENT MACRO_OP_DOUBLE MACRO_OP_HALVE
%token MACRO_OP_ZERO MACRO_OP_COPY

%token DIRECTIVE_CONSTANT DIRECTIVE_SPACE

%token <rval> REGISTER
%token <ival> IMMEDIATE
%token <sval> SYMBOL ALIAS
%token <ival> BITMASK4


%%
Q: /* empty */ |
   F
;
F: F asm | asm;
asm:
   directive | statement ;

directive:
	DIRECTIVE_ORG IMMEDIATE { state.address = ($2 & iris18::bitmask24); } |
	directive_word |
	directive_dword |
	DIRECTIVE_REGISTER_AT_START REGISTER IMMEDIATE { state.setRegisterAtStartup($2, $3); } |
	DIRECTIVE_CONSTANT IMMEDIATE ALIAS { 
		try {
			state.registerConstant($3, $2);
		} catch(iris::Problem err) {
			iris18error(err.what().c_str());
		}
	};



directive_word:
	DIRECTIVE_WORD SYMBOL {
		state.declarations.emplace_back(iris18lineno, state.address, 1, $2);
		++state.address;
	} |
	DIRECTIVE_WORD IMMEDIATE {
		state.declarations.emplace_back(iris18lineno, state.address, 1, static_cast<iris18::Word>($2 & iris18::lower16Mask));
		++state.address;
	};

directive_dword:
	DIRECTIVE_DWORD SYMBOL {
		state.declarations.emplace_back(iris18lineno, state.address, 2, $2);
		state.address += 2;
	} |
	DIRECTIVE_DWORD IMMEDIATE {
		state.declarations.emplace_back(iris18lineno, state.address, 2, static_cast<iris18::RegisterValue>($2 & iris18::bitmask24));
		state.address += 2;
	};
statement:
         label |
         operation {
		 	op.currentLine = iris18lineno;
		 	state.registerDynamicOperation(op);
			op.clear();
			op.currentLine = iris18lineno;
		 };

label:
     LABEL SYMBOL {
        auto str = std::string($2);
        state.registerLabel(str);
     };
operation:
		macro_op |
		OP_SHIFT shift_op { op.type = iris18::Operation::Shift; }|
		OP_LOGICAL logical_op { op.type = iris18::Operation::Logical; } |
		OP_COMPARE compare_op { op.type = iris18::Operation::Compare; } |
		OP_ARITHMETIC arithmetic_op { op.type = iris18::Operation::Arithmetic; } |
		OP_BRANCH branch_op { op.type = iris18::Operation::Branch; } |
		OP_SYSTEM system_op { op.type = iris18::Operation::SystemCall; }|
		OP_MOVE move_op { op.type = iris18::Operation::Move; } |
		OP_SET set_op { op.type = iris18::Operation::Set; } |
		OP_SWAP swap_op { op.type = iris18::Operation::Swap; } |
		OP_MEMORY memory_op { op.type = iris18::Operation::Memory; } |
		OP_NOP {
            op.type = iris18::Operation::Swap;
            op.arg0 = 0;
            op.arg1 = 0;
        } |
		OP_RETURN {
            op.type = iris18::Operation::Branch;
            op.isIf = false;
            op.isConditional = false;
            op.immediate = false;
            op.isCall = false;
            op.arg0 = static_cast<byte>(iris18::ArchitectureConstants::LinkRegister);
        } |
		OP_COMPLEX complex_type {
			op.type = iris18::Operation::Complex; 
		};
complex_type:
			COMPLEX_OP_ENCODING encoding_subtype { op.subType = static_cast<byte>(iris18::ComplexSubTypes::Encoding); };
encoding_subtype:
				COMPLEX_OP_ENCODING_BITSET   { op.bitmask = static_cast<byte>(iris18::EncodingOperation::BitSet); } |
				COMPLEX_OP_ENCODING_BITUNSET { op.bitmask = static_cast<byte>(iris18::EncodingOperation::BitUnset); } |
				COMPLEX_OP_ENCODING_ENCODE   { op.bitmask = static_cast<byte>(iris18::EncodingOperation::Encode); } |
				COMPLEX_OP_ENCODING_DECODE   { op.bitmask = static_cast<byte>(iris18::EncodingOperation::Decode); };
compare_op:
		  compare_type combine_type compare_args;

compare_args:
		 uses_immediate destination_register IMMEDIATE { op.arg1= static_cast<byte>($3); } |
		 uses_immediate destination_register ALIAS { 
				try {
					op.arg1 = static_cast<byte>(state.getConstantValue($3));
				} catch(iris::Problem err) {
					iris18error(err.what().c_str());
				}
		 } |
		 destination_register source_register { op.immediate = false; };

compare_type:
		COMPARE_OP_EQ { op.subType = static_cast<byte>(iris18::CompareStyle::Equals); } |
		COMPARE_OP_NEQ { op.subType = static_cast<byte>(iris18::CompareStyle::NotEquals); } |
		COMPARE_OP_LT { op.subType = static_cast<byte>(iris18::CompareStyle::LessThan); } |
		COMPARE_OP_LT_EQ { op.subType = static_cast<byte>(iris18::CompareStyle::LessThanOrEqualTo); } |
		COMPARE_OP_GT { op.subType = static_cast<byte>(iris18::CompareStyle::GreaterThanOrEqualTo); } |
		COMPARE_OP_GT_EQ { op.subType = static_cast<byte>(iris18::CompareStyle::GreaterThan); };

combine_type:
		ACTION_NONE { op.combineType = iris18::CompareCombine::None; } |
		ACTION_AND { op.combineType = iris18::CompareCombine::And; } |
		ACTION_OR { op.combineType = iris18::CompareCombine::Or; } |
		ACTION_XOR { op.combineType = iris18::CompareCombine::Xor; };

logical_op:
		logical_subop logical_args {
			op.subType = op.immediate ? ifImmediate : ifNotImmediate;
		} |
		LOGICAL_OP_NOT destination_register {
			op.immediate = false;
			op.subType = static_cast<byte>(iris18::LogicalOps::Not);
		};

logical_args:
		uses_immediate bitmask destination_register lexeme |
		destination_register source_register { op.immediate = false; };

logical_subop:
		ACTION_AND {
			ifImmediate = static_cast<byte>(iris18::ImmediateLogicalOps::And);
			ifNotImmediate = static_cast<byte>(iris18::LogicalOps::And);
		} |
		ACTION_OR {
			ifImmediate = static_cast<byte>(iris18::ImmediateLogicalOps::Or);
			ifNotImmediate = static_cast<byte>(iris18::LogicalOps::Or);
		} |
		ACTION_XOR {
			ifImmediate = static_cast<byte>(iris18::ImmediateLogicalOps::Xor);
			ifNotImmediate = static_cast<byte>(iris18::LogicalOps::Xor);
		} |
		LOGICAL_OP_NAND {
			ifImmediate = static_cast<byte>(iris18::ImmediateLogicalOps::Nand);
			ifNotImmediate = static_cast<byte>(iris18::LogicalOps::Nand);
		};
shift_op:
		shift_left_or_right shift_args;


shift_args:
		uses_immediate destination_register IMMEDIATE { op.arg1 = $3 & 0b11111; } |
		destination_register source_register { op.immediate = false; };

shift_left_or_right:
		SHIFT_FLAG_LEFT { op.shiftLeft = true; } |
		SHIFT_FLAG_RIGHT { op.shiftLeft = false; };

system_op:
		destination_register;

move_op: bitmask destination_register source_register;

set_op:
	  bitmask destination_register lexeme;

swap_op:
	   destination_register source_register;

branch_op:
		 branch;

branch:
	  	BRANCH_FLAG_IF if_op {
			op.isIf = true;
			op.immediate = false;
			op.isConditional = false;
		} |
		jump_op {
			op.isIf = false;
			op.isCall = false;
		} |
		BRANCH_FLAG_CALL call_op {
			op.isIf = false;
			op.isCall = true;
			op.isConditional = false;
		};
if_op:
	 destination_register source_register {
		op.isCall = false;
	 } |
	 BRANCH_FLAG_CALL destination_register source_register {
		op.isCall = true;
	 };
call_op:
	   uses_immediate lexeme |
	   destination_register {
			op.immediate = false;
	   };
jump_op:
	uses_immediate lexeme {
		op.isConditional = false;
	} |
	BRANCH_FLAG_COND uses_immediate lexeme {
		op.isConditional = true;
	} |
	destination_register {
		op.immediate = false;
		op.isConditional = false;
	} |
	BRANCH_FLAG_COND destination_register {
		op.immediate = false;
		op.isConditional = true;
	};
memory_op:
		load_store_combined { op.indirect = false; } |
		load_store_combined TAG_INDIRECT { op.indirect = true; } |
		stack_operation bitmask destination_register;

load_store_combined:
			load_store_op bitmask IMMEDIATE { op.arg0 = ($3 & 0b1111); } |
			load_store_op bitmask ALIAS { 
				try {
					op.arg0 = static_cast<byte>(state.getConstantValue($3));
				} catch(iris::Problem err) {
					iris18error(err.what().c_str());
				}
			};

load_store_op:
			 MEMORY_OP_LOAD {
				op.subType = static_cast<byte>(iris18::MemoryOperation::Load);
			 } |
			 MEMORY_OP_STORE {
				op.subType = static_cast<byte>(iris18::MemoryOperation::Store);
			 };
stack_operation:
			   MEMORY_OP_PUSH {
					op.subType = static_cast<byte>(iris18::MemoryOperation::Push);
			   } |
			   MEMORY_OP_POP {
					op.subType = static_cast<byte>(iris18::MemoryOperation::Pop);
			   };
arithmetic_op:
		arithmetic_subop uses_immediate REGISTER IMMEDIATE {
			op.arg0 = $3;
			op.arg1 = $4;
		} |
		arithmetic_subop REGISTER REGISTER {
			op.immediate = false;
			op.arg0 = $2;
			op.arg1 = $3;
		};
arithmetic_subop:
				ARITHMETIC_OP_ADD {
					op.subType = static_cast<byte>(iris18::ArithmeticOps::Add);
				} |
				ARITHMETIC_OP_SUB {
					op.subType = static_cast<byte>(iris18::ArithmeticOps::Sub);
				} |
				ARITHMETIC_OP_MUL {
					op.subType = static_cast<byte>(iris18::ArithmeticOps::Mul);
				} |
				ARITHMETIC_OP_DIV {
					op.subType = static_cast<byte>(iris18::ArithmeticOps::Div);
				} |
				ARITHMETIC_OP_REM {
					op.subType = static_cast<byte>(iris18::ArithmeticOps::Rem);
				};
macro_op:
		MACRO_OP_COPY destination_register source_register {
			op.type = iris18::Operation::Move;
			op.bitmask = 0b1111;
		} |
		MACRO_OP_ZERO destination_register {
			op.type = iris18::Operation::Move;
			op.bitmask = 0x0;
			op.arg1 = op.arg0;
		} |
		MACRO_OP_INCREMENT destination_register {
			op.type = iris18::Operation::Arithmetic;
			op.immediate = true;
			op.subType = static_cast<byte>(iris18::ArithmeticOps::Add);
			op.arg1 = 0x1;
		} | 
		MACRO_OP_DECREMENT destination_register {
			op.type = iris18::Operation::Arithmetic;
			op.immediate = true;
			op.subType = static_cast<byte>(iris18::ArithmeticOps::Sub);
			op.arg1 = 0x1;
		} | 
		MACRO_OP_DOUBLE destination_register {
			op.type = iris18::Operation::Arithmetic;
			op.immediate = true;
			op.subType = static_cast<byte>(iris18::ArithmeticOps::Mul);
			op.arg1 = 0x2;
		} |
		MACRO_OP_HALVE destination_register {
			op.type = iris18::Operation::Arithmetic;
			op.immediate = true;
			op.subType = static_cast<byte>(iris18::ArithmeticOps::Div);
			op.arg1 = 0x2;
		};
bitmask:
	   BITMASK4 {
			op.bitmask = $1;
	   };
lexeme:
	SYMBOL {
		op.isLabel = true;
		op.labelValue = $1;
		op.fullImmediate = 0;
	} |
	ALIAS {
		op.isLabel = false;
		try {
			op.fullImmediate = state.getConstantValue($1);
		} catch(iris::Problem err) {
			iris18error(err.what().c_str());
		}
	} |
	IMMEDIATE {
		op.isLabel = false;
		op.fullImmediate = $1;
	};
uses_immediate: FLAG_IMMEDIATE { op.immediate = true; };
destination_register: REGISTER { op.arg0 = $1; };
source_register: REGISTER { op.arg1 = $1; };
%%
namespace iris18 {
	void assemble(FILE* input, std::ostream* output) {
      initialize(output, input);
      do {
         yyparse();
      } while(!feof(iris18in));
      resolveLabels();
	  saveEncoding();
	}
}
void iris18error(const char* s) {
   printf("%d: %s\n", iris18lineno, s);
   exit(-1);
}
