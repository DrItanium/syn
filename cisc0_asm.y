%define api.prefix {cisc0}
%{
#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include "cisc0.h"
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include "asm_interact.h"

#include "cisc0_asm.tab.h"
#define YYERROR_VERBOSE 1
extern int yylex();
extern int yyparse();
extern FILE* cisc0in;
extern int cisc0lineno;

void cisc0error(const char* s);
namespace cisc0 {
enum class InstructionFields : byte {
#define X(title, mask, shift, type, post) title,
#include "def/cisc0/instruction.def"
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
#include "def/cisc0/instruction.def"
#undef X

template<InstructionFields field>
typename InstructionFieldInformation<field>::AssociatedType encodeInstruction(RawInstruction base, typename InstructionFieldInformation<field>::AssociatedType value) {
	return syn::encodeBits<RawInstruction, InstructionFieldInformation<field>::AssociatedType, InstructionFieldInformation<field>::mask, InstructionFieldInformation<field>::shiftCount>(base, value);
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
		throw syn::Problem("Out of range register set!");
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
		throw syn::Problem(stream.str());
	}

}
RegisterValue asmstate::getConstantValue(const std::string& text) {
	auto result = constants.find(text);
	if (result != constants.end()) {
		return result->second;
	} else {
		std::stringstream stream;
		stream << "Undefined constant " << text << "!";
		throw syn::Problem(stream.str());
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


namespace cisc0 {
	void initialize(std::ostream* output, FILE* input) {
		cisc0in = input;
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
					throw syn::Problem(stream.str());
				}
				op.fullImmediate = state.labels[label];
			}
		}
		for (auto &reg : state.declarations) {
			if (reg.setImmediate) {
				if (!state.labels.count(reg.label)) {
					std::stringstream stream;
					stream << reg.currentLine << ": label " << reg.label << " does not exist!\n";
					throw syn::Problem(stream.str());
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
					throw syn::Problem(str);
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
					throw syn::Problem(str);
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
%token <sval> IRIS18_SYMBOL ALIAS
%token <ival> BITMASK4


%%
Q: /* empty */ |
   F
;
F: F asm | asm;
asm:
   directive | statement ;

directive:
	DIRECTIVE_ORG IMMEDIATE { cisc0::state.address = ($2 & cisc0::bitmask24); } |
	directive_word |
	directive_dword |
	DIRECTIVE_REGISTER_AT_START REGISTER IMMEDIATE { cisc0::state.setRegisterAtStartup($2, $3); } |
	DIRECTIVE_CONSTANT IMMEDIATE ALIAS {
		try {
			cisc0::state.registerConstant($3, $2);
		} catch(syn::Problem err) {
			cisc0error(err.what().c_str());
		}
	};



directive_word:
	DIRECTIVE_WORD IRIS18_SYMBOL {
		cisc0::state.declarations.emplace_back(cisc0lineno, cisc0::state.address, 1, $2);
		++cisc0::state.address;
	} |
	DIRECTIVE_WORD IMMEDIATE {
		cisc0::state.declarations.emplace_back(cisc0lineno, cisc0::state.address, 1, static_cast<cisc0::Word>($2 & cisc0::lower16Mask));
		++cisc0::state.address;
	};

directive_dword:
	DIRECTIVE_DWORD IRIS18_SYMBOL {
		cisc0::state.declarations.emplace_back(cisc0lineno, cisc0::state.address, 2, $2);
		cisc0::state.address += 2;
	} |
	DIRECTIVE_DWORD IMMEDIATE {
		cisc0::state.declarations.emplace_back(cisc0lineno, cisc0::state.address, 2, static_cast<cisc0::RegisterValue>($2 & cisc0::bitmask24));
		cisc0::state.address += 2;
	};
statement:
         label |
         operation {
		 	cisc0::op.currentLine = cisc0lineno;
		 	cisc0::state.registerDynamicOperation(cisc0::op);
			cisc0::op.clear();
			cisc0::op.currentLine = cisc0lineno;
		 };

label:
     LABEL IRIS18_SYMBOL {
        auto str = std::string($2);
        cisc0::state.registerLabel(str);
     };
operation:
		macro_op |
		OP_SHIFT shift_op { cisc0::op.type = cisc0::Operation::Shift; }|
		OP_LOGICAL logical_op { cisc0::op.type = cisc0::Operation::Logical; } |
		OP_COMPARE compare_op { cisc0::op.type = cisc0::Operation::Compare; } |
		OP_ARITHMETIC arithmetic_op { cisc0::op.type = cisc0::Operation::Arithmetic; } |
		OP_BRANCH branch_op { cisc0::op.type = cisc0::Operation::Branch; } |
		OP_SYSTEM system_op { cisc0::op.type = cisc0::Operation::SystemCall; }|
		OP_MOVE move_op { cisc0::op.type = cisc0::Operation::Move; } |
		OP_SET set_op { cisc0::op.type = cisc0::Operation::Set; } |
		OP_SWAP swap_op { cisc0::op.type = cisc0::Operation::Swap; } |
		OP_MEMORY memory_op { cisc0::op.type = cisc0::Operation::Memory; } |
		OP_NOP {
            cisc0::op.type = cisc0::Operation::Swap;
            cisc0::op.arg0 = 0;
            cisc0::op.arg1 = 0;
        } |
		OP_RETURN {
			cisc0::op.type = cisc0::Operation::Memory;
			cisc0::op.indirect = false;
			cisc0::op.subType = static_cast<byte>(cisc0::MemoryOperation::Pop);
            cisc0::op.arg0 = static_cast<byte>(cisc0::ArchitectureConstants::InstructionPointer);
			cisc0::op.bitmask = 0b1111;
        } |
		OP_COMPLEX complex_type {
			cisc0::op.type = cisc0::Operation::Complex;
		};
complex_type:
			COMPLEX_OP_ENCODING encoding_subtype { cisc0::op.subType = static_cast<byte>(cisc0::ComplexSubTypes::Encoding); };
encoding_subtype:
				COMPLEX_OP_ENCODING_BITSET   { cisc0::op.bitmask = static_cast<byte>(cisc0::EncodingOperation::BitSet); } |
				COMPLEX_OP_ENCODING_BITUNSET { cisc0::op.bitmask = static_cast<byte>(cisc0::EncodingOperation::BitUnset); } |
				COMPLEX_OP_ENCODING_ENCODE   { cisc0::op.bitmask = static_cast<byte>(cisc0::EncodingOperation::Encode); } |
				COMPLEX_OP_ENCODING_DECODE   { cisc0::op.bitmask = static_cast<byte>(cisc0::EncodingOperation::Decode); };
compare_op:
		  compare_type combine_type compare_args;

compare_args:
		 uses_immediate destination_register IMMEDIATE { cisc0::op.arg1= static_cast<byte>($3); } |
		 uses_immediate destination_register ALIAS {
				try {
					cisc0::op.arg1 = static_cast<byte>(cisc0::state.getConstantValue($3));
				} catch(syn::Problem err) {
					cisc0error(err.what().c_str());
				}
		 } |
		 destination_register source_register { cisc0::op.immediate = false; };

compare_type:
		COMPARE_OP_EQ { cisc0::op.subType = static_cast<byte>(cisc0::CompareStyle::Equals); } |
		COMPARE_OP_NEQ { cisc0::op.subType = static_cast<byte>(cisc0::CompareStyle::NotEquals); } |
		COMPARE_OP_LT { cisc0::op.subType = static_cast<byte>(cisc0::CompareStyle::LessThan); } |
		COMPARE_OP_LT_EQ { cisc0::op.subType = static_cast<byte>(cisc0::CompareStyle::LessThanOrEqualTo); } |
		COMPARE_OP_GT { cisc0::op.subType = static_cast<byte>(cisc0::CompareStyle::GreaterThanOrEqualTo); } |
		COMPARE_OP_GT_EQ { cisc0::op.subType = static_cast<byte>(cisc0::CompareStyle::GreaterThan); };

combine_type:
		ACTION_NONE { cisc0::op.combineType = cisc0::CompareCombine::None; } |
		ACTION_AND { cisc0::op.combineType = cisc0::CompareCombine::And; } |
		ACTION_OR { cisc0::op.combineType = cisc0::CompareCombine::Or; } |
		ACTION_XOR { cisc0::op.combineType = cisc0::CompareCombine::Xor; };

logical_op:
		logical_subop logical_args {
			cisc0::op.subType = cisc0::op.immediate ? cisc0::ifImmediate : cisc0::ifNotImmediate;
		} |
		LOGICAL_OP_NOT destination_register {
			cisc0::op.immediate = false;
			cisc0::op.subType = static_cast<byte>(cisc0::LogicalOps::Not);
		};

logical_args:
		uses_immediate bitmask destination_register lexeme |
		destination_register source_register { cisc0::op.immediate = false; };

logical_subop:
		ACTION_AND {
			cisc0::ifImmediate = static_cast<byte>(cisc0::ImmediateLogicalOps::And);
			cisc0::ifNotImmediate = static_cast<byte>(cisc0::LogicalOps::And);
		} |
		ACTION_OR {
			cisc0::ifImmediate = static_cast<byte>(cisc0::ImmediateLogicalOps::Or);
			cisc0::ifNotImmediate = static_cast<byte>(cisc0::LogicalOps::Or);
		} |
		ACTION_XOR {
			cisc0::ifImmediate = static_cast<byte>(cisc0::ImmediateLogicalOps::Xor);
			cisc0::ifNotImmediate = static_cast<byte>(cisc0::LogicalOps::Xor);
		} |
		LOGICAL_OP_NAND {
			cisc0::ifImmediate = static_cast<byte>(cisc0::ImmediateLogicalOps::Nand);
			cisc0::ifNotImmediate = static_cast<byte>(cisc0::LogicalOps::Nand);
		};
shift_op:
		shift_left_or_right shift_args;


shift_args:
		uses_immediate destination_register IMMEDIATE { cisc0::op.arg1 = $3 & 0b11111; } |
		destination_register source_register { cisc0::op.immediate = false; };

shift_left_or_right:
		SHIFT_FLAG_LEFT { cisc0::op.shiftLeft = true; } |
		SHIFT_FLAG_RIGHT { cisc0::op.shiftLeft = false; };


move_op: bitmask destination_register source_register;

set_op:
	  bitmask destination_register lexeme;

swap_op:
	   destination_register source_register;

branch_op:
		 branch;

branch:
	  	BRANCH_FLAG_IF if_op {
			cisc0::op.isIf = true;
			cisc0::op.immediate = false;
			cisc0::op.isConditional = false;
		} |
		jump_op {
			cisc0::op.isIf = false;
			cisc0::op.isCall = false;
		} |
		BRANCH_FLAG_CALL call_op {
			cisc0::op.isIf = false;
			cisc0::op.isCall = true;
			cisc0::op.isConditional = false;
		};
if_op:
	 if_uses_call destination_register source_register;
if_uses_call:
	BRANCH_FLAG_CALL { cisc0::op.isCall = true; } |
	{ cisc0::op.isCall = false; };
call_op:
	   uses_immediate lexeme |
	   destination_register {
			cisc0::op.immediate = false;
	   };
jump_op:
	cond_decl uses_immediate lexeme |
	cond_decl destination_register {
		cisc0::op.immediate = false;
	};
cond_decl:
		 BRANCH_FLAG_COND {
			cisc0::op.isConditional = true;
		 } | {
		 	cisc0::op.isConditional = false;
		 };
memory_op:
		load_store_combined { cisc0::op.indirect = false; } |
		load_store_combined TAG_INDIRECT { cisc0::op.indirect = true; } |
		stack_operation bitmask stack_operation_choose;

stack_operation_choose:
		destination_register { cisc0::op.readNextWord = false; } |
		destination_register source_register { 
			// check and see if we are looking at sp
			// no need to waste a word so just use the default version
			// implicitly. SourceRegister in this case is the stack pointer stand in
			auto target_sp = cisc0::op.arg1; 
			cisc0::op.readNextWord = (target_sp != cisc0::ArchitectureConstants::StackPointer);
		};


system_op:
		IMMEDIATE source_register {
            cisc0::op.arg0 = ($1 & 0b1111);
        };
load_store_combined:
			load_store_op bitmask immediate_or_alias read_next_word;
immediate_or_alias:
		IMMEDIATE { cisc0::op.arg0 = ($1 & 0b1111); } |
		ALIAS {
				try {
					cisc0::op.arg0 = static_cast<byte>(cisc0::state.getConstantValue($1));
				} catch(syn::Problem err) {
					cisc0error(err.what().c_str());
				}
		};
read_next_word: 
		REGISTER REGISTER {
			cisc0::op.arg1 = $1;
			cisc0::op.arg2 = $2;
			// check and see if arg1 == address and arg2 == value
			// if so, then use the compressed version!
			auto usingImplicitRegisters = (cisc0::op.arg1 == cisc0::ArchitectureConstants::AddressRegister) && (cisc0::op.arg2 == cisc0::ArchitectureConstants::ValueRegister);
#ifdef DEBUG
			std::cout << "\tusingImplicitRegisters: " << usingImplicitRegisters << std::endl;
#endif
			cisc0::op.readNextWord = !usingImplicitRegisters;
		} | {
			cisc0::op.readNextWord = false;
#ifdef DEBUG
			std::cout << "\tUsing implicit registers!" << std::endl;
#endif
		}; 


load_store_op:
			 MEMORY_OP_LOAD {
				cisc0::op.subType = static_cast<byte>(cisc0::MemoryOperation::Load);
			 } |
			 MEMORY_OP_STORE {
				cisc0::op.subType = static_cast<byte>(cisc0::MemoryOperation::Store);
			 };
stack_operation:
			   MEMORY_OP_PUSH {
					cisc0::op.subType = static_cast<byte>(cisc0::MemoryOperation::Push);
			   } |
			   MEMORY_OP_POP {
					cisc0::op.subType = static_cast<byte>(cisc0::MemoryOperation::Pop);
			   };
arithmetic_op:
		arithmetic_subop uses_immediate REGISTER IMMEDIATE {
			cisc0::op.arg0 = $3;
			cisc0::op.arg1 = $4;
		} |
		arithmetic_subop REGISTER REGISTER {
			cisc0::op.immediate = false;
			cisc0::op.arg0 = $2;
			cisc0::op.arg1 = $3;
		};
arithmetic_subop:
				ARITHMETIC_OP_ADD {
					cisc0::op.subType = static_cast<byte>(cisc0::ArithmeticOps::Add);
				} |
				ARITHMETIC_OP_SUB {
					cisc0::op.subType = static_cast<byte>(cisc0::ArithmeticOps::Sub);
				} |
				ARITHMETIC_OP_MUL {
					cisc0::op.subType = static_cast<byte>(cisc0::ArithmeticOps::Mul);
				} |
				ARITHMETIC_OP_DIV {
					cisc0::op.subType = static_cast<byte>(cisc0::ArithmeticOps::Div);
				} |
				ARITHMETIC_OP_REM {
					cisc0::op.subType = static_cast<byte>(cisc0::ArithmeticOps::Rem);
				};
macro_op:
		MACRO_OP_COPY destination_register source_register {
			cisc0::op.type = cisc0::Operation::Move;
			cisc0::op.bitmask = 0b1111;
		} |
		MACRO_OP_ZERO destination_register {
			cisc0::op.type = cisc0::Operation::Move;
			cisc0::op.bitmask = 0x0;
			cisc0::op.arg1 = cisc0::op.arg0;
		} |
		MACRO_OP_INCREMENT destination_register {
			cisc0::op.type = cisc0::Operation::Arithmetic;
			cisc0::op.immediate = true;
			cisc0::op.subType = static_cast<byte>(cisc0::ArithmeticOps::Add);
			cisc0::op.arg1 = 0x1;
		} |
		MACRO_OP_DECREMENT destination_register {
			cisc0::op.type = cisc0::Operation::Arithmetic;
			cisc0::op.immediate = true;
			cisc0::op.subType = static_cast<byte>(cisc0::ArithmeticOps::Sub);
			cisc0::op.arg1 = 0x1;
		} |
		MACRO_OP_DOUBLE destination_register {
			cisc0::op.type = cisc0::Operation::Arithmetic;
			cisc0::op.immediate = true;
			cisc0::op.subType = static_cast<byte>(cisc0::ArithmeticOps::Mul);
			cisc0::op.arg1 = 0x2;
		} |
		MACRO_OP_HALVE destination_register {
			cisc0::op.type = cisc0::Operation::Arithmetic;
			cisc0::op.immediate = true;
			cisc0::op.subType = static_cast<byte>(cisc0::ArithmeticOps::Div);
			cisc0::op.arg1 = 0x2;
		} |
		COMPARE_OP_EQ destination_register source_register { 
			cisc0::op.type = cisc0::Operation::Compare;
			cisc0::op.subType = static_cast<byte>(cisc0::CompareStyle::Equals); 
			cisc0::op.combineType = cisc0::CompareCombine::None; 
			cisc0::op.immediate = false;
		} |
		COMPARE_OP_NEQ destination_register source_register {
			cisc0::op.type = cisc0::Operation::Compare;
			cisc0::op.subType = static_cast<byte>(cisc0::CompareStyle::NotEquals); 
			cisc0::op.combineType = cisc0::CompareCombine::None; 
			cisc0::op.immediate = false;
		};
bitmask:
	   BITMASK4 {
			cisc0::op.bitmask = $1;
	   };
lexeme:
	IRIS18_SYMBOL {
		cisc0::op.isLabel = true;
		cisc0::op.labelValue = $1;
		cisc0::op.fullImmediate = 0;
	} |
	ALIAS {
		cisc0::op.isLabel = false;
		try {
			cisc0::op.fullImmediate = cisc0::state.getConstantValue($1);
		} catch(syn::Problem err) {
			cisc0error(err.what().c_str());
		}
	} |
	IMMEDIATE {
		cisc0::op.isLabel = false;
		cisc0::op.fullImmediate = $1;
	};
uses_immediate: FLAG_IMMEDIATE { cisc0::op.immediate = true; };
destination_register: REGISTER { cisc0::op.arg0 = $1; };
source_register: REGISTER { cisc0::op.arg1 = $1; };
%%
namespace cisc0 {
	void assemble(FILE* input, std::ostream* output) {
      initialize(output, input);
      do {
         yyparse();
      } while(!feof(cisc0in));
      resolveLabels();
	  saveEncoding();
	}
}
void cisc0error(const char* s) {
   printf("%d: %s\n", cisc0lineno, s);
   exit(-1);
}
