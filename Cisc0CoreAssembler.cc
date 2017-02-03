// Cisc0CoreAssembler rewritten to use pegtl
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include "Base.h"
#include "AssemblerBase.h"
#include "Problem.h"
#include "Cisc0Core.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>
#include <pegtl/parse.hh>
#include <vector>

namespace cisc0 {
    using Separator = syn::AsmSeparator;
    template<typename R> struct Action : pegtl::nothing<R> { };
    void reportError(const std::string& msg) {
        throw syn::Problem(msg);
    }

    class AssemblerWord {
        public:
            AssemblerWord(Address currAddress, Word value) : _currAddress(currAddress), _value(value), _isLabel(false) { }
            AssemblerWord(Address currAddress, const std::string& labelTitle) : _currAddress(currAddress), _value(0), _isLabel(true), _label(labelTitle) { }
            virtual ~AssemblerWord() { }
            Address getAddress() noexcept const { return _currAddress; }
            Word getValue() noexcept const { return _value; }
            void setValue(Word value) noexcept { _value = value; }
            bool isLabel() noexcept const { return _isLabel; }
            std::string getLabel() noexcept const { return _label; }
        private:
            Address _currAddress;
            Word _value;
            bool _isLabel;
            std::string _label;
    };
    class AssemblerState {
        cisc0::InstructionEncoder current;
    };
#define DefSymbol(title, str) \
    struct Symbol ## title : public pegtl_string_t( #str ) { }
#define DefAction(rule) template<> struct Action < rule >
#define DefApplyGeneric(type) template<typename Input> static void apply(const Input& in, type& state)
#define DefApplyInstruction DefApplyGeneric(cisc0::InstructionEncoder)
#define DefGroup(title, str) \
    DefSymbol(title, str); \
    struct Group ## title : syn::Indirection<Symbol ## title> { }; \
    DefAction(Group ## title) { \
        DefApplyInstruction { \
            state.type = Operation:: title ; \
        } \
    }

//DefSymbol(Nop, nop);
DefGroup(Shift, shift);
DefGroup(Arithmetic, arithmetic);
DefGroup(Logical, logical);
DefGroup(Compare, compare);
DefGroup(Branch, branch);
//DefSymbol(Return, return);
DefGroup(System, system);
DefGroup(Move, move);
DefGroup(Set, set);
DefGroup(Swap, swap);
DefGroup(Memory, memory);
//DefSymbol(Complex, complex);

DefSymbol(Immediate, immediate);



struct UsesImmediate : pegtl::seq<SymbolImmediate> { };

DefAction(UsesImmediate) {
    DefApplyInstruction {
        state.immediate = true;
    }
};

	template<char delim, typename T>
	using Numeral = syn::GenericNumeral<delim, T>;
    struct HexadecimalNumber : public Numeral<'x', pegtl::xdigit> { };
	DefAction(HexadecimalNumber) {
		DefApplyInstruction {
			state.fullImmediate = syn::getHexImmediate<RegisterValue>(in.string(), reportError);
		}
	};
    struct BinaryNumber : public Numeral<'b', pegtl::abnf::BIT> { };
	DefAction(BinaryNumber) {
		DefApplyInstruction {
			state.fullImmediate = syn::getBinaryImmediate<RegisterValue>(in.string(), reportError);
		}
	};
    struct DecimalNumber : public pegtl::plus<pegtl::digit> { };
	DefAction(DecimalNumber) {
		DefApplyInstruction {
			state.fullImmediate = syn::getDecimalImmediate<RegisterValue>(in.string().c_str(), reportError);
		}
	};
    struct Number : public pegtl::sor<HexadecimalNumber, DecimalNumber, BinaryNumber> { };
	DefAction(Number) {
		DefApplyInstruction {
			//state.current.hasLexeme = false;
		}
	};

	struct GeneralPurposeRegister : public syn::GenericRegister<'r'> { };
	using IndirectGPR = syn::Indirection<GeneralPurposeRegister>;
#define DefIndirectGPR(title) \
	struct title : public IndirectGPR { }

    DefIndirectGPR(DestinationRegister);
	DefAction(DestinationRegister) {
		DefApplyInstruction {
            state.arg0 = syn::getRegister<Word, ArchitectureConstants::RegisterCount>(in.string(), reportError);
		}
	};

    DefIndirectGPR(SourceRegister);
	DefAction(SourceRegister) {
		DefApplyInstruction {
            state.arg1 = syn::getRegister<Word, ArchitectureConstants::RegisterCount>(in.string(), reportError);
		}
	};
    struct TwoGPRs : pegtl::seq<DestinationRegister, Separator, SourceRegister> { };
DefSymbol(Left, left);
DefSymbol(Right, right);

struct ShiftLeftOrRight : pegtl::sor<SymbolLeft, SymbolRight> { };

DefAction(ShiftLeftOrRight) {
    DefApplyInstruction {
        state.shiftLeft = (in.string() == "left");
    }
};
struct ShiftImmediateArgs : pegtl::seq<UsesImmediate, Separator, DestinationRegister, Separator, Number> { };
DefAction(ShiftImmediateArgs) {
    DefApplyInstruction {
        state.arg1 = static_cast<byte>(state.fullImmediate) & 0b11111;
    }
};
struct ShiftRegisterArgs : TwoGPRs { };
DefAction(ShiftRegisterArgs) {
    DefApplyInstruction {
        state.immediate = false;
    }
};
struct ShiftArgs : pegtl::sor<ShiftRegisterArgs, ShiftImmediateArgs> { };

struct ShiftOperation : pegtl::seq<GroupShift, Separator, ShiftLeftOrRight, Separator, ShiftArgs> { };


}



