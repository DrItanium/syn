// Cisc0CoreAssembler rewritten to use pegtl
#include <string>
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
            Address getAddress() const noexcept { return _currAddress; }
            Word getValue() const noexcept { return _value; }
            void setValue(Word value) noexcept { _value = value; }
            bool isLabel() const noexcept { return _isLabel; }
            std::string getLabel() const noexcept { return _label; }
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
    // done
    DefGroup(Shift, shift);
    DefGroup(Compare, compare);
    DefGroup(Logical, logical);

    //Still left to do
    DefGroup(Arithmetic, arithmetic);
    DefGroup(Branch, branch);
    DefGroup(SystemCall, system);
    DefGroup(Move, move);
    DefGroup(Set, set);
    DefGroup(Swap, swap);
    DefGroup(Memory, memory);
    DefGroup(Complex, complex);

    //DefSymbol(Nop, nop);
    //DefSymbol(Return, return);
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
    DefAction(TwoGPRs) {
        DefApplyInstruction {
            state.immediate = false;
        }
    };
    DefSymbol(Left, left);
    DefSymbol(Right, right);

    struct ShiftLeftOrRight : pegtl::sor<SymbolLeft, SymbolRight> { };

    DefAction(ShiftLeftOrRight) {
        DefApplyInstruction {
            state.shiftLeft = (in.string() == "left");
        }
    };
    template<typename Source>
        struct ImmediateOperationArgs : pegtl::seq<UsesImmediate, Separator, DestinationRegister, Separator, Source> { };

    struct ShiftImmediateValue : pegtl::seq<Number> { };
    DefAction(ShiftImmediateValue) {
        DefApplyInstruction {
            state.arg1 = static_cast<byte>(state.fullImmediate) & 0b11111;
        }
    };
    struct ShiftArgs : pegtl::sor<TwoGPRs, ImmediateOperationArgs<ShiftImmediateValue>> { };

    struct ShiftOperation : pegtl::seq<GroupShift, Separator, ShiftLeftOrRight, Separator, ShiftArgs> { };

    struct ByteCastImmediate : pegtl::seq<Number> { };
    DefAction(ByteCastImmediate) {
        DefApplyInstruction {
            state.arg1 = static_cast<byte>(state.fullImmediate);
        }
    };
#define DefSubType(title, str, subgroup) \
    struct SubGroup ## subgroup ## title : syn::Indirection<Symbol ## title> { }; \
    DefAction(SubGroup ## subgroup ## title) { \
        DefApplyInstruction { \
            state.subType = static_cast < decltype(state.subType) > ( cisc0 :: subgroup :: title ) ; \
        } \
    }

#define DefSubTypeWithSymbol(title, str, subgroup) \
    DefSymbol(title, str); \
    DefSubType(title, str, subgroup)

#define DefCompareStyle(title, str) DefSubType(title, str, CompareStyle)

#define DefCompareStyleWithSymbol(title, str) DefSubTypeWithSymbol(title, str, CompareStyle)

    DefCompareStyleWithSymbol(Equals, ==);
    DefCompareStyleWithSymbol(NotEquals, !=);
    DefCompareStyleWithSymbol(LessThan, <);
    DefCompareStyleWithSymbol(LessThanOrEqualTo, <=);
    DefCompareStyleWithSymbol(GreaterThan, >);
    DefCompareStyleWithSymbol(GreaterThanOrEqualTo, >=);
    struct CompareType : pegtl::sor<
                         SubGroupCompareStyleEquals,
                         SubGroupCompareStyleNotEquals,
                         SubGroupCompareStyleLessThan,
                         SubGroupCompareStyleLessThanOrEqualTo,
                         SubGroupCompareStyleGreaterThan,
                         SubGroupCompareStyleGreaterThanOrEqualTo> { };
    struct CompareArgs : pegtl::sor<TwoGPRs, ImmediateOperationArgs<ByteCastImmediate>> { };
    struct CompareOperation : pegtl::seq<GroupCompare, Separator, CompareType, Separator, CompareArgs> { };
    DefAction(CompareOperation) {
        DefApplyInstruction {
            // Just disable the combine ability since it is dumb
            state.combineType = CompareCombine::None;
        }
    };

#define DefLogicalOps(title, str) DefSubType(title, str, LogicalOps)
#define DefImmediateLogicalOps(title, str) DefSubType(title, str, ImmediateLogicalOps)
    DefSubTypeWithSymbol(Not, not, LogicalOps);

    struct LogicalNotOperation : pegtl::seq<SubGroupLogicalOpsNot, Separator, DestinationRegister> { };
    DefAction(LogicalNotOperation) {
        DefApplyInstruction {
            state.immediate = false;
        }
    };

    //DefLogicalOps(
    //struct LogicalOperation : pegtl::seq<GroupLogical, Separator, LogicalActions> { };
}



