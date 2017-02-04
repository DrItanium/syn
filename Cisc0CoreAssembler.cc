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
    DefGroup(Move, move);
    DefGroup(Set, set);
    DefGroup(Swap, swap);
    DefGroup(SystemCall, system);
    DefGroup(Arithmetic, arithmetic);
    DefGroup(Memory, memory);
    DefGroup(Logical, logical);
    DefGroup(Complex, complex);
    DefGroup(Branch, branch);

    //Still left to do

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
            state.isLabel = false;
        }
    };

    struct BitmaskNumber : public Numeral<'m', pegtl::abnf::BIT> { };

    DefAction(BitmaskNumber) {
        DefApplyInstruction {
            state.bitmask = syn::decodeBits<RegisterValue, byte, 0x000000FF, 0>(syn::getBinaryImmediate<RegisterValue>(in.string(), reportError));
        }
    };
    using Lexeme = syn::Lexeme;
    DefAction(Lexeme) {
        DefApplyInstruction {
            state.labelValue = in.string();
            state.fullImmediate = 0;
            state.isLabel = true;
        }
    };
    struct LexemeOrNumber : public syn::LexemeOr<Number> { };

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
    template<typename S>
        struct TwoArgumentOperation : pegtl::seq<DestinationRegister, Separator, S> { };
    struct TwoGPRs : TwoArgumentOperation<SourceRegister> { };
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
        struct ImmediateOperationArgs : pegtl::seq<UsesImmediate, Separator, TwoArgumentOperation<Source>> { };
    template<typename Source>
        struct ImmediateOperationArgsWithBitmask : pegtl::seq<UsesImmediate, Separator, BitmaskNumber, Separator, TwoArgumentOperation<Source>> { };

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
    struct MoveOperation : pegtl::seq<
                           GroupMove,
                           Separator,
                           BitmaskNumber,
                           Separator,
                           TwoGPRs> { };
    struct SetOperation : pegtl::seq<
                          GroupSet,
                          Separator,
                          BitmaskNumber,
                          Separator,
                          DestinationRegister,
                          Separator,
                          LexemeOrNumber> { };

    struct SwapOperation : pegtl::seq<
                           GroupSwap,
                           Separator,
                           TwoGPRs> { };

    struct Arg0ImmediateValue : pegtl::seq<Number> { };
    DefAction(Arg0ImmediateValue) {
        DefApplyInstruction {
            state.arg0 = static_cast<byte>(state.fullImmediate) & 0b1111;
        }
    };
    struct SystemCallOperation : pegtl::seq<
                                 GroupSystemCall,
                                 Separator,
                                 Arg0ImmediateValue,
                                 Separator,
                                 SourceRegister> { };
#define DefArithmeticOperation(title, str) \
    DefSubTypeWithSymbol(title, str, ArithmeticOps)

    DefArithmeticOperation(Add, add);
    DefArithmeticOperation(Sub, sub);
    DefArithmeticOperation(Mul, mul);
    DefArithmeticOperation(Div, div);
    DefArithmeticOperation(Rem, rem);
    struct ArithmeticType : pegtl::sor<
                            SubGroupArithmeticOpsAdd,
                            SubGroupArithmeticOpsSub,
                            SubGroupArithmeticOpsMul,
                            SubGroupArithmeticOpsDiv,
                            SubGroupArithmeticOpsRem> { };

    struct ArithmeticArgs : pegtl::sor<
                            TwoGPRs,
                            ImmediateOperationArgs<ByteCastImmediate>> { };
    struct ArithmeticOperation : pegtl::seq<
                                 GroupArithmetic,
                                 Separator,
                                 ArithmeticType,
                                 Separator,
                                 ArithmeticArgs> { };

#define DefMemoryOperation(title, str) \
    DefSubTypeWithSymbol(title, str, MemoryOperation)
    DefMemoryOperation(Load, load);
    DefMemoryOperation(Store, store);
    DefMemoryOperation(Push, push);
    DefMemoryOperation(Pop, pop);

    struct LoadStoreType : pegtl::sor<
                           SubGroupMemoryOperationLoad,
                           SubGroupMemoryOperationStore> { };
    struct StackOperationFull : pegtl::seq<TwoGPRs> { };
    DefAction(StackOperationFull) {
        DefApplyInstruction {
            // check and see if we are looking at sp
            // no need to waste a word so just use the default version
            // implicitly. SourceRegister in this case is the stack pointer stand in
            state.readNextWord = (state.arg1 != ArchitectureConstants::StackPointer);
        }
    };
    struct StackMemoryType : pegtl::sor<
                             SubGroupMemoryOperationPush,
                             SubGroupMemoryOperationPop> { };
    struct StackOperation : pegtl::seq<
                            StackMemoryType,
                            Separator,
                            BitmaskNumber,
                            Separator,
                            StackOperationFull> { };
    DefSymbol(Indirect, indirect);
    struct FlagIndirect : public syn::Indirection<SymbolIndirect> { };
    DefAction(FlagIndirect) {
        DefApplyInstruction {
            state.indirect = true;
        }
    };
    DefSymbol(Direct, direct);
    struct FlagDirect : public syn::Indirection<SymbolDirect> { };
    DefAction(FlagDirect) {
        DefApplyInstruction {
            state.indirect = false;
        }
    };
    struct FlagDirectOrIndirect : pegtl::sor<FlagDirect, FlagIndirect> { };
    struct LoadStoreOperation : pegtl::seq<
                                LoadStoreType,
                                Separator,
                                BitmaskNumber,
                                Separator,
                                FlagDirectOrIndirect,
                                Separator,
                                Arg0ImmediateValue,
                                Separator,
                                TwoGPRs> { };

    DefAction(LoadStoreOperation) {
        DefApplyInstruction {
            state.readNextWord = (state.arg1 != ArchitectureConstants::AddressRegister) &&
                (state.arg2 != ArchitectureConstants::ValueRegister);
        }
    };

    struct MemoryInstruction : pegtl::seq<
                               GroupMemory,
                               Separator,
                               pegtl::sor<
                               StackOperation,
                               LoadStoreOperation>> { };

#define DefLogicalOperation(title, str) \
    DefSubTypeWithSymbol(title, str, LogicalOps)
    DefLogicalOperation(And, and);
    DefLogicalOperation(Or, or);
    DefLogicalOperation(Not, not);
    DefLogicalOperation(Xor, xor);
    DefLogicalOperation(Nand, nand);


    struct LogicalOpsType : pegtl::sor<
                            SubGroupLogicalOpsAnd,
                            SubGroupLogicalOpsOr,
                            SubGroupLogicalOpsNot,
                            SubGroupLogicalOpsXor,
                            SubGroupLogicalOpsNand> { };
    struct LogicalArgs : pegtl::sor<
                         TwoGPRs,
                         ImmediateOperationArgsWithBitmask<LexemeOrNumber>> { };
    struct LogicalOperation : pegtl::seq<
                              GroupLogical,
                              Separator,
                              LogicalOpsType,
                              Separator,
                              LogicalArgs> { };



#define DefEncodingSubType(title, str) \
    DefSubTypeWithSymbol(title, str, EncodingOperation)
    DefEncodingSubType(BitSet, bitset);
    DefEncodingSubType(BitUnset, bitunset);
    DefEncodingSubType(Encode, encode);
    DefEncodingSubType(Decode, decode);
    struct ComplexEncodingSubOperation : pegtl::sor<
                                         SubGroupEncodingOperationDecode,
                                         SubGroupEncodingOperationEncode,
                                         SubGroupEncodingOperationBitSet,
                                         SubGroupEncodingOperationBitUnset> { };
#define DefComplexOperation(title, str) \
    DefSubTypeWithSymbol(title, str, ComplexSubTypes)
    DefComplexOperation(Encoding, encoding);


    struct ComplexEncodingOperation : pegtl::seq<
                                      SubGroupComplexSubTypesEncoding,
                                      Separator,
                                      ComplexEncodingSubOperation> { };
    struct ComplexSubOperations : pegtl::sor<
                                  ComplexEncodingOperation> { };

    struct ComplexOperation : pegtl::seq<
                              GroupComplex,
                              Separator,
                              ComplexSubOperations> { };

    DefSymbol(If, if);
    DefSymbol(Call, call);
    DefSymbol(NoCall, nocall);
    DefSymbol(Conditional, conditional);
    DefSymbol(Unconditional, unconditional);

    template<typename T, typename F>
        struct ChoiceFlag : pegtl::sor<T, F> { };

    struct BranchFlagIf : public syn::Indirection<SymbolIf> { };
    DefAction(BranchFlagIf) {
        DefApplyInstruction {
            state.isIf = true;
            state.isConditional = false;
        }
    };

    struct BranchFlagCall : public syn::Indirection<SymbolCall> { };
    DefAction(BranchFlagCall) {
        DefApplyInstruction {
            state.isCall = true;
        }
    };

    struct BranchFlagNoCall : public syn::Indirection<SymbolNoCall> { };
    DefAction(BranchFlagNoCall) {
        DefApplyInstruction {
            state.isCall = false;
        }
    };

    struct ChooseBranchFlagCall : ChoiceFlag<BranchFlagCall, BranchFlagNoCall> { };

    struct BranchFlagConditional : public syn::Indirection<SymbolConditional> { };
    DefAction(BranchFlagConditional) {
        DefApplyInstruction {
            state.isConditional = true;
        }
    };

    struct BranchFlagUnconditional : public syn::Indirection<SymbolUnconditional> { };
    DefAction(BranchFlagUnconditional) {
        DefApplyInstruction {
            state.isConditional = false;
        }
    };

    struct ChooseBranchFlagUsePredicate : ChoiceFlag<BranchFlagConditional, BranchFlagUnconditional> { };

    struct BranchIfOperation : pegtl::seq<
                               BranchFlagIf,
                               Separator,
                               ChooseBranchFlagCall,
                               Separator,
                               TwoGPRs> { };
    struct BranchNormalArgs : pegtl::sor<
                              pegtl::seq<
                              UsesImmediate,
                              Separator,
                              LexemeOrNumber>,
                              DestinationRegister> { };
    struct BranchCallOperation : pegtl::seq<BranchFlagCall,
    Separator,
    BranchNormalArgs> { };
    struct BranchJumpOperation : pegtl::seq<ChooseBranchFlagUsePredicate,
    Separator,
    BranchNormalArgs> { };

    struct BranchOperation : pegtl::seq<
                             GroupBranch,
                             Separator,
                             pegtl::sor<BranchIfOperation,
                             BranchCallOperation,
                             BranchJumpOperation>> { };

    struct Instructions : pegtl::sor<
                          BranchOperation,
                          ComplexOperation,
                          MemoryInstruction,
                          MoveOperation,
                          SetOperation,
                          SwapOperation,
                          ArithmeticOperation,
                          ShiftOperation,
                          CompareOperation,
                          SystemCallOperation,
                          LogicalOperation> { };

}
