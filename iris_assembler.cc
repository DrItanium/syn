// iris_assembler rewritten to use pegtl
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include "syn_base.h"
#include "syn_asm_base.h"
#include "Problem.h"
#include "iris.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>
#include <pegtl/parse.hh>
#include <vector>

namespace iris {
    template<typename Rule > struct Action : public pegtl::nothing<Rule> { };
    void reportError(const std::string& msg) {
        throw syn::Problem(msg);
    }
    struct AssemblerData {
        AssemblerData() : instruction(false), address(0) { }
        bool instruction;
        word address;
        word dataValue;

        byte group;
        byte operation;
        byte destination;
        byte source0;
        byte source1;
        bool hasLexeme;
        std::string currentLexeme;
        bool fullImmediate;
        void reset() noexcept;
        void setImmediate(word value) noexcept;
        bool shouldResolveLabel() noexcept;
    };
    void AssemblerData::reset() noexcept {
        instruction = false;
        address = 0;
        dataValue = 0;
        group = 0;
        operation = 0;
        destination = 0;
        source0 = 0;
        source1 = 0;
        hasLexeme = false;
        currentLexeme.clear();
        fullImmediate = false;
    }
    struct AssemblerState {
        AssemblerState() : currentDataIndex(0), currentCodeIndex(0), inData(false) { }
        word currentDataIndex;
        word currentCodeIndex;
        bool inData;
        word temporaryWord;
        byte temporaryByte;
        AssemblerData current;
        std::map<std::string, word> labelMap;
        std::vector<AssemblerData> finishedData;
        void resetCurrentData() noexcept;
        void setImmediate(word value) noexcept;
        void setHalfImmediate(byte value) noexcept;
        void setGroup(InstructionGroup value) noexcept;
        template<typename T>
            void setOperation(T value) noexcept {
                current.operation = static_cast<byte>(value);
            }
        bool inCodeSection() const noexcept;
        bool inDataSection() const noexcept;
        void nowInCodeSection() noexcept;
        void nowInDataSection() noexcept;
        void setCurrentAddress(word value) noexcept;
        void registerLabel(const std::string& label) noexcept;
        word getCurrentAddress() noexcept;
        void incrementCurrentAddress() noexcept;
        void saveToFinished() noexcept;
    };
#define DefAction(rule) template<> struct Action < rule >
#define DefApply template<typename Input> static void apply(const Input& in, AssemblerState& state)
    using Separator = syn::AsmSeparator;
    using SingleLineComment = syn::SingleLineComment<';'>;
    struct GeneralPurposeRegister : public syn::GenericRegister<'r'> { };
    DefAction(GeneralPurposeRegister) {
        DefApply {
            state.temporaryByte = syn::getRegister<word, ArchitectureConstants::RegisterCount>(in.string(), reportError);
        }
    };
    struct PredicateRegister : public syn::GenericRegister<'p'> { };
    DefAction(PredicateRegister) {
        DefApply {
            state.temporaryByte = syn::getRegister<word, ArchitectureConstants::ConditionRegisterCount>(in.string(), reportError);
        }
    };
    using IndirectGPR = syn::Indirection<GeneralPurposeRegister>;
#define DefIndirectGPR(title) \
    struct title : public IndirectGPR { }
    DefIndirectGPR(DestinationGPR);
    DefAction(DestinationGPR) {
        DefApply {
            state.current.destination = state.temporaryByte;
        }
    };
    DefIndirectGPR(Source0GPR);
    DefAction(Source0GPR) {
        DefApply {
            state.current.source0 = state.temporaryByte;
        }
    };
    struct Source1GPR : public IndirectGPR { };
    DefAction(Source1GPR) {
        DefApply {
            state.current.source1 = state.temporaryByte;
        }
    };
#undef DefIndirectGPR
    struct DoubleGeneralPurposeRegister : public syn::GenericRegister<'d'> { };
    DefAction(DoubleGeneralPurposeRegister) {
        DefApply {
            state.temporaryByte = syn::getRegister<word, ArchitectureConstants::DoubleRegisterCount>(in.string(), reportError);
        }
    };
    using IndirectDGPR = syn::Indirection<DoubleGeneralPurposeRegister>;
#define DefIndirectDGPR(title) \
    struct title : public IndirectDGPR { }
    DefIndirectDGPR(DestinationDGPR);
    DefIndirectDGPR(Source0DGPR);
    DefIndirectDGPR(Source1DGPR);
#undef DefIndirectDGPR
    using SourceRegisters = syn::SourceRegisters<Source0GPR, Source1GPR>;
    struct OneGPR : public syn::OneRegister<DestinationGPR> { };
    struct TwoGPR : public syn::TwoRegister<DestinationGPR, Source0GPR> { };
    struct ThreeGPR : public syn::TwoRegister<DestinationGPR, SourceRegisters> { };
    using IndirectPredicateRegister = syn::Indirection<PredicateRegister>;
    struct DestinationPredicateRegister : public IndirectPredicateRegister { };
    DefAction(DestinationPredicateRegister) {
        DefApply {
            state.current.destination = iris::encodeLower4Bits(state.current.destination, state.temporaryByte);
        }
    };
    struct DestinationPredicateInverseRegister : public IndirectPredicateRegister { };
    DefAction(DestinationPredicateInverseRegister) {
        DefApply {
            state.current.destination = iris::encodeUpper4Bits(state.current.destination, state.temporaryByte);
        }
    };
    struct DestinationPredicates : public syn::TwoRegister<DestinationPredicateRegister, DestinationPredicateInverseRegister> { };
    struct Source0Predicate : public IndirectPredicateRegister { };
    DefAction(Source0Predicate) {
        DefApply {
            state.current.source0 = iris::encodeLower4Bits(state.current.source0, state.temporaryByte);
        }
    };
    struct Source1Predicate : public IndirectPredicateRegister { };
    DefAction(Source1Predicate) {
        DefApply {
            state.current.source0 = iris::encodeUpper4Bits(state.current.source0, state.temporaryByte);
        }
    };

    template<char delim, typename T>
        using Numeral = syn::GenericNumeral<delim, T>;
    struct HexadecimalNumber : public Numeral<'x', pegtl::xdigit> { };
    DefAction(HexadecimalNumber) {
        DefApply {
            state.temporaryWord = syn::getHexImmediate<word>(in.string(), reportError);
        }
    };
    struct BinaryNumber : public Numeral<'b', pegtl::abnf::BIT> { };
    DefAction(BinaryNumber) {
        DefApply {
            state.temporaryWord = syn::getBinaryImmediate<word>(in.string(), reportError);
        }
    };
    struct DecimalNumber : public pegtl::plus<pegtl::digit> { };
    DefAction(DecimalNumber) {
        DefApply {
            state.temporaryWord = syn::getDecimalImmediate<word>(in.string().c_str(), reportError);
        }
    };
    struct Number : public pegtl::sor<HexadecimalNumber, DecimalNumber, BinaryNumber> { };
    DefAction(Number) {
        DefApply {
            state.current.hasLexeme = false;
        }
    };
    using Lexeme = syn::Lexeme;
    DefAction(Lexeme) {
        DefApply {
            state.current.hasLexeme = true;
            state.current.currentLexeme = in.string();
        }
    };
    struct LexemeOrNumber : public syn::LexemeOr<Number> { };
#define DefSymbol(title, str) \
    struct Symbol ## title : public pegtl_string_t( #str ) { }
    // directives
    DefSymbol(LabelDirective, .label);
    DefSymbol(DataDirective, .data);
    DefSymbol(CodeDirective, .code);
    DefSymbol(OrgDirective, .org);
    DefSymbol(DeclareDirective, .declare);
#define DefOperation(title, str, type) \
    DefSymbol(title, str); \
    DefAction(Symbol ## title ) { \
        DefApply { \
            state.setOperation< CURRENT_TYPE >( CURRENT_TYPE :: type ) ; \
        } \
    }

#define DefOperationSameTitle(title, str) DefOperation(title, str, title)
    template<typename T>
        using ZeroArgumentDirective = pegtl::seq<T>;
    template<typename T, typename F>
        using OneArgumentDirective = syn::TwoPartComponent<T, F, Separator>;



    struct CodeDirective : public ZeroArgumentDirective<SymbolCodeDirective> { };
    DefAction(CodeDirective) { DefApply { state.nowInCodeSection(); } };
    struct DataDirective : public ZeroArgumentDirective<SymbolDataDirective> { };
    DefAction(DataDirective) { DefApply { state.nowInDataSection(); } };


    struct OrgDirective : public OneArgumentDirective<SymbolOrgDirective, Number> { };
    DefAction(OrgDirective) { DefApply { state.setCurrentAddress(state.temporaryWord); } };

    struct LabelDirective : public OneArgumentDirective<SymbolLabelDirective, Lexeme> { };
    DefAction(LabelDirective) {
        DefApply {
            state.registerLabel(state.current.currentLexeme);
            state.resetCurrentData();
        }
    };

    template<typename T>
        struct LexemeOrNumberDirective : public OneArgumentDirective<T, LexemeOrNumber> { };
    struct DeclareDirective : public LexemeOrNumberDirective<SymbolDeclareDirective> { };
    DefAction(DeclareDirective) {
        DefApply {
            if (state.inDataSection()) {
                state.current.instruction = false;
                if (!state.current.hasLexeme) {
                    state.current.dataValue = state.temporaryWord;
                }
                state.saveToFinished();
                state.incrementCurrentAddress();
            } else {
                throw syn::Problem("can't use a declare in a non data section!");
            }
        }
    };
    struct Directive : public pegtl::sor<OrgDirective, LabelDirective, CodeDirective, DataDirective, DeclareDirective> { };
    struct Immediate : public pegtl::sor<LexemeOrNumber> { };
    DefAction(Immediate) {
        DefApply {
            state.current.fullImmediate = true;
            if (!state.current.hasLexeme) {
                state.setImmediate(state.temporaryWord);
            }
        }
    };
    struct HalfImmediate : public pegtl::sor<Number> { };
    DefAction(HalfImmediate) {
        DefApply {
            state.current.fullImmediate = false;
            state.setHalfImmediate(state.temporaryWord);
        }
    };

    template<typename Operation, typename Operands>
        using GenericInstruction = syn::Instruction<Operation, Operands>;

    template<typename Operation>
        using OneGPRInstruction = GenericInstruction<Operation, OneGPR>;
    template<typename Operation>
        using ThreeGPRInstruction = GenericInstruction<Operation, ThreeGPR>;
    template<typename Operation>
        using TwoGPRInstruction = GenericInstruction<Operation, TwoGPR>;
#define CURRENT_TYPE ArithmeticOp
    DefOperationSameTitle(Add, add);
    DefOperationSameTitle(Sub, sub);
    DefOperationSameTitle(Mul, mul);
    DefOperationSameTitle(Div, div);
    DefOperationSameTitle(Rem, rem);
    DefOperationSameTitle(ShiftLeft, shl);
    DefOperationSameTitle(ShiftRight, shr);
    DefOperation(And, and, BinaryAnd);
    DefOperation(Or, or, BinaryOr);
    DefOperation(Xor, xor, BinaryXor);
    DefOperation(Nand, nand, BinaryNand);
    DefOperation(Nor, nor, BinaryNor);
    DefOperationSameTitle(Min, min);
    DefOperationSameTitle(Max, max);
    struct OperationArithmeticThreeGPR : public pegtl::sor<SymbolAdd, SymbolSub, SymbolMul, SymbolDiv, SymbolRem, SymbolShiftLeft, SymbolShiftRight, SymbolAnd, SymbolOr, SymbolXor, SymbolMin, SymbolMax> { };
    struct ArithmeticThreeGPRInstruction : public ThreeGPRInstruction<OperationArithmeticThreeGPR> { };
    DefOperation(Not, not, BinaryNot);
    struct OperationArithmeticTwoGPR : public pegtl::sor<SymbolNot> { };
    struct ArithmeticTwoGPRInstruction : public TwoGPRInstruction<OperationArithmeticTwoGPR> { };

    DefOperationSameTitle(AddImmediate, addi);
    DefOperationSameTitle(SubImmediate, subi);
    DefOperationSameTitle(MulImmediate, muli);
    DefOperationSameTitle(DivImmediate, divi);
    DefOperationSameTitle(RemImmediate, remi);
    DefOperationSameTitle(ShiftLeftImmediate, shli);
    DefOperationSameTitle(ShiftRightImmediate, shri);
    struct OperationArithmeticTwoGPRHalfImmediate : public pegtl::sor< SymbolAddImmediate, SymbolSubImmediate, SymbolMulImmediate, SymbolDivImmediate, SymbolRemImmediate, SymbolShiftLeftImmediate, SymbolShiftRightImmediate> { };
    struct ArithmeticTwoGPRHalfImmediateInstruction : public pegtl::seq<OperationArithmeticTwoGPRHalfImmediate, Separator, TwoGPR, Separator, HalfImmediate> { };

    struct ArithmeticInstruction : public pegtl::sor<ArithmeticTwoGPRHalfImmediateInstruction, ArithmeticTwoGPRInstruction, ArithmeticThreeGPRInstruction> { };

#define DefGroupSet(rule, group) DefAction( rule ) { DefApply { state.setGroup(InstructionGroup:: group ); } }
    DefGroupSet(ArithmeticInstruction, Arithmetic);
#undef CURRENT_TYPE
#define CURRENT_TYPE MoveOp

    DefOperationSameTitle(MoveToIP, mtip);
    DefOperationSameTitle(MoveFromIP, mfip);
    DefOperationSameTitle(MoveToLR, mtlr);
    DefOperationSameTitle(MoveFromLR, mflr);
    struct OperationMoveOneGPR : public pegtl::sor<SymbolMoveToIP, SymbolMoveFromIP, SymbolMoveToLR, SymbolMoveFromLR> { };
    struct MoveOneGPRInstruction : public OneGPRInstruction<OperationMoveOneGPR> { };
    DefOperationSameTitle(Move, move);
    DefOperationSameTitle(Swap, swap);
    DefOperationSameTitle(Load, ld);
    DefOperationSameTitle(Store, st);
    DefOperation(LoadIO, ldio, IORead);
    DefOperation(StoreIO, stio, IOWrite);
    DefOperationSameTitle(Push, push);
    DefOperationSameTitle(Pop, pop);
    struct OperationMoveTwoGPR : public pegtl::sor<SymbolMove, SymbolSwap, SymbolLoadIO, SymbolStoreIO, SymbolLoad, SymbolStore, SymbolPush, SymbolPop> { };
    struct MoveTwoGPRInstruction : public TwoGPRInstruction<OperationMoveTwoGPR> { };
    DefOperationSameTitle(LoadWithOffset, ldwo);
    DefOperationSameTitle(StoreWithOffset, stwo);
    DefOperation(LoadIOWithOffset, ldiowo, IOReadWithOffset);
    DefOperation(StoreIOWithOffset, stiowo, IOWriteWithOffset);
    struct OperationMoveTwoGPRHalfImmediate : public pegtl::sor<SymbolLoadWithOffset, SymbolStoreWithOffset, SymbolLoadIOWithOffset, SymbolStoreIOWithOffset> { };
    struct MoveTwoGPRHalfImmediateInstruction : public pegtl::seq<OperationMoveTwoGPRHalfImmediate, Separator, TwoGPR, Separator, HalfImmediate> { };

    DefOperationSameTitle(LoadCode, ldc);
    DefOperationSameTitle(StoreCode, stc);
    struct OperationMoveThreeGPR : public pegtl::sor<SymbolLoadCode, SymbolStoreCode> { };
    struct MoveThreeGPRInstruction : public ThreeGPRInstruction<OperationMoveThreeGPR> { };

    DefOperationSameTitle(PushImmediate, pushi);
    DefOperationSameTitle(Set, set);
    DefSymbol(LoadImmediatePrimary, ldi);
    DefSymbol(LoadImmediateSecondary, ldm);
    struct SymbolLoadImmediate : public pegtl::sor<SymbolLoadImmediatePrimary, SymbolLoadImmediateSecondary> { };
    DefAction(SymbolLoadImmediate) { DefApply { state.setOperation<CURRENT_TYPE>(CURRENT_TYPE :: LoadImmediate); } };
    DefSymbol(StoreImmediatePrimary, sti);
    DefSymbol(StoreImmediateAlternateTitle, memset);
    struct SymbolStoreImmediate : public pegtl::sor<SymbolStoreImmediatePrimary, SymbolStoreImmediateAlternateTitle> { };
    DefAction(SymbolStoreImmediate) { DefApply { state.setOperation<CURRENT_TYPE>(CURRENT_TYPE :: Memset); } };
    struct OperationMoveGPRImmediate : public pegtl::sor<SymbolStoreImmediate, SymbolLoadImmediate, SymbolSet, SymbolPushImmediate> { };

    struct MoveGPRImmediateInstruction : public pegtl::seq<OperationMoveGPRImmediate, Separator, DestinationGPR, Separator, Immediate> { };

    struct MoveInstruction : public pegtl::sor<MoveGPRImmediateInstruction, MoveThreeGPRInstruction, MoveTwoGPRHalfImmediateInstruction, MoveTwoGPRInstruction, MoveOneGPRInstruction> { };
    DefGroupSet(MoveInstruction, Move);
#undef CURRENT_TYPE
#define CURRENT_TYPE JumpOp
    // branch
    template<typename Op, typename S>
        struct BranchUnconditional : public pegtl::seq<Op, Separator, S> { };
    DefOperationSameTitle(BranchUnconditional, b);
    DefOperationSameTitle(BranchUnconditionalLink, bl);
    struct OperationBranchOneGPR : public pegtl::sor<SymbolBranchUnconditional, SymbolBranchUnconditionalLink> { };
    struct BranchOneGPRInstruciton : public BranchUnconditional<OperationBranchOneGPR, DestinationGPR> { };
    DefOperationSameTitle(BranchUnconditionalImmediate, bi);
    DefOperationSameTitle(BranchUnconditionalImmediateLink, bil);
    struct OperationBranchImmediate : public pegtl::sor<SymbolBranchUnconditionalImmediateLink, SymbolBranchUnconditionalImmediate> { };
    struct BranchImmediateInstruction : public BranchUnconditional<OperationBranchImmediate, Immediate> { };

    struct GroupBranchUnconditional : public pegtl::sor<BranchOneGPRInstruciton, BranchImmediateInstruction> { };
    template<typename Op, typename S>
        struct BranchConditional : public pegtl::seq<Op, Separator, DestinationPredicateRegister, Separator, S> { };
    DefOperationSameTitle(BranchConditional, bc);
    DefOperationSameTitle(BranchConditionalLink, bcl);
    struct OperationBranchConditionalGPR : public pegtl::sor<
                                           SymbolBranchConditionalLink,
                                           SymbolBranchConditional
                                           > { };
    struct BranchConditionalGPRInstruction : public BranchConditional<OperationBranchConditionalGPR, Source0GPR> { };
    DefOperationSameTitle(BranchConditionalImmediate, bic);
    DefOperationSameTitle(BranchConditionalImmediateLink, bicl);
    struct OperationBranchConditionalImmediate : public pegtl::sor<
                                                 SymbolBranchConditionalImmediateLink,
                                                 SymbolBranchConditionalImmediate
                                                 > { };
    struct BranchConditionalImmediateInstruction : public BranchConditional<OperationBranchConditionalImmediate, Immediate> { };
    DefOperationSameTitle(IfThenElse, "if");
    DefOperationSameTitle(IfThenElseLink, ifl);
    struct OperationBranchIfStatement : public pegtl::sor<
                                        SymbolIfThenElse,
                                        SymbolIfThenElseLink
                                        > { };
    struct BranchIfInstruction : public BranchConditional<OperationBranchIfStatement, SourceRegisters> { };
    DefOperationSameTitle(BranchConditionalLR, blrc);
    DefOperationSameTitle(BranchConditionalLRAndLink, blrcl);
    struct OperationBranchCondtionalNoArgs : public pegtl::sor<
                                             SymbolBranchConditionalLR,
                                             SymbolBranchConditionalLRAndLink
                                             > { };
    struct BranchConditionalNoArgsInstruction : public pegtl::seq<OperationBranchCondtionalNoArgs, Separator, DestinationPredicateRegister> { };
    DefOperationSameTitle(BranchUnconditionalLR, blr);
    DefOperationSameTitle(BranchUnconditionalLRAndLink, blrl);
    struct OperationBranchNoArgs : public pegtl::sor<SymbolBranchUnconditionalLR,  SymbolBranchUnconditionalLRAndLink> { };
    struct BranchNoArgsInstruction : public pegtl::seq<OperationBranchNoArgs> { };

    struct BranchInstruction : public pegtl::sor<GroupBranchUnconditional, BranchConditionalGPRInstruction, BranchConditionalImmediateInstruction, BranchIfInstruction, BranchConditionalNoArgsInstruction, BranchNoArgsInstruction> { };
    DefGroupSet(BranchInstruction, Jump);

#undef CURRENT_TYPE
#define CURRENT_TYPE CompareOp
    // compare operations
    DefOperationSameTitle(Eq, eq);
    DefOperationSameTitle(Neq, ne);
    DefOperationSameTitle(LessThan, lt);
    DefOperationSameTitle(GreaterThan, gt);
    DefOperationSameTitle(LessThanOrEqualTo, le);
    DefOperationSameTitle(GreaterThanOrEqualTo, ge);
    DefOperationSameTitle(EqImmediate, eqi);
    DefOperationSameTitle(NeqImmediate, nei);
    DefOperationSameTitle(LessThanImmediate, lti);
    DefOperationSameTitle(GreaterThanImmediate, gti);
    DefOperationSameTitle(LessThanOrEqualToImmediate, lei);
    DefOperationSameTitle(GreaterThanOrEqualToImmediate, gei);
    DefOperationSameTitle(SaveCRs, svcr);
    DefOperationSameTitle(RestoreCRs, recr);
    DefOperationSameTitle(CRXor, crxor);
    DefOperationSameTitle(CRNot, crnot);
    DefOperationSameTitle(CRAnd, crand);
    DefOperationSameTitle(CROr, cror);
    DefOperationSameTitle(CRNand, crnand);
    DefOperationSameTitle(CRNor, crnor);
    DefOperationSameTitle(CRSwap, crswap);
    DefOperationSameTitle(CRMove, crmove);
    struct CompareRegisterOperation : public pegtl::sor< SymbolEq, SymbolNeq, SymbolLessThan, SymbolGreaterThan, SymbolLessThanOrEqualTo, SymbolGreaterThanOrEqualTo> { };
    struct CompareImmediateOperation : public pegtl::sor<SymbolEqImmediate, SymbolNeqImmediate, SymbolLessThanImmediate, SymbolGreaterThanImmediate, SymbolLessThanOrEqualToImmediate, SymbolGreaterThanOrEqualToImmediate> { };
    struct CompareRegisterInstruction : public pegtl::seq<CompareRegisterOperation, Separator, DestinationPredicates, Separator, SourceRegisters> { };
    struct CompareImmediateInstruction : public pegtl::seq<CompareImmediateOperation, Separator, DestinationPredicates, Separator, Source0GPR, Separator, HalfImmediate> { };
    struct OperationPredicateTwoArgs : public pegtl::sor<SymbolCRSwap, SymbolCRMove> { };
    struct OperationPredicateThreeArgs : public pegtl::sor<SymbolCRNot> { };
    struct OperationPredicateOneGPR : public pegtl::sor<SymbolSaveCRs, SymbolRestoreCRs> { };
    struct OperationPredicateFourArgs : public pegtl::sor<SymbolCRXor, SymbolCRAnd, SymbolCROr, SymbolCRNand, SymbolCRNor> { };
    struct PredicateInstructionOneGPR : public pegtl::seq<OperationPredicateOneGPR, DestinationGPR> { };
    struct PredicateInstructionTwoArgs : public pegtl::seq<OperationPredicateTwoArgs, DestinationPredicates> { };
    struct PredicateInstructionThreeArgs : public pegtl::seq<OperationPredicateThreeArgs, DestinationPredicates, Source0Predicate> { };
    struct PredicateInstructionFourArgs : public pegtl::seq<OperationPredicateFourArgs, DestinationPredicates, Source0Predicate, Source1Predicate> { };
    struct PredicateInstruction : public pegtl::sor<PredicateInstructionTwoArgs, PredicateInstructionThreeArgs, PredicateInstructionFourArgs> { };
    struct CompareInstruction : public pegtl::sor<CompareRegisterInstruction, CompareImmediateInstruction, PredicateInstruction> { };
    DefGroupSet(CompareInstruction, Compare);

#undef DefGroupSet
#undef DefOperation
#undef DefOperationSameTitle
#undef CURRENT_TYPE
#undef DefSymbol
    struct Instruction : public pegtl::sor<ArithmeticInstruction, MoveInstruction, BranchInstruction, CompareInstruction, PredicateInstruction> { };
    DefAction(Instruction) {
        DefApply {
            if (state.inCodeSection()) {
                state.current.instruction = true;
                state.saveToFinished();
                state.incrementCurrentAddress();
            } else {
                throw syn::Problem("Can't construct instructions in the data section");
            }
        }
    };
    struct Statement : public pegtl::sor<Instruction, Directive> { };
    struct Anything : public pegtl::sor<Separator, SingleLineComment,Statement> { };
    struct Main : public syn::MainFileParser<Anything> { };

    void AssemblerState::setCurrentAddress(word value) noexcept {
        if (inData) {
            currentDataIndex = value;
        } else {
            currentCodeIndex = value;
        }
    }
    void AssemblerState::nowInCodeSection() noexcept {
        inData = false;
    }
    void AssemblerState::nowInDataSection() noexcept {
        inData = true;
    }
    bool AssemblerState::inCodeSection() const noexcept {
        return !inData;
    }
    bool AssemblerState::inDataSection() const noexcept {
        return inData;
    }
    void AssemblerState::setImmediate(word value) noexcept {
        current.setImmediate(value);
    }
    void AssemblerData::setImmediate(word value) noexcept {
        source0 = syn::getLowerHalf<word>(value);
        source1 = syn::getUpperHalf<word>(value);
    }
    void AssemblerState::setGroup(InstructionGroup value) noexcept {
        current.group = static_cast<byte>(value);
    }
    void AssemblerState::setHalfImmediate(byte value) noexcept {
        current.source1 = value;
    }

    void AssemblerState::resetCurrentData() noexcept {
        current.reset();
    }

    void AssemblerState::registerLabel(const std::string& value) noexcept {
        labelMap.emplace(value, getCurrentAddress());
    }
    word AssemblerState::getCurrentAddress() noexcept {
        return inData ? currentDataIndex : currentCodeIndex;
    }
    void AssemblerState::incrementCurrentAddress() noexcept {
        if (inData) {
            ++currentDataIndex;
        } else {
            ++currentCodeIndex;
        }
    }
    void AssemblerState::saveToFinished() noexcept {
        current.address = getCurrentAddress();
        auto copy = current;
        finishedData.emplace_back(copy);
        resetCurrentData();
    }
    void resolveLabels(AssemblerState& state, std::ostream& output) {
        // now that we have instructions, we need to print them out as hex values
        char buf[8] = { 0 };
        auto resolveLabel = [&state](AssemblerData& data) {
            auto result = state.labelMap.find(data.currentLexeme);
            if (result == state.labelMap.end()) {
                std::stringstream msg;
                msg << "ERROR: label " << data.currentLexeme << " is undefined!" << std::endl;
                auto str = msg.str();
                throw syn::Problem(str);
            } else {
                return result->second;
            }
        };
        for (auto & value : state.finishedData) {
            buf[0] = 0;
            buf[2] = static_cast<char>(syn::getLowerHalf<word>(value.address));
            buf[3] = static_cast<char>(syn::getUpperHalf<word>(value.address));
            output << std::hex << value.address << " ";
            if (value.instruction) {
                buf[1] = 0;
                buf[4] = static_cast<char>(iris::encodeOperationByte(iris::encodeGroupByte(0, value.group), value.operation));
                buf[5] = static_cast<char>(value.destination);
                if (value.shouldResolveLabel()) {
                    value.setImmediate(resolveLabel(value));
                }
                buf[6] = static_cast<char>(value.source0);
                buf[7] = static_cast<char>(value.source1);
            } else {
                buf[1] = 1;
                if (value.shouldResolveLabel()) {
                    value.dataValue = resolveLabel(value);
                }
                buf[4] = syn::getLowerHalf<word>(value.dataValue);
                buf[5] = syn::getUpperHalf<word>(value.dataValue);
                buf[6] = 0;
                buf[7] = 0;
            }
            output.write(static_cast<char*>(buf), sizeof(buf));
        }
    }

    void assemble(const std::string& iName, FILE* input, std::ostream* output) {
        iris::AssemblerState state;
        pegtl::analyze<iris::Main>();
        // put a sufficently large amount of space to read from the cstream
        pegtl::parse_cstream<iris::Main, iris::Action>(input, iName.c_str(), 16777216, state);
        resolveLabels(state, *output);
    }
    bool AssemblerData::shouldResolveLabel() noexcept {
        return fullImmediate && hasLexeme;
    }
}
