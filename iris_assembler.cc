// iris_assembler rewritten to use pegtl
#include "iris.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>

namespace iris {
    struct Comment : public pegtl::until<pegtl::eolf> { };
    struct SingleLineComment : public pegtl::disable<pegtl::one<';'>, Comment> { };
    struct GeneralPurposeRegister : public pegtl::if_must<pegtl::one<'r'>, pegtl::plus<pegtl::digit>> { };
    struct PredicateRegister : public pegtl::if_must<pegtl::one<'p'>, pegtl::plus<pegtl::digit>> { };
    struct DestinationGPR : public pegtl::seq<GeneralPurposeRegister> { };
    struct Source0GPR : public pegtl::seq<GeneralPurposeRegister> { };
    struct Source1GPR : public pegtl::seq<GeneralPurposeRegister> { };
    struct SourceRegisters : public pegtl::seq<Source0GPR, Source1GPR> { };
    struct OneGPR : public pegtl::seq<DestinationGPR> { };
    struct TwoGPR : public pegtl::seq<DestinationGPR, Source0GPR> { };
    struct ThreeGPR : public pegtl::seq<DestinationGPR, SourceRegisters> { };
    struct DestinationPredicateRegister : public pegtl::seq<PredicateRegister> { };
    struct DestinationPredicateInverseRegister : public pegtl::seq<PredicateRegister> { };
    struct DestinationPredicate : public pegtl::seq<DestinationPredicateRegister, DestinationPredicateInverseRegister> { };
    struct Source0Predicate : public pegtl::seq<PredicateRegister> { };
    struct Source1Predicate : public pegtl::seq<PredicateRegister> { };

    struct Register : public pegtl::sor<GeneralPurposeRegister, PredicateRegister> { };
    struct HexadecimalNumber : public pegtl::if_must<pegtl::istring< '0', 'x'>, pegtl::plus<pegtl::xdigit>> { };
    struct BinaryNumber : public pegtl::if_must<pegtl::istring<  '0', 'b' >, pegtl::plus<pegtl::abnf::BIT>> { };
    struct DecimalNumber : public pegtl::seq<pegtl::plus<pegtl::digit>> { };
    struct Number : public pegtl::sor<HexadecimalNumber, DecimalNumber, Number> { };
    struct Lexeme : public pegtl::identifier { };
    struct LexemeOrNumber : public pegtl::sor<Lexeme, Number> { };
#define DefSymbol(title, str) \
    struct Symbol ## title : public pegtl_string_t( #str ) { }
    // directives
    DefSymbol(LabelDirective, @label);
    DefSymbol(DataDirective, @data);
    DefSymbol(CodeDirective, @code);
    DefSymbol(OrgDirective, @org);
    DefSymbol(DeclareDirective, @declare);
    DefSymbol(HiDirective, @hi);
    DefSymbol(LoDirective, @lo);
    struct SymbolDirectiveOperation : public pegtl::sor<SymbolLabelDirective, SymbolDataDirective, SymbolCodeDirective, SymbolOrgDirective, SymbolDeclareDirective, SymbolHiDirective, SymbolLoDirective> { };

    template<typename T>
    struct ZeroArgumentDirective : public pegtl::seq<T> { };

    template<typename T, typename F>
    struct OneArgumentDirective : public pegtl::seq<T, F> { };



    struct CodeDirective : public ZeroArgumentDirective<SymbolCodeDirective> {
    };
    struct DataDirective : public ZeroArgumentDirective<SymbolDataDirective> {
    };

    struct OrgDirective : public OneArgumentDirective<SymbolOrgDirective, Number> {
    };
    struct LabelDirective : public OneArgumentDirective<LabelDirective, Lexeme> {
    };

    template<typename T>
    struct LexemeOrNumberDirective : public OneArgumentDirective<T, LexemeOrNumber> { };
    struct DeclareDirective : public LexemeOrNumberDirective<SymbolDeclareDirective> {
    };
    struct HiDirective : public LexemeOrNumberDirective<SymbolHiDirective> {
    };
    struct LoDirective : public LexemeOrNumberDirective<SymbolLoDirective> {
    };
    struct Immediate : public pegtl::sor<Lexeme, Number, HiDirective, LoDirective> { };
    struct HalfImmediate : public pegtl::sor<Number, HiDirective, LoDirective> { };
    DefSymbol(Add, add);
    DefSymbol(Sub, sub);
    DefSymbol(Mul, mul);
    DefSymbol(Div, div);
    DefSymbol(Rem, rem);
    DefSymbol(ShiftLeft, shl);
    DefSymbol(ShiftRight, shr);
    DefSymbol(And, and);
    DefSymbol(Or, or);
    DefSymbol(Xor, xor);
    DefSymbol(Min, min);
    DefSymbol(Max, max);
    struct OperationArithmeticThreeGPR : public pegtl::sor<SymbolAdd, SymbolSub, SymbolMul, SymbolDiv, SymbolRem, SymbolShiftLeft, SymbolShiftRight, SymbolAnd, SymbolOr, SymbolXor, SymbolMin, SymbolMax> { };
    struct ArithmeticThreeGPRInstruction : public pegtl::seq<OperationArithmeticThreeGPR, ThreeGPR> { };
    DefSymbol(Not, not);
    struct OperationArithmeticTwoGPR : public pegtl::sor<SymbolNot> { };
    struct ArithmeticTwoGPRInstruction : public pegtl::seq<OperationArithmeticTwoGPR, TwoGPR> { };

    DefSymbol(AddImmediate, addi);
    DefSymbol(SubImmediate, subi);
    DefSymbol(MulImmediate, muli);
    DefSymbol(DivImmediate, divi);
    DefSymbol(RemImmediate, remi);
    DefSymbol(ShiftLeftImmediate, shli);
    DefSymbol(ShiftRightImmediate, shri);
    struct OperationArithmeticTwoGPRHalfImmediate : public pegtl::sor< SymbolAddImmediate, SymbolSubImmediate, SymbolMulImmediate, SymbolDivImmediate, SymbolRemImmediate, SymbolShiftLeftImmediate, SymbolShiftRightImmediate> { };
    struct ArithmeticTwoGPRHalfImmediateInstruction : public pegtl::seq<OperationArithmeticTwoGPRHalfImmediate, TwoGPR, HalfImmediate> { };

    struct ArithmeticInstruction : public pegtl::sor<ArithmeticTwoGPRHalfImmediateInstruction, ArithmeticTwoGPRInstruction, ArithmeticThreeGPRInstruction> { };

    DefSymbol(MoveToIP, mtip);
    DefSymbol(MoveFromIP, mfip);
    DefSymbol(MoveToLR, mtlr);
    DefSymbol(MoveFromLR, mflr);
    struct OperationMoveOneGPR : public pegtl::sor<SymbolMoveToIP, SymbolMoveFromIP, SymbolMoveToLR, SymbolMoveFromLR> { };
    struct MoveOneGPRInstruction : public pegtl::sor<OperationMoveOneGPR, OneGPR> { };
    DefSymbol(Move, move);
    DefSymbol(Swap, swap);
    DefSymbol(Load, ld);
    DefSymbol(Store, st);
    DefSymbol(LoadIO, ldio);
    DefSymbol(StoreIO, stio);
    DefSymbol(Push, push);
    DefSymbol(Pop, pop);
    struct OperationMoveTwoGPR : public pegtl::sor<SymbolMove, SymbolSwap, SymbolLoad, SymbolStore, SymbolLoadIO, SymbolStoreIO, SymbolPush, SymbolPop> { };
    struct MoveTwoGPRInstruction : public pegtl::seq<OperationMoveTwoGPR, TwoGPR> { };
    DefSymbol(LoadWithOffset, ldwo);
    DefSymbol(StoreWithOffset, stwo);
    DefSymbol(LoadIOWithOffset, ldiowo);
    DefSymbol(StoreIOWithOffset, stiowo);
    struct OperationMoveTwoGPRHalfImmediate : public pegtl::sor<SymbolLoadWithOffset, SymbolStoreWithOffset, SymbolLoadIOWithOffset, SymbolStoreIOWithOffset> { };
    struct MoveTwoGPRHalfImmediateInstruction : public pegtl::seq<OperationMoveTwoGPRHalfImmediate, TwoGPR, HalfImmediate> { };

    DefSymbol(LoadCode, ldc);
    DefSymbol(StoreCode, stc);
    struct OperationMoveThreeGPR : public pegtl::sor<SymbolLoadCode, SymbolStoreCode> { };
    struct MoveThreeGPRInstruction : public pegtl::seq<OperationMoveThreeGPR, ThreeGPR> { };

    DefSymbol(PushImmediate, pushi);
    DefSymbol(Set, set);
    DefSymbol(LoadImmediatePrimary, ldi);
    DefSymbol(LoadImmediateSecondary, ldm);
    struct SymbolLoadImmediate : public pegtl::sor<SymbolLoadImmediatePrimary, SymbolLoadImmediateSecondary> { };
    DefSymbol(StoreImmediatePrimary, sti);
    DefSymbol(StoreImmediateAlternateTitle, memset);
    struct SymbolStoreImmediate : public pegtl::sor<SymbolStoreImmediatePrimary, SymbolStoreImmediateAlternateTitle> { };
    struct OperationMoveGPRImmediate : public pegtl::sor<SymbolStoreImmediate, SymbolLoadImmediate, SymbolSet, SymbolPushImmediate> { };

    struct MoveGPRImmediateInstruction : public pegtl::seq<OperationMoveGPRImmediate, DestinationGPR, Immediate> { };

    struct MoveInstruction : public pegtl::sor<MoveGPRImmediateInstruction, MoveThreeGPRInstruction, MoveTwoGPRHalfImmediateInstruction, MoveTwoGPRInstruction, MoveOneGPRInstruction> { };

    // branch
    DefSymbol(BranchUnconditional, b);
    DefSymbol(BranchUnconditionalLink, bl);
    struct OperationBranchOneGPR : public pegtl::sor<SymbolBranchUnconditional, SymbolBranchUnconditionalLink> { };
    struct BranchOneGPRInstruciton : public pegtl::seq<OperationBranchOneGPR, DestinationGPR> { };
    DefSymbol(BranchUnconditionalImmediate, bi);
    DefSymbol(BranchUnconditionalImmediateLink, bil);
    struct OperationBranchImmediate : public pegtl::sor<SymbolBranchUnconditionalImmediate, SymbolBranchUnconditionalImmediateLink> { };
    struct BranchImmediateInstruction : public pegtl::seq<OperationBranchImmediate, Immediate> { };
    template<typename Op, typename S>
    struct BranchConditional : public pegtl::seq<Op, DestinationPredicateRegister, S> { };
    DefSymbol(BranchConditionalTrue, bt);
    DefSymbol(BranchConditionalTrueLink, btl);
    DefSymbol(BranchConditionalFalse, bf);
    DefSymbol(BranchConditionalFalseLink, bfl);
    struct OperationBranchConditionalGPR : public pegtl::sor<SymbolBranchConditionalTrue, SymbolBranchConditionalTrueLink, SymbolBranchConditionalFalse, SymbolBranchConditionalFalseLink> { };
    struct BranchConditionalGPRInstruction : public BranchConditional<OperationBranchConditionalGPR, Source0GPR> { };
    DefSymbol(BranchConditionalTrueImmediate, bit);
    DefSymbol(BranchConditionalTrueImmediateLink, bitl);
    DefSymbol(BranchConditionalFalseImmediate, bif);
    DefSymbol(BranchConditionalFalseImmediateLink, bifl);
    struct OperationBranchConditionalImmediate : public pegtl::sor<SymbolBranchConditionalTrueImmediate, SymbolBranchConditionalTrueImmediateLink, SymbolBranchConditionalFalseImmediate, SymbolBranchConditionalFalseImmediateLink> { };
    struct BranchConditionalImmediateInstruction : public BranchConditional<OperationBranchConditionalImmediate, Immediate> { };
    DefSymbol(IfThenElseTrue, ift);
    DefSymbol(IfThenElseFalse, iff);
    DefSymbol(IfThenElseTrueLink, iftl);
    DefSymbol(IfThenElseFalseLink, iffl);
    struct OperationBranchIfStatement : public pegtl::sor<SymbolIfThenElseTrue, SymbolIfThenElseTrueLink, SymbolIfThenElseFalse, SymbolIfThenElseFalseLink> { };
    struct BranchIfInstruction : public BranchConditional<OperationBranchIfStatement, SourceRegisters> { };
    DefSymbol(BranchLRTrue, blrt);
    DefSymbol(BranchLRTrueLink, blrtl);
    DefSymbol(BranchLRFalse, blrf);
    DefSymbol(BranchLRFalseLink, blrfl);
    struct OperationBranchCondtionalNoArgs : public pegtl::sor<SymbolBranchLRTrue, SymbolBranchLRTrueLink, SymbolBranchLRFalse, SymbolBranchLRFalseLink> { };
    struct BranchConditionalNoArgsInstruction : public pegtl::seq<OperationBranchCondtionalNoArgs, DestinationPredicateRegister> { };
    DefSymbol(BranchLR, blr);
    DefSymbol(BranchLRLink, blrl);
    struct OperationBranchNoArgs : public pegtl::sor<SymbolBranchLR,  SymbolBranchLRLink> { };
    struct BranchNoArgsInstruction : public pegtl::seq<OperationBranchNoArgs> { };

    struct BranchInstruction : public pegtl::sor<BranchOneGPRInstruciton, BranchImmediateInstruction, BranchConditionalGPRInstruction, BranchConditionalImmediateInstruction, BranchIfInstruction, BranchConditionalNoArgsInstruction, BranchNoArgsInstruction> { };

    // compare operations
    DefSymbol(Eq, eq);
    DefSymbol(Neq, ne);
    DefSymbol(Lt, lt);
    DefSymbol(Gt, gt);
    DefSymbol(Le, le);
    DefSymbol(Ge, ge);
    DefSymbol(EqImmediate, eqi);
    DefSymbol(NeqImmediate, nei);
    DefSymbol(LtImmediate, lti);
    DefSymbol(GtImmediate, gti);
    DefSymbol(LeImmediate, lei);
    DefSymbol(GeImmediate, gei);
    struct CompareRegisterOperation : public pegtl::sor< SymbolEq, SymbolNeq, SymbolLt, SymbolGt, SymbolLe, SymbolGe> { };
    struct CompareImmediateOperation : public pegtl::sor<SymbolEqImmediate, SymbolNeqImmediate, SymbolLtImmediate, SymbolGtImmediate, SymbolLeImmediate, SymbolGeImmediate> { };
    struct CompareRegisterInstruction : public pegtl::seq<CompareRegisterOperation, DestinationPredicate, SourceRegisters> { };
    struct CompareImmediateInstruction : public pegtl::seq<CompareImmediateOperation, DestinationPredicate, Source0GPR, HalfImmediate> { };
    struct CompareInstruction : public pegtl::seq<CompareRegisterInstruction, CompareImmediateInstruction> { };

    // conditional register actions
    DefSymbol(SaveCRs, svcr);
    DefSymbol(RestoreCRs, recr);
    DefSymbol(CRXor, crxor);
    DefSymbol(CRNot, crnot);
    DefSymbol(CRAnd, crand);
    DefSymbol(CROr, cror);
    DefSymbol(CRNand, crnand);
    DefSymbol(CRNor, crnor);
    DefSymbol(CRSwap, crswap);
    DefSymbol(CRMove, crmove);
    struct OperationPredicateTwoArgs : public pegtl::sor<SymbolCRSwap, SymbolCRMove> { };
    struct OperationPredicateThreeArgs : public pegtl::sor<SymbolCRNot> { };
    struct OperationPredicateOneGPR : public pegtl::sor<SymbolSaveCRs, SymbolRestoreCRs> { };
    struct OperationPredicateFourArgs : public pegtl::sor<SymbolCRXor, SymbolCRAnd, SymbolCROr, SymbolCRNand, SymbolCRNor> { };
    struct PredicateInstructionOneGPR : public pegtl::seq<OperationPredicateOneGPR, DestinationGPR> { };
    struct PredicateInstructionTwoArgs : public pegtl::seq<OperationPredicateTwoArgs, DestinationPredicate> { };
    struct PredicateInstructionThreeArgs : public pegtl::seq<OperationPredicateThreeArgs, DestinationPredicate, Source0Predicate> { };
    struct PredicateInstructionFourArgs : public pegtl::seq<OperationPredicateFourArgs, DestinationPredicate, Source0Predicate, Source1Predicate> { };
    struct PredicateInstruction : public pegtl::sor<PredicateInstructionTwoArgs, PredicateInstructionThreeArgs, PredicateInstructionFourArgs> { };



    // registers
    // basic define

#undef DefSymbol
    struct Instruction : public pegtl::sor<ArithmeticInstruction, MoveInstruction, BranchInstruction, CompareInstruction, PredicateInstruction> { };

    //struct Directive { };
    //struct Statement : public pegtl::sor<Instruction, Directive> { };
}
