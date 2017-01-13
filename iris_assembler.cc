// iris_assembler rewritten to use pegtl
#include "iris.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>

namespace iris {
#define DefSymbol(title, str) \
    struct Symbol ## title : public pegtl_string_t( #str ) { }
    struct SymbolAdd : public pegtl_string_t( "add" ) { };
    struct SymbolSub : public pegtl_string_t( "sub" ) { };
    struct SymbolMul : public pegtl_string_t( "mul" ) { };
    struct SymbolDiv : public pegtl_string_t( "div" ) { };
    struct SymbolRem : public pegtl_string_t( "rem" ) { };
    struct SymbolShiftLeft : public pegtl_string_t("shl") { };
    struct SymbolShiftRight : public pegtl_string_t("shr") { };
    struct SymbolAnd : public pegtl_string_t("and") { };
    struct SymbolOr : public pegtl_string_t("or") { };
    struct SymbolNot : public pegtl_string_t("not") { };
    struct SymbolXor : public pegtl_string_t("xor") { };
    struct SymbolAddImmediate : public pegtl_string_t( "addi" ) { };
    struct SymbolSubImmediate : public pegtl_string_t( "subi" ) { };
    struct SymbolMulImmediate : public pegtl_string_t( "muli" ) { };
    struct SymbolDivImmediate : public pegtl_string_t( "divi" ) { };
    struct SymbolRemImmediate : public pegtl_string_t( "remi" ) { };
    struct SymbolShiftLeftImmediate : public pegtl_string_t("shli") { };
    struct SymbolShiftRightImmediate : public pegtl_string_t("shri") { };
    DefSymbol(Min, min);
    DefSymbol(Max, max);
    struct ArithmeticOperation : public pegtl::sor<SymbolAdd, SymbolSub, SymbolMul, SymbolDiv, SymbolRem, SymbolShiftLeft, SymbolShiftRight, SymbolAnd, SymbolOr, SymbolNot, SymbolXor, SymbolAddImmediate, SymbolSubImmediate, SymbolMulImmediate, SymbolDivImmediate, SymbolRemImmediate, SymbolShiftLeftImmediate, SymbolShiftRightImmediate, SymbolMin, SymbolMax> { };

    struct SymbolMove : public pegtl_string_t("move") { };
    struct SymbolSwap : public pegtl_string_t("swap") { };
    struct SymbolSet : public pegtl_string_t("set") { };
    struct SymbolLoad : public pegtl_string_t("ld") { };
    struct SymbolLoadImmediate : public pegtl_string_t("ldi") { };
    struct SymbolLoadMemory : public pegtl_string_t("ldm") { };
    struct SymbolLoadWithOffset : public pegtl_string_t("ldwo") { };
    struct SymbolLoadCode : public pegtl_string_t("ldc") { };
    struct SymbolStore : public pegtl_string_t("st") { };
    struct SymbolStoreImmediate : public pegtl_string_t("memset") { };
    struct SymbolStoreWithOffset : public pegtl_string_t("stwo") { };
    struct SymbolStoreCode : public pegtl_string_t("stc") { };
    DefSymbol(LoadIO, ldio);
    DefSymbol(StoreIO, stio);
    DefSymbol(LoadIOWithOffset, ldiowo);
    DefSymbol(StoreIOWithOffset, stiowo);
    DefSymbol(MoveToIP, mtip);
    DefSymbol(MoveFromIP, mfip);
    DefSymbol(MoveToLR, mtlr);
    DefSymbol(MoveFromLR, mflr);
    DefSymbol(Push, push);
    DefSymbol(PushImmediate, pushi);
    DefSymbol(Pop, pop);
    struct ManipulationOperation : public pegtl::sor<SymbolMove, SymbolSwap, SymbolSet, SymbolLoad, SymbolLoadImmediate, SymbolLoadMemory, SymbolLoadWithOffset, SymbolLoadCode, SymbolStore, SymbolStoreImmediate, SymbolStoreWithOffset, SymbolStoreCode, SymbolLoadIO, SymbolStoreIO, SymbolLoadIOWithOffset, SymbolStoreIOWithOffset, SymbolMoveToIP, SymbolMoveFromIP, SymbolMoveToLR, SymbolMoveFromLR, SymbolPush, SymbolPushImmediate, SymbolPop> { };

    // branch
    DefSymbol(BranchUnconditionalImmediate, bi);
    DefSymbol(BranchUnconditionalImmediateLink, bil);
    DefSymbol(BranchUnconditional, b);
    DefSymbol(BranchUnconditionalLink, bl);
    DefSymbol(BranchConditionalTrueImmediate, bit);
    DefSymbol(BranchConditionalTrueImmediateLink, bitl);
    DefSymbol(BranchConditionalTrue, bt);
    DefSymbol(BranchConditionalTrueLink, btl);
    DefSymbol(BranchConditionalFalse, bf);
    DefSymbol(BranchConditionalFalseLink, bfl);
    DefSymbol(BranchConditionalFalseImmediate, bif);
    DefSymbol(BranchConditionalFalseImmediateLink, bifl);
    DefSymbol(IfThenElseTrue, ift);
    DefSymbol(IfThenElseFalse, iff);
    DefSymbol(IfThenElseTrueLink, iftl);
    DefSymbol(IfThenElseFalseLink, iffl);
    DefSymbol(BranchLR, blr);
    DefSymbol(BranchLRLink, blrl);
    DefSymbol(BranchLRTrue, blrt);
    DefSymbol(BranchLRTrueLink, blrtl);
    DefSymbol(BranchLRFalse, blrf);
    DefSymbol(BranchLRFalseLink, blrfl);
    struct BranchOperation : public pegtl::sor<SymbolBranchUnconditionalImmediate, SymbolBranchUnconditionalImmediateLink, SymbolBranchUnconditional, SymbolBranchUnconditionalLink, SymbolBranchConditionalTrueImmediate, SymbolBranchConditionalTrueImmediateLink, SymbolBranchConditionalTrue, SymbolBranchConditionalTrueLink, SymbolBranchConditionalFalseImmediate, SymbolBranchConditionalFalseImmediateLink, SymbolBranchConditionalFalse, SymbolBranchConditionalFalseLink, SymbolIfThenElseTrue, SymbolIfThenElseTrueLink, SymbolIfThenElseFalse, SymbolIfThenElseFalseLink, SymbolBranchLR, SymbolBranchLRLink, SymbolBranchLRTrue, SymbolBranchLRTrueLink, SymbolBranchLRFalse, SymbolBranchLRFalseLink> { };


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
    struct ConditionalRegisterActionOperation : public pegtl::sor<SymbolSaveCRs, SymbolRestoreCRs, SymbolCRXor, SymbolCRNot, SymbolCRAnd, SymbolCROr, SymbolCRNand, SymbolCRNor, SymbolCRSwap, SymbolCRMove> { };
    // directives
    DefSymbol(LabelDirective, @label);
    DefSymbol(DataDirective, @data);
    DefSymbol(CodeDirective, @code);
    DefSymbol(OrgDirective, @org);
    DefSymbol(DeclareDirective, @declare);
    DefSymbol(HiDirective, @hi);
    DefSymbol(LoDirective, @lo);
    struct SymbolDirectiveOperation : public pegtl::sor<SymbolLabelDirective, SymbolDataDirective, SymbolCodeDirective, SymbolOrgDirective, SymbolDeclareDirective, SymbolHiDirective, SymbolLoDirective> { };

    // registers
    // basic define

#undef DefSymbol
    struct Operation : public pegtl::sor<ConditionalRegisterActionOperation, BranchOperation, ManipulationOperation, ArithmeticOperation, CompareOperation> { };
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
    struct CompareRegister : public pegtl::seq<CompareRegisterOperation, DestinationPredicate, SourceRegisters> { };
    struct CompareInstruction : public pegtl::seq<CompareRegister, CompareImmediate> { };

    struct Register : public pegtl::sor<GeneralPurposeRegister, PredicateRegsiter> { };
    struct HexadecimalNumber : public pegtl::if_must<pegtl::istring< '0', 'x'>, pegtl::plus<pegtl::xdigit>> { };
    struct BinaryNumber : public pegtl::if_must<pegtl::istring<  '0', 'b' >, pegtl::plus<pegtl::abnf::BIT>> { };
    struct DecimalNumber : public pegtl::seq<pegtl::plus<pegtl::digit>> { };
    struct Number : public pegtl::sor<HexadecimalNumber, DecimalNumber, Number> { };
    struct Lexeme : public pegtl::identifier { };
    struct Instruction : public pegtl::sor<ArithmeticInstruction, CompareInstruction, ConditionalRegisterInstruction, BranchOperation, ManipulationOperation> { };
    //struct Instruction : public pegtl::seq<Operation, InstructionBits> { };
    //struct Directive { };
    //struct Statement : public pegtl::sor<Instruction, Directive> { };
}
