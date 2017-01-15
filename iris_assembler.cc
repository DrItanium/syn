// iris_assembler rewritten to use pegtl
#include <iostream>
#include "syn_base.h"
#include "iris.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>

namespace iris {
    template<typename Rule > struct Action : public pegtl::nothing<Rule> { };
    struct Comment : public pegtl::until<pegtl::eolf> { };
    struct SingleLineComment : public pegtl::disable<pegtl::one<';'>, Comment> { };
	struct Separator : public pegtl::plus<pegtl::ascii::space> { };
    struct GeneralPurposeRegister : public pegtl::if_must<pegtl::one<'r'>, pegtl::plus<pegtl::digit>> { };
    struct PredicateRegister : public pegtl::if_must<pegtl::one<'p'>, pegtl::plus<pegtl::digit>> { };
    struct DestinationGPR : public pegtl::seq<GeneralPurposeRegister> { };
    struct Source0GPR : public pegtl::seq<GeneralPurposeRegister> { };
    struct Source1GPR : public pegtl::seq<GeneralPurposeRegister> { };
    struct SourceRegisters : public pegtl::seq<Source0GPR, Separator, Source1GPR> { };
    struct OneGPR : public pegtl::seq<DestinationGPR> { };
    struct TwoGPR : public pegtl::seq<DestinationGPR, Separator, Source0GPR> { };
    struct ThreeGPR : public pegtl::seq<DestinationGPR, Separator, SourceRegisters> { };
    struct DestinationPredicateRegister : public pegtl::seq<PredicateRegister> { };
    struct DestinationPredicateInverseRegister : public pegtl::seq<PredicateRegister> { };
    struct DestinationPredicate : public pegtl::seq<DestinationPredicateRegister, Separator, DestinationPredicateInverseRegister> { };
    struct Source0Predicate : public pegtl::seq<PredicateRegister> { };
    struct Source1Predicate : public pegtl::seq<PredicateRegister> { };

    struct HexadecimalNumber : public pegtl::if_must<pegtl::istring< '0', 'x'>, pegtl::plus<pegtl::xdigit>> { };
    struct BinaryNumber : public pegtl::if_must<pegtl::istring<  '0', 'b' >, pegtl::plus<pegtl::abnf::BIT>> { };
    struct DecimalNumber : public pegtl::plus<pegtl::digit> { };
    struct Number : public pegtl::sor<HexadecimalNumber, DecimalNumber, BinaryNumber> { };
    struct Lexeme : public pegtl::identifier { };
    struct LexemeOrNumber : public pegtl::sor<Lexeme, Number> { };
#define DefSymbol(title, str) \
    struct Symbol ## title : public pegtl_string_t( #str ) { }
    // directives
    DefSymbol(LabelDirective, .label);
    DefSymbol(DataDirective, .data);
    DefSymbol(CodeDirective, .code);
    DefSymbol(OrgDirective, .org);
    DefSymbol(DeclareDirective, .declare);
    DefSymbol(HiDirective, @hi);
    DefSymbol(LoDirective, @lo);

    template<typename T>
    struct ZeroArgumentDirective : public pegtl::seq<T> { };

    template<typename T, typename F>
    struct OneArgumentDirective : public pegtl::seq<T, Separator, F> { };



    struct CodeDirective : public ZeroArgumentDirective<SymbolCodeDirective> {
    };
    struct DataDirective : public ZeroArgumentDirective<SymbolDataDirective> {
    };

    struct OrgDirective : public OneArgumentDirective<SymbolOrgDirective, Number> {
    };
    struct LabelDirective : public OneArgumentDirective<SymbolLabelDirective, Lexeme> {
    };

    template<typename T>
    struct LexemeOrNumberDirective : public OneArgumentDirective<T, LexemeOrNumber> { };
    struct DeclareDirective : public LexemeOrNumberDirective<SymbolDeclareDirective> {
    };
    struct Directive : public pegtl::sor<OrgDirective, LabelDirective, CodeDirective, DataDirective, DeclareDirective> {

    };
    struct HiDirective : public LexemeOrNumberDirective<SymbolHiDirective> {
    };
    struct LoDirective : public LexemeOrNumberDirective<SymbolLoDirective> {
    };
    struct Immediate : public pegtl::sor<LexemeOrNumber, HiDirective, LoDirective> { };
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
    struct ArithmeticThreeGPRInstruction : public pegtl::seq<OperationArithmeticThreeGPR, Separator, ThreeGPR> { };
    DefSymbol(Not, not);
    struct OperationArithmeticTwoGPR : public pegtl::sor<SymbolNot> { };
    struct ArithmeticTwoGPRInstruction : public pegtl::seq<OperationArithmeticTwoGPR, Separator, TwoGPR> { };

    DefSymbol(AddImmediate, addi);
    DefSymbol(SubImmediate, subi);
    DefSymbol(MulImmediate, muli);
    DefSymbol(DivImmediate, divi);
    DefSymbol(RemImmediate, remi);
    DefSymbol(ShiftLeftImmediate, shli);
    DefSymbol(ShiftRightImmediate, shri);
    struct OperationArithmeticTwoGPRHalfImmediate : public pegtl::sor< SymbolAddImmediate, SymbolSubImmediate, SymbolMulImmediate, SymbolDivImmediate, SymbolRemImmediate, SymbolShiftLeftImmediate, SymbolShiftRightImmediate> { };
    struct ArithmeticTwoGPRHalfImmediateInstruction : public pegtl::seq<OperationArithmeticTwoGPRHalfImmediate, Separator, TwoGPR, Separator, HalfImmediate> { };

    struct ArithmeticInstruction : public pegtl::sor<ArithmeticTwoGPRHalfImmediateInstruction, ArithmeticTwoGPRInstruction, ArithmeticThreeGPRInstruction> { };

    DefSymbol(MoveToIP, mtip);
    DefSymbol(MoveFromIP, mfip);
    DefSymbol(MoveToLR, mtlr);
    DefSymbol(MoveFromLR, mflr);
    struct OperationMoveOneGPR : public pegtl::sor<SymbolMoveToIP, SymbolMoveFromIP, SymbolMoveToLR, SymbolMoveFromLR> { };
    struct MoveOneGPRInstruction : public pegtl::sor<OperationMoveOneGPR, Separator, OneGPR> { };
    DefSymbol(Move, move);
    DefSymbol(Swap, swap);
    DefSymbol(Load, ld);
    DefSymbol(Store, st);
    DefSymbol(LoadIO, ldio);
    DefSymbol(StoreIO, stio);
    DefSymbol(Push, push);
    DefSymbol(Pop, pop);
    struct OperationMoveTwoGPR : public pegtl::sor<SymbolMove, SymbolSwap, SymbolLoadIO, SymbolStoreIO, SymbolLoad, SymbolStore, SymbolPush, SymbolPop> { };
    struct MoveTwoGPRInstruction : public pegtl::seq<OperationMoveTwoGPR, Separator, TwoGPR> { };
    DefSymbol(LoadWithOffset, ldwo);
    DefSymbol(StoreWithOffset, stwo);
    DefSymbol(LoadIOWithOffset, ldiowo);
    DefSymbol(StoreIOWithOffset, stiowo);
    struct OperationMoveTwoGPRHalfImmediate : public pegtl::sor<SymbolLoadWithOffset, SymbolStoreWithOffset, SymbolLoadIOWithOffset, SymbolStoreIOWithOffset> { };
    struct MoveTwoGPRHalfImmediateInstruction : public pegtl::seq<OperationMoveTwoGPRHalfImmediate, Separator, TwoGPR, Separator, HalfImmediate> { };

    DefSymbol(LoadCode, ldc);
    DefSymbol(StoreCode, stc);
    struct OperationMoveThreeGPR : public pegtl::sor<SymbolLoadCode, SymbolStoreCode> { };
    struct MoveThreeGPRInstruction : public pegtl::seq<OperationMoveThreeGPR, Separator, ThreeGPR> { };

    DefSymbol(PushImmediate, pushi);
    DefSymbol(Set, set);
    DefSymbol(LoadImmediatePrimary, ldi);
    DefSymbol(LoadImmediateSecondary, ldm);
    struct SymbolLoadImmediate : public pegtl::sor<SymbolLoadImmediatePrimary, SymbolLoadImmediateSecondary> { };
    DefSymbol(StoreImmediatePrimary, sti);
    DefSymbol(StoreImmediateAlternateTitle, memset);
    struct SymbolStoreImmediate : public pegtl::sor<SymbolStoreImmediatePrimary, SymbolStoreImmediateAlternateTitle> { };
    struct OperationMoveGPRImmediate : public pegtl::sor<SymbolStoreImmediate, SymbolLoadImmediate, SymbolSet, SymbolPushImmediate> { };

    struct MoveGPRImmediateInstruction : public pegtl::seq<OperationMoveGPRImmediate, Separator, DestinationGPR, Separator, Immediate> { };

    struct MoveInstruction : public pegtl::sor<MoveGPRImmediateInstruction, MoveThreeGPRInstruction, MoveTwoGPRHalfImmediateInstruction, MoveTwoGPRInstruction, MoveOneGPRInstruction> { };

    // branch
	template<typename Op, typename S>
	struct BranchUnconditional : public pegtl::seq<Op, Separator, S> { };
    DefSymbol(BranchUnconditional, b);
    DefSymbol(BranchUnconditionalLink, bl);
    struct OperationBranchOneGPR : public pegtl::sor<SymbolBranchUnconditional, SymbolBranchUnconditionalLink> { };
    struct BranchOneGPRInstruciton : public BranchUnconditional<OperationBranchOneGPR, DestinationGPR> { };
    DefSymbol(BranchUnconditionalImmediate, bi);
    DefSymbol(BranchUnconditionalImmediateLink, bil);
    struct OperationBranchImmediate : public pegtl::sor<SymbolBranchUnconditionalImmediateLink, SymbolBranchUnconditionalImmediate> { };
    struct BranchImmediateInstruction : public BranchUnconditional<OperationBranchImmediate, Immediate> { };

	struct GroupBranchUnconditional : public pegtl::sor<BranchOneGPRInstruciton, BranchImmediateInstruction> { };
    template<typename Op, typename S>
    struct BranchConditional : public pegtl::seq<Op, Separator, DestinationPredicateRegister, Separator, S> { };
    DefSymbol(BranchConditionalTrue, bt);
    DefSymbol(BranchConditionalTrueLink, btl);
    DefSymbol(BranchConditionalFalse, bf);
    DefSymbol(BranchConditionalFalseLink, bfl);
    struct OperationBranchConditionalGPR : public pegtl::sor<SymbolBranchConditionalFalseLink, SymbolBranchConditionalTrueLink, SymbolBranchConditionalFalse, SymbolBranchConditionalTrue> { };
    struct BranchConditionalGPRInstruction : public BranchConditional<OperationBranchConditionalGPR, Source0GPR> { };
    DefSymbol(BranchConditionalTrueImmediate, bit);
    DefSymbol(BranchConditionalTrueImmediateLink, bitl);
    DefSymbol(BranchConditionalFalseImmediate, bif);
    DefSymbol(BranchConditionalFalseImmediateLink, bifl);
    struct OperationBranchConditionalImmediate : public pegtl::sor<SymbolBranchConditionalTrueImmediateLink, SymbolBranchConditionalFalseImmediateLink, SymbolBranchConditionalTrueImmediate, SymbolBranchConditionalFalseImmediate> { };
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
    struct BranchConditionalNoArgsInstruction : public pegtl::seq<OperationBranchCondtionalNoArgs, Separator, DestinationPredicateRegister> { };
    DefSymbol(BranchLR, blr);
    DefSymbol(BranchLRLink, blrl);
    struct OperationBranchNoArgs : public pegtl::sor<SymbolBranchLR,  SymbolBranchLRLink> { };
    struct BranchNoArgsInstruction : public pegtl::seq<OperationBranchNoArgs> { };

    struct BranchInstruction : public pegtl::sor<GroupBranchUnconditional, BranchConditionalGPRInstruction, BranchConditionalImmediateInstruction, BranchIfInstruction, BranchConditionalNoArgsInstruction, BranchNoArgsInstruction> { };

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
    struct CompareRegisterInstruction : public pegtl::seq<CompareRegisterOperation, Separator, DestinationPredicate, Separator, SourceRegisters> { };
    struct CompareImmediateInstruction : public pegtl::seq<CompareImmediateOperation, Separator, DestinationPredicate, Separator, Source0GPR, Separator, HalfImmediate> { };
    struct CompareInstruction : public pegtl::sor<CompareRegisterInstruction, CompareImmediateInstruction> { };

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
    struct Statement : public pegtl::sor<Instruction, Directive> {
    };

   //template< typename E >
   //struct statement_list : pegtl::seq< seps, pegtl::until< pegtl::sor< E, pegtl::if_must< key_return, statement_return, E > >, statement, seps > > {};

	struct Anything : public pegtl::sor<Separator, SingleLineComment,Statement> { };

	struct Main : public pegtl::until<pegtl::eof, pegtl::must<Anything>> { };

    //struct Main : public pegtl::must<pegtl::seq<Separators, pegtl::until<pegtl::eof, Statement, Separators>>> { };

	struct AssemblerState {
		AssemblerState() : currentDataIndex(0), currentCodeIndex(0), inData(false), currentDataValue(0), group(0), operation(0), destination(0), source0(0), source1(0)  { }
		word currentDataIndex;
		word currentCodeIndex;
		bool inData;
		word currentDataValue;
		InstructionGroup group;
		byte operation;
		byte destination;
		byte source0;
		byte source1;
		Core core;
		void resetCurrentData() noexcept;
		void setImmediate(word value) noexcept;
		void setHalfImmediate(byte value) noexcept;
		void setHiHalfImmediate(word value) noexcept;
		void setLoHalfImmediate(word value) noexcept;
		void setGroup(InstructionGroup value) noexcept;
		template<typename T>
		void setOperation(T value) noexcept {
			operation = static_cast<byte>(value);
		}
		bool inCodeSection() const noexcept;
		bool inDataSection() const noexcept;
		void nowInCodeSection() noexcept;
		void nowInDataSection() noexcept;
		void setCurrentAddress(word value) noexcept;
	};
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
		source0 = syn::getLowerHalf<word>(value);
		source1 = syn::getUpperHalf<word>(value);
	}
	void AssemblerState::setGroup(InstructionGroup  value) noexcept {
		group = value;
	}
	void AssemblerState::setHalfImmediate(byte value) noexcept {
		source1 = value;
	}
	void AssemblerState::setHiHalfImmediate(word value) noexcept {
		setHalfImmediate(syn::getUpperHalf(value));
	}
	void AssemblerState::setLoHalfImmediate(word value) noexcept {
		setHalfImmediate(syn::getLowerHalf(value));
	}


	void AssemblerState::resetCurrentData() noexcept {
		if (inData) {
			currentDataValue = 0;
		} else {
			control = 0;
			destination = 0;
			source0 = 0;
			source1 = 0;
		}
	}

}

int main(int argc, char** argv) {
    pegtl::analyze<iris::Main>();
	iris::AssemblerState state;
    for(int i = 1; i < argc; ++i) {
		pegtl::file_parser(argv[i]).parse<iris::Main, iris::Action>(state);
    }
    return 0;
}
