/*
 * syn
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


// IrisCoreAssembler rewritten to use pegtl
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include "Base.h"
#include "AssemblerBase.h"
#include "Problem.h"
#include "IrisCore.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>
#include <pegtl/parse.hh>
#include <vector>
#include "IrisClipsExtensions.h"
#include "ClipsExtensions.h"

namespace iris {
    class AssemblerState;
}
namespace syn {
    DefWrapperSymbolicName(iris::AssemblerState, "iris:assembly-parsing-state");
}
namespace iris {
	template<typename R> struct Action : syn::Action<R> { };
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
		bool shouldResolveLabel() noexcept { return fullImmediate && hasLexeme; }
        dword encode();


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

	class AssemblerState {
		public:
			using LabelMap = std::map<std::string, word>;
			using LabelIterator = LabelMap::iterator;
			using ConstLabelIterator = LabelMap::const_iterator;
		public:
			AssemblerState() : currentDataIndex(0), currentCodeIndex(0), inData(false) { }
			void resetCurrentData() noexcept;
			void setImmediate(word value) noexcept;
			void setHalfImmediate(byte value) noexcept;
			void setGroup(InstructionGroup value) noexcept;
			template<typename T>
				void setOperation(T value) noexcept {
					current.operation = static_cast<byte>(value);
				}
			bool inCodeSection() const noexcept { return !inData; }
			bool inDataSection() const noexcept { return inData; }
			void nowInCodeSection() noexcept { inData = false; }
			void nowInDataSection() noexcept { inData = true; }
			void setCurrentAddress(word value) noexcept;
			void registerLabel(const std::string& label) noexcept;
			word getCurrentAddress() noexcept;
			void incrementCurrentAddress() noexcept;
			void saveToFinished() noexcept;
			void setTemporaryByte(byte value) noexcept { temporaryByte = value; }
			byte getTemporaryByte() const noexcept { return temporaryByte; }
			void setTemporaryWord(word value) noexcept { temporaryWord = value; }
			word getTemporaryWord() const noexcept { return temporaryWord; }
			void setDestination(byte destination) noexcept { current.destination = destination; }
			void setSource0(byte destination) noexcept { current.source0 = destination; }
			void setSource1(byte destination) noexcept { current.source1 = destination; }
			void stashTemporaryByteInDestination() noexcept { setDestination(temporaryByte); }
			void stashTemporaryByteInSource0() noexcept { setSource0(temporaryByte); }
			void stashTemporaryByteInSource1() noexcept { setSource1(temporaryByte); }
			template<bool upper>
			void encodeDestinationPredicate() noexcept {
				setDestination(iris::encode4Bits<upper>(current.destination, temporaryByte));
			}
			template<bool upper>
			void encodeSource0Predicate() noexcept {
				setSource0(iris::encode4Bits<upper>(current.source0, temporaryByte));
			}
			void markHasLexeme() noexcept {
				current.hasLexeme = true;
			}
			void markNotLexeme() noexcept {
				current.hasLexeme = false;
			}
			void setLexeme(const std::string& lexeme) noexcept {
				markHasLexeme();
				current.currentLexeme = lexeme;
			}
			void markIsInstruction() noexcept {
				current.instruction = true;
			}
			void markIsNotInstruction() noexcept {
				current.instruction = false;
			}
			bool hasLexeme() const noexcept { return current.hasLexeme; }
			void setDataValue(word value) noexcept {
				current.dataValue = value;
			}
			void stashTemporaryWordIntoDataValue() noexcept {
				setDataValue(getTemporaryWord());
			}
			void markHasFullImmediate() noexcept {
				current.fullImmediate = true;
			}
			void markNotFullImmediate() noexcept {
				current.fullImmediate = false;
			}
			std::string getCurrentLexeme() const noexcept {
				return current.currentLexeme;
			}
			LabelIterator findLabel(const std::string& k) {
				return labelMap.find(k);
			}
			ConstLabelIterator findLabel(const std::string& k) const {
				return labelMap.find(k);
			}
			LabelIterator endLabel() { return labelMap.end(); }
			ConstLabelIterator endLabel() const { return labelMap.end(); }
			void applyToFinishedData(std::function<void(AssemblerData&)> fn) {
                applyToFinishedData([fn](AssemblerData& value, size_t _) { fn(value); });
			}
            void applyToFinishedData(std::function<void(AssemblerData&, size_t)> fn) {
                size_t index = 0;
				for(auto & value : finishedData) {
					fn(value, index);
                    ++index;
				}
            }
            size_t getNumberOfFinishedElements() const {
                return finishedData.size();
            }
		private:
			word currentDataIndex;
			word currentCodeIndex;
			bool inData;
			word temporaryWord;
			byte temporaryByte;
			AssemblerData current;
			LabelMap labelMap;
			std::vector<AssemblerData> finishedData;
	};
	struct ImmediateContainer : syn::NumberContainer<word> {
		using syn::NumberContainer<word>::NumberContainer;
		template<typename Input>
		void success(const Input& in, AssemblerState& parent) {
			parent.markNotLexeme();
			parent.setTemporaryWord(getValue());
		}
	};
	struct RegisterIndexContainer : syn::NumberContainer<byte> {
		using syn::NumberContainer<byte>::NumberContainer;
		template<typename Input>
			void success(const Input& in, AssemblerData& parent) {
				switch(_index) {
					case 0:
						parent.destination = getValue();
						break;
					case 1:
						parent.source0 = getValue();
						break;
					case 2:
						parent.source1 = getValue();
						break;
					default:
						syn::reportError("Illegal index provided!");
				}
			}
		template<typename Input>
			void success(const Input& in, AssemblerState& parent) {
				switch(_index) {
					case 0:
						parent.setDestination(getValue());
						break;
					case 1:
						parent.setSource0(getValue());
						break;
					case 2:
						parent.setSource1(getValue());
						break;
					default:
						syn::reportError("Illegal index provided!");
				}
			}
		unsigned int _index;
	};
#define DefAction(rule) template<> struct Action < rule >
#define DefApplyGeneric(type) template<typename Input> static void apply(const Input& in, type & state)
#define DefApply DefApplyGeneric(AssemblerState)
	using Separator = syn::AsmSeparator;
	using SingleLineComment = syn::SingleLineComment<';'>;
	struct GeneralPurposeRegister : syn::GenericRegister<'r'> { };
	struct PredicateRegister : syn::GenericRegister<'p'> { };
	DefAction(GeneralPurposeRegister) {
		DefApplyGeneric(RegisterIndexContainer) {
            state.setValue(syn::getRegister<word, ArchitectureConstants::RegisterCount>(in.string(), syn::reportError));
		}
        DefApply { }
	};
	DefAction(PredicateRegister) {
		DefApply {
			state.setTemporaryByte(syn::getRegister<word, ArchitectureConstants::ConditionRegisterCount>(in.string(), syn::reportError));
		}
	};
	using IndirectGPR = syn::Indirection<GeneralPurposeRegister>;
#define DefIndirectGPR(title) \
	struct title : IndirectGPR { }
	DefIndirectGPR(DestinationGPR);
	DefAction(DestinationGPR) {
		DefApplyGeneric(RegisterIndexContainer) {
            state._index = 0;
		}
        DefApply { }
	};
	DefIndirectGPR(Source0GPR);
	DefAction(Source0GPR) {
		DefApplyGeneric(RegisterIndexContainer) {
            state._index = 1;
		}
        DefApply { }
	};
    DefIndirectGPR(Source1GPR);
	DefAction(Source1GPR) {
		DefApplyGeneric(RegisterIndexContainer) {
            state._index = 2;
		}
        DefApply { }
	};
    using StatefulDestinationGPR = pegtl::state<RegisterIndexContainer, DestinationGPR>;
#undef DefIndirectGPR
	using SourceRegisters = syn::SourceRegisters<Source0GPR, Source1GPR>;
	struct OneGPR : pegtl::state<RegisterIndexContainer, syn::OneRegister<DestinationGPR>> { };
    struct TwoGPR : pegtl::state<RegisterIndexContainer, syn::TwoRegister<DestinationGPR, Source0GPR>> { };
	struct ThreeGPR : pegtl::state<RegisterIndexContainer, syn::TwoRegister<DestinationGPR, SourceRegisters>> { };
    using IndirectPredicateRegister = syn::Indirection<PredicateRegister>;
    struct DestinationPredicateRegister : IndirectPredicateRegister { };
	DefAction(DestinationPredicateRegister) {
		DefApply {
			state.encodeDestinationPredicate<false>();
		}
	};
    struct DestinationPredicateInverseRegister : public IndirectPredicateRegister { };
	DefAction(DestinationPredicateInverseRegister) {
		DefApply {
			state.encodeDestinationPredicate<true>();
		}
	};
	struct DestinationPredicates : public syn::TwoRegister<DestinationPredicateRegister, DestinationPredicateInverseRegister> { };
	struct Source0Predicate : public IndirectPredicateRegister { };
	DefAction(Source0Predicate) {
		DefApply {
			state.encodeSource0Predicate<false>();
		}
	};
	struct Source1Predicate : public IndirectPredicateRegister { };
	DefAction(Source1Predicate) {
		DefApply {
			state.encodeSource0Predicate<true>();
		}
	};

	using HexadecimalNumber = syn::HexadecimalNumber;
	DefAction(HexadecimalNumber) {
		template<typename Input>
		static void apply(const Input& in, ImmediateContainer& parent) {
			syn::populateContainer<word, syn::KnownNumberTypes::Hexadecimal>(in.string(), parent);
		}
	};
	using BinaryNumber = syn::BinaryNumber;
	DefAction(BinaryNumber) {
		template<typename Input>
		static void apply(const Input& in, ImmediateContainer& parent) {
			syn::populateContainer<word, syn::KnownNumberTypes::Binary>(in.string(), parent);
		}
	};
	using DecimalNumber = syn::Base10Number;
	DefAction(DecimalNumber) {
		template<typename Input>
		static void apply(const Input& in, ImmediateContainer& parent) {
			syn::populateContainer<word, syn::KnownNumberTypes::Decimal>(in.string(), parent);
		}
	};
    struct Number : public pegtl::state<ImmediateContainer, pegtl::sor<HexadecimalNumber, DecimalNumber, BinaryNumber>> { };
	using Lexeme = syn::Lexeme;
	DefAction(Lexeme) {
		DefApply {
			state.setLexeme(in.string());
		}
	};
	struct LexemeOrNumber : public syn::LexemeOr<Number> { };
    template<bool toCode>
    struct ModifySection {
        template<typename Input>
        ModifySection(const Input& in, AssemblerState& parent) { }

        template<typename Input>
        void success(const Input& in, AssemblerState& parent) {
            if (toCode) {
                parent.nowInCodeSection();
            } else {
                parent.nowInDataSection();
            }
        }

    };
    // directives
#define DefOperation(title, str, type) \
	DefSymbol(title, str); \
	DefAction(Symbol ## title ) { \
		DefApply { \
			state.setOperation< CURRENT_TYPE >( CURRENT_TYPE :: type ) ; \
		} \
	}

#define DefOperationSameTitle(title, str) DefOperation(title, str, title)
    DefSymbol(DataDirective, .data);
    DefSymbol(CodeDirective, .code);
    struct CodeDirective : syn::StatefulIndirection<ModifySection<true>, SymbolCodeDirective> { };
    struct DataDirective : syn::StatefulIndirection<ModifySection<false>, SymbolDataDirective> { };

    struct OrgDirective : syn::OneArgumentDirective<syn::SymbolOrgDirective, Number> { };
	DefAction(OrgDirective) { DefApply { state.setCurrentAddress(state.getTemporaryWord()); } };

    struct LabelDirective : syn::OneArgumentDirective<syn::SymbolLabelDirective, Lexeme> { };
	DefAction(LabelDirective) {
		DefApply {
			state.registerLabel(state.getCurrentLexeme());
			state.resetCurrentData();
		}
	};

    template<typename T>
    struct LexemeOrNumberDirective : public syn::OneArgumentDirective<T, LexemeOrNumber> { };
    struct DeclareDirective : public LexemeOrNumberDirective<syn::SymbolWordDirective> { };
	DefAction(DeclareDirective) {
		DefApply {
			if (state.inDataSection()) {
				state.markIsNotInstruction();
				if (!state.hasLexeme()) {
					state.stashTemporaryWordIntoDataValue();
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
			state.markHasFullImmediate();
			if (!state.hasLexeme()) {
				state.setImmediate(state.getTemporaryWord());
			}
		}
	};
    struct HalfImmediate : public pegtl::sor<Number> { };
	DefAction(HalfImmediate) {
		DefApply {
			state.markNotFullImmediate();
			state.setHalfImmediate(state.getTemporaryWord());
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

    struct MoveGPRImmediateInstruction : public pegtl::seq<OperationMoveGPRImmediate, Separator, StatefulDestinationGPR, Separator, Immediate> { };

    struct MoveInstruction : public pegtl::sor<MoveGPRImmediateInstruction, MoveThreeGPRInstruction, MoveTwoGPRHalfImmediateInstruction, MoveTwoGPRInstruction, MoveOneGPRInstruction> { };
	DefGroupSet(MoveInstruction, Move);
#undef CURRENT_TYPE
#define CURRENT_TYPE JumpOp
    // branch
	template<typename Op, typename S>
	struct BranchUnconditional : public pegtl::seq<Op, Separator, S> { };
    DefOperationSameTitle(BranchUnconditional, b);
    DefOperationSameTitle(BranchUnconditionalLink, bl);
    struct OperationBranchOneGPR : public pegtl::sor<SymbolBranchUnconditionalLink, SymbolBranchUnconditional> { };
    struct BranchOneGPRInstruction : public BranchUnconditional<OperationBranchOneGPR, StatefulDestinationGPR> { };
    DefOperationSameTitle(BranchUnconditionalImmediate, bi);
    DefOperationSameTitle(BranchUnconditionalImmediateLink, bil);
    struct OperationBranchImmediate : public pegtl::sor<SymbolBranchUnconditionalImmediateLink, SymbolBranchUnconditionalImmediate> { };
    struct BranchImmediateInstruction : public BranchUnconditional<OperationBranchImmediate, Immediate> { };

	struct GroupBranchUnconditional : public pegtl::sor<BranchOneGPRInstruction, BranchImmediateInstruction> { };
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
	DefOperation(BranchReturnFromError, rfe, ReturnFromError);
    struct OperationBranchNoArgs : public pegtl::sor<SymbolBranchUnconditionalLR,  SymbolBranchUnconditionalLRAndLink, SymbolBranchReturnFromError> { };
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
    struct CompareRegisterOperation : public pegtl::sor< SymbolEq, SymbolNeq, SymbolLessThan, SymbolGreaterThan, SymbolLessThanOrEqualTo, SymbolGreaterThanOrEqualTo> { };
    struct CompareImmediateOperation : public pegtl::sor<SymbolEqImmediate, SymbolNeqImmediate, SymbolLessThanImmediate, SymbolGreaterThanImmediate, SymbolLessThanOrEqualToImmediate, SymbolGreaterThanOrEqualToImmediate> { };
    struct CompareRegisterInstruction : public pegtl::seq<CompareRegisterOperation, Separator, DestinationPredicates, Separator, SourceRegisters> { };
    struct CompareImmediateInstruction : public pegtl::seq<CompareImmediateOperation, Separator, DestinationPredicates, Separator, Source0GPR, Separator, HalfImmediate> { };
    struct CompareInstruction : public pegtl::sor<CompareRegisterInstruction, CompareImmediateInstruction> { };
	DefGroupSet(CompareInstruction, Compare);

#undef CURRENT_TYPE
#define CURRENT_TYPE ConditionRegisterOp

    // conditional register actions
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
    struct OperationPredicateTwoArgs : public pegtl::sor<SymbolCRSwap, SymbolCRMove> { };
    struct OperationPredicateThreeArgs : public pegtl::sor<SymbolCRNot> { };
    struct OperationPredicateOneGPR : public pegtl::sor<SymbolSaveCRs, SymbolRestoreCRs> { };
    struct OperationPredicateFourArgs : public pegtl::sor<SymbolCRXor, SymbolCRAnd, SymbolCROr, SymbolCRNand, SymbolCRNor> { };
    struct PredicateInstructionOneGPR : public pegtl::seq<OperationPredicateOneGPR, Separator, StatefulDestinationGPR> { };
    struct PredicateInstructionTwoArgs : public pegtl::seq<OperationPredicateTwoArgs, Separator, DestinationPredicates> { };
    struct PredicateInstructionThreeArgs : public pegtl::seq<OperationPredicateThreeArgs, Separator, DestinationPredicates, Separator, Source0Predicate> { };
    struct PredicateInstructionFourArgs : public pegtl::seq<OperationPredicateFourArgs, Separator, DestinationPredicates, Separator, Source0Predicate, Separator, Source1Predicate> { };
    struct PredicateInstruction : public pegtl::sor<PredicateInstructionOneGPR, PredicateInstructionTwoArgs, PredicateInstructionThreeArgs, PredicateInstructionFourArgs> { };
	DefGroupSet(PredicateInstruction, ConditionalRegister);


#undef DefGroupSet
#undef DefOperation
#undef DefOperationSameTitle
#undef CURRENT_TYPE
#undef DefSymbol
    struct Instruction : public pegtl::sor<ArithmeticInstruction, MoveInstruction, BranchInstruction, CompareInstruction, PredicateInstruction> { };
	DefAction(Instruction) {
		DefApply {
			if (state.inCodeSection()) {
				state.markIsInstruction();
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
    raw_instruction AssemblerData::encode() {
        if (instruction) {
            return iris::encodeInstruction(group, operation, destination, source0, source1);
        } else {
            return dataValue;
        }
    }
	void resolveLabels(AssemblerState& state, std::ostream& output) {
		// now that we have instructions, we need to print them out as hex values
		char buf[8] = { 0 };
		auto resolveLabel = [&state](AssemblerData& data) {
			auto result = state.findLabel(data.currentLexeme);
			if (result == state.endLabel()) {
				std::stringstream msg;
				msg << "ERROR: label " << data.currentLexeme << " is undefined!" << std::endl;
				auto str = msg.str();
				throw syn::Problem(str);
			} else {
				return result->second;
			}
		};
		state.applyToFinishedData([&buf, &output, resolveLabel](auto value) {
					buf[0] = 0;
					buf[1] = value.instruction ? 0 : 1;
					buf[2] = static_cast<char>(syn::getLowerHalf<word>(value.address));
					buf[3] = static_cast<char>(syn::getUpperHalf<word>(value.address));
					if (value.instruction) {
						buf[4] = static_cast<char>(iris::encodeOperationByte(iris::encodeGroupByte(0, value.group), value.operation));
						buf[5] = static_cast<char>(value.destination);
						if (value.shouldResolveLabel()) {
							value.setImmediate(resolveLabel(value));
						}
					} else {
						if (value.shouldResolveLabel()) {
							value.dataValue = resolveLabel(value);
						}
						buf[4] = syn::getLowerHalf<word>(value.dataValue);
						buf[5] = syn::getUpperHalf<word>(value.dataValue);
					}
					buf[6] = value.instruction ? static_cast<char>(value.source0) : 0;
					buf[7] = value.instruction ? static_cast<char>(value.source1) : 0;
					output.write(static_cast<char*>(buf), sizeof(buf));
				});
	}

	void assemble(const std::string& iName, FILE* input, std::ostream* output) {
		iris::AssemblerState state;
    	pegtl::analyze<iris::Main>();
		// put a sufficently large amount of space to read from the cstream
		pegtl::parse_cstream<iris::Main, iris::Action>(input, iName.c_str(), 16777216, state);
		resolveLabels(state, *output);
	}

    class AssemblerStateWrapper : public syn::ExternalAddressWrapper<AssemblerState> {

        public:
            using Self = AssemblerStateWrapper;
            using Parent = syn::ExternalAddressWrapper<AssemblerState>;
            static Self* make() noexcept { return new Self(); }
            static void registerWithEnvironment(void* env, const char* title) {
                Parent::registerWithEnvironment(env, title, callFunction);
            }
            static void registerWithEnvironment(void* env) {
                static bool init = true;
                static std::string func;
                if (init) {
                    init = false;
                    func = Self::getType();
                }
                registerWithEnvironment(env, func.c_str());
            }
            static bool callFunction(void* env, syn::DataObjectPtr value, syn::DataObjectPtr ret) {
                static bool init = true;
                static std::string funcStr;
                static std::string funcErrorPrefix;
                static std::map<std::string, Operations> ops = {
                    { "parse", Operations::Parse },
                    { "resolve", Operations::Resolve },
                    { "get", Operations::Get },
                };
                static std::map<Operations, int> opArgCount = {
                    { Operations::Parse, 1 },
                    { Operations::Resolve, 0 },
                    { Operations::Get, 0 },
                };
				auto callErrorMessage = [env, ret](const std::string& subOp, const std::string& rest) {
					CVSetBoolean(ret, false);
					std::stringstream stm;
					stm << " " << subOp << ": " << rest << std::endl;
					auto msg = stm.str();
					return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, msg);
				};

                if (init) {
                    init = false;
                    auto functions = syn::retrieveFunctionNames<AssemblerState>("call");
                    funcStr = std::get<1>(functions);
                    funcErrorPrefix = std::get<2>(functions);
                }
                if (GetpType(value) != EXTERNAL_ADDRESS) {
                    return syn::errorMessage(env, "CALL", 1, funcErrorPrefix, "Function call expected an external address as the first argument!");
                }
                CLIPSValue operation;
                if (!EnvArgTypeCheck(env, funcStr.c_str(), 2, SYMBOL, &operation)) {
                    return syn::errorMessage(env, "CALL", 2, funcErrorPrefix, "expected a function name to call!");
                }
                std::string str(EnvDOToString(env, operation));
                auto result = ops.find(str);
                if (result == ops.end()) {
                    CVSetBoolean(ret, false);
                    return callErrorMessage(str, " <- unknown operation requested!");
                }
                auto theOp = result->second;
                auto cResult = opArgCount.find(theOp);
                if (cResult == opArgCount.end()) {
                    CVSetBoolean(ret, false);
                    return callErrorMessage(str, " <- illegal argument count!");
                }
                auto aCount = 2 + cResult->second;
                if (aCount != EnvRtnArgCount(env)) {
                    CVSetBoolean(ret, false);
                    return callErrorMessage(str, " too many arguments provided!");
                }
                auto ptr = static_cast<Self*>(DOPToExternalAddress(value));
                auto parseLine = [env, ret, ptr]() {
                    CLIPSValue line;
                    if (!EnvArgTypeCheck(env, funcStr.c_str(), 3, STRING, &line)) {
                        CVSetBoolean(ret, false);
                        return syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "provided assembly line is not a string!");
                    }
                    std::string str(EnvDOToString(env, line));
                    auto result = ptr->parseLine(str);
                    CVSetBoolean(ret, result);
                    if (!result) {
                        syn::errorMessage(env, "CALL", 3, funcErrorPrefix, "parse: error during parsing!");
                    }
                    return result;
                };
                switch(theOp) {
                    case Operations::Parse:
                        return parseLine();
                    case Operations::Resolve:
                        return ptr->resolve();
                        return true;
                    case Operations::Get:
                        ptr-> getMultifield(env, ret);
                        return true;
                    default:
                        CVSetBoolean(ret, false);
                        return callErrorMessage(str, "<- unimlemented operation!!!!");
                }
                return false;
            }
        public:
            enum Operations {
                Parse,
                Resolve,
                Get,
                Count,
            };
         public:
            AssemblerStateWrapper() : Parent(std::move(std::make_unique<AssemblerState>())) { }
            bool parseLine(const std::string& line);
            bool resolve();
            void getMultifield(void* env, CLIPSValuePtr ret);
         private:
            void output(void* env, CLIPSValue* ret) noexcept;
    };

    void AssemblerStateWrapper::getMultifield(void* env, CLIPSValuePtr ret) {
        //get()->output(env, ret);
        output(env, ret);
    }
    bool AssemblerStateWrapper::resolve() {
        auto & state = *(get());
		auto resolveLabel = [&state](AssemblerData& data) {
			auto result = state.findLabel(data.currentLexeme);
			if (result == state.endLabel()) {
				std::stringstream msg;
				msg << "ERROR: label " << data.currentLexeme << " is undefined!" << std::endl;
				auto str = msg.str();
				throw syn::Problem(str);
			} else {
				return result->second;
			}
		};
        state.applyToFinishedData([resolveLabel](auto value, auto index) {
                    if (value.shouldResolveLabel()) {
                        if (value.instruction) {
                            value.setImmediate(resolveLabel(value));
                        } else {
                            value.dataValue = resolveLabel(value);
                        }
                    }
                });
        return true;
    }
    bool AssemblerStateWrapper::parseLine(const std::string& line) {
        auto& ref = *(get());
        return pegtl::parse_string<iris::Main, iris::Action>(line, "clips-input", ref);
    }
    void AssemblerStateWrapper::output(void* env, CLIPSValue* ret) noexcept {
        // we need to build a multifield out of the finalWords
        auto & state = *(get());
        syn::MultifieldBuilder f(env, state.getNumberOfFinishedElements() * 3);
        auto body = [&f, env, ret](auto value, auto baseIndex) {
            auto i = (3 * baseIndex) + 1;
            f.setField(i + 0, INTEGER, EnvAddLong(env, value.instruction ? 0 : 1));
            f.setField(i + 1, INTEGER, EnvAddLong(env, value.address));
            f.setField(i + 2, INTEGER, EnvAddLong(env, value.encode()));
        };
        state.applyToFinishedData(body);
        f.assign(ret);
    }

    void installAssemblerParsingState(void* env) {
        pegtl::analyze<iris::Main>();
        AssemblerStateWrapper::registerWithEnvironment(env);
        AssemblerStateWrapper::registerWithEnvironment(env, "iris-asm-parser");
        AssemblerStateWrapper::registerWithEnvironment(env, "iris-assembler");
    }
}
