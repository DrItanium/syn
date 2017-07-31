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
#ifndef IRIS_CORE_ASSEMBLER_H__
#define IRIS_CORE_ASSEMBLER_H__
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include "Base.h"
#include "AssemblerBase.h"
#include "Problem.h"
#include "IrisCoreEncodingOperations.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>
#include <pegtl/parse.hh>
#include <vector>
#include "IrisClipsExtensions.h"
#include "ClipsExtensions.h"
#include "IrisCoreAssemblerKeywords.h"
#include "IrisCoreAssemblerStructures.h"

namespace iris {

    template<typename R> struct Action : syn::Action<R> { };

	struct HalfImmediateContainer;
    struct ImmediateContainer : syn::NumberContainer<word> {
        using syn::NumberContainer<word>::NumberContainer;
		template<typename Input>
			void success(const Input& in, HalfImmediateContainer& parent);
		template<typename Input>
			void success(const Input& in, AssemblerInstruction& parent);
		template<typename Input>
			void success(const Input& in, AssemblerDirective& parent);
    };
    struct RegisterIndexContainer : syn::NumberContainer<byte> {
        using syn::NumberContainer<byte>::NumberContainer;
        template<typename Input>
            void success(const Input& in, AssemblerInstruction& parent) {
				parent.setField(_index, getValue());
            }
        RegisterPositionType _index;
    };
	template<InstructionGroup type>
	struct SetInstructionGroup {
		template<typename Input>
		static void apply(const Input& in, AssemblerInstruction& instruction) {
			instruction.group = static_cast<byte>(type);
		}
	};
    using Separator = syn::AsmSeparator;
    template<typename First, typename Second, typename Sep = Separator>
    using SeparatedBinaryThing = syn::TwoPartComponent<First, Second, Sep>;
    template<typename First, typename Second, typename Third, typename Sep = Separator>
    using SeparatedTrinaryThing = syn::ThreePartComponent<First, Second, Third, Sep, Sep>;
    using SingleLineComment = syn::SingleLineComment<';'>;
    using GeneralPurposeRegister = syn::GPR;
    using PredicateRegister = syn::PredicateRegister;
	template<word count>
	struct SetRegisterGeneric {
		DefApplyGeneric(AssemblerInstruction) { }
		DefApplyGeneric(RegisterIndexContainer) {
			state.setValue(syn::getRegister<word, count>(in.string(), syn::reportError));
		}
	};
    DefAction(GeneralPurposeRegister) : SetRegisterGeneric<ArchitectureConstants::RegisterCount> { };
    DefAction(PredicateRegister) : SetRegisterGeneric<ArchitectureConstants::ConditionRegisterCount> { };
	template<RegisterPositionType pos>
	struct GenericRegisterIndexContainerAction {
		DefApplyGeneric(AssemblerInstruction) { }
		DefApplyGeneric(RegisterIndexContainer) {
			state._index = pos;
		}
	};
    // GPRs
    using IndirectGPR = syn::SingleEntrySequence<GeneralPurposeRegister>;
    struct DestinationGPR : IndirectGPR {  };
    struct Source0GPR : IndirectGPR { };
    struct Source1GPR : IndirectGPR { };
	DefAction(DestinationGPR) : GenericRegisterIndexContainerAction<RegisterPositionType::DestinationGPR> { };
	DefAction(Source0GPR) : GenericRegisterIndexContainerAction<RegisterPositionType::Source0GPR> { };
	DefAction(Source1GPR) : GenericRegisterIndexContainerAction<RegisterPositionType::Source1GPR> { };
    template<typename T>
    using StatefulRegister = pegtl::state<RegisterIndexContainer, T>;
    using StatefulDestinationGPR = StatefulRegister<DestinationGPR>;
    using SourceRegisters = syn::SourceRegisters<StatefulRegister<Source0GPR>, StatefulRegister<Source1GPR>>;
    using OneGPR = syn::OneRegister<StatefulDestinationGPR>;
    using TwoGPR = syn::TwoRegister<StatefulDestinationGPR, StatefulRegister<Source0GPR>>;
    using ThreeGPR = syn::TwoRegister<StatefulDestinationGPR, SourceRegisters>;

    // predicate registers
    using IndirectPredicateRegister = syn::SingleEntrySequence<PredicateRegister>;
    struct DestinationPredicateRegister : IndirectPredicateRegister { };
    struct DestinationPredicateInverseRegister : IndirectPredicateRegister { };
    struct Source0Predicate : IndirectPredicateRegister { };
    struct Source1Predicate : IndirectPredicateRegister { };
    DefAction(DestinationPredicateRegister) : GenericRegisterIndexContainerAction<RegisterPositionType::PredicateDestination> { };
    DefAction(DestinationPredicateInverseRegister) : GenericRegisterIndexContainerAction<RegisterPositionType::PredicateInverseDestination> { };
    DefAction(Source0Predicate) : GenericRegisterIndexContainerAction<RegisterPositionType::PredicateSource0> { };
    DefAction(Source1Predicate) : GenericRegisterIndexContainerAction<RegisterPositionType::PredicateSource1> { };

    using DestinationPredicates = syn::TwoRegister<StatefulRegister<DestinationPredicateRegister>, StatefulRegister<DestinationPredicateInverseRegister>>;

	struct FullImmediateContainer;
	template<syn::KnownNumberTypes t>
	struct PopulateNumberType {
		DefApplyGeneric(ImmediateContainer) {
			syn::populateContainer<word, t>(in.string(), state);
		}
		DefApplyGeneric(FullImmediateContainer) {
			syn::populateContainer<word, t>(in.string(), state);
		}
	};
    DefAction(syn::HexadecimalNumber) : PopulateNumberType<syn::KnownNumberTypes::Hexadecimal>{ };
    DefAction(syn::BinaryNumber) : PopulateNumberType<syn::KnownNumberTypes::Binary> { };
    DefAction(syn::Base10Number) : PopulateNumberType<syn::KnownNumberTypes::Decimal> { };
	template<typename State = ImmediateContainer>
    struct Number : syn::StatefulNumberAll<State> { };
    using Lexeme = syn::Lexeme;

    DefAction(Lexeme) {
        DefApplyGeneric(AssemblerData) {
			state.hasLexeme = true;
			state.currentLexeme = in.string();
        }
		DefApplyGeneric(syn::StringContainer) {
			state.setValue(in.string());
		}
		DefApplyGeneric(syn::NumberOrStringContainer<word>) {
			state.setStringValue(in.string());
		}
    };
	template<typename State = ImmediateContainer>
    struct LexemeOrNumber : syn::LexemeOr<Number<State>> { };
    template<SectionType section>
        struct ModifySection {
			template<typename Input>
				ModifySection(const Input& in, AssemblerDirective& parent) {
					parent.action = AssemblerDirectiveAction::ChangeSection;
					parent.section = section;
				}

			template<typename Input>
			void success(const Input& in, AssemblerDirective& parent) { }
        };
    // directives
    template<SectionType modSection, typename Directive>
        struct StatefulSpaceDirective : syn::StatefulSingleEntrySequence<ModifySection<modSection>, Directive> { };
    struct CodeDirective : StatefulSpaceDirective<SectionType::Code, SymbolCodeDirective> { };
    struct DataDirective : StatefulSpaceDirective<SectionType::Data, SymbolDataDirective> { };

	struct OrgDirectiveHandler : public ImmediateContainer {
		using Parent = ImmediateContainer;
		template<typename Input>
		OrgDirectiveHandler(const Input& in, AssemblerDirective& parent) : Parent(in, parent){
			parent.action = AssemblerDirectiveAction::ChangeCurrentAddress;
		}

		template<typename Input>
		void success(const Input& in, AssemblerDirective& parent) {
			parent.address = getValue();
		}
	};
    struct OrgDirective : syn::OneArgumentDirective<syn::SymbolOrgDirective, Number<OrgDirectiveHandler>> { };
	struct LabelDirectiveHandler : public syn::StringContainer {
		using Parent = syn::StringContainer;
		template<typename Input>
		LabelDirectiveHandler(const Input& in, AssemblerDirective& parent) : Parent(in, parent){
			parent.action = AssemblerDirectiveAction::DefineLabel;
		}
		template<typename Input>
		void success(const Input& in, AssemblerDirective& parent) {
			parent.currentLexeme = getValue();
		}
	};
    struct LabelDirective : pegtl::state<LabelDirectiveHandler, syn::OneArgumentDirective<syn::SymbolLabelDirective, Lexeme>> { };
	struct FullImmediateContainer : public syn::NumberOrStringContainer<word> {
		public:
			using Parent = syn::NumberOrStringContainer<word>;
		public:
            using Parent::Parent;

			template<typename Input>
			void success(const Input& in, AssemblerInstruction& parent) {
				parent.fullImmediate = true;
				parent.hasLexeme = !isNumber();
				if (isNumber()) {
					parent.setImmediate(getNumberValue());
				} else {
					parent.currentLexeme = getStringValue();
				}
			}
			template<typename Input>
			void success(const Input& in, AssemblerDirective& parent) {
				parent.action = AssemblerDirectiveAction::StoreWord;
				parent.fullImmediate = true;
				parent.hasLexeme = !isNumber();
				if (isNumber()) {
					parent.dataValue = getNumberValue();
				} else {
					parent.currentLexeme = getStringValue();
				}
			}
	};
    template<typename T, typename State = ImmediateContainer>
    using LexemeOrNumberDirective = syn::OneArgumentDirective<T, LexemeOrNumber<State>>;
    struct DeclareDirective : LexemeOrNumberDirective<syn::SymbolWordDirective, FullImmediateContainer> { };
    struct Directive : pegtl::state<
                       AssemblerDirective,
                       pegtl::sor<
                         OrgDirective,
                         LabelDirective,
                         CodeDirective,
                         DataDirective,
                         DeclareDirective>> { };
    using Immediate = LexemeOrNumber<FullImmediateContainer>;
	struct HalfImmediateContainer : ImmediateContainer {
		using ImmediateContainer::ImmediateContainer;
		template<typename I>
		void success(const I& in, AssemblerInstruction& inst) {
			inst.hasLexeme = false;
			inst.fullImmediate = false;
			inst.source1 = getValue();
		}
	};

	template<typename Input>
	void ImmediateContainer::success(const Input& in, HalfImmediateContainer& parent) {
		parent.setValue(getValue());
	}

	template<typename Input>
	void ImmediateContainer::success(const Input& in, AssemblerInstruction& parent) {
		parent.setImmediate(getValue());
	}

	template<typename Input>
	void ImmediateContainer::success(const Input& in, AssemblerDirective& parent) {
		parent.setImmediate(getValue());
	}
    using HalfImmediate = Number<HalfImmediateContainer>;

    template<InstructionGroup op>
    struct SubTypeSelector {
        static constexpr bool legalInstructionGroup(InstructionGroup group) noexcept {
            switch(group) {
                case InstructionGroup::Arithmetic:
                case InstructionGroup::ConditionalRegister:
                case InstructionGroup::Jump:
                case InstructionGroup::Move:
                case InstructionGroup::Compare:
                    return true;
                default:
                    return false;
            }
        }
        static_assert(legalInstructionGroup(op), "Instruction group has no subtypes or is unimplemented!");
        DefApplyGeneric(AssemblerInstruction) {
            switch(op) {
                case InstructionGroup::Arithmetic:
                    state.operation = (byte)stringToArithmeticOp(in.string());
                    break;
                case InstructionGroup::ConditionalRegister:
                    state.operation = (byte)stringToConditionRegisterOp(in.string());
                    break;
                case InstructionGroup::Jump:
                    state.operation = (byte)stringToJumpOp(in.string());
                    break;
                case InstructionGroup::Move:
                    state.operation = (byte)stringToMoveOp(in.string());
                    break;
                case InstructionGroup::Compare:
                    state.operation = (byte)stringToCompareOp(in.string());
                    break;
            }
        }
    };

    template<typename Operation, typename Operands>
    using GenericInstruction = syn::Instruction<Operation, Operands>;
    template<typename Operation>
    using OneGPRInstruction = GenericInstruction<Operation, OneGPR>;
    template<typename Operation>
    using TwoGPRInstruction = GenericInstruction<Operation, TwoGPR>;
    template<typename Operation>
    using ThreeGPRInstruction = GenericInstruction<Operation, ThreeGPR>;

    // Arithmetic group
    using ArithmeticSubTypeSelector = SubTypeSelector<InstructionGroup::Arithmetic>;
    struct OperationArithmeticThreeGPR : pegtl::sor<
                                         SymbolAdd,
                                         SymbolSub,
                                         SymbolMul,
                                         SymbolDiv,
                                         SymbolRem,
                                         SymbolShiftLeft,
                                         SymbolShiftRight,
                                         SymbolBinaryAnd,
                                         SymbolBinaryOr,
                                         SymbolBinaryXor,
                                         SymbolMin,
                                         SymbolMax> { };
    // Just extend off of this single type for now
    struct OperationArithmeticTwoGPR : SymbolBinaryNot { };
    struct ArithmeticImmediateOperation : pegtl::sor<
                                          SymbolAddImmediate,
                                          SymbolSubImmediate,
                                          SymbolMulImmediate,
                                          SymbolDivImmediate,
                                          SymbolRemImmediate,
                                          SymbolShiftLeftImmediate,
                                          SymbolShiftRightImmediate> { };
	DefAction(OperationArithmeticThreeGPR) : ArithmeticSubTypeSelector { };
	DefAction(OperationArithmeticTwoGPR) : ArithmeticSubTypeSelector { };
	DefAction(ArithmeticImmediateOperation) : ArithmeticSubTypeSelector { };

    struct ArithmeticThreeGPRInstruction : ThreeGPRInstruction<OperationArithmeticThreeGPR> { };
    struct ArithmeticTwoGPRInstruction : TwoGPRInstruction<OperationArithmeticTwoGPR> { };

    struct ArithmeticTwoGPRHalfImmediateInstruction : SeparatedTrinaryThing<
                                                      ArithmeticImmediateOperation,
                                                      TwoGPR,
                                                      HalfImmediate> { };
    struct ArithmeticInstruction : pegtl::sor<
                                   ArithmeticTwoGPRHalfImmediateInstruction,
                                   ArithmeticTwoGPRInstruction,
                                   ArithmeticThreeGPRInstruction> { };
	DefAction(ArithmeticInstruction) : SetInstructionGroup<InstructionGroup::Arithmetic> { };

    // Move operations
	using MoveOpSubTypeSelector = SubTypeSelector<InstructionGroup::Move>;
    struct OperationMoveOneGPR : pegtl::sor<
                                 SymbolMoveToIP,
                                 SymbolMoveFromIP,
                                 SymbolMoveToLR,
                                 SymbolMoveFromLR,
                                 SymbolRestoreAllRegisters,
                                 SymbolSaveAllRegisters> { };
    struct OperationMoveTwoGPR : pegtl::sor<
                                 SymbolMove,
                                 SymbolSwap,
                                 SymbolLoadIO,
                                 SymbolStoreIO,
                                 SymbolLoad,
                                 SymbolStore,
                                 SymbolPush,
                                 SymbolPop> { };
    struct OperationMoveTwoGPRHalfImmediate : pegtl::sor<
                                              SymbolLoadWithOffset,
                                              SymbolStoreWithOffset,
                                              SymbolLoadIOWithOffset,
                                              SymbolStoreIOWithOffset> { };
    struct OperationMoveThreeGPR : pegtl::sor<
                                   SymbolLoadCode,
                                   SymbolStoreCode> { };
    struct OperationMoveGPRImmediate : pegtl::sor<
                                       SymbolStoreImmediate,
                                       SymbolLoadImmediate,
                                       SymbolSet,
                                       SymbolPushImmediate> { };

	DefAction(OperationMoveOneGPR) : MoveOpSubTypeSelector { };
	DefAction(OperationMoveTwoGPR) : MoveOpSubTypeSelector { };
	DefAction(OperationMoveTwoGPRHalfImmediate) : MoveOpSubTypeSelector { };
	DefAction(OperationMoveThreeGPR) : MoveOpSubTypeSelector { };
	DefAction(OperationMoveGPRImmediate) : MoveOpSubTypeSelector { };

    struct MoveOneGPRInstruction : OneGPRInstruction<OperationMoveOneGPR> { };
    struct MoveTwoGPRInstruction : TwoGPRInstruction<OperationMoveTwoGPR> { };
    struct MoveTwoGPRHalfImmediateInstruction : SeparatedTrinaryThing<
                                                OperationMoveTwoGPRHalfImmediate,
                                                TwoGPR,
                                                HalfImmediate> { };
    struct MoveThreeGPRInstruction : ThreeGPRInstruction<OperationMoveThreeGPR> { };
    struct MoveGPRImmediateInstruction : SeparatedTrinaryThing<
                                         OperationMoveGPRImmediate,
                                         StatefulDestinationGPR,
                                         Immediate> { };
    struct MoveInstruction : pegtl::sor<
                             MoveGPRImmediateInstruction,
                             MoveThreeGPRInstruction,
                             MoveTwoGPRHalfImmediateInstruction,
                             MoveTwoGPRInstruction,
                             MoveOneGPRInstruction> { };
	DefAction(MoveInstruction) : SetInstructionGroup<InstructionGroup::Move> { };

    // branch
	using BranchOpSubTypeSelector = SubTypeSelector<InstructionGroup::Jump>;
    template<typename Op, typename S>
    using BranchUnconditional = SeparatedBinaryThing<Op, S>;
    template<typename Op, typename S>
    using BranchConditional = SeparatedTrinaryThing<Op, DestinationPredicateRegister, S>;

    struct OperationBranchOneGPR : pegtl::sor<
                                   SymbolBranchUnconditionalLink,
                                   SymbolBranchUnconditional> { };
    struct OperationBranchImmediate : pegtl::sor<
                                      SymbolBranchUnconditionalImmediateLink,
                                      SymbolBranchUnconditionalImmediate> { };
    struct OperationBranchConditionalGPR : pegtl::sor<
                                           SymbolBranchConditionalLink,
                                           SymbolBranchConditional
                                           > { };
    struct OperationBranchConditionalImmediate : pegtl::sor<
                                                 SymbolBranchConditionalImmediateLink,
                                                 SymbolBranchConditionalImmediate
                                                 > { };
    struct OperationBranchConditionalNoArgs : pegtl::sor<
                                              SymbolBranchConditionalLRAndLink,
                                              SymbolBranchConditionalLR
                                              > { };
    struct BranchNoArgsInstruction : pegtl::sor<
                                     SymbolBranchUnconditionalLRAndLink,
                                     SymbolBranchUnconditionalLR,
                                     SymbolReturnFromError> { };

	DefAction(OperationBranchOneGPR) : BranchOpSubTypeSelector { };
	DefAction(OperationBranchImmediate) : BranchOpSubTypeSelector { };
	DefAction(OperationBranchConditionalGPR) : BranchOpSubTypeSelector { };
	DefAction(OperationBranchConditionalImmediate) : BranchOpSubTypeSelector { };
	DefAction(OperationBranchConditionalNoArgs) : BranchOpSubTypeSelector { };
	DefAction(BranchNoArgsInstruction) : BranchOpSubTypeSelector { };

    struct BranchOneGPRInstruction : BranchUnconditional<
                                     OperationBranchOneGPR,
                                     StatefulDestinationGPR> { };
    struct BranchImmediateInstruction : BranchUnconditional<
                                        OperationBranchImmediate,
                                        Immediate> { };
    struct BranchConditionalGPRInstruction : BranchConditional<
                                             OperationBranchConditionalGPR,
                                             Source0GPR> { };
    struct BranchConditionalImmediateInstruction : BranchConditional<
                                                   OperationBranchConditionalImmediate,
                                                   Immediate> { };
    struct BranchConditionalNoArgsInstruction : SeparatedBinaryThing<
                                                OperationBranchConditionalNoArgs,
                                                DestinationPredicateRegister> { };

    struct BranchInstruction : pegtl::sor<
                               BranchOneGPRInstruction,
                               BranchImmediateInstruction,
                               BranchConditionalGPRInstruction,
                               BranchConditionalImmediateInstruction,
                               BranchConditionalNoArgsInstruction,
                               BranchNoArgsInstruction> { };
	DefAction(BranchInstruction) : SetInstructionGroup<InstructionGroup::Jump> { };

    // compare operations
	using CompareOpSubTypeSelector = SubTypeSelector<InstructionGroup::Compare>;
    struct CompareRegisterOperation : pegtl::sor<
                                      SymbolEq,
                                      SymbolNeq,
                                      SymbolLessThan,
                                      SymbolGreaterThan,
                                      SymbolLessThanOrEqualTo,
                                      SymbolGreaterThanOrEqualTo> { };
    struct CompareImmediateOperation : pegtl::sor<
                                       SymbolEqImmediate,
                                       SymbolNeqImmediate,
                                       SymbolLessThanImmediate,
                                       SymbolGreaterThanImmediate,
                                       SymbolLessThanOrEqualToImmediate,
                                       SymbolGreaterThanOrEqualToImmediate> { };

	DefAction(CompareRegisterOperation) : CompareOpSubTypeSelector { };
	DefAction(CompareImmediateOperation) : CompareOpSubTypeSelector { };

	template<typename Operation, typename ... Sources>
	using PredicateDestinationInstruction = pegtl::seq<Operation, Separator, DestinationPredicates, Separator, Sources...>;
	struct CompareRegisterInstruction : PredicateDestinationInstruction<
										CompareRegisterOperation,
										SourceRegisters> { };
	struct CompareImmediateInstruction : PredicateDestinationInstruction<
										 CompareImmediateOperation,
										 Source0GPR,
										 Separator,
										 HalfImmediate> { };
    struct CompareInstruction : pegtl::sor<
                                CompareImmediateInstruction,
                                CompareRegisterInstruction
                                > { };
	DefAction(CompareInstruction) : SetInstructionGroup<InstructionGroup::Compare> { };

    // conditional register actions
	using ConditionalRegisterSubTypeSelector = SubTypeSelector<InstructionGroup::ConditionalRegister>;
	using StatefulSource0Predicate = StatefulRegister<Source0Predicate>;
    struct OperationPredicateTwoArgs : pegtl::sor<
                                       SymbolCRSwap,
                                       SymbolCRMove> { };
    struct OperationPredicateOneGPR : pegtl::sor<
                                      SymbolSaveCRs,
                                      SymbolRestoreCRs> { };
    struct OperationPredicateFourArgs : pegtl::sor<
                                        SymbolCRXor,
                                        SymbolCRAnd,
                                        SymbolCROr,
                                        SymbolCRNand,
                                        SymbolCRNor> { };

	DefAction(OperationPredicateTwoArgs) : ConditionalRegisterSubTypeSelector { };
	DefAction(OperationPredicateOneGPR) : ConditionalRegisterSubTypeSelector { };
	DefAction(OperationPredicateFourArgs) : ConditionalRegisterSubTypeSelector { };

    struct PredicateInstructionOneGPR : pegtl::seq<
                                        OperationPredicateOneGPR,
										Separator,
										StatefulDestinationGPR> { };
    struct PredicateInstructionTwoArgs : pegtl::seq<
                                         OperationPredicateTwoArgs,
										 Separator,
										 DestinationPredicates> { };
    struct PredicateInstructionThreeArgs : PredicateDestinationInstruction<
                                           SymbolCRNot,
										   StatefulSource0Predicate> { };
    struct PredicateInstructionFourArgs : PredicateDestinationInstruction<
                                          OperationPredicateFourArgs,
										  StatefulSource0Predicate,
										  Separator,
										  StatefulRegister<Source1Predicate>> { };
    struct PredicateInstruction : pegtl::sor<
                                  PredicateInstructionOneGPR,
                                  PredicateInstructionTwoArgs,
                                  PredicateInstructionThreeArgs,
                                  PredicateInstructionFourArgs> { };
	DefAction(PredicateInstruction) : SetInstructionGroup<InstructionGroup::ConditionalRegister> { };

    struct Instruction : pegtl::state<
                         AssemblerInstruction,
                         pegtl::sor<
                            ArithmeticInstruction,
                            MoveInstruction,
                            BranchInstruction,
                            CompareInstruction,
                            PredicateInstruction>> { };
    struct Statement : pegtl::sor<
                       Instruction,
                       Directive> { };
    struct Anything : pegtl::sor<
                      Separator,
                      SingleLineComment,Statement> { };
    struct Main : syn::MainFileParser<Anything> { };
} // end namespace iris
#endif // end IRIS_CORE_ASSEMBLER_H__
