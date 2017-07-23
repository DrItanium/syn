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

namespace iris {
    enum class SectionType {
        Code,
        Data,
        Count,
    };
    template<typename R> struct Action : syn::Action<R> { };
    struct AssemblerData {
        public:
            AssemblerData() noexcept;
            void reset() noexcept;
            void setImmediate(word value) noexcept;
            bool shouldResolveLabel() const noexcept { return fullImmediate && hasLexeme; }
            dword encode() const noexcept;
        public:
            bool instruction;
            word address;
            word dataValue;

            byte group;
            byte operation;
            byte destination;
            byte source0;
            byte source1;
            bool hasLexeme;
            bool fullImmediate;
            std::string currentLexeme;
    };

    class AssemblerState : public syn::LabelTracker<word>, public syn::FinishedDataTracker<AssemblerData> {
        public:
            using LabelTracker = syn::LabelTracker<word>;
            AssemblerState() : inData(false), temporaryWord(0), temporaryByte(0) { }
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
            template<SectionType section>
                void changeSection() noexcept {
                    static_assert(!syn::isErrorState<SectionType>(section), "Illegal state!");
                    switch(section) {
                        case SectionType::Code:
                            nowInCodeSection();
                            break;
                        case SectionType::Data:
                            nowInDataSection();
                            break;
                    }
                }
            void setCurrentAddress(word value) noexcept;
            void registerLabel(const std::string& label) noexcept;
            word getCurrentAddress() const noexcept;
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
                void encodeDestinationPredicate(byte value) noexcept {
                    setDestination(iris::encode4Bits<upper>(current.destination, value));
                }
            template<bool upper>
                void encodeDestinationPredicate() noexcept {
                    encodeDestinationPredicate<upper>(temporaryByte);
                }
            template<bool upper>
                void encodeSource0Predicate(byte value) noexcept {
                    setSource0(iris::encode4Bits<upper>(current.source0, value));
                }
            template<bool upper>
                void encodeSource0Predicate() noexcept {
                    encodeSource0Predicate<upper>(temporaryByte);
                }
            void markHasLexeme() noexcept { current.hasLexeme = true; }
            void markNotLexeme() noexcept { current.hasLexeme = false; }
            void setLexeme(const std::string& lexeme) noexcept {
                markHasLexeme();
                current.currentLexeme = lexeme;
            }
            void markIsInstruction() noexcept { current.instruction = true; }
            void markIsNotInstruction() noexcept { current.instruction = false; }
            bool hasLexeme() const noexcept { return current.hasLexeme; }
            void setDataValue(word value) noexcept { current.dataValue = value; }
            void stashTemporaryWordIntoDataValue() noexcept { setDataValue(getTemporaryWord()); }
            void markHasFullImmediate() noexcept { current.fullImmediate = true; }
            void markNotFullImmediate() noexcept { current.fullImmediate = false; }
            std::string getCurrentLexeme() const noexcept { return current.currentLexeme; }
        private:
            using AddressSpaceTracker = syn::AddressTracker<word>;
            AddressSpaceTracker data;
            AddressSpaceTracker code;
            bool inData;
            word temporaryWord;
            byte temporaryByte;
            AssemblerData current;
    };
	struct HalfImmediateContainer;
	struct AssemblerInstruction;
	struct AssemblerDirective;
    struct ImmediateContainer : syn::NumberContainer<word> {
        using syn::NumberContainer<word>::NumberContainer;
        template<typename Input>
            void success(const Input& in, AssemblerState& parent) {
                parent.markNotLexeme();
                parent.setTemporaryWord(getValue());
            }
		template<typename Input>
			void success(const Input& in, HalfImmediateContainer& parent);
		template<typename Input>
			void success(const Input& in, AssemblerInstruction& parent);
		template<typename Input>
			void success(const Input& in, AssemblerDirective& parent);
    };
	enum class RegisterPositionType {
		DestinationGPR,
		Source0GPR,
		Source1GPR,
		PredicateDestination,
		PredicateInverseDestination,
		PredicateSource0,
		PredicateSource1,
		Count,
	};
	struct AssemblerInstruction : public AssemblerData {
		template<typename Input>
		AssemblerInstruction(const Input& in, AssemblerState& parent) {
			if (!parent.inCodeSection()) {
				throw syn::Problem("Must be in a code section to add an instruction!");
			}
			instruction = true;
			address = parent.getCurrentAddress();
		}

		template<typename Input>
			void success(const Input& in, AssemblerState& parent) {
				parent.incrementCurrentAddress();
				parent.addToFinishedData(*this);
			}
		void setField(RegisterPositionType type, byte value);
	};
	enum class AssemblerDirectiveAction {
		ChangeCurrentAddress,
		ChangeSection,
		DefineLabel,
		StoreWord,
		Count,
	};
	struct AssemblerDirective : public AssemblerData {
		template<typename I>
		AssemblerDirective(const I& in, AssemblerState& parent) {
			instruction = false;
			address = parent.getCurrentAddress();
		}
		template<typename Input>
		void success(const Input& in, AssemblerState& parent) {
			// TODO: insert code
			if (shouldChangeSectionToCode()) {
				parent.nowInCodeSection();
			} else if (shouldChangeSectionToData()) {
				parent.nowInDataSection();
			} else if (shouldChangeCurrentAddress()) {
				parent.setCurrentAddress(address);
			} else if (shouldDefineLabel()) {
				parent.registerLabel(currentLexeme);
			} else if (shouldStoreWord()) {
				if (parent.inDataSection()) {
					parent.addToFinishedData(*this);
					parent.incrementCurrentAddress();
				} else {
					throw syn::Problem("can't use a declare in a non data section!");
				}
			} else {
				throw syn::Problem("Undefined directive action!");
			}
		}
		bool shouldChangeSectionToCode() const noexcept { return (action == AssemblerDirectiveAction::ChangeSection) && (section == SectionType::Code); }
		bool shouldChangeSectionToData() const noexcept { return (action == AssemblerDirectiveAction::ChangeSection) && (section == SectionType::Data); }
		bool shouldChangeCurrentAddress() const noexcept { return (action == AssemblerDirectiveAction::ChangeCurrentAddress); }
		bool shouldDefineLabel() const noexcept { return (action == AssemblerDirectiveAction::DefineLabel); }
		bool shouldStoreWord() const noexcept { return (action == AssemblerDirectiveAction::StoreWord); }

		AssemblerDirectiveAction action = syn::defaultErrorState<AssemblerDirectiveAction>;
		SectionType section = syn::defaultErrorState<SectionType>;
	};
	struct LexemeOrNumberContainer : syn::NumberOrStringContainer<word> {
		public:
			using Parent = syn::NumberOrStringContainer<word>;
		public:
			template<typename Input>
			LexemeOrNumberContainer(const Input& in, AssemblerDirective& parent) { }

			template<typename Input>
			void success(const Input& in, AssemblerDirective& parent) {
				
			}
	};
    struct RegisterIndexContainer : syn::NumberContainer<byte> {
        using syn::NumberContainer<byte>::NumberContainer;
        template<typename Input>
            void success(const Input& in, AssemblerInstruction& parent) {
				parent.setField(_index, getValue());
            }
        template<typename Input>
            void success(const Input& in, AssemblerState& parent) {
				using Type = RegisterPositionType;
                switch(_index) {
                    case Type::DestinationGPR:
                        parent.setDestination(getValue());
                        break;
                    case Type::Source0GPR:
                        parent.setSource0(getValue());
                        break;
                    case Type::Source1GPR:
                        parent.setSource1(getValue());
                        break;
                    case Type::PredicateDestination:
                        parent.encodeDestinationPredicate<false>(getValue());
                        break;
                    case Type::PredicateInverseDestination:
                        parent.encodeDestinationPredicate<true>(getValue());
                        break;
                    case Type::PredicateSource0:
                        parent.encodeSource0Predicate<false>(getValue());
                        break;
                    case Type::PredicateSource1:
                        parent.encodeSource0Predicate<true>(getValue());
                        break;
                    default:
                        syn::reportError("Illegal index provided!");
                }
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
#define DefApply DefApplyGeneric(AssemblerState)
#define DefApplyEmpty DefApply { }
    using Separator = syn::AsmSeparator;
    template<typename First, typename Second, typename Sep = Separator>
        struct SeparatedBinaryThing : syn::TwoPartComponent<First, Second, Sep> { };
    template<typename First, typename Second, typename Third, typename Sep = Separator>
        struct SeparatedTrinaryThing : syn::ThreePartComponent<First, Second, Third, Sep, Sep> { };
    using SingleLineComment = syn::SingleLineComment<';'>;
    using GeneralPurposeRegister = syn::GPR;
    using PredicateRegister = syn::PredicateRegister;
    template<typename Input, word count>
        void setRegisterValue(const Input& in, RegisterIndexContainer & state) {
            state.setValue(syn::getRegister<word, count>(in.string(), syn::reportError));
        }
    DefAction(GeneralPurposeRegister) {
        DefApplyGeneric(RegisterIndexContainer) {
            setRegisterValue<Input, ArchitectureConstants::RegisterCount>(in, state);
        }
        DefApplyEmpty
		DefApplyGeneric(AssemblerInstruction) {

		}
    };
    DefAction(PredicateRegister) {
        DefApplyGeneric(RegisterIndexContainer) {
            setRegisterValue<Input, ArchitectureConstants::ConditionRegisterCount>(in, state);
        }
        DefApplyEmpty
		DefApplyGeneric(AssemblerInstruction) {

		}
    };
    struct IndirectGPR : syn::SingleEntrySequence<GeneralPurposeRegister> { };
#define DefRegisterIndexContainerAction(title, theType) \
    DefAction( title ) { \
	    static constexpr auto targetType = RegisterPositionType :: theType ; \
        DefApply { } \
		DefApplyGeneric(AssemblerInstruction) { }  \
        DefApplyGeneric(RegisterIndexContainer) { \
            state._index = targetType; \
        } \
    }
    struct DestinationGPR : IndirectGPR { };
    DefRegisterIndexContainerAction(DestinationGPR, DestinationGPR);
    struct Source0GPR : IndirectGPR { };
    DefRegisterIndexContainerAction(Source0GPR, Source0GPR);
    struct Source1GPR : IndirectGPR { };
    DefRegisterIndexContainerAction(Source1GPR, Source1GPR);
    template<typename T>
        using StatefulRegister = pegtl::state<RegisterIndexContainer, T>;
    using StatefulDestinationGPR = StatefulRegister<DestinationGPR>;
    using SourceRegisters = syn::SourceRegisters<StatefulRegister<Source0GPR>, StatefulRegister<Source1GPR>>;
    struct OneGPR : syn::OneRegister<StatefulDestinationGPR> { };
    struct TwoGPR : syn::TwoRegister<StatefulDestinationGPR, StatefulRegister<Source0GPR>> { };
    struct ThreeGPR : syn::TwoRegister<StatefulDestinationGPR, SourceRegisters> { };
    struct IndirectPredicateRegister : syn::SingleEntrySequence<PredicateRegister> { };
    struct DestinationPredicateRegister : IndirectPredicateRegister { };
    DefRegisterIndexContainerAction(DestinationPredicateRegister, PredicateDestination);
    struct DestinationPredicateInverseRegister : IndirectPredicateRegister { };
    DefRegisterIndexContainerAction(DestinationPredicateInverseRegister, PredicateInverseDestination);
    struct DestinationPredicates : syn::TwoRegister<StatefulRegister<DestinationPredicateRegister>, StatefulRegister<DestinationPredicateInverseRegister>> { };

    struct Source0Predicate : IndirectPredicateRegister { };
    DefRegisterIndexContainerAction(Source0Predicate, PredicateSource0);
    struct Source1Predicate : IndirectPredicateRegister { };
    DefRegisterIndexContainerAction(Source1Predicate, PredicateSource1);

    template<syn::KnownNumberTypes v, typename Input>
        static void populateContainer(const Input& in, ImmediateContainer& parent) {
            syn::populateContainer<word, v>(in.string(), parent);
        }
    DefAction(syn::HexadecimalNumber) {
        template<typename Input>
            static void apply(const Input& in, ImmediateContainer& parent) {
                populateContainer<syn::KnownNumberTypes::Hexadecimal, Input>(in, parent);
            }
    };
    DefAction(syn::BinaryNumber) {
        template<typename Input>
            static void apply(const Input& in, ImmediateContainer& parent) {
                populateContainer<syn::KnownNumberTypes::Binary, Input>(in, parent);
            }
    };
    DefAction(syn::Base10Number) {
        template<typename Input>
            static void apply(const Input& in, ImmediateContainer& parent) {
                populateContainer<syn::KnownNumberTypes::Decimal, Input>(in, parent);
            }
    };
	template<typename State = ImmediateContainer>
    struct Number : syn::StatefulNumberAll<State> { };
    using Lexeme = syn::Lexeme;
	
    DefAction(Lexeme) {
        DefApply {
            state.setLexeme(in.string());
        }
		DefApplyGeneric(AssemblerInstruction) { 
			state.hasLexeme = true;
			state.currentLexeme = in.string();
		}
		DefApplyGeneric(syn::StringContainer) {
			state.setValue(in.string());
		}
		DefApplyGeneric(syn::NumberOrStringContainer<word>) {
			state.setStringValue(in.string());
		}
		DefApplyGeneric(AssemblerDirective) {
			state.hasLexeme = true;
			state.currentLexeme = in.string();
		}
    };
    struct LexemeOrNumber : syn::LexemeOr<Number<ImmediateContainer>> { };
    template<SectionType section>
        struct ModifySection {
            template<typename Input>
                ModifySection(const Input& in, AssemblerState& parent) { }
			template<typename Input>
				ModifySection(const Input& in, AssemblerDirective& parent) { 
					parent.action = AssemblerDirectiveAction::ChangeSection;
					parent.section = section;
				}

            template<typename Input>
                void success(const Input& in, AssemblerState& parent) {
                    parent.changeSection<section>();
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
    DefAction(OrgDirective) { 
		DefApply { 
			state.setCurrentAddress(state.getTemporaryWord()); 
		}
		DefApplyGeneric(AssemblerDirective) { }
	};

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

    template<typename T>
        struct LexemeOrNumberDirective : syn::OneArgumentDirective<T, LexemeOrNumber> { };
    struct DeclareDirective : LexemeOrNumberDirective<syn::SymbolWordDirective> { };
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
		DefApplyGeneric(AssemblerDirective) {
		}
    };
    struct Directive : pegtl::state<AssemblerDirective, pegtl::sor<OrgDirective, LabelDirective, CodeDirective, DataDirective, DeclareDirective>> { };
    struct Immediate : pegtl::sor<LexemeOrNumber> { };
    DefAction(Immediate) {
        DefApply {
            state.markHasFullImmediate();
            if (!state.hasLexeme()) {
                state.setImmediate(state.getTemporaryWord());
            }
        }
		DefApplyGeneric(AssemblerInstruction) { }
    };
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
	
    struct HalfImmediate : pegtl::sor<Number<HalfImmediateContainer>> { };

    template<typename Operation, typename Operands>
        using GenericInstruction = syn::Instruction<Operation, Operands>;

    template<typename Operation>
        using OneGPRInstruction = GenericInstruction<Operation, OneGPR>;
    template<typename Operation>
        using ThreeGPRInstruction = GenericInstruction<Operation, ThreeGPR>;
    template<typename Operation>
        using TwoGPRInstruction = GenericInstruction<Operation, TwoGPR>;
	ArithmeticOp stringToArithmeticOp(const std::string& title) noexcept;
	struct ArithmeticSubTypeSelector {
		DefApplyGeneric(AssemblerInstruction) {
			state.operation = (byte)stringToArithmeticOp(in.string());
		}
	};
	
    struct OperationArithmeticThreeGPR : pegtl::sor<SymbolAdd, SymbolSub, SymbolMul, SymbolDiv, SymbolRem, SymbolShiftLeft, SymbolShiftRight, SymbolAnd, SymbolOr, SymbolXor, SymbolMin, SymbolMax> { };
	DefAction(OperationArithmeticThreeGPR) : public ArithmeticSubTypeSelector { };
    struct ArithmeticThreeGPRInstruction : ThreeGPRInstruction<OperationArithmeticThreeGPR> { };
    struct OperationArithmeticTwoGPR : pegtl::sor<SymbolNot> { };
	DefAction(OperationArithmeticTwoGPR) : public ArithmeticSubTypeSelector { };
    struct ArithmeticTwoGPRInstruction : TwoGPRInstruction<OperationArithmeticTwoGPR> { };

    struct OperationArithmeticTwoGPRHalfImmediate : pegtl::sor<
                                                    SymbolAddImmediate,
                                                    SymbolSubImmediate,
                                                    SymbolMulImmediate,
                                                    SymbolDivImmediate,
                                                    SymbolRemImmediate,
                                                    SymbolShiftLeftImmediate,
                                                    SymbolShiftRightImmediate> { };
	DefAction(OperationArithmeticTwoGPRHalfImmediate) : public ArithmeticSubTypeSelector { };
    struct ArithmeticTwoGPRHalfImmediateInstruction : SeparatedTrinaryThing<OperationArithmeticTwoGPRHalfImmediate, TwoGPR, HalfImmediate> { };
    struct ArithmeticInstruction : pegtl::sor<
                                   ArithmeticTwoGPRHalfImmediateInstruction,
                                   ArithmeticTwoGPRInstruction,
                                   ArithmeticThreeGPRInstruction> { };
	DefAction(ArithmeticInstruction) : public SetInstructionGroup<InstructionGroup::Arithmetic> { };

	MoveOp stringToMoveOp(const std::string& title) noexcept;
	struct MoveOpSubTypeSelector {
		DefApplyGeneric(AssemblerInstruction) {
			state.operation = (byte)stringToMoveOp(in.string());
		}
	};
    struct OperationMoveOneGPR : pegtl::sor<SymbolMoveToIP, SymbolMoveFromIP, SymbolMoveToLR, SymbolMoveFromLR, SymbolRestoreAllRegisters, SymbolSaveAllRegisters> { };
	DefAction(OperationMoveOneGPR) : public MoveOpSubTypeSelector { };
    struct MoveOneGPRInstruction : OneGPRInstruction<OperationMoveOneGPR> { };
    struct OperationMoveTwoGPR : pegtl::sor<SymbolMove, SymbolSwap, SymbolLoadIO, SymbolStoreIO, SymbolLoad, SymbolStore, SymbolPush, SymbolPop> { };
	DefAction(OperationMoveTwoGPR) : public MoveOpSubTypeSelector { };
    struct MoveTwoGPRInstruction : TwoGPRInstruction<OperationMoveTwoGPR> { };
    struct OperationMoveTwoGPRHalfImmediate : pegtl::sor<SymbolLoadWithOffset, SymbolStoreWithOffset, SymbolLoadIOWithOffset, SymbolStoreIOWithOffset> { };
	DefAction(OperationMoveTwoGPRHalfImmediate) : public MoveOpSubTypeSelector { };

    struct MoveTwoGPRHalfImmediateInstruction : SeparatedTrinaryThing<OperationMoveTwoGPRHalfImmediate, TwoGPR, HalfImmediate> { };

    struct OperationMoveThreeGPR : pegtl::sor<SymbolLoadCode, SymbolStoreCode> { };
	DefAction(OperationMoveThreeGPR) : public MoveOpSubTypeSelector { };
    struct MoveThreeGPRInstruction : ThreeGPRInstruction<OperationMoveThreeGPR> { };

    struct OperationMoveGPRImmediate : pegtl::sor<SymbolStoreImmediate, SymbolLoadImmediate, SymbolSet, SymbolPushImmediate> { };
	DefAction(OperationMoveGPRImmediate) : public MoveOpSubTypeSelector { };

    struct MoveGPRImmediateInstruction : SeparatedTrinaryThing<OperationMoveGPRImmediate, StatefulDestinationGPR, Immediate> { };

    struct MoveInstruction : pegtl::sor<MoveGPRImmediateInstruction, MoveThreeGPRInstruction, MoveTwoGPRHalfImmediateInstruction, MoveTwoGPRInstruction, MoveOneGPRInstruction> { };
	DefAction(MoveInstruction) : public SetInstructionGroup<InstructionGroup::Move> { };
    // branch
	JumpOp stringToJumpOp(const std::string& title) noexcept;
	struct BranchOpSubTypeSelector {
		DefApplyGeneric(AssemblerInstruction) {
			state.operation = (byte)stringToJumpOp(in.string());
		}
	};
    template<typename Op, typename S>
        struct BranchUnconditional : SeparatedBinaryThing<Op, S> { };
    struct OperationBranchOneGPR : pegtl::sor<SymbolBranchUnconditionalLink, SymbolBranchUnconditional> { };
	DefAction(OperationBranchOneGPR) : public BranchOpSubTypeSelector { };
    struct BranchOneGPRInstruction : BranchUnconditional<OperationBranchOneGPR, StatefulDestinationGPR> { };
    struct OperationBranchImmediate : pegtl::sor<SymbolBranchUnconditionalImmediateLink, SymbolBranchUnconditionalImmediate> { };
	DefAction(OperationBranchImmediate) : public BranchOpSubTypeSelector { };
    struct BranchImmediateInstruction : BranchUnconditional<OperationBranchImmediate, Immediate> { };

    struct GroupBranchUnconditional : pegtl::sor<BranchOneGPRInstruction, BranchImmediateInstruction> { };
    template<typename Op, typename S>
        struct BranchConditional : SeparatedTrinaryThing<Op, DestinationPredicateRegister, S> { };
    struct OperationBranchConditionalGPR : pegtl::sor<
                                           SymbolBranchConditionalLink,
                                           SymbolBranchConditional
                                           > { };
	DefAction(OperationBranchConditionalGPR) : public BranchOpSubTypeSelector { };
    struct BranchConditionalGPRInstruction : BranchConditional<OperationBranchConditionalGPR, Source0GPR> { };
    struct OperationBranchConditionalImmediate : pegtl::sor<
                                                 SymbolBranchConditionalImmediateLink,
                                                 SymbolBranchConditionalImmediate
                                                 > { };
	DefAction(OperationBranchConditionalImmediate) : public BranchOpSubTypeSelector { };
    struct BranchConditionalImmediateInstruction : BranchConditional<OperationBranchConditionalImmediate, Immediate> { };
    struct OperationBranchIfStatement : pegtl::sor<
                                        SymbolIfThenElseLink,
                                        SymbolIfThenElse
                                        > { };
	DefAction(OperationBranchIfStatement) : public BranchOpSubTypeSelector { };
    struct BranchIfInstruction : BranchConditional<OperationBranchIfStatement, SourceRegisters> { };
    struct OperationBranchConditionalNoArgs : pegtl::sor<
                                              SymbolBranchConditionalLRAndLink,
                                              SymbolBranchConditionalLR
                                              > { };
	DefAction(OperationBranchConditionalNoArgs) : public BranchOpSubTypeSelector { };
    struct BranchConditionalNoArgsInstruction : SeparatedBinaryThing<OperationBranchConditionalNoArgs, DestinationPredicateRegister> { };
    struct BranchNoArgsInstruction : pegtl::sor<SymbolBranchUnconditionalLRAndLink, SymbolBranchUnconditionalLR, SymbolBranchReturnFromError> { };
	DefAction(BranchNoArgsInstruction) : public BranchOpSubTypeSelector { };

    struct BranchInstruction : pegtl::sor<GroupBranchUnconditional, BranchConditionalGPRInstruction, BranchConditionalImmediateInstruction, BranchIfInstruction, BranchConditionalNoArgsInstruction, BranchNoArgsInstruction> { };
	DefAction(BranchInstruction) : public SetInstructionGroup<InstructionGroup::Jump> { };
    //DefGroupSet(BranchInstruction, Jump);

    template<typename T>
        struct ThenField : pegtl::seq<Separator, T> { };
    struct ThenDestinationPredicates : ThenField<DestinationPredicates> { };
#define CURRENT_TYPE CompareOp
    // compare operations
	CompareOp stringToCompareOp(const std::string& title) noexcept;
	struct CompareOpSubTypeSelector {
		DefApplyGeneric(AssemblerInstruction) {
			state.operation = (byte)stringToCompareOp(in.string());
		}
	};
    struct CompareRegisterOperation : pegtl::sor<SymbolEq, SymbolNeq, SymbolLessThan, SymbolGreaterThan, SymbolLessThanOrEqualTo, SymbolGreaterThanOrEqualTo> { };
	DefAction(CompareRegisterOperation) : public CompareOpSubTypeSelector { };
    struct CompareImmediateOperation : pegtl::sor<SymbolEqImmediate, SymbolNeqImmediate, SymbolLessThanImmediate, SymbolGreaterThanImmediate, SymbolLessThanOrEqualToImmediate, SymbolGreaterThanOrEqualToImmediate> { };
	DefAction(CompareImmediateOperation) : public CompareOpSubTypeSelector { };
    struct CompareRegisterInstruction : pegtl::seq<CompareRegisterOperation, ThenDestinationPredicates, ThenField<SourceRegisters>> { };
    struct CompareImmediateInstruction : pegtl::seq<CompareImmediateOperation, ThenDestinationPredicates, ThenField<Source0GPR>, ThenField<HalfImmediate>> { };
    struct CompareInstruction : pegtl::sor<
                                CompareImmediateInstruction,
                                CompareRegisterInstruction
                                > { };
    //DefGroupSet(CompareInstruction, Compare);
	DefAction(CompareInstruction) : public SetInstructionGroup<InstructionGroup::Compare> { };

    // conditional register actions
	ConditionRegisterOp stringToConditionRegisterOp(const std::string& title) noexcept;
	struct CompareRegisterOpTranslationLogic {
		DefApplyGeneric(AssemblerInstruction) {
			state.operation = (byte)stringToConditionRegisterOp(in.string());
		}
	};
    struct ThenSource0Predicate : ThenField<StatefulRegister<Source0Predicate>> { };
    struct OperationPredicateTwoArgs : pegtl::sor<SymbolCRSwap, SymbolCRMove> { };
	DefAction(OperationPredicateTwoArgs) : public CompareRegisterOpTranslationLogic { };
    struct OperationPredicateOneGPR : pegtl::sor<SymbolSaveCRs, SymbolRestoreCRs> { };
	DefAction(OperationPredicateOneGPR) : public CompareRegisterOpTranslationLogic { };
    struct OperationPredicateFourArgs : pegtl::sor<SymbolCRXor, SymbolCRAnd, SymbolCROr, SymbolCRNand, SymbolCRNor> { };
	DefAction(OperationPredicateFourArgs) : public CompareRegisterOpTranslationLogic { };
    struct PredicateInstructionOneGPR : pegtl::seq<OperationPredicateOneGPR, ThenField<StatefulDestinationGPR>> { };
    struct PredicateInstructionTwoArgs : pegtl::seq<OperationPredicateTwoArgs, ThenDestinationPredicates> { };
    struct PredicateInstructionThreeArgs : pegtl::seq<SymbolCRNot, ThenDestinationPredicates, ThenSource0Predicate> { };
    struct PredicateInstructionFourArgs : pegtl::seq<OperationPredicateFourArgs, ThenDestinationPredicates, ThenSource0Predicate, ThenField<StatefulRegister<Source1Predicate>>> { };
    struct PredicateInstruction : pegtl::sor<PredicateInstructionOneGPR, PredicateInstructionTwoArgs, PredicateInstructionThreeArgs, PredicateInstructionFourArgs> { };
	DefAction(PredicateInstruction) : public SetInstructionGroup<InstructionGroup::ConditionalRegister> { };

    struct Instruction : pegtl::state<AssemblerInstruction, pegtl::sor<ArithmeticInstruction, MoveInstruction, BranchInstruction, CompareInstruction, PredicateInstruction>> { };
    struct Statement : pegtl::sor<Instruction, Directive> { };
    struct Anything : pegtl::sor<Separator, SingleLineComment,Statement> { };
    struct Main : syn::MainFileParser<Anything> { };
} // end namespace iris
#endif // end IRIS_CORE_ASSEMBLER_H__
