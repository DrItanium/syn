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

namespace iris {
	template<typename R> struct Action : syn::Action<R> { };
	struct AssemblerData {
		AssemblerData() : instruction(false), address(0), dataValue(0), group(0), operation(0), destination(0), source0(0), source1(0), hasLexeme(false), fullImmediate(false) { }
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

		void reset() noexcept;
		void setImmediate(word value) noexcept;
		bool shouldResolveLabel() noexcept { return fullImmediate && hasLexeme; }
        dword encode();
	};

	class AssemblerState {
		public:
			using LabelMap = std::map<std::string, word>;
			using LabelIterator = LabelMap::iterator;
			using ConstLabelIterator = LabelMap::const_iterator;
		public:
			AssemblerState() : currentDataIndex(0), currentCodeIndex(0), inData(false), temporaryWord(0), temporaryByte(0) { }
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
            template<bool toCodeSection>
            void changeSection() noexcept {
                if (toCodeSection) {
                    nowInCodeSection();
                } else {
                    nowInDataSection();
                }
            }
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
			LabelIterator findLabel(const std::string& k) { return labelMap.find(k); }
			ConstLabelIterator findLabel(const std::string& k) const { return labelMap.find(k); }
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
        enum class Type {
            DestinationGPR,
            Source0GPR,
            Source1GPR,
            PredicateDestination,
            PredicateInverseDestination,
            PredicateSource0,
            PredicateSource1,
        };
		template<typename Input>
			void success(const Input& in, AssemblerData& parent) {
				switch(_index) {
					case Type::DestinationGPR:
						parent.destination = getValue();
						break;
					case Type::Source0GPR:
						parent.source0 = getValue();
						break;
                    case Type::Source1GPR:
						parent.source1 = getValue();
						break;
					default:
						syn::reportError("Illegal index provided!");
				}
			}
		template<typename Input>
			void success(const Input& in, AssemblerState& parent) {
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
		Type _index;
	};
#define DefAction(rule) template<> struct Action < rule >
#define DefApplyGeneric(type) template<typename Input> static void apply(const Input& in, type & state)
#define DefApply DefApplyGeneric(AssemblerState)
	using Separator = syn::AsmSeparator;
	using SingleLineComment = syn::SingleLineComment<';'>;
    using GeneralPurposeRegister = syn::GPR;
    using PredicateRegister = syn::PredicateRegister;
	DefAction(GeneralPurposeRegister) {
		DefApplyGeneric(RegisterIndexContainer) {
            state.setValue(syn::getRegister<word, ArchitectureConstants::RegisterCount>(in.string(), syn::reportError));
		}
        DefApply { }
	};
	DefAction(PredicateRegister) {
		DefApplyGeneric(RegisterIndexContainer) {
            state.setValue(syn::getRegister<word, ArchitectureConstants::ConditionRegisterCount>(in.string(), syn::reportError));
		}
		DefApply { }
	};
	using IndirectGPR = syn::Indirection<GeneralPurposeRegister>;
#define DefIndirectGPR(title) \
	struct title : IndirectGPR { }
	DefIndirectGPR(DestinationGPR);
	DefAction(DestinationGPR) {
		DefApplyGeneric(RegisterIndexContainer) {
            state._index = RegisterIndexContainer::Type::DestinationGPR;
		}
        DefApply { }
	};
	DefIndirectGPR(Source0GPR);
	DefAction(Source0GPR) {
		DefApplyGeneric(RegisterIndexContainer) {
            state._index = RegisterIndexContainer::Type::Source0GPR;
		}
        DefApply { }
	};
    DefIndirectGPR(Source1GPR);
	DefAction(Source1GPR) {
		DefApplyGeneric(RegisterIndexContainer) {
            state._index = RegisterIndexContainer::Type::Source1GPR;
		}
        DefApply { }
	};
    template<typename T>
    using StatefulRegister = pegtl::state<RegisterIndexContainer, T>;
    using StatefulDestinationGPR = StatefulRegister<DestinationGPR>;
#undef DefIndirectGPR
	using SourceRegisters = syn::SourceRegisters<StatefulRegister<Source0GPR>, StatefulRegister<Source1GPR>>;
	struct OneGPR : syn::OneRegister<StatefulDestinationGPR> { };
    struct TwoGPR : syn::TwoRegister<StatefulDestinationGPR, StatefulRegister<Source0GPR>> { };
	struct ThreeGPR : syn::TwoRegister<StatefulDestinationGPR, SourceRegisters> { };
    using IndirectPredicateRegister = syn::Indirection<PredicateRegister>;
    struct DestinationPredicateRegister : IndirectPredicateRegister { };
	DefAction(DestinationPredicateRegister) {
        DefApplyGeneric(RegisterIndexContainer) {
            state._index = RegisterIndexContainer::Type::PredicateDestination;
        }
		DefApply { }
	};
    struct DestinationPredicateInverseRegister : IndirectPredicateRegister { };
	DefAction(DestinationPredicateInverseRegister) {
        DefApplyGeneric(RegisterIndexContainer) {
            state._index = RegisterIndexContainer::Type::PredicateInverseDestination;
        }
		DefApply { }
	};
	struct DestinationPredicates : syn::TwoRegister<StatefulRegister<DestinationPredicateRegister>, StatefulRegister<DestinationPredicateInverseRegister>> { };

	struct Source0Predicate : IndirectPredicateRegister { };
	DefAction(Source0Predicate) {
        DefApplyGeneric(RegisterIndexContainer) {
            state._index = RegisterIndexContainer::Type::PredicateSource0;
        }
		DefApply { }
	};
	struct Source1Predicate : IndirectPredicateRegister { };
	DefAction(Source1Predicate) {
        DefApplyGeneric(RegisterIndexContainer) {
            state._index = RegisterIndexContainer::Type::PredicateSource1;
        }
		DefApply { }
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
    struct Number : pegtl::state<ImmediateContainer, pegtl::sor<HexadecimalNumber, DecimalNumber, BinaryNumber>> { };
	using Lexeme = syn::Lexeme;
	DefAction(Lexeme) {
		DefApply {
			state.setLexeme(in.string());
		}
	};
	struct LexemeOrNumber : syn::LexemeOr<Number> { };
    template<bool toCode>
    struct ModifySection {
        template<typename Input>
        ModifySection(const Input& in, AssemblerState& parent) { }

        template<typename Input>
        void success(const Input& in, AssemblerState& parent) {
            parent.changeSection<toCode>();
        }

    };
    // directives
#define ConstructOperationSetter(title, type) \
    DefAction(Symbol ## title ) { \
        DefApply { \
            state.setOperation< CURRENT_TYPE >(CURRENT_TYPE :: type ) ; \
        } \
    }

#define DefOperation(title, str, type) \
	DefSymbol(title, str); \
    ConstructOperationSetter(title, type)

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
	};
    struct Directive : pegtl::sor<OrgDirective, LabelDirective, CodeDirective, DataDirective, DeclareDirective> { };
    struct Immediate : pegtl::sor<LexemeOrNumber> { };
	DefAction(Immediate) {
		DefApply {
			state.markHasFullImmediate();
			if (!state.hasLexeme()) {
				state.setImmediate(state.getTemporaryWord());
			}
		}
	};
    struct HalfImmediate : pegtl::sor<Number> { };
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
#define DefActionUsingPredefinedSymbol(title) \
    using Symbol ## title = syn:: Symbol ## title ## Keyword ; \
    ConstructOperationSetter(title, title)

    DefActionUsingPredefinedSymbol(Add);
    DefActionUsingPredefinedSymbol(Sub);
    DefActionUsingPredefinedSymbol(Mul);
    DefActionUsingPredefinedSymbol(Div);
    DefActionUsingPredefinedSymbol(Rem);
    DefOperationSameTitle(ShiftLeft, shift.left);
    DefOperationSameTitle(ShiftRight, shift.right);
    DefOperation(And, and, BinaryAnd);
    DefOperation(Or, or, BinaryOr);
    DefOperation(Xor, xor, BinaryXor);
    DefOperation(Nand, nand, BinaryNand);
    DefOperation(Nor, nor, BinaryNor);
    DefOperationSameTitle(Min, min);
    DefOperationSameTitle(Max, max);
    struct OperationArithmeticThreeGPR : pegtl::sor<SymbolAdd, SymbolSub, SymbolMul, SymbolDiv, SymbolRem, SymbolShiftLeft, SymbolShiftRight, SymbolAnd, SymbolOr, SymbolXor, SymbolMin, SymbolMax> { };
	struct ArithmeticThreeGPRInstruction : ThreeGPRInstruction<OperationArithmeticThreeGPR> { };
    DefOperation(Not, not, BinaryNot);
    struct OperationArithmeticTwoGPR : pegtl::sor<SymbolNot> { };
    struct ArithmeticTwoGPRInstruction : TwoGPRInstruction<OperationArithmeticTwoGPR> { };

    DefOperationSameTitle(AddImmediate, add.imm);
    DefOperationSameTitle(SubImmediate, sub.imm);
    DefOperationSameTitle(MulImmediate, mul.imm);
    DefOperationSameTitle(DivImmediate, div.imm);
    DefOperationSameTitle(RemImmediate, rem.imm);
    DefOperationSameTitle(ShiftLeftImmediate, shift.left.imm);
    DefOperationSameTitle(ShiftRightImmediate, shift.right.imm);
    struct OperationArithmeticTwoGPRHalfImmediate : pegtl::sor< SymbolAddImmediate, SymbolSubImmediate, SymbolMulImmediate, SymbolDivImmediate, SymbolRemImmediate, SymbolShiftLeftImmediate, SymbolShiftRightImmediate> { };
    struct ArithmeticTwoGPRHalfImmediateInstruction : pegtl::seq<OperationArithmeticTwoGPRHalfImmediate, Separator, TwoGPR, Separator, HalfImmediate> { };
    struct ArithmeticInstruction : pegtl::sor<ArithmeticTwoGPRHalfImmediateInstruction, ArithmeticTwoGPRInstruction, ArithmeticThreeGPRInstruction> { };

#define DefGroupSet(rule, group) DefAction( rule ) { DefApply { state.setGroup(InstructionGroup:: group ); } }
	DefGroupSet(ArithmeticInstruction, Arithmetic);
#undef CURRENT_TYPE
#define CURRENT_TYPE MoveOp

    DefOperationSameTitle(MoveToIP, move.to.ip);
    DefOperationSameTitle(MoveFromIP, move.from.ip);
    DefOperationSameTitle(MoveToLR, move.to.lr);
    DefOperationSameTitle(MoveFromLR, move.from.lr);
    DefOperationSameTitle(RestoreAllRegisters, restore.regs);
    DefOperationSameTitle(SaveAllRegisters, save.regs);
    struct OperationMoveOneGPR : pegtl::sor<SymbolMoveToIP, SymbolMoveFromIP, SymbolMoveToLR, SymbolMoveFromLR, SymbolRestoreAllRegisters, SymbolSaveAllRegisters> { };
    struct MoveOneGPRInstruction : OneGPRInstruction<OperationMoveOneGPR> { };
    DefOperationSameTitle(Move, move);
    DefOperationSameTitle(Swap, swap);
    DefOperationSameTitle(Load, load);
    DefOperationSameTitle(Store, store);
    DefOperation(LoadIO, load.io, IORead);
    DefOperation(StoreIO, store.io, IOWrite);
    DefOperationSameTitle(Push, push);
    DefOperationSameTitle(Pop, pop);
    struct OperationMoveTwoGPR : pegtl::sor<SymbolMove, SymbolSwap, SymbolLoadIO, SymbolStoreIO, SymbolLoad, SymbolStore, SymbolPush, SymbolPop> { };
    struct MoveTwoGPRInstruction : TwoGPRInstruction<OperationMoveTwoGPR> { };
    DefOperationSameTitle(LoadWithOffset, load.offset);
    DefOperationSameTitle(StoreWithOffset, store.offset);
    DefOperation(LoadIOWithOffset, load.with.offset, IOReadWithOffset);
    DefOperation(StoreIOWithOffset, store.with.offset, IOWriteWithOffset);
    struct OperationMoveTwoGPRHalfImmediate : pegtl::sor<SymbolLoadWithOffset, SymbolStoreWithOffset, SymbolLoadIOWithOffset, SymbolStoreIOWithOffset> { };
    struct MoveTwoGPRHalfImmediateInstruction : pegtl::seq<OperationMoveTwoGPRHalfImmediate, Separator, TwoGPR, Separator, HalfImmediate> { };

    DefOperationSameTitle(LoadCode, load.code);
    DefOperationSameTitle(StoreCode, store.code);
    struct OperationMoveThreeGPR : pegtl::sor<SymbolLoadCode, SymbolStoreCode> { };
    struct MoveThreeGPRInstruction : ThreeGPRInstruction<OperationMoveThreeGPR> { };

    DefOperationSameTitle(PushImmediate, push.imm);
    DefOperationSameTitle(Set, set);
    DefOperationSameTitle(LoadImmediate, load.imm);
    DefOperationSameTitle(StoreImmediate, store.imm);
    struct OperationMoveGPRImmediate : pegtl::sor<SymbolStoreImmediate, SymbolLoadImmediate, SymbolSet, SymbolPushImmediate> { };

    struct MoveGPRImmediateInstruction : pegtl::seq<OperationMoveGPRImmediate, Separator, StatefulDestinationGPR, Separator, Immediate> { };

    struct MoveInstruction : pegtl::sor<MoveGPRImmediateInstruction, MoveThreeGPRInstruction, MoveTwoGPRHalfImmediateInstruction, MoveTwoGPRInstruction, MoveOneGPRInstruction> { };
	DefGroupSet(MoveInstruction, Move);
#undef CURRENT_TYPE
#define CURRENT_TYPE JumpOp
    // branch
	template<typename Op, typename S>
	struct BranchUnconditional : pegtl::seq<Op, Separator, S> { };
    DefOperationSameTitle(BranchUnconditional, branch);
    DefOperationSameTitle(BranchUnconditionalLink, branch.link);
    struct OperationBranchOneGPR : pegtl::sor<SymbolBranchUnconditionalLink, SymbolBranchUnconditional> { };
    struct BranchOneGPRInstruction : BranchUnconditional<OperationBranchOneGPR, StatefulDestinationGPR> { };
    DefOperationSameTitle(BranchUnconditionalImmediate, branch.imm);
    DefOperationSameTitle(BranchUnconditionalImmediateLink, branch.imm.link);
    struct OperationBranchImmediate : pegtl::sor<SymbolBranchUnconditionalImmediateLink, SymbolBranchUnconditionalImmediate> { };
    struct BranchImmediateInstruction : BranchUnconditional<OperationBranchImmediate, Immediate> { };

	struct GroupBranchUnconditional : pegtl::sor<BranchOneGPRInstruction, BranchImmediateInstruction> { };
    template<typename Op, typename S>
    struct BranchConditional : pegtl::seq<Op, Separator, DestinationPredicateRegister, Separator, S> { };
    DefOperationSameTitle(BranchConditional, branch.cond);
    DefOperationSameTitle(BranchConditionalLink, branch.cond.link);
    struct OperationBranchConditionalGPR : pegtl::sor<
                                           SymbolBranchConditionalLink,
                                           SymbolBranchConditional
                                           > { };
    struct BranchConditionalGPRInstruction : BranchConditional<OperationBranchConditionalGPR, Source0GPR> { };
    DefOperationSameTitle(BranchConditionalImmediate, branch.cond.imm);
    DefOperationSameTitle(BranchConditionalImmediateLink, branch.cond.imm.link);
    struct OperationBranchConditionalImmediate : pegtl::sor<
                                                 SymbolBranchConditionalImmediateLink,
                                                 SymbolBranchConditionalImmediate
                                                 > { };
    struct BranchConditionalImmediateInstruction : BranchConditional<OperationBranchConditionalImmediate, Immediate> { };
    DefOperationSameTitle(IfThenElse, if);
    DefOperationSameTitle(IfThenElseLink, if.link);
    struct OperationBranchIfStatement : pegtl::sor<
                                        SymbolIfThenElse,
                                        SymbolIfThenElseLink
                                        > { };
    struct BranchIfInstruction : BranchConditional<OperationBranchIfStatement, SourceRegisters> { };
    DefOperationSameTitle(BranchConditionalLR, branch.cond.lr);
    DefOperationSameTitle(BranchConditionalLRAndLink, branch.cond.lr.link);
    struct OperationBranchConditionalNoArgs : pegtl::sor<
                                             SymbolBranchConditionalLR,
                                             SymbolBranchConditionalLRAndLink
                                             > { };
    struct BranchConditionalNoArgsInstruction : pegtl::seq<OperationBranchConditionalNoArgs, Separator, DestinationPredicateRegister> { };
    DefOperationSameTitle(BranchUnconditionalLR, branch.lr);
    DefOperationSameTitle(BranchUnconditionalLRAndLink, branch.lr.link);
	DefOperation(BranchReturnFromError, return.from.error, ReturnFromError);
    struct BranchNoArgsInstruction : pegtl::sor<SymbolBranchUnconditionalLR,  SymbolBranchUnconditionalLRAndLink, SymbolBranchReturnFromError> { };

    struct BranchInstruction : pegtl::sor<GroupBranchUnconditional, BranchConditionalGPRInstruction, BranchConditionalImmediateInstruction, BranchIfInstruction, BranchConditionalNoArgsInstruction, BranchNoArgsInstruction> { };
	DefGroupSet(BranchInstruction, Jump);

#undef CURRENT_TYPE
    template<typename T>
    struct ThenField : pegtl::seq<Separator, T> { };
    struct ThenDestinationPredicates : ThenField<DestinationPredicates> { };
#define CURRENT_TYPE CompareOp
    // compare operations
    DefOperationSameTitle(Eq, =);
    DefOperationSameTitle(Neq, !=);
    DefOperationSameTitle(LessThan, <);
    DefOperationSameTitle(GreaterThan, >);
    DefOperationSameTitle(LessThanOrEqualTo, <=);
    DefOperationSameTitle(GreaterThanOrEqualTo, >=);
    DefOperationSameTitle(EqImmediate, =.imm);
    DefOperationSameTitle(NeqImmediate, !=.imm);
    DefOperationSameTitle(LessThanImmediate, <.imm);
    DefOperationSameTitle(GreaterThanImmediate, >.imm);
    DefOperationSameTitle(LessThanOrEqualToImmediate, <=.imm);
    DefOperationSameTitle(GreaterThanOrEqualToImmediate, >=.imm);
    struct CompareRegisterOperation : pegtl::sor< SymbolEq, SymbolNeq, SymbolLessThan, SymbolGreaterThan, SymbolLessThanOrEqualTo, SymbolGreaterThanOrEqualTo> { };
    struct CompareImmediateOperation : pegtl::sor<SymbolEqImmediate, SymbolNeqImmediate, SymbolLessThanImmediate, SymbolGreaterThanImmediate, SymbolLessThanOrEqualToImmediate, SymbolGreaterThanOrEqualToImmediate> { };
    struct CompareRegisterInstruction : pegtl::seq<CompareRegisterOperation, ThenDestinationPredicates, ThenField<SourceRegisters>> { };
    struct CompareImmediateInstruction : pegtl::seq<CompareImmediateOperation, ThenDestinationPredicates, ThenField<Source0GPR>, ThenField<HalfImmediate>> { };
    struct CompareInstruction : pegtl::sor<CompareRegisterInstruction, CompareImmediateInstruction> { };
	DefGroupSet(CompareInstruction, Compare);

#undef CURRENT_TYPE
#define CURRENT_TYPE ConditionRegisterOp

    // conditional register actions
    DefOperationSameTitle(SaveCRs, pred.save);
    DefOperationSameTitle(RestoreCRs, pred.restore);
    DefOperationSameTitle(CRXor, pred.xor);
    DefOperationSameTitle(CRNot, pred.not);
    DefOperationSameTitle(CRAnd, pred.and);
    DefOperationSameTitle(CROr, pred.or);
    DefOperationSameTitle(CRNand, pred.nand);
    DefOperationSameTitle(CRNor, pred.nor);
    DefOperationSameTitle(CRSwap, pred.swap);
    DefOperationSameTitle(CRMove, pred.move);
    struct ThenSource0Predicate : ThenField<StatefulRegister<Source0Predicate>> { };
    struct OperationPredicateTwoArgs : pegtl::sor<SymbolCRSwap, SymbolCRMove> { };
    struct OperationPredicateOneGPR : pegtl::sor<SymbolSaveCRs, SymbolRestoreCRs> { };
    struct OperationPredicateFourArgs : pegtl::sor<SymbolCRXor, SymbolCRAnd, SymbolCROr, SymbolCRNand, SymbolCRNor> { };
    struct PredicateInstructionOneGPR : pegtl::seq<OperationPredicateOneGPR, ThenField<StatefulDestinationGPR>> { };
    struct PredicateInstructionTwoArgs : pegtl::seq<OperationPredicateTwoArgs, ThenDestinationPredicates> { };
    struct PredicateInstructionThreeArgs : pegtl::seq<SymbolCRNot, ThenDestinationPredicates, ThenSource0Predicate> { };
    struct PredicateInstructionFourArgs : pegtl::seq<OperationPredicateFourArgs, ThenDestinationPredicates, ThenSource0Predicate, ThenField<StatefulRegister<Source1Predicate>>> { };
    struct PredicateInstruction : pegtl::sor<PredicateInstructionOneGPR, PredicateInstructionTwoArgs, PredicateInstructionThreeArgs, PredicateInstructionFourArgs> { };
	DefGroupSet(PredicateInstruction, ConditionalRegister);


#undef DefGroupSet
#undef DefOperation
#undef DefOperationSameTitle
#undef CURRENT_TYPE
#undef DefSymbol
    struct Instruction : pegtl::sor<ArithmeticInstruction, MoveInstruction, BranchInstruction, CompareInstruction, PredicateInstruction> { };
	DefAction(Instruction) {
		DefApply {
            if (!state.inCodeSection()) {
                throw syn::Problem("Can't construct instructions in the data section!");
            }
            state.markIsInstruction();
            state.saveToFinished();
            state.incrementCurrentAddress();
		}
	};
    struct Statement : pegtl::sor<Instruction, Directive> { };
	struct Anything : pegtl::sor<Separator, SingleLineComment,Statement> { };
	struct Main : syn::MainFileParser<Anything> { };

} // end namespace iris
#endif // end IRIS_CORE_ASSEMBLER_H__
