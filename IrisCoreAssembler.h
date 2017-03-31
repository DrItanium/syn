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
		DefApply {
			//state.setTemporaryByte(syn::getRegister<word, ArchitectureConstants::ConditionRegisterCount>(in.string(), syn::reportError));
		}
	};
	using IndirectGPR = syn::Indirection<GeneralPurposeRegister>;
#define DefIndirectGPR(title) \
	struct title : IndirectGPR { }
	DefIndirectGPR(DestinationGPR);
	DefAction(DestinationGPR) {
		DefApplyGeneric(RegisterIndexContainer) {
            state._index = RegisterIndexContainer::Type::DestinationGPR;
		}
        DefApply {
            //state.stashTemporaryByteInDestination();
        }
	};
	DefIndirectGPR(Source0GPR);
	DefAction(Source0GPR) {
		DefApplyGeneric(RegisterIndexContainer) {
            state._index = RegisterIndexContainer::Type::Source0GPR;
		}
        DefApply {
            //state.stashTemporaryByteInSource0();
        }
	};
    DefIndirectGPR(Source1GPR);
	DefAction(Source1GPR) {
		DefApplyGeneric(RegisterIndexContainer) {
            state._index = RegisterIndexContainer::Type::Source1GPR;
		}
        DefApply {
            //state.stashTemporaryByteInSource1();
        }
	};
    template<typename T>
    using StatefulRegister = pegtl::state<RegisterIndexContainer, T>;
    using StatefulDestinationGPR = StatefulRegister<DestinationGPR>;
#undef DefIndirectGPR
	using SourceRegisters = syn::SourceRegisters<StatefulRegister<Source0GPR>, StatefulRegister<Source1GPR>>;
	struct OneGPR :  syn::OneRegister<StatefulDestinationGPR> { };
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
            if (toCode) {
                parent.nowInCodeSection();
            } else {
                parent.nowInDataSection();
            }
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
    struct PredicateInstructionThreeArgs : public pegtl::seq<OperationPredicateThreeArgs, Separator, DestinationPredicates, Separator, StatefulRegister<Source0Predicate>> { };
    struct PredicateInstructionFourArgs : public pegtl::seq<OperationPredicateFourArgs, Separator, DestinationPredicates, Separator, StatefulRegister<Source0Predicate>, Separator, StatefulRegister<Source1Predicate>> { };
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

	void resolveLabels(AssemblerState& state, std::ostream& output);
} // end namespace iris
#endif // end IRIS_CORE_ASSEMBLER_H__
