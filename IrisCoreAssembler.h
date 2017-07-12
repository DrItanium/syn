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
        AssemblerData();
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

    class AssemblerState : public syn::LabelTracker<word>, public syn::FinishedDataTracker<AssemblerData> {
        public:
            using LabelTracker = syn::LabelTracker<word>;
            AssemblerState();
            virtual ~AssemblerState();
            void resetCurrentData() noexcept;
            void setImmediate(word value) noexcept;
            void setHalfImmediate(byte value) noexcept;
            void setGroup(InstructionGroup value) noexcept;
            void nowInCodeSection() noexcept;
            void nowInDataSection() noexcept;
            void setCurrentAddress(word value) noexcept;
            void registerLabel(const std::string& label) noexcept;
            word getCurrentAddress() noexcept;
            void incrementCurrentAddress() noexcept;
            void saveToFinished() noexcept;
            void setTemporaryByte(byte value) noexcept;
            void setTemporaryWord(word value) noexcept;
            void setDestination(byte destination) noexcept;
            void setSource0(byte value) noexcept;
            void setSource1(byte value) noexcept;
            void stashTemporaryByteInDestination() noexcept;
            void stashTemporaryByteInSource0() noexcept;
            void stashTemporaryByteInSource1() noexcept;
            void markHasLexeme() noexcept;
            void markNotLexeme() noexcept;
            void setLexeme(const std::string& lexeme) noexcept;
            void markIsInstruction() noexcept;
            void markIsNotInstruction() noexcept;
            void setDataValue(word value) noexcept;
            void stashTemporaryWordIntoDataValue() noexcept;
            void markHasFullImmediate() noexcept;
            void markNotFullImmediate() noexcept;
        public:
            template<typename T>
                void setOperation(T value) noexcept {
                    current.operation = static_cast<byte>(value);
                }
            template<bool toCodeSection>
                void changeSection() noexcept {
                    if (toCodeSection) {
                        nowInCodeSection();
                    } else {
                        nowInDataSection();
                    }
                }
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
        public:
            bool inCodeSection() const noexcept { return !inData; }
            bool inDataSection() const noexcept { return inData; }
            bool hasLexeme() const noexcept { return current.hasLexeme; }
            std::string getCurrentLexeme() const noexcept { return current.currentLexeme; }
            byte getTemporaryByte() const noexcept { return temporaryByte; }
            word getTemporaryWord() const noexcept { return temporaryWord; }
        private:
            using AddressSpaceTracker = syn::AddressTracker<word>;
            AddressSpaceTracker data;
            AddressSpaceTracker code;
            bool inData;
            word temporaryWord;
            byte temporaryByte;
            AssemblerData current;
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
    };
    DefAction(PredicateRegister) {
        DefApplyGeneric(RegisterIndexContainer) {
            setRegisterValue<Input, ArchitectureConstants::ConditionRegisterCount>(in, state);
        }
        DefApplyEmpty
    };
    struct IndirectGPR : syn::SingleEntrySequence<GeneralPurposeRegister> { };
#define DefRegisterIndexContainerAction(title, theType) \
    DefAction( title ) { \
        DefApply { } \
        DefApplyGeneric(RegisterIndexContainer) { \
            state._index = RegisterIndexContainer :: Type :: theType ; \
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
    struct Number : syn::StatefulNumberAll<ImmediateContainer> { };
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
    template<bool modSection, typename Directive>
        struct StatefulSpaceDirective : syn::StatefulSingleEntrySequence<ModifySection<modSection>, Directive> { };
    struct CodeDirective : StatefulSpaceDirective<true, SymbolCodeDirective> { };
    struct DataDirective : StatefulSpaceDirective<false, SymbolDataDirective> { };

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
    template<typename ... Operations>
        using ThreeGPRInstructionWithMultipleOps = ThreeGPRInstruction<pegtl::sor<Operations...>>;
    template<typename Operation>
        using TwoGPRInstruction = GenericInstruction<Operation, TwoGPR>;
    template<typename ... Operations>
        using TwoGPRInstructionWithMultipleOps = TwoGPRInstruction<pegtl::sor<Operations...>>;
#define CURRENT_TYPE ArithmeticOp
#define DefActionUsingPredefinedSymbol(title) \
    using Symbol ## title = syn:: Symbol ## title ## Keyword ; \
    ConstructOperationSetter(title, title)

#define DefActionUsingPredefinedSymbolWithSeparateType(title, type) \
    using Symbol ## title = syn:: Symbol ## title ## Keyword ; \
    ConstructOperationSetter(title, type)
    DefActionUsingPredefinedSymbol(Add);
    DefActionUsingPredefinedSymbol(Sub);
    DefActionUsingPredefinedSymbol(Mul);
    DefActionUsingPredefinedSymbol(Div);
    DefActionUsingPredefinedSymbol(Rem);
    DefOperationSameTitle(ShiftLeft, shift.left);
    DefOperationSameTitle(ShiftRight, shift.right);
    DefActionUsingPredefinedSymbolWithSeparateType(And, BinaryAnd);
    DefActionUsingPredefinedSymbolWithSeparateType(Or, BinaryOr);
    DefOperation(Xor, xor, BinaryXor);
    DefOperation(Nand, nand, BinaryNand);
    DefOperation(Nor, nor, BinaryNor);
    DefOperationSameTitle(Min, min);
    DefOperationSameTitle(Max, max);
    struct ArithmeticThreeGPRInstruction : ThreeGPRInstructionWithMultipleOps<SymbolAdd, SymbolSub, SymbolMul, SymbolDiv, SymbolRem, SymbolShiftLeft, SymbolShiftRight, SymbolAnd, SymbolOr, SymbolXor, SymbolMin, SymbolMax> { };
    DefOperation(Not, not, BinaryNot);
    struct ArithmeticTwoGPRInstruction : TwoGPRInstructionWithMultipleOps< SymbolNot > { };

    DefOperationSameTitle(AddImmediate, add.imm);
    DefOperationSameTitle(SubImmediate, sub.imm);
    DefOperationSameTitle(MulImmediate, mul.imm);
    DefOperationSameTitle(DivImmediate, div.imm);
    DefOperationSameTitle(RemImmediate, rem.imm);
    DefOperationSameTitle(ShiftLeftImmediate, shift.left.imm);
    DefOperationSameTitle(ShiftRightImmediate, shift.right.imm);
    struct OperationArithmeticTwoGPRHalfImmediate : pegtl::sor<
                                                    SymbolAddImmediate,
                                                    SymbolSubImmediate,
                                                    SymbolMulImmediate,
                                                    SymbolDivImmediate,
                                                    SymbolRemImmediate,
                                                    SymbolShiftLeftImmediate,
                                                    SymbolShiftRightImmediate> { };
    struct ArithmeticTwoGPRHalfImmediateInstruction : SeparatedTrinaryThing<OperationArithmeticTwoGPRHalfImmediate, TwoGPR, HalfImmediate> { };
    struct ArithmeticInstruction : pegtl::sor<
                                   ArithmeticTwoGPRHalfImmediateInstruction,
                                   ArithmeticTwoGPRInstruction,
                                   ArithmeticThreeGPRInstruction> { };

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
    struct MoveTwoGPRInstruction : TwoGPRInstructionWithMultipleOps<SymbolMove, SymbolSwap, SymbolLoadIO, SymbolStoreIO, SymbolLoad, SymbolStore, SymbolPush, SymbolPop> { };
    DefOperationSameTitle(LoadWithOffset, load.offset);
    DefOperationSameTitle(StoreWithOffset, store.offset);
    DefOperation(LoadIOWithOffset, load.with.offset, IOReadWithOffset);
    DefOperation(StoreIOWithOffset, store.with.offset, IOWriteWithOffset);
    struct OperationMoveTwoGPRHalfImmediate : pegtl::sor<SymbolLoadWithOffset, SymbolStoreWithOffset, SymbolLoadIOWithOffset, SymbolStoreIOWithOffset> { };

    struct MoveTwoGPRHalfImmediateInstruction : SeparatedTrinaryThing<OperationMoveTwoGPRHalfImmediate, TwoGPR, HalfImmediate> { };

    DefOperationSameTitle(LoadCode, load.code);
    DefOperationSameTitle(StoreCode, store.code);
    struct OperationMoveThreeGPR : pegtl::sor<SymbolLoadCode, SymbolStoreCode> { };
    struct MoveThreeGPRInstruction : ThreeGPRInstruction<OperationMoveThreeGPR> { };

    DefOperationSameTitle(PushImmediate, push.imm);
    DefOperationSameTitle(Set, set);
    DefOperationSameTitle(LoadImmediate, load.imm);
    DefOperationSameTitle(StoreImmediate, store.imm);
    struct OperationMoveGPRImmediate : pegtl::sor<SymbolStoreImmediate, SymbolLoadImmediate, SymbolSet, SymbolPushImmediate> { };

    struct MoveGPRImmediateInstruction : SeparatedTrinaryThing<OperationMoveGPRImmediate, StatefulDestinationGPR, Immediate> { };

    struct MoveInstruction : pegtl::sor<MoveGPRImmediateInstruction, MoveThreeGPRInstruction, MoveTwoGPRHalfImmediateInstruction, MoveTwoGPRInstruction, MoveOneGPRInstruction> { };
    DefGroupSet(MoveInstruction, Move);
#undef CURRENT_TYPE
#define CURRENT_TYPE JumpOp
    // branch
    template<typename Op, typename S>
        struct BranchUnconditional : SeparatedBinaryThing<Op, S> { };
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
        struct BranchConditional : SeparatedTrinaryThing<Op, DestinationPredicateRegister, S> { };
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
    struct BranchConditionalNoArgsInstruction : SeparatedBinaryThing<OperationBranchConditionalNoArgs, DestinationPredicateRegister> { };
    DefOperationSameTitle(BranchUnconditionalLR, branch.lr);
    DefOperationSameTitle(BranchUnconditionalLRAndLink, branch.lr.link);
    DefOperation(BranchReturnFromError, return.from.error, ReturnFromError);
    struct BranchNoArgsInstruction : pegtl::sor<SymbolBranchUnconditionalLR,  SymbolBranchUnconditionalLRAndLink, SymbolBranchReturnFromError> { };

    struct BranchInstruction : pegtl::sor<GroupBranchUnconditional, BranchConditionalGPRInstruction, BranchConditionalImmediateInstruction, BranchIfInstruction, BranchConditionalNoArgsInstruction, BranchNoArgsInstruction> { };
    DefGroupSet(BranchInstruction, Jump);

#undef CURRENT_TYPE
    struct ThenDestinationPredicates : syn::ThenField<DestinationPredicates> { };
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
    struct CompareRegisterOperation : pegtl::sor<SymbolEq, SymbolNeq, SymbolLessThan, SymbolGreaterThan, SymbolLessThanOrEqualTo, SymbolGreaterThanOrEqualTo> { };
    struct CompareImmediateOperation : pegtl::sor<SymbolEqImmediate, SymbolNeqImmediate, SymbolLessThanImmediate, SymbolGreaterThanImmediate, SymbolLessThanOrEqualToImmediate, SymbolGreaterThanOrEqualToImmediate> { };
    struct CompareRegisterInstruction : pegtl::seq<CompareRegisterOperation, ThenDestinationPredicates, syn::ThenField<SourceRegisters>> { };
    struct CompareImmediateInstruction : pegtl::seq<CompareImmediateOperation, ThenDestinationPredicates, syn::ThenField<Source0GPR>, syn::ThenField<HalfImmediate>> { };
    struct CompareInstruction : pegtl::sor<
                                CompareRegisterInstruction,
                                CompareImmediateInstruction> { };
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
    struct ThenSource0Predicate : syn::ThenField<StatefulRegister<Source0Predicate>> { };
    struct OperationPredicateTwoArgs : pegtl::sor<SymbolCRSwap, SymbolCRMove> { };
    struct OperationPredicateOneGPR : pegtl::sor<SymbolSaveCRs, SymbolRestoreCRs> { };
    struct OperationPredicateFourArgs : pegtl::sor<SymbolCRXor, SymbolCRAnd, SymbolCROr, SymbolCRNand, SymbolCRNor> { };
    struct PredicateInstructionOneGPR : pegtl::seq<OperationPredicateOneGPR, syn::ThenField<StatefulDestinationGPR>> { };
    struct PredicateInstructionTwoArgs : pegtl::seq<OperationPredicateTwoArgs, ThenDestinationPredicates> { };
    struct PredicateInstructionThreeArgs : pegtl::seq<SymbolCRNot, ThenDestinationPredicates, ThenSource0Predicate> { };
    struct PredicateInstructionFourArgs : pegtl::seq<OperationPredicateFourArgs, ThenDestinationPredicates, ThenSource0Predicate, syn::ThenField<StatefulRegister<Source1Predicate>>> { };
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
