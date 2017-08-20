/**
 * @file
 * Implementation of iris assembler structures and related functions
 * @copyright
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
#include "IrisClipsExtensions.h"
#include "ClipsExtensions.h"
#include "IrisCoreAssemblerStructures.h"
#include "IrisCoreEncodingOperations.h"

namespace iris {
    namespace assembler {
        AssemblerData::AssemblerData() noexcept : instruction(false), address(0), dataValue(0), group(0), operation(0), destination(0), source0(0), source1(0), hasLexeme(false), fullImmediate(false) { }


        void AssemblerData::setImmediate(word value) noexcept {
            source0 = syn::getLowerHalf<word>(value);
            source1 = syn::getUpperHalf<word>(value);
        }
        raw_instruction AssemblerData::encode() const noexcept {
            if (instruction) {
                return iris::encodeInstruction(group, operation, destination, source0, source1);
            } else {
                return dataValue;
            }
        }

        void AssemblerState::setCurrentAddress(word value) noexcept {
            if (inDataSection()) {
                data.setCurrentAddress(value);
            } else {
                code.setCurrentAddress(value);
            }
        }
        void AssemblerState::registerLabel(const std::string& value) noexcept {
            LabelParent::registerLabel(value, getCurrentAddress());
        }
        word AssemblerState::getCurrentAddress() const noexcept {
            return inDataSection() ? data.getCurrentAddress() : code.getCurrentAddress();
        }
        void AssemblerState::incrementCurrentAddress() noexcept {
            if (inDataSection()) {
                data.incrementCurrentAddress();
            } else {
                code.incrementCurrentAddress();
            }
        }

		void AssemblerState::reset() noexcept {
			LabelParent::reset();
			FinishedDataParent::reset();
			data.reset();
			code.reset();
			_section = assembler::SectionType::Code;
		}
        void AssemblerInstruction::setField(RegisterPositionType type, byte value) {
            using Type = RegisterPositionType;
            switch(type) {
                case Type::DestinationGPR:
                    destination = value;
                    break;
                case Type::Source0GPR:
                    source0 = value;
                    break;
                case Type::Source1GPR:
                    source1 = value;
                    break;
                case Type::PredicateDestination:
                    destination = iris::encode4Bits<false>(destination, value);
                    break;
                case Type::PredicateInverseDestination:
                    destination = iris::encode4Bits<true>(destination, value);
                    break;
                case Type::PredicateSource0:
                    source0 = iris::encode4Bits<false>(source0, value);
                    break;
                case Type::PredicateSource1:
                    source0 = iris::encode4Bits<true>(source0, value);
                    break;
                default:
                    syn::reportError("Illegal index provided!");
            }
        }
        bool AssemblerDirective::shouldChangeSectionToCode() const noexcept {
            return (action == AssemblerDirectiveAction::ChangeSection) && (section == SectionType::Code);
        }
        bool AssemblerDirective::shouldChangeSectionToData() const noexcept {
            return (action == AssemblerDirectiveAction::ChangeSection) && (section == SectionType::Data);
        }
        bool AssemblerDirective::shouldChangeCurrentAddress() const noexcept {
            return (action == AssemblerDirectiveAction::ChangeCurrentAddress);
        }
        bool AssemblerDirective::shouldDefineLabel() const noexcept {
            return (action == AssemblerDirectiveAction::DefineLabel);
        }
        bool AssemblerDirective::shouldStoreWord() const noexcept {
            return (action == AssemblerDirectiveAction::StoreWord);
        }
        bool AssemblerData::shouldResolveLabel() const noexcept {
            return fullImmediate && hasLexeme;
        }

        void AssemblerState::nowInCodeSection() noexcept {
            _section = SectionType::Code;
        }
        void AssemblerState::nowInDataSection() noexcept {
            _section = SectionType::Data;
        }
        bool AssemblerState::inCodeSection() const noexcept { return _section == SectionType::Code; }
        bool AssemblerState::inDataSection() const noexcept { return _section == SectionType::Data; }
    } // end namespace assembler

    const std::string& translateRegister(byte index) noexcept {
        static bool init = true;
        static std::string names[ArchitectureConstants::RegisterCount];
        if (init) {
            init = false;
            std::stringstream tmp;
            for (int i = 0; i < int(ArchitectureConstants::RegisterCount); ++i) {
                tmp.str("");
                tmp << "r" << i;
                names[i] = tmp.str();
            }
        }
        return names[index];
    }

    const std::string& translatePredicateRegister(byte index) noexcept {
        static bool init = true;
        static constexpr auto count = ArchitectureConstants::ConditionRegisterCount;
        static std::string names[count];
        if (init) {
            init = false;
            std::stringstream tmp;
            for (int i = 0; i < int(count); ++i) {
                tmp.str("");
                tmp << "p" << i;
                names[i] = tmp.str();
            }
        }
        return names[index & 0xF];
    }
    void outputRegister(std::ostream& out, byte input) noexcept {
        out << translateRegister(input) << " ";
    }
    void outputPredicateRegister(std::ostream& out, byte input) noexcept {
        out << translatePredicateRegister(input) << " ";
    }
    void translateDestinationRegister(std::ostream& out, raw_instruction input) noexcept {
        outputRegister(out, InstructionDecoder::getDestinationIndex(input));
    }
    void translatePredicateDestinationRegister(std::ostream& out, raw_instruction input) noexcept {
        outputPredicateRegister(out, InstructionDecoder::getPredicateResultIndex(input));
    }
    void translatePredicateDestinationInverseRegister(std::ostream& out, raw_instruction input) noexcept {
        outputPredicateRegister(out, InstructionDecoder::getPredicateInverseResultIndex(input));
    }
    void translateSource0Register(std::ostream& out, raw_instruction input) noexcept {
        outputRegister(out, InstructionDecoder::getSource0Index(input));
    }
    void translatePredicateSource0Register(std::ostream& out, raw_instruction input) noexcept {
        outputPredicateRegister(out, InstructionDecoder::getPredicateSource0Index(input));
    }
    void translatePredicateSource1Register(std::ostream& out, raw_instruction input) noexcept {
        outputPredicateRegister(out, InstructionDecoder::getPredicateSource1Index(input));
    }
    void translateSource1Register(std::ostream& out, raw_instruction input) noexcept {
        outputRegister(out, InstructionDecoder::getSource1Index(input));
    }
    void outputWord(std::ostream& out, word value) noexcept {
        out << "0x" << std::hex << value << " ";
    }
    void translateHalfImmediate(std::ostream& out, raw_instruction input) noexcept {
        outputWord(out, InstructionDecoder::getHalfImmediate(input));
    }
    void translateFullImmediate(std::ostream& out, raw_instruction input) noexcept {
        outputWord(out, InstructionDecoder::getImmediate(input));
    }
    template<typename T>
    constexpr bool isImmediate(T op) noexcept {
        return false;
    }
    template<>
    constexpr bool isImmediate<ArithmeticOp>(ArithmeticOp op) noexcept {
        switch(op) {
            case ArithmeticOp::AddImmediate:
            case ArithmeticOp::SubImmediate:
            case ArithmeticOp::MulImmediate:
            case ArithmeticOp::DivImmediate:
            case ArithmeticOp::RemImmediate:
            case ArithmeticOp::ShiftRightImmediate:
            case ArithmeticOp::ShiftLeftImmediate:
                return true;
            default:
                return false;
        }
    }
    template<>
    constexpr bool isImmediate<CompareOp>(CompareOp op) noexcept {
        switch(op) {
            case CompareOp::EqImmediate:
            case CompareOp::NeqImmediate:
            case CompareOp::LessThanImmediate:
            case CompareOp::LessThanOrEqualToImmediate:
            case CompareOp::GreaterThanImmediate:
            case CompareOp::GreaterThanOrEqualToImmediate:
                return true;
            default:
                return false;
        }
    }
    template<typename T>
    void translateSource1(std::ostream& out, raw_instruction input, T op) noexcept {
        if (isImmediate<T>(op)) {
            translateHalfImmediate(out, input);
        } else {
            translateSource1Register(out, input);
        }
    }
    void translateInstruction(std::ostream& out, raw_instruction input, ArithmeticOp op) noexcept {
         out << arithmeticOpToString(op) << " ";
         translateDestinationRegister(out, input);
         translateSource0Register(out, input);
         translateSource1(out, input, op);
    }
    void translateInstruction(std::ostream& out, raw_instruction input, CompareOp op) noexcept {
        out << compareOpToString(op) << " ";
        translatePredicateDestinationRegister(out, input);
        translatePredicateDestinationInverseRegister(out, input);
        translateSource0Register(out, input);
        translateSource1(out, input, op);
    }
    void translateInstruction(std::ostream& out, raw_instruction input, ConditionRegisterOp op) noexcept {
        out << conditionRegisterOpToString(op) << " ";
        switch (op) {
            case ConditionRegisterOp::RestoreCRs:
            case ConditionRegisterOp::SaveCRs:
                translateDestinationRegister(out, input);
                translateFullImmediate(out, input);
                break;
            case ConditionRegisterOp::CRSwap:
            case ConditionRegisterOp::CRMove:
                translatePredicateDestinationRegister(out, input);
                translatePredicateDestinationInverseRegister(out, input);
                break;
            case ConditionRegisterOp::CRNot:
                translatePredicateDestinationRegister(out, input);
                translatePredicateDestinationInverseRegister(out, input);
                translatePredicateSource0Register(out, input);
                break;

            case ConditionRegisterOp::CROr:
            case ConditionRegisterOp::CRAnd:
            case ConditionRegisterOp::CRXor:
            case ConditionRegisterOp::CRNor:
            case ConditionRegisterOp::CRNand:
                translatePredicateDestinationRegister(out, input);
                translatePredicateDestinationInverseRegister(out, input);
                translatePredicateSource0Register(out, input);
                translatePredicateSource1Register(out, input);
                break;
            default:
                break;
        }
    }
    void translateInstruction(std::ostream& out, raw_instruction input, MoveOp op) noexcept {
        out << moveOpToString(op) << " ";
        translateDestinationRegister(out, input);
        switch(op) {
            case MoveOp::StoreCode:
            case MoveOp::LoadCode:
                translateSource0Register(out, input);
                translateSource1Register(out, input);
                break;
            case MoveOp::LoadWithOffset:
            case MoveOp::StoreWithOffset:
            case MoveOp::LoadIOWithOffset:
            case MoveOp::StoreIOWithOffset:
                translateSource0Register(out, input);
                translateHalfImmediate(out, input);
                break;
                break;
            case MoveOp::StoreImmediate:
            case MoveOp::LoadImmediate:
            case MoveOp::Set:
            case MoveOp::PushImmediate:
                translateFullImmediate(out, input);
                break;
            case MoveOp::Move:
            case MoveOp::Swap:
            case MoveOp::LoadIO:
            case MoveOp::StoreIO:
            case MoveOp::Load:
            case MoveOp::Store:
            case MoveOp::Push:
            case MoveOp::Pop:
                translateSource0Register(out, input);
                break;
            case MoveOp::MoveToIP:
            case MoveOp::MoveToLR:
            case MoveOp::MoveFromIP:
            case MoveOp::MoveFromLR:
            case MoveOp::RestoreAllRegisters:
            case MoveOp::SaveAllRegisters:
            default:
                break;

        }
    }
    void translateInstruction(std::ostream& out, raw_instruction input, JumpOp op) noexcept {
        out << jumpOpToString(op) << " ";
        switch(op) {
            case JumpOp::BranchUnconditionalLink:
            case JumpOp::BranchUnconditional:
                translateDestinationRegister(out, input);
                break;
            case JumpOp::BranchUnconditionalImmediateLink:
            case JumpOp::BranchUnconditionalImmediate:
                translateFullImmediate(out, input);
                break;
            case JumpOp::BranchConditionalLink:
            case JumpOp::BranchConditional:
                translatePredicateDestinationRegister(out, input);
                translateSource0Register(out, input);
                break;
            case JumpOp::BranchConditionalImmediate:
            case JumpOp::BranchConditionalImmediateLink:
                translatePredicateDestinationRegister(out, input);
                translateFullImmediate(out, input);
                break;
            case JumpOp::BranchConditionalLRAndLink:
            case JumpOp::BranchConditionalLR:
                translatePredicateDestinationRegister(out, input);
                break;
            case JumpOp::ReturnFromError:
            case JumpOp::BranchUnconditionalLR:
            case JumpOp::BranchUnconditionalLRAndLink:
                break;
            default:
                break;
        }
    }
    std::string translateInstruction(raw_instruction input) noexcept {
        std::stringstream output;
        auto group = InstructionDecoder::getGroup(input);
        switch(group) {
            case InstructionGroup::Arithmetic:
                translateInstruction(output, input, InstructionDecoder::getOperation<ArithmeticOp>(input));
                break;
            case InstructionGroup::Compare:
                translateInstruction(output, input, InstructionDecoder::getOperation<CompareOp>(input));
                break;
            case InstructionGroup::ConditionalRegister:
                translateInstruction(output, input, InstructionDecoder::getOperation<ConditionRegisterOp>(input));
                break;
            case InstructionGroup::Move:
                translateInstruction(output, input, InstructionDecoder::getOperation<MoveOp>(input));
                break;
            case InstructionGroup::Jump:
                translateInstruction(output, input, InstructionDecoder::getOperation<JumpOp>(input));
                break;
            case InstructionGroup::Unused0:
                output << ".dword 0x" << std::hex << input << " ; unused instruction group 0 instruction!";
                break;
            case InstructionGroup::CustomInstructionReserved:
                output << ".dword 0x" << std::hex << input << " ; custom instruction reserved group instruction!";
                break;
            default:
                break;
        }
        auto result = output.str();
        return result;
    }

#define StringToEnumEntry(str, type) { str ,  type },
#define EnumToStringEntry(str, type) { type , str },
#define DefBeginStringToEnumFn(type) \
    type stringTo ## type (const std::string& str) noexcept { \
        static std::map<std::string, type > translation = {
#define DefEndStringToEnumFn(type) \
        }; \
        auto x = translation.find(str); \
        if (x == translation.end()) { \
            return syn::defaultErrorState< type > ; \
        } else { \
            return x->second; \
        } \
    }
#define DefBeginEnumToStringFn(type, title) \
    const std::string& title ## ToString ( type value ) noexcept { \
        static std::string errorState; \
        static std::map < type , std::string > translation = {

#define DefEndEnumToStringFn(type) \
        }; \
        auto x = translation.find(value); \
        if (x == translation.end()) { \
            return errorState; \
        } else { \
            return x->second; \
        } \
    }
    DefBeginEnumToStringFn(CompareOp, compareOp)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/iris/CompareOp.desc"
#undef X
        DefEndEnumToStringFn(CompareOp)

        DefBeginStringToEnumFn(CompareOp)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/iris/CompareOp.desc"
#undef X
        DefEndStringToEnumFn(CompareOp)

        DefBeginEnumToStringFn(JumpOp, jumpOp)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/iris/JumpOp.desc"
#undef X
        DefEndEnumToStringFn(JumpOp)

        DefBeginStringToEnumFn(JumpOp)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/iris/JumpOp.desc"
#undef X
        DefEndStringToEnumFn(JumpOp)

        DefBeginEnumToStringFn(ConditionRegisterOp, conditionRegisterOp)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/iris/ConditionRegisterOp.desc"
#undef X
        DefEndEnumToStringFn(ConditionRegisterOp)

        DefBeginStringToEnumFn(ConditionRegisterOp)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/iris/ConditionRegisterOp.desc"
#undef X
        DefEndStringToEnumFn(ConditionRegisterOp)

        DefBeginEnumToStringFn(ArithmeticOp, arithmeticOp)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/iris/ArithmeticOp.desc"
#undef X
        DefEndEnumToStringFn(ArithmeticOp)

        DefBeginStringToEnumFn(ArithmeticOp)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/iris/ArithmeticOp.desc"
#undef X
        DefEndStringToEnumFn(ArithmeticOp)

        DefBeginEnumToStringFn(MoveOp, moveOp)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/iris/MoveOp.desc"
#undef X
        DefEndEnumToStringFn(MoveOp)

        DefBeginStringToEnumFn(MoveOp)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/iris/MoveOp.desc"
#undef X
        DefEndStringToEnumFn(MoveOp)

} // end namespace iris
