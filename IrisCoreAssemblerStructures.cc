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
#include "IrisCoreAssemblerStructures.h"

namespace iris {
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
        LabelTracker::registerLabel(value, getCurrentAddress());
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

#define StringToEnumEntry(str, type) { str , type },
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
