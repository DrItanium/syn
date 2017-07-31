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


// Cisc0CoreAssembler rewritten to use pegtl
#include "Cisc0ClipsExtensions.h"
#include "Cisc0CoreAssembler.h"

namespace cisc0 {
	void AssemblerState::resolveInstructions() {
		applyToFinishedData([this](InstructionEncoder& op) {
			if (op.isLabel()) {
				auto label = op.getLabelValue();
				auto f = findLabel(label);
				if (f == labelsEnd()) {
					std::stringstream stream;
					stream << "label " << label << " does not exist!\n";
					throw syn::Problem(stream.str());
				}
				op.setFullImmediate(f->second);
			}
			// now that it has been resolved, we need to go through and setup
			// the encoding correctly!
			auto address = op.getAddress();
            auto encoding = op.encode();
			switch(encoding.getNumWords()) {
				case 3:
					finalWords.emplace_back(address + 2, encoding.getWord2());
				case 2:
					finalWords.emplace_back(address + 1, encoding.getWord1());
				case 1:
					finalWords.emplace_back(address, encoding.getWord0());
					break;
				default:
					throw syn::Problem("Number of words described is not possible!");
			}
		});
	}
	void AssemblerState::resolveDeclarations() {
		for (auto & op: wordsToResolve) {
			if (op.isLabel()) {
				auto label = op.getLabel();
				auto f = findLabel(label);
				if (f == labelsEnd()) {
					std::stringstream stream;
					stream << "label " << label << " does not exist!\n";
					throw syn::Problem(stream.str());
				}
				op.setValue(f->second);
			}
			switch(op.getWidth()) {
				case 2:
					finalWords.emplace_back(op.getAddress() + 1, syn::getUpperHalf(op.getValue()));
				case 1:
					finalWords.emplace_back(op.getAddress(), syn::getLowerHalf(op.getValue()));
					break;
				default:
					throw syn::Problem("Got a declaration of with a width that was not 1 or 2");
			}
		}
	}

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


	Word translateRegister(const std::string& input) {
		static std::map<std::string, Word> builtinAliases = {
			{ "addr", static_cast<Word>(ArchitectureConstants::AddressRegister) },
			{ "ip", static_cast<Word>(ArchitectureConstants::InstructionPointer) },
			{ "sp", static_cast<Word>(ArchitectureConstants::StackPointer) },
			{ "value", static_cast<Word>(ArchitectureConstants::ValueRegister) },
			{ "mask", static_cast<Word>(ArchitectureConstants::MaskRegister) },
			{ "shift", static_cast<Word>(ArchitectureConstants::ShiftRegister) },
			{ "field", static_cast<Word>(ArchitectureConstants::FieldRegister) },
			{ "csp", static_cast<Word>(ArchitectureConstants::CallStackPointer) },
		};
		auto result = builtinAliases.find(input);
		if (result == builtinAliases.end()) {
			return syn::getRegister<Word, ArchitectureConstants::RegisterCount>(input, syn::reportError);
		} else {
			return result->second;
		}
	}
DefBeginStringToEnumFn(CompareStyle)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/CompareStyle.desc"
#undef X
DefEndStringToEnumFn(CompareStyle)

DefBeginStringToEnumFn(ArithmeticOps)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/ArithmeticOps.desc"
#undef X
DefEndStringToEnumFn(ArithmeticOps)

DefBeginStringToEnumFn(MemoryOperation)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/MemoryOperation.desc"
#undef X
DefEndStringToEnumFn(MemoryOperation)

DefBeginStringToEnumFn(LogicalOps)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/LogicalOps.desc"
#undef X
DefEndStringToEnumFn(LogicalOps)

DefBeginStringToEnumFn(EncodingOperation)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/EncodingOperation.desc"
#undef X
DefEndStringToEnumFn(EncodingOperation)

DefBeginStringToEnumFn(ExtendedOperation)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/ExtendedOperation.desc"
#undef X
DefEndStringToEnumFn(ExtendedOperation)

DefBeginStringToEnumFn(ComplexSubTypes)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/ComplexSubTypes.desc"
#undef X
DefEndStringToEnumFn(ComplexSubTypes)

DefBeginStringToEnumFn(ParsingOperation)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/ParsingOperation.desc"
#undef X
DefEndStringToEnumFn(ParsingOperation)

DefBeginEnumToStringFn(CompareStyle, compareStyle)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/CompareStyle.desc"
#undef X
DefEndEnumToStringFn(CompareStyle)

DefBeginEnumToStringFn(ArithmeticOps, arithmeticOps)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/ArithmeticOps.desc"
#undef X
DefEndEnumToStringFn(ArithmeticOps)

DefBeginEnumToStringFn(MemoryOperation, memoryOperation)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/MemoryOperation.desc"
#undef X
DefEndEnumToStringFn(MemoryOperation)

DefBeginEnumToStringFn(LogicalOps, logicalOps)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/LogicalOps.desc"
#undef X
DefEndEnumToStringFn(LogicalOps)

DefBeginEnumToStringFn(EncodingOperation, encodingOperation)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/EncodingOperation.desc"
#undef X
DefEndEnumToStringFn(EncodingOperation)

DefBeginEnumToStringFn(ExtendedOperation, extendedOperation)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/ExtendedOperation.desc"
#undef X
DefEndEnumToStringFn(ExtendedOperation)

DefBeginEnumToStringFn(ComplexSubTypes, complexSubTypes)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/ComplexSubTypes.desc"
#undef X
DefEndEnumToStringFn(ComplexSubTypes)

DefBeginEnumToStringFn(ParsingOperation, parsingOperation)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/ParsingOperation.desc"
#undef X
DefEndEnumToStringFn(ParsingOperation)

}
