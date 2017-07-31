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

#define DefBeginStringToFn(type) \
    type stringTo ## type (const std::string& str) noexcept { \
        static std::map<std::string, type > translation = {
#define DefEndStringToFn(type) \
        }; \
        auto x = translation.find(str); \
        if (x == translation.end()) { \
            return syn::defaultErrorState< type > ; \
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
    DefBeginStringToFn(CompareStyle)
       #include "desc/cisc0/CompareStyle.desc"
    DefEndStringToFn(CompareStyle)
    DefBeginStringToFn(ArithmeticOps)
       #include "desc/cisc0/ArithmeticOps.desc"
    DefEndStringToFn(ArithmeticOps)
    MemoryOperation stringToMemoryOperation(const std::string& str) noexcept {
        static std::map<std::string, MemoryOperation> translation = {
            { "store", MemoryOperation::Store},
            { "load", MemoryOperation::Load},
            { "push", MemoryOperation::Push},
            { "pop", MemoryOperation::Pop},
        };
        auto x = translation.find(str);
        if (x == translation.end()) {
            return syn::defaultErrorState<MemoryOperation>;
        } else {
            return x->second;
        }
    }
    LogicalOps stringToLogicalOps(const std::string& str) noexcept {
        static std::map<std::string, LogicalOps> translation = {
            { "and", LogicalOps::And},
            { "or", LogicalOps::Or},
            { "not", LogicalOps::Not},
            { "xor", LogicalOps::Xor},
            { "nand", LogicalOps::Nand},
        };
        auto x = translation.find(str);
        if (x == translation.end()) {
            return syn::defaultErrorState<LogicalOps>;
        } else {
            return x->second;
        }
    }
    EncodingOperation stringToEncodingOperation(const std::string& str) noexcept {
        static std::map<std::string, EncodingOperation> translation = {
#define X(str, type) \
            { str, type },
#include "desc/cisc0/EncodingOperation.desc"
#undef X
        };
        auto x = translation.find(str);
        if (x == translation.end()) {
            return syn::defaultErrorState<EncodingOperation>;
        } else {
            return x->second;
        }
    }
    ExtendedOperation stringToExtendedOperation(const std::string& str) noexcept {
        static std::map<std::string, ExtendedOperation> translation = {
#define X(str, type) \
            { str, type },
#include "desc/cisc0/ExtendedOperation.desc"
#undef X
        };
        auto x = translation.find(str);
        if (x == translation.end()) {
            return syn::defaultErrorState<ExtendedOperation>;
        } else {
            return x->second;
        }
    }
DefBeginStringToFn(ComplexSubTypes)
#include "desc/cisc0/ComplexSubTypes.desc"
DefEndStringToFn(ComplexSubTypes)

    ParsingOperation stringToParsingOperation(const std::string& str) noexcept {
        static std::map<std::string, ParsingOperation> translation = {
            { "Hex8ToRegister", ParsingOperation::Hex8ToRegister},
            { "RegisterToHex8", ParsingOperation::RegisterToHex8 },
            { "MemCopy", ParsingOperation::MemCopy },
        };
        auto x = translation.find(str);
        if (x == translation.end()) {
            return syn::defaultErrorState<ParsingOperation>;
        } else {
            return x->second;
        }
    }
}
