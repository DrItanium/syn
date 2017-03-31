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
		for (auto & op : finishedInstructions) {
			if (op.isLabel) {
				auto label = op.labelValue;
				auto f = labels.find(label);
				if (f == labels.end()) {
					std::stringstream stream;
					stream << "label " << label << " does not exist!\n";
					throw syn::Problem(stream.str());
				}
				op.fullImmediate = f->second;
			}
			// now that it has been resolved, we need to go through and setup
			// the encoding correctly!
			auto address = op.address;
			int count;
			Word first, second, third;
			std::tie(count, first, second, third) = op.encode();
			//std::cerr << "count = " << count << std::endl;
			//std::cerr << "- first = " << std::hex << first << std::endl;
			//std::cerr << "- second = " << std::hex << second << std::endl;
			//std::cerr << "- third = " << std::hex << third << std::endl;
			switch(count) {
				case 3:
					finalWords.emplace_back(address + 2, third);
				case 2:
					finalWords.emplace_back(address + 1, second);
				case 1:
					finalWords.emplace_back(address, first);
					break;
				default:
					throw syn::Problem("Number of words described is not possible!");
			}
		}
	}
	void AssemblerState::resolveDeclarations() {
		for (auto & op: wordsToResolve) {
			if (op.isLabel()) {
				auto label = op.getLabel();
				auto f = labels.find(label);
				if (f == labels.end()) {
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

	void AssemblerState::output(std::ostream* out) noexcept {
		char buf[8] = { 0 };
		for(auto const & address : finalWords) {
			buf[0] = 0;
			buf[1] = 0;
			buf[2] = static_cast<char>(address.getAddress());
			buf[3] = static_cast<char>(address.getAddress() >> 8);
			buf[4] = static_cast<char>(address.getAddress() >> 16);
			buf[5] = static_cast<char>(address.getAddress() >> 24);
			buf[6] = static_cast<char>(address.getValue());
			buf[7] = static_cast<char>(address.getValue() >> 8);
			out->write(buf, 8);
		}
	}

    void AssemblerState::output(void* env, CLIPSValue* ret) noexcept {
        // we need to build a multifield out of the finalWords
        syn::MultifieldBuilder f(env, finalWords.size() * 2);
        int i = 1;
        for (auto q : finalWords) {
            // add them two at a time!
            f.setField(i, INTEGER, EnvAddLong(env, q.getAddress()));
            f.setField(i + 1, INTEGER, EnvAddLong(env, q.getValue()));
            i += 2;
        }
        f.assign(ret);
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
			{ "cond", static_cast<Word>(ArchitectureConstants::ConditionRegister) },
		};
		auto result = builtinAliases.find(input);
		if (result == builtinAliases.end()) {
			return syn::getRegister<Word, ArchitectureConstants::RegisterCount>(input, syn::reportError);
		} else {
			return result->second;
		}
	}
}
