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
#include "IrisCoreAssembler.h"

namespace iris {
	void AssemblerData::reset() noexcept {
		instruction = false;
		address = 0;
		dataValue = 0;
		group = 0;
		operation = 0;
		destination = 0;
		source0 = 0;
		source1 = 0;
		hasLexeme = false;
		currentLexeme.clear();
		fullImmediate = false;
	}


	void AssemblerState::setCurrentAddress(word value) noexcept {
		if (inData) {
			currentDataIndex = value;
		} else {
			currentCodeIndex = value;
		}
	}
	void AssemblerState::setImmediate(word value) noexcept {
		current.setImmediate(value);
	}
	void AssemblerData::setImmediate(word value) noexcept {
		source0 = syn::getLowerHalf<word>(value);
		source1 = syn::getUpperHalf<word>(value);
	}
	void AssemblerState::setGroup(InstructionGroup value) noexcept {
		current.group = static_cast<byte>(value);
	}
	void AssemblerState::setHalfImmediate(byte value) noexcept {
		current.source1 = value;
	}

	void AssemblerState::resetCurrentData() noexcept {
		current.reset();
	}

	void AssemblerState::registerLabel(const std::string& value) noexcept {
		labelMap.emplace(value, getCurrentAddress());
	}
	word AssemblerState::getCurrentAddress() noexcept {
		return inData ? currentDataIndex : currentCodeIndex;
	}
	void AssemblerState::incrementCurrentAddress() noexcept {
		if (inData) {
			++currentDataIndex;
		} else {
			++currentCodeIndex;
		}
	}
	void AssemblerState::saveToFinished() noexcept {
		current.address = getCurrentAddress();
		auto copy = current;
		finishedData.emplace_back(copy);
		resetCurrentData();
	}
    raw_instruction AssemblerData::encode() {
        if (instruction) {
            return iris::encodeInstruction(group, operation, destination, source0, source1);
        } else {
            return dataValue;
        }
    }
	void resolveLabels(AssemblerState& state, std::ostream& output) {
		// now that we have instructions, we need to print them out as hex values
		char buf[8] = { 0 };
		auto resolveLabel = [&state](AssemblerData& data) {
			auto result = state.findLabel(data.currentLexeme);
			if (result == state.endLabel()) {
				std::stringstream msg;
				msg << "ERROR: label " << data.currentLexeme << " is undefined!" << std::endl;
				auto str = msg.str();
				throw syn::Problem(str);
			} else {
				return result->second;
			}
		};
		state.applyToFinishedData([&buf, &output, resolveLabel](auto value) {
					buf[0] = 0;
					buf[1] = value.instruction ? 0 : 1;
					buf[2] = static_cast<char>(syn::getLowerHalf<word>(value.address));
					buf[3] = static_cast<char>(syn::getUpperHalf<word>(value.address));
					if (value.instruction) {
						buf[4] = static_cast<char>(iris::encodeOperationByte(iris::encodeGroupByte(0, value.group), value.operation));
						buf[5] = static_cast<char>(value.destination);
						if (value.shouldResolveLabel()) {
							value.setImmediate(resolveLabel(value));
						}
					} else {
						if (value.shouldResolveLabel()) {
							value.dataValue = resolveLabel(value);
						}
						buf[4] = syn::getLowerHalf<word>(value.dataValue);
						buf[5] = syn::getUpperHalf<word>(value.dataValue);
					}
					buf[6] = value.instruction ? static_cast<char>(value.source0) : 0;
					buf[7] = value.instruction ? static_cast<char>(value.source1) : 0;
					output.write(static_cast<char*>(buf), sizeof(buf));
				});
	}

	void assemble(const std::string& iName, FILE* input, std::ostream* output) {
		iris::AssemblerState state;
    	pegtl::analyze<iris::Main>();
		// put a sufficently large amount of space to read from the cstream
		pegtl::parse_cstream<iris::Main, iris::Action>(input, iName.c_str(), 16777216, state);
		resolveLabels(state, *output);
	}

} // end namespace iris
