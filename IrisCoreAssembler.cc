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
    AssemblerData::AssemblerData() : instruction(false), address(0), dataValue(0), group(0), operation(0), destination(0), source0(0), source1(0), hasLexeme(false), fullImmediate(false) { }
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

    AssemblerState::AssemblerState() : inData(false), temporaryWord(0), temporaryByte(0) { }
    AssemblerState::~AssemblerState() { }

	void AssemblerState::setCurrentAddress(word value) noexcept {
		if (inData) {
			data.setCurrentAddress(value);
		} else {
			code.setCurrentAddress(value);
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
		LabelTracker::registerLabel(value, getCurrentAddress());
	}
	word AssemblerState::getCurrentAddress() noexcept {
		return inData ? data.getCurrentAddress() : code.getCurrentAddress();
	}
	void AssemblerState::incrementCurrentAddress() noexcept {
		if (inData) {
			data.incrementCurrentAddress();
		} else {
			code.incrementCurrentAddress();
		}
	}
	void AssemblerState::saveToFinished() noexcept {
		current.address = getCurrentAddress();
		auto copy = current;
		addToFinishedData(copy);
		resetCurrentData();
	}
    raw_instruction AssemblerData::encode() {
        if (instruction) {
            return iris::encodeInstruction(group, operation, destination, source0, source1);
        } else {
            return dataValue;
        }
    }
    void AssemblerState::markHasLexeme() noexcept {
        current.hasLexeme = true;
    }
    void AssemblerState::markNotLexeme() noexcept {
        current.hasLexeme = false;
    }
    void AssemblerState::setLexeme(const std::string& lexeme) noexcept {
        markHasLexeme();
        current.currentLexeme = lexeme;
    }
    void AssemblerState::markIsInstruction() noexcept {
        current.instruction = true;
    }
    void AssemblerState::markIsNotInstruction() noexcept {
        current.instruction = false;
    }
    void AssemblerState::nowInCodeSection() noexcept {
        inData = false;
    }
    void AssemblerState::nowInDataSection() noexcept {
        inData = true;
    }
    void AssemblerState::setDataValue(word value) noexcept {
        current.dataValue = value;
    }
    void AssemblerState::stashTemporaryWordIntoDataValue() noexcept {
        setDataValue(getTemporaryWord());
    }
    void AssemblerState::setDestination(byte dest) noexcept {
        current.destination = dest;
    }
    void AssemblerState::setSource0(byte src0) noexcept {
        current.source0 = src0;
    }
    void AssemblerState::setSource1(byte src1) noexcept {
        current.source1 = src1;
    }
    void AssemblerState::stashTemporaryByteInDestination() noexcept {
        setDestination(temporaryByte);
    }
    void AssemblerState::stashTemporaryByteInSource0() noexcept {
        setSource0(temporaryByte);
    }
    void AssemblerState::stashTemporaryByteInSource1() noexcept {
        setSource1(temporaryByte);
    }
    void AssemblerState::setTemporaryByte(byte value) noexcept {
        temporaryByte = value;
    }
    void AssemblerState::setTemporaryWord(word value) noexcept {
        temporaryWord = value;
    }
    void AssemblerState::markHasFullImmediate() noexcept {
        current.fullImmediate = true;
    }
    void AssemblerState::markNotFullImmediate() noexcept {
        current.fullImmediate = false;
    }
} // end namespace iris
