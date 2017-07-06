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


#include "Cisc0Core.h"
#include <functional>
#include <sstream>
#include "Problem.h"
#include <utility>
#include <map>
#include "Cisc0ClipsExtensions.h"

namespace cisc0 {

    void Core::incrementAddress(RegisterValue& ptr) noexcept {
        ++ptr;
    }
    void Core::decrementAddress(RegisterValue& ptr) noexcept {
        --ptr;
    }
    void Core::incrementInstructionPointer() noexcept {
        incrementAddress(getInstructionPointer());
    }

    void illegalInstruction(const DecodedInstruction& current, RegisterValue ip) {
        std::stringstream str;
        str << "Illegal instruction " << std::hex << static_cast<int>(current.getControl()) << std::endl;
        str << "Location: " << std::hex << ip << std::endl;
        auto s = str.str();
        throw syn::Problem(s);
    }


    void Core::pushWord(Word value) {
		pushWord(value, getStackPointer());
    }
	void Core::pushWord(Word value, RegisterValue& sp) {
		decrementAddress(sp);
		storeWord(sp, value);
	}
    void Core::pushRegisterValue(RegisterValue value) {
		pushRegisterValue(value, getStackPointer());
    }

	void Core::pushRegisterValue(RegisterValue value, RegisterValue& sp) {
		pushWord(decodeUpperHalf(value), sp);
		pushWord(decodeLowerHalf(value), sp);
	}

    Word Core::popWord() {
		return popWord(getStackPointer());
    }
	Word Core::popWord(RegisterValue& sp) {
		auto result = loadWord(sp);
		incrementAddress(sp);
		return result;
	}
    RegisterValue Core::popRegisterValue(RegisterValue& sp) {
        auto lower = popWord(sp);
        auto upper = popWord(sp);
        return encodeRegisterValue(upper, lower);
    }
	RegisterValue Core::popRegisterValue() {
		return popRegisterValue(getStackPointer());
	}
}
