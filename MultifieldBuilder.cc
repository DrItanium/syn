/**
 * @file
 * implementation of a MultifieldBuilder
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


#include "BaseTypes.h"
#include "ClipsExtensions.h"
#include "Base.h"
#include "ExecutionUnits.h"

#include <cstdint>
#include <climits>
#include <sstream>
#include <memory>
#include <map>
#include <iostream>

extern "C" {
#include "clips.h"
}

namespace syn {

    MultifieldBuilder::MultifieldBuilder(void* env, long capacity) : _size(capacity), _rawMultifield(EnvCreateMultifield(env, capacity)), _env(env) { }

    void MultifieldBuilder::setField(int index, int type, void* value) {
        if (index <= 0) {
            throw syn::Problem("Can't set a value to a field with a negative index!");
        } else if (index > _size) {
            throw syn::Problem("Attempted to set a field which was out of range of the multifield!");
        }
        SetMFType(_rawMultifield, index, type);
        SetMFValue(_rawMultifield, index, value);
    }
    void MultifieldBuilder::setField(int index, MayaType type, void* value) {
        setField(index, static_cast<int>(type), value);
    }
	void MultifieldBuilder::assign(DataObjectPtr ptr) noexcept {
		ptr->type = MULTIFIELD;
		ptr->begin = 0;
		ptr->end = _size - 1;
		ptr->value = _rawMultifield;
	}

	void MultifieldBuilder::setField(int index, CLIPSInteger value) {
		setField(index, MayaType::Integer, EnvAddLong(_env, value));
	}

	void MultifieldBuilder::setField(int index, double value) {
		setField(index, MayaType::Float, EnvAddDouble(_env, value));
	}

	void MultifieldBuilder::setField(int index, const char* value) {
		setField(index, MayaType::String, EnvAddSymbol(_env, value));
	}

	void MultifieldBuilder::setField(int index, const std::string& value) {
		setField(index, value.c_str());
	}

	void MultifieldBuilder::setField(int index, MultifieldCell cell) {
		setField(index, std::get<0>(cell), std::get<1>(cell));
	}

}
