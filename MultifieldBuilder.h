/**
 * @file
 * Wrappers for multifield related types and manipulations
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


#ifndef __MULTIFIELD_BUILDER_H
#define __MULTIFIELD_BUILDER_H
#include <map>
#include <memory>
#include <sstream>
#include <iostream>
#include <typeinfo>
#include "BaseArithmetic.h"
#include "Problem.h"
#include "ClipsExtensions.h"
#include "ExternalAddressWrapper.h"

extern "C" {
	#include "clips.h"
}

namespace syn {
/// Wrapper over the CLIPS data objet type

using MultifieldCell = std::tuple<MayaType, void*>;

MultifieldCell makeSymbol(void* env, const char* value);
MultifieldCell makeSymbol(void* env, const std::string& value);
MultifieldCell makeString(void* env, const char* value);
MultifieldCell makeString(void* env, const std::string& value);
MultifieldCell makeInteger(void* env, CLIPSInteger value);
MultifieldCell makeFloat(void* env, double value);
template<typename T>
MultifieldCell makeExternalAddress(void* env, T* value) {
	static_assert(TypeToName::hasSymbolicImplementation<T>, "Given external address type is not registered!");
	return std::make_tuple(MayaType::ExternalAddress, EnvAddExternalAddress(env, value, ExternalAddressRegistrar<T>::getExternalAddressId(env)));
}

/**
 * Class which makes building multifields much easier. It is a wrapper class so
 * if it goes out of scope then the underlying raw multifield will not be
 * destroyed.
 */
class MultifieldBuilder {
    public:
        /**
         * construct a new multifield builder
         * @param env the environment where the multifield will be installed
         * @param capacity the number of elements in the multifield
         */
        MultifieldBuilder(void* env, long capacity);
        virtual ~MultifieldBuilder() noexcept { }
        /**
         * set the given cell to the given type and corresponding value
         */
        void setField(int index, int type, void* value);

        /**
         * set the given cell to the give wrapped type and corresponding value
         */
        void setField(int index, MayaType type, void* value);
        /// retrieve the number of elements in the multifield
        long getSize() const noexcept { return _size; }
        /// get the actual multifield pointer itself
        void* getRawMultifield() const noexcept { return _rawMultifield; }
		/// Set the given index with the given cell, used for custom types
		void setField(int index, MultifieldCell cell);
		/// Set the given cell as an integer
		void setField(int index, CLIPSInteger value);
		void setField(int index, uint32_t value) { setField(index, CLIPSInteger(value)); }
		void setField(int index, int32_t value) { setField(index, CLIPSInteger(value)); }
		void setField(int index, uint16_t value) { setField(index, CLIPSInteger(value)); }
		void setField(int index, int16_t value) { setField(index, CLIPSInteger(value)); }
		/// Set the given cell as a float64 value
		void setField(int index, double value);
		/// Set the given cell as a string 
		void setField(int index, const char* value);
		/// Set the given cell as a string 
		void setField(int index, const std::string& value);

        /**
         * install the multifield into a data object pointer.
         * Use this method when extracting the data out, it does more than just
         * assignment.
         * @param ptr the data object which will contain the multifield
         */
        void assign(DataObjectPtr ptr) noexcept;
    private:
        long _size;
        void* _rawMultifield;
		void* _env;
};



/**
 * A wrapper class that allows construction of multifields with compile-time
 * checks. Use this class if the number of elements that make up a multifield
 * will always be the same.
 * @tparam capacity the number of elements that will make up the multifield
 */
template<long capacity>
class FixedSizeMultifieldBuilder {
    public:
        using CapacityType = long;
    public:
        FixedSizeMultifieldBuilder(void* env) noexcept : _rawMultifield(EnvCreateMultifield(env, capacity)), _env(env) { }
        virtual ~FixedSizeMultifieldBuilder() noexcept { }
        static constexpr CapacityType getSize() noexcept { return capacity; }
        void* getRawMultifield() const noexcept { return _rawMultifield; }
        void assign(DataObjectPtr ptr) noexcept {
            ptr->type = MULTIFIELD;
            ptr->begin = 0;
            ptr->end = capacity - 1;
            ptr->value = _rawMultifield;
        }

        void setField(int index, int type, void* value) {
            if (index <= 0) {
                throw syn::Problem("Can't set a value to a field with a negative index!");
            } else if (index > capacity) {
                throw syn::Problem("Attempted to set a field which was out of range of the multifield!");
            } else {
                SetMFType(_rawMultifield, index, type);
                SetMFValue(_rawMultifield, index, value);
            }
        }

        void setField(int index, MayaType type, void* value) {
            setField(index, static_cast<int>(type), value);
        }

        template<int index>
        void setField(int type, void* value) noexcept {
            static_assert(index > 0, "Negative index or zero index not allowed!");
            static_assert(index <= capacity, "Provided index is out of range!");
            SetMFType(_rawMultifield, index, type);
            SetMFValue(_rawMultifield, index, value);
        }
        template<int index>
        void setField(MayaType type, void* value) noexcept {
            setField<index>(static_cast<int>(type), value);
        }
        template<int index, MayaType type>
        void setField(void* value) noexcept {
            setField<index>(type, value);
        }

		template<int index>
		void setField(MultifieldCell pair) {
			setField<index>(std::get<0>(pair), std::get<1>(pair));
		}

		template<typename T>
		MultifieldCell build(T* value) {
			return makeExternalAddress(_env, value);
		}
		MultifieldCell build(const std::string& value) { return makeString(_env, value); }
		MultifieldCell build(const char* value) { return makeString(_env, value); }
		MultifieldCell build(CLIPSInteger value) { return makeInteger(_env, value); }
		MultifieldCell build(double value) { return makeFloat(_env, value); }
		MultifieldCell build(MultifieldCell value) noexcept { return value; }

		template<int index, typename T>
		void setField(T value) {
			setField<index>(build(value));
		}


		template<typename T, typename ... Rest>
		void setField(T external, Rest ... rest) {
			setField<capacity - sizeof...(Rest), T>(external);
			setField(rest...);
		}
		// do nothing since we have no arguments to speak of!
		void setField() { }

    private:
        void* _rawMultifield;
		void* _env;
};

template<typename... Arguments>
void createMultifield(void* env, DataObjectPtr storage, Arguments... args) {
	FixedSizeMultifieldBuilder<sizeof...(Arguments)> tmp(env);
	tmp.setField(args...);
	tmp.assign(storage);
}

void emptyMultifield(void* env, DataObjectPtr storage);

}
#endif // end __MULTIFIELD_BUILDER_H
