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


#ifndef __IRIS_CLIPS_H
#define __IRIS_CLIPS_H
#include "Base.h"
#include "Problem.h"
#include <map>
#include <memory>
#include <sstream>
extern "C" {
	#include "clips.h"
}

namespace syn {
using DataObject = DATA_OBJECT;
using DataObjectPtr = DATA_OBJECT_PTR;
void installExtensions(void* theEnv);

template<typename T>
struct ExternalAddressRegistrar {
	public:
		ExternalAddressRegistrar() = delete;
		~ExternalAddressRegistrar() = delete;
		ExternalAddressRegistrar(const ExternalAddressRegistrar&) = delete;
		ExternalAddressRegistrar(ExternalAddressRegistrar&&) = delete;
		static unsigned int getExternalAddressId(void* env) {
			auto found = _cache.find(env);
			if (found != _cache.end()) {
				return found->second;
			} else {
				throw syn::Problem("unregistered external address type!");
			}
		}
		static void registerExternalAddressId(void* env, unsigned int value) noexcept {
			_cache.emplace(env, value);
		}
		static bool isOfType(void* env, DataObjectPtr ptr) {
			return static_cast<struct externalAddressHashNode*>(ptr->value)->type == getExternalAddressId(env);
		}
	private:
		static std::map<void*, unsigned int> _cache;
};


template<typename T>
std::map<void*, unsigned int> ExternalAddressRegistrar<T>::_cache;

template<typename T>
inline constexpr bool inRange(T capacity, T address) noexcept {
	return address >= 0 && address < capacity;
}

namespace TypeToName {
    template<typename T>
    constexpr bool hasSymbolicImplementation() noexcept {
        return false;
    }
    template<typename T>
    std::string getSymbolicName() noexcept {
        static_assert(hasSymbolicImplementation<T>(), "Provided type does not have a symbolic name");
        return "";
    }

}
#define DefWrapperSymbolicName(t, name) \
    namespace TypeToName { \
        template<> constexpr bool hasSymbolicImplementation < t > () noexcept { return true; } \
        template<> std::string getSymbolicName < t > () noexcept { return name; } \
    }

void CLIPS_basePrintAddress(void* env, const char* logicalName, void* theValue, const char* func, const char* majorType);

template<typename T>
void CLIPS_basePrintAddress(void* env, const char* logicalName, void* theValue) {
	static bool init = true;
	static std::string func;
	if (init) {
		init = false;
		func = TypeToName::getSymbolicName<T>();
	}
	CLIPS_basePrintAddress(env, logicalName, theValue, func.c_str(), "Wrapper");
}

bool errorMessage(void* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept;

// Have to do it this way because std::function's will go out of scope and
// everything breaks
typedef void PrintFunction(void*, const char*, void*);
typedef bool DeleteFunction(void*, void*);
typedef bool CallFunction(void*, DataObjectPtr, DataObjectPtr);
typedef void NewFunction(void*, DataObjectPtr);


template<typename T>
struct WrappedNewCallBuilder {
    WrappedNewCallBuilder() = delete;
    ~WrappedNewCallBuilder() = delete;
    WrappedNewCallBuilder(const WrappedNewCallBuilder&) = delete;
    WrappedNewCallBuilder(WrappedNewCallBuilder&&) = delete;
    // by default, any wrapped IO device can accept zero arguments
    static T* invokeNewFunction(void* env, CLIPSValuePtr ret, const std::string& funcErrorPrefix, const std::string& function) noexcept {
        using InternalType = T;
        try {
            if (EnvRtnArgCount(env) == 1) {
                return new InternalType();
            } else {
                errorMessage(env, "NEW", 2, funcErrorPrefix, " no arguments should be provided for function new!");
            }
        } catch (syn::Problem p) {
            CVSetBoolean(ret, false);
            std::stringstream s;
            s << "an exception was thrown: " << p.what();
            auto str = s.str();
            errorMessage(env, "NEW", 2, funcErrorPrefix, str);
        }
        return nullptr;
    }
};
using FunctionStrings = std::tuple<std::string, std::string, std::string>;
template<typename T>
static FunctionStrings retrieveFunctionNames(const std::string& action) noexcept {
    auto title = TypeToName::getSymbolicName<T>();
    std::stringstream ss, ss2;
    ss << action << " (" << title << ")";
    auto str0 = ss.str();
    ss2 << "Function " << str0;
    auto str1 = ss2.str();
    return std::make_tuple(title, str0, str1);
}
template<typename T>
class ExternalAddressWrapper {
	public:
		using InternalType = T;
		using BaseClass = ExternalAddressWrapper<T>;
        using Self = BaseClass;
		static std::string getType() noexcept { return TypeToName::getSymbolicName<InternalType>(); }
		static unsigned int getAssociatedEnvironmentId(void* env) { return ExternalAddressRegistrar<InternalType>::getExternalAddressId(env); }
		static void registerWithEnvironment(void* env, externalAddressType* description) noexcept {
			ExternalAddressRegistrar<InternalType>::registerExternalAddressId(env, InstallExternalAddressType(env, description));
		}
		static void printAddress(void* env, const char* logicalName, void* theValue) {
			CLIPS_basePrintAddress<InternalType>(env, logicalName, theValue);
		}
		static bool deleteWrapper(void* env, void* obj) {
			if (obj != nullptr) {
				auto result = static_cast<ExternalAddressWrapper<T>*>(obj);
				delete result;
			}
			return true;
		}
        static void newFunction(void* env, CLIPSValue* ret) {
            static auto init = true;
            static std::string funcStr;
            static std::string funcErrorPrefix;
            if (init) {
                init = false;
                auto functions = retrieveFunctionNames<InternalType>("new");
                funcStr = std::get<1>(functions);
                funcErrorPrefix = std::get<2>(functions);
            }
            auto ptr = WrappedNewCallBuilder<InternalType>::invokeNewFunction(env, ret, funcErrorPrefix, funcStr);
            if (ptr) {
                auto idIndex = Self::getAssociatedEnvironmentId(env);
                ret->bitType = EXTERNAL_ADDRESS_TYPE;
                SetpType(ret, EXTERNAL_ADDRESS);
                SetpValue(ret, EnvAddExternalAddress(env, new Self(ptr), idIndex));
            } else {
                CVSetBoolean(ret, false);
            }
        }
		static void registerWithEnvironment(void* env, const char* title, CallFunction _call, NewFunction _new = newFunction, DeleteFunction _delete = deleteWrapper, PrintFunction _print = printAddress) {
			externalAddressType tmp = {
				title,
				_print,
				_print,
				_delete,
				_new,
				_call,
			};
			registerWithEnvironment(env, &tmp);
		}
		static bool isOfType(void* env, DataObjectPtr ptr) noexcept {
			return ExternalAddressRegistrar<InternalType>::isOfType(env, ptr);
		}
	public:
		ExternalAddressWrapper(std::unique_ptr<T>&& value) : _value(std::move(value)) { }
        ExternalAddressWrapper(T* ptr) : _value(std::move(std::unique_ptr<T>(ptr))) { }
		inline T* get() const noexcept { return _value.get(); }
        T* operator->() const noexcept { return get(); }
	protected:
		std::unique_ptr<T> _value;
};

class MultifieldBuilder {
    public:
        MultifieldBuilder(void* env, long capacity);
        virtual ~MultifieldBuilder() noexcept { }
        void setField(int index, int type, void* value);
        long getSize() const noexcept { return _size; }
        void* getRawMultifield() const noexcept { return _rawMultifield; }
        void assign(DataObjectPtr ptr) noexcept;
    private:
        long _size;
        void* _rawMultifield;
};

template<long capacity>
class FixedSizeMultifieldBuilder {
    public:
        using CapacityType = long;
    public:
        FixedSizeMultifieldBuilder(void* env) noexcept : _rawMultifield(EnvCreateMultifield(env, capacity)) { }
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
        template<int index>
        void setField(int type, void* value) noexcept {
            static_assert(index > 0, "Negative index or zero index not allowed!");
            static_assert(index <= capacity, "Provided index is out of range!");
            SetMFType(_rawMultifield, index, type);
            SetMFValue(_rawMultifield, index, value);
        }
        void setFirst(int type, void* value) noexcept {
            setField<1>(type, value);
        }
        void setSecond(int type, void* value) noexcept {
            setField<2>(type, value);
        }
    private:
        void* _rawMultifield;
};


}
#endif
