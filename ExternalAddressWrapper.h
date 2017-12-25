/**
 * @file
 * External address wrapper class
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


#ifndef EXTERNAL_ADDRESS_WRAPPER_H__
#define EXTERNAL_ADDRESS_WRAPPER_H__
#include <map>
#include <memory>
#include <sstream>
#include <iostream>
#include <typeinfo>
#include "BaseArithmetic.h"
#include "Problem.h"
extern "C" {
	#include "clips.h"
}
#include "ClipsExtensions.h"

namespace syn {


/**
 * Tracks all of the environments that the associated type is registered with
 * as an external address. When InstallExternalAddressType is called, it
 * returns an integer which is the external address id. Normally this requires
 * extra tracking by the program to check the given type during user defined
 * functions. With this class, the backing map keeps track of the association.
 * @tparam T The type that is to be registered with environments.
 */
template<typename T>
struct ExternalAddressRegistrar {
	public:
		ExternalAddressRegistrar() = delete;
		~ExternalAddressRegistrar() = delete;
		ExternalAddressRegistrar(const ExternalAddressRegistrar&) = delete;
		ExternalAddressRegistrar(ExternalAddressRegistrar&&) = delete;
        /**
         * Retrieve the index associated with the given environment
         * @param env the clips environment to check
         * @throw syn::Problem the given type is not registered with the
         * provided environment.
         * @return the index of the target type in the given environment
         */
		static unsigned short getExternalAddressId(Environment* env) {
			auto found = _cache.find(env);
            if (found == _cache.end()) {
                throw syn::Problem("unregistered external address type!");
            }
            return found->second;
		}
        /**
         * install the described externalAddressType into the target
         * environment; then register the index with the backing map.
         * @param env the environment to install the external type into
         * @param type the externalAddressType description to install
         */
        static void registerExternalAddress(Environment* env, externalAddressType* type) noexcept {
            registerExternalAddressId(env, InstallExternalAddressType(env, type));
        }
        /**
         * See if the given UDFValue* is of this external address type
         * @param env the environment to query
         * @param ptr the UDFValue* that may be of the desired type
         * @return true if the given data object is of the correct external
         * address type.
         */
		static bool isOfType(Environment* env, UDFValue* ptr) {
			return ptr->externalAddressValue->type == getExternalAddressId(env);
		}
	private:
        /**
         * Register the environment with the corresponding type, not meant to
         * be called directly!
         * @param env the environment to register with
         * @param value the index to keep track of
         */
		static void registerExternalAddressId(Environment* env, unsigned short value) noexcept {
			_cache.emplace(env, value);
		}
		static std::map<void*, unsigned short> _cache;
};


template<typename T>
std::map<void*, unsigned short> ExternalAddressRegistrar<T>::_cache;


/**
 * Provides the ability to bind a type to a string.
 */
namespace TypeToName {
    /**
     * Describes if there is a string associated with the given type.
     * By default, an unspecialized implementation of this will return false
     * @tparam T the type of the thing that may have a string associated with
     * it.
     */
    template<typename T>
    constexpr bool hasSymbolicImplementation = false;
    /**
     * retrieve the symbolic name of the given type if possible. If the
     * provided type does not have a symbolic implementation then the program
     * will _NOT_ compile.
     */
    template<typename T>
    const std::string& getSymbolicName() noexcept {
        static_assert(hasSymbolicImplementation<T>, "Provided type does not have a symbolic name");
		static std::string empty;
		return empty;
    }

}
#define DefWrapperSymbolicName(t, name) \
    namespace TypeToName { \
        template<> constexpr bool hasSymbolicImplementation < t > = true; \
        template<> const std::string& getSymbolicName < t > () noexcept { \
			static std::string _tmp{ name } ; \
			return _tmp; \
		} \
    }

/**
 * Uses the symbolic implementation of the specified type when printing from
 * CLIPS. This is the method that should be used when installing external
 * address types. Once the name of the type is extracted once, it is cached for
 * future uses and should be super fast. This should be used unless you have
 * really specific printing requirements.
 * @param env the environment that requested the printing
 * @param logicalName the io router to output to
 * @param theValue the raw value itself to print
 * @tparam T the type to grab the symbolic implementation from
 */
template<typename T>
void CLIPS_basePrintAddress(Environment* env, const char* logicalName, void* theValue) {
	CLIPS_basePrintAddress(env, logicalName, theValue, TypeToName::getSymbolicName<T>().c_str(), "Wrapper");
}


template<typename T>
class ExternalAddressWrapper;

template<typename T>
struct ExternalAddressWrapperType {
    ExternalAddressWrapperType() = delete;
    ~ExternalAddressWrapperType() = delete;
    ExternalAddressWrapperType(const ExternalAddressWrapperType&) = delete;
    ExternalAddressWrapperType(ExternalAddressWrapperType&&) = delete;
    using TheType = ExternalAddressWrapper<T>;
	static constexpr bool customImpl = false;
};

#define DefExternalAddressWrapperType(internalType, theWrapperType) \
    template<> \
    struct ExternalAddressWrapperType< internalType > { \
        using Self = ExternalAddressWrapperType< internalType >; \
        using TheType = theWrapperType ; \
		static constexpr bool customImpl = true; \
        ExternalAddressWrapperType() = delete; \
        ~ExternalAddressWrapperType() = delete; \
        ExternalAddressWrapperType(const ExternalAddressWrapperType < internalType > &) = delete; \
        ExternalAddressWrapperType(ExternalAddressWrapperType< internalType >&&) = delete; \
    }

// Have to do it this way because std::function's will go out of scope and
// everything breaks
/**
 * Signature for printing external address types
 */
using PrintFunction = void (*)(Environment*, const char*, void*);
/**
 * When an external address goes out of scope and is reclaimed, a function of
 * this form is called to handle cleanup.
 */
using DeleteFunction = bool(*)(Environment*, void*);
/**
 * When the call function is invoked within CLIPS, a function with this
 * signature is invoked by CLIPS if registered. The first argument is the
 * context, the second is the external address itself, the third value is the
 * return result from the execution.
 */
using CallFunction = bool(*)(UDFContext*, UDFValue*, UDFValue*);

/**
 * When the new function is invoked within CLIPS, the function corresponding to
 * the given type with this signature is invoked. It is responsible for setting
 * up the external address type (if necessary) and such. Think of it as a
 * constructor as the memory has already been allocated.
 */
using NewFunction = void(*)(UDFContext *, UDFValue*);

/**
 * Makes building new functions much easier!
 */
namespace WrappedNewCallBuilder {
    /**
     * Provides a default new implementation where construction of the provided
     * type requires no arguments.
     * @param env the environment that requested the new
     * @param ret the container to store errors in
     * @param funcErrorPrefix the name of the invoking function with added
     * decoration, used when an error occurs.
     * @param function the name of the function itself
     * @tparam T the type of the thing to allocate
     * @return a newly allocated thing of type T
     */
    template<typename T>
    T* invokeNewFunction(Environment* env, UDFContext* context, UDFValue* ret, const std::string& funcErrorPrefix, const std::string& function) noexcept {
        using InternalType = T;
        static_assert(std::is_constructible<InternalType>::value, "Must be constructable with no args");
        try {
			if (!UDFHasNextArgument(context)) {
				return new InternalType();
			} else {
				// TODO: put an error message here
			}
        } catch (const syn::Problem& p) {
			setBoolean(env, ret, false);
            std::stringstream s;
            s << "an exception was thrown: " << p.what();
            auto str = s.str();
            errorMessage(env, "NEW", 2, funcErrorPrefix, str);
        }
        return nullptr;
    }
}

void buildFunctionErrorString(std::ostream& stream, const std::string& action, const std::string& name) noexcept;

void buildFunctionString(std::ostream& stream, const std::string& action, const std::string& name) noexcept;

template<typename T>
const std::string& getFunctionErrorPrefixCall() noexcept {
    static bool init = true;
    static std::string str;
    if (init) {
        init = false;
        std::stringstream ss;
        buildFunctionErrorString(ss, "call", TypeToName::getSymbolicName<T>());
        str = ss.str();
    }
    return str;
}

template<typename T>
const std::string& getFunctionErrorPrefixNew() noexcept {
    static bool init = true;
    static std::string str;
    if (init) {
        init = false;
        std::stringstream ss;
        buildFunctionErrorString(ss, "new", TypeToName::getSymbolicName<T>());
        str = ss.str();
    }
    return str;
}

template<typename T>
const std::string& getFunctionPrefixCall() noexcept {
    static bool init = true;
    static std::string str;
    if (init) {
        init = false;
        std::stringstream ss;
        buildFunctionString(ss, "call", TypeToName::getSymbolicName<T>());
        str = ss.str();
    }
    return str;
}

template<typename T>
const std::string& getFunctionPrefixNew() noexcept {
    static bool init = true;
    static std::string str;
    if (init) {
        init = false;
        std::stringstream ss;
        buildFunctionString(ss, "new", TypeToName::getSymbolicName<T>());
        str = ss.str();
    }
    return str;
}


/**
 * Generic method that is used to generate an error message if the given
 * argument when the call function is invoked.
 * @param env the environment where the badCallArgument happened
 * @param ret the return value to stash the FALSE into
 * @param code the error code that clips uses during error message formation
 * @param msg the error message itself
 * @tparam T the external address type
 */
template<typename T>
bool badCallArgument(Environment* env, UDFValue* ret, int code, const std::string& msg) noexcept {
	setBoolean(env, ret, false);
    return syn::errorMessage(env, "CALL", code, getFunctionErrorPrefixCall<T>(), msg);
}

/**
 * A class which simplifies the act of interfacing C++ types with CLIPS. It
 * provides default implementations for printing, registering, errorReporting,
 * and other common operations.
 * @tparam T the type that is wrapped by this class
 */
template<typename T>
class ExternalAddressWrapper {
	public:
		using InternalType = T;
		using BaseClass = ExternalAddressWrapper<T>;
        using Self = BaseClass;
		static const std::string& getType() noexcept {
            return TypeToName::getSymbolicName<InternalType>();
        }
        static void setType(Environment* env, UDFValue* ret) noexcept {
			setString(env, ret, getType());
        }
		static inline void setType(UDFContext* context, UDFValue* ret) noexcept {
			setType(context->environment, ret);
		}

		static int getAssociatedEnvironmentId(Environment* env) {
            return ExternalAddressRegistrar<InternalType>::getExternalAddressId(env);
        }
		static void registerWithEnvironment(Environment* env, externalAddressType* description) noexcept {
			ExternalAddressRegistrar<InternalType>::registerExternalAddress(env, description);
		}
		static void printAddress(Environment* env, const char* logicalName, void* theValue) {
			CLIPS_basePrintAddress<InternalType>(env, logicalName, theValue);
		}
		static bool deleteWrapper(Environment* env, void* obj) {
			if (obj != nullptr) {
				auto result = static_cast<typename ExternalAddressWrapperType<T>::TheType*>(obj);
				delete result;
			}
			return true;
		}
        static void newFunction(UDFContext* context, UDFValue* ret) {
			newFunctionWithEnvironment(context->environment, context, ret);
		}
		static void newFunctionWithEnvironment(Environment* env, UDFContext* context, UDFValue* ret) {
            InternalType* ptr = WrappedNewCallBuilder::invokeNewFunction<InternalType>(env, context, ret, getFunctionErrorPrefixNew<InternalType>(), getFunctionPrefixNew<InternalType>());
            if (ptr) {
                using CorrespondingType = typename ExternalAddressWrapperType<T>::TheType;
                auto s = new CorrespondingType(ptr);
				ret->externalAddressValue = CreateExternalAddress(env, s, Self::getAssociatedEnvironmentId(env));
            } else {
				setBoolean(env, ret, false);
            }
        }
		static void registerWithEnvironment(Environment* env, const char* title, CallFunction _call, NewFunction _new = newFunction, DeleteFunction _delete = deleteWrapper, PrintFunction _print = nullptr) {
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
		static bool isOfType(Environment* env, UDFValue* ptr) noexcept {
			return ExternalAddressRegistrar<InternalType>::isOfType(env, ptr);
		}

	public:
		ExternalAddressWrapper(std::unique_ptr<T>&& value) : _value(std::move(value)) { }
        ExternalAddressWrapper(T* ptr) : _value(std::move(std::unique_ptr<T>(ptr))) { }
        template<typename ... Args>
        ExternalAddressWrapper(Args ... args) : _value(std::move(std::make_unique<T>(args...))) { }
		virtual ~ExternalAddressWrapper() { }
		inline T* get() const noexcept { return _value.get(); }
        T* operator->() const noexcept { return get(); }
	protected:
		std::unique_ptr<T> _value;
};

/**
 * Extract the function name (symbol) for a call operation.
 */
inline bool extractFunctionName(UDFContext* context, UDFValue& storage) noexcept {
	return UDFNextArgument(context, MayaType::SYMBOL_BIT, &storage);
}


}
#endif
