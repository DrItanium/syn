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
		static unsigned int getExternalAddressId(Environment* env) {
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
			return static_cast<struct externalAddressHashNode*>(ptr->value)->type == getExternalAddressId(env);
		}
	private:
        /**
         * Register the environment with the corresponding type, not meant to
         * be called directly!
         * @param env the environment to register with
         * @param value the index to keep track of
         */
		static void registerExternalAddressId(Environment* env, unsigned int value) noexcept {
			_cache.emplace(env, value);
		}
		static std::map<void*, unsigned int> _cache;
};


template<typename T>
std::map<void*, unsigned int> ExternalAddressRegistrar<T>::_cache;


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
 * signature is invoked by CLIPS if registered.
 */
using CallFunction = bool(*)(UDFContext *, UDFValue*);
/**
 * When the new function is invoked within CLIPS, the function corresponding to
 * the given type with this signature is invoked. It is responsible for setting
 * up the external address type (if necessary) and such. Think of it as a
 * constructor as the memory has already been allocated.
 */
using NewFunction = void(*)(UDFContext*, UDFValue*, UDFValue*);

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
    T* invokeNewFunction(Environment* env, UDFValue* ret, const std::string& funcErrorPrefix, const std::string& function) noexcept {
        using InternalType = T;
        static_assert(std::is_constructible<InternalType>::value, "Must be constructable with no args");
        try {
            if (getArgCount(env) == 1) {
                return new InternalType();
            } else {
                errorMessage(env, "NEW", 2, funcErrorPrefix, " no arguments should be provided for function new!");
            }
        } catch (const syn::Problem& p) {
            CVSetBoolean(ret, false);
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
    CVSetBoolean(ret, false);
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
		static void setString(UDFValue* val, const std::string& str) noexcept {
			CVSetString(val, str.c_str());
		}
		static const std::string& getType() noexcept {
            return TypeToName::getSymbolicName<InternalType>();
        }
        static void setType(UDFValue* ret) noexcept {
            CVSetString(ret, getType().c_str());
        }
        static bool checkArgumentCount(Environment* env, UDFValue* ret, const std::string& operation, int inputArgCount) {
            auto aCount = baseArgumentIndex + inputArgCount;
            if (!syn::hasCorrectArgCount(env, aCount)) {
                return callErrorMessageCode3(env, ret, operation, " too many arguments provided!");
            }
            return true;
        }
		static int getCorrectArgCount(Environment* env) noexcept {
			return syn::getArgCount(env) - baseArgumentIndex;
		}
		template<typename I = int>
		static bool checkArgumentCount(Environment* env, UDFValue* ret, const std::string& operation, syn::ArgCountChecker<I> fn) {
			if (!syn::hasCorrectArgCount<I>(env, fn, [](auto count) { return count - baseArgumentIndex; })) {
				return callErrorMessageCode3(env, ret, operation, " too many or too few arguments provided!");
			}
			return true;
		}

		static unsigned int getAssociatedEnvironmentId(Environment* env) {
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
        static void newFunction(Environment* env, UDFValue* ret) {
            InternalType* ptr = WrappedNewCallBuilder::invokeNewFunction<InternalType>(env, ret, getFunctionErrorPrefixNew<InternalType>(), getFunctionPrefixNew<InternalType>());
            if (ptr) {
                using CorrespondingType = typename ExternalAddressWrapperType<T>::TheType;
                auto s = new CorrespondingType(ptr);
                CVSetExternalAddress(ret, s, Self::getAssociatedEnvironmentId(env));
            } else {
                CVSetBoolean(ret, false);
            }
        }
		static void registerWithEnvironment(Environment* env, const char* title, CallFunction _call, NewFunction _new = newFunction, DeleteFunction _delete = deleteWrapper, PrintFunction _print = printAddress) {
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
        static inline bool badCallArgument(Environment* env, UDFValue* ret, int code, const std::string& msg) noexcept {
            return syn::badCallArgument<T>(env, ret, code, msg);
        }


        static bool callErrorMessage(Environment* env, UDFValue* ret, int code, const std::string& subOp, const std::string& rest) {
            std::stringstream stm;
            stm << " " << subOp << ": " << rest << std::endl;
            auto msg = stm.str();
            return badCallArgument(env, ret, code, msg);
        }

        static inline bool callErrorMessageCode3(Environment* env, UDFValue* ret, const std::string& subOp, const std::string& rest) noexcept {
            return callErrorMessage(env, ret, 3, subOp, rest);
        }
        static inline bool callErrorMessageCode3(Environment* env, UDFValue* ret, const std::string& subOp, const char* rest) noexcept {
            return callErrorMessageCode3(env, ret, subOp, std::string(rest));
        }
        template<typename E>
        static inline bool callErrorMessageCode3(Environment* env, UDFValue* ret, const std::string& subOp, const E& problem) noexcept {
            return callErrorMessageCode3(env, ret, subOp, problem.what());
        }

        static bool tryGetArgument(Environment* env, UDFValue* ret, int pos, MayaType type) noexcept {
            return checkThenGetArgument(env, getFunctionPrefixCall<T>(), pos, type, ret);
        }
        static constexpr int baseArgumentIndex = 2;
        template<int index>
        static constexpr int getArgumentIndex() noexcept {
            static_assert(index >= 0, "No negative argument index");
            return baseArgumentIndex + index;
        }

        static bool tryExtractArgument(Environment* env, UDFValue* ret, UDFValue* storage, MayaType type, int pos, int errorCode, const std::string& msg) noexcept {
            if (!tryGetArgument(env, storage, pos, type)) {
                return badCallArgument(env, ret, errorCode, msg);
            }
            return true;
        }
        template<int index>
        static inline bool tryExtractArgument(Environment* env, UDFValue* ret, UDFValue* storage, MayaType type, int errorCode, const std::string& errorMsg) noexcept {
            return tryExtractArgument(env, ret, storage, type, getArgumentIndex<index>(), errorCode, errorMsg);
        }

        static bool tryExtractFunctionName(Environment* env, UDFValue* ret, UDFValue* storage) noexcept {
            return tryExtractArgument<0>(env, ret, storage, MayaType::Symbol, 2, "expected a function name to call!");
        }

        static inline bool tryExtractArgument1(Environment* env, UDFValue* ret, UDFValue* storage, MayaType type, const std::string& errorMsg) noexcept {
            return tryExtractArgument<1>(env, ret, storage, type, 3, errorMsg);
        }

        static inline bool tryExtractArgument2(Environment* env, UDFValue* ret, UDFValue* storage, MayaType type, const std::string& errorMsg) noexcept {
            return tryExtractArgument<2>(env, ret, storage, type, 3, errorMsg);
        }

        static inline bool tryExtractArgument3(Environment* env, UDFValue* ret, UDFValue* storage, MayaType type, const std::string& errorMsg) noexcept {
            return tryExtractArgument<3>(env, ret, storage, type, 3, errorMsg);
        }

        static inline bool tryExtractArgument4(Environment* env, UDFValue* ret, UDFValue* storage, MayaType type, const std::string& errorMsg) noexcept {
            return tryExtractArgument<4>(env, ret, storage, type, 3, errorMsg);
        }
        template<typename Thing, typename Against>
        static bool isLegalOperation(Environment* env, UDFValue* ret, const std::string& op, Thing thing, Against against) {
            if (thing == against) {
                return callErrorMessageCode3(env, ret, op, " <- unknown operation requested!");
            }
            return true;
        }
        static bool isExternalAddress(Environment* env, UDFValue* ret, UDFValue* value) noexcept {
            if (!syn::isExternalAddress(value)) {
                return badCallArgument(env, ret, 1, "Function call expected an external address as the first argument!");
            }
            return true;
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


}
#endif
