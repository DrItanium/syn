/**
 * @file
 * Types, functions, classes, and concepts for making it far easier to
 * interface with CLIPS from c++.
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


#ifndef __SYN_CLIPS_H
#define __SYN_CLIPS_H
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

namespace syn {
/// Wrapper over the CLIPS data objet type
using DataObject = DATA_OBJECT;
/// Wrapper over the CLIPS data object pointer type
using DataObjectPtr = DATA_OBJECT_PTR;

/**
 * Install extended user functions to make life easier.
 * @param theEnv the environment to install the extended user functions into
 */
void installExtensions(void* theEnv);

/**
 * A wrapper enum for interfacing with CLIPS' constants
 */
enum class MayaType {
    Integer = INTEGER,
    Float = FLOAT,
    ExternalAddress = EXTERNAL_ADDRESS,
    Symbol = SYMBOL,
    String = STRING,
    Lexeme = SYMBOL_OR_STRING,
    Multifield = MULTIFIELD,
};

/**
 * retrieves the argument count of the function call originating in CLIPS.
 * @param env the environment where the function call took place
 * @return the number of arguments that were passed
 */
int getArgCount(void* env) noexcept;
/**
 * performs a check to see if the number of arguments equals the expected count
 * @param env the environment to check
 * @param compare the expected number of args
 * @return true if compare equals the actual argument count
 */
bool hasCorrectArgCount(void* env, int compare) noexcept;

/**
 * Return true if the given dataObjetPtr is tagged as an ExternalAddress type
 * @param value the dataObjectPtr to check
 * @return true if the given dataObjectPtr contains an external address
 */
bool isExternalAddress(DataObjectPtr value) noexcept;

CLIPSInteger extractLong(void* env, DataObjectPtr value) noexcept;
CLIPSInteger extractLong(void* env, DataObject& value) noexcept;
template<typename Ret>
Ret extractLong(void* env, DataObjectPtr value) noexcept {
    return static_cast<Ret>(extractLong(env, value));
}

template<typename Ret>
Ret extractLong(void* env, DataObject& value) noexcept {
    return static_cast<Ret>(extractLong(env, value));
}

const char* extractLexeme(void* env, DataObjectPtr value) noexcept;
const char* extractLexeme(void* env, DataObject& value) noexcept;

bool checkThenGetArgument(void* env, const std::string& function, int position, MayaType type, DataObjectPtr saveTo) noexcept;
bool tryGetArgumentAsInteger(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept;
bool tryGetArgumentAsSymbol(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept;
bool tryGetArgumentAsString(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept;


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
		static unsigned int getExternalAddressId(void* env) {
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
        static void registerExternalAddress(void* env, externalAddressType* type) noexcept {
            registerExternalAddressId(env, InstallExternalAddressType(env, type));
        }
        /**
         * See if the given DataObjectPtr is of this external address type
         * @param env the environment to query
         * @param ptr the DataObjectPtr that may be of the desired type
         * @return true if the given data object is of the correct external
         * address type.
         */
		static bool isOfType(void* env, DataObjectPtr ptr) {
			return static_cast<struct externalAddressHashNode*>(ptr->value)->type == getExternalAddressId(env);
		}
	private:
        /**
         * Register the environment with the corresponding type, not meant to
         * be called directly!
         * @param env the environment to register with
         * @param value the index to keep track of
         */
		static void registerExternalAddressId(void* env, unsigned int value) noexcept {
			_cache.emplace(env, value);
		}
		static std::map<void*, unsigned int> _cache;
};


template<typename T>
std::map<void*, unsigned int> ExternalAddressRegistrar<T>::_cache;

/**
 * Check and see if the given value is the range of [0, capacity).
 * @param capacity the max size that the value can be minus 1
 * @param address the value to see range on
 * @tparam T the type of the things to check range on
 * @return true if the given value is in the range of [0, capacity)
 */
template<typename T>
constexpr bool inRange(T capacity, T address) noexcept {
	return address >= 0 && address < capacity;
}

template<>
constexpr bool inRange<uint16>(uint16 capacity, uint16 address) noexcept {
    return address < capacity;
}

template<>
constexpr bool inRange<uint32>(uint32 capacity, uint32 address) noexcept {
    return address < capacity;
}

template<>
constexpr bool inRange<uint64>(uint64 capacity, uint64 address) noexcept {
    return address < capacity;
}

template<>
constexpr bool inRange<uint8>(uint8 capacity, uint8 address) noexcept {
    return address < capacity;
}

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
			static std::string _tmp( name ) ; \
			return _tmp; \
		} \
    }

/**
 * Common implementation for printing out an external address from within
 * CLIPS. Unless you've got really specific or odd requirements, it is
 * suggested that this be used as a base.
 * @param env the environment that called this function
 * @param logicalName the io router to output to
 * @param theValue the raw value that is printed (well it's address)
 * @param func The type of the given externalAddressType
 * @param majorType Used for appending Wrapper, etc to the output name
 */
void CLIPS_basePrintAddress(void* env, const char* logicalName, void* theValue, const char* func, const char* majorType);

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
void CLIPS_basePrintAddress(void* env, const char* logicalName, void* theValue) {
	CLIPS_basePrintAddress(env, logicalName, theValue, TypeToName::getSymbolicName<T>().c_str(), "Wrapper");
}

/**
 * Output an error message through clips
 * @param env the environment where the error happened
 * @param idClass the error class
 * @param idIndex the error index
 * @param msgPrefix the prefix to add
 * @param msg the message to display
 * @return bool signifying if successful error output occurred
 */
bool errorMessage(void* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept;

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

// Have to do it this way because std::function's will go out of scope and
// everything breaks
/**
 * Signature for printing external address types
 */
typedef void PrintFunction(void*, const char*, void*);
/**
 * When an external address goes out of scope and is reclaimed, a function of
 * this form is called to handle cleanup.
 */
typedef bool DeleteFunction(void*, void*);
/**
 * When the call function is invoked within CLIPS, a function with this
 * signature is invoked by CLIPS if registered.
 */
typedef bool CallFunction(void*, DataObjectPtr, DataObjectPtr);
/**
 * When the new function is invoked within CLIPS, the function corresponding to
 * the given type with this signature is invoked. It is responsible for setting
 * up the external address type (if necessary) and such. Think of it as a
 * constructor as the memory has already been allocated.
 */
typedef void NewFunction(void*, DataObjectPtr);

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
    T* invokeNewFunction(void* env, CLIPSValuePtr ret, const std::string& funcErrorPrefix, const std::string& function) noexcept {
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
using FunctionStrings = std::tuple<std::string, std::string, std::string>;
template<typename T>
FunctionStrings retrieveFunctionNames(const std::string& action) noexcept {
    std::stringstream ss, ss2;
    buildFunctionString(ss, action, TypeToName::getSymbolicName<T>());
    buildFunctionErrorString(ss2, action, TypeToName::getSymbolicName<T>());
    auto str0 = ss.str();
    auto str1 = ss2.str();
    return std::make_tuple(TypeToName::getSymbolicName<T>(), str0, str1);
}






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
bool badCallArgument(void* env, CLIPSValue* ret, int code, const std::string& msg) noexcept {
    CVSetBoolean(ret, false);
    return syn::errorMessage(env, "CALL", code, getFunctionErrorPrefixCall<T>(), msg);
}

/**
 * Wrapper method for setting a clips value to a boolean value. The boolean
 * value is also returned as a way to set the ret pointer and return a boolean
 * value from a function.
 * @param ret the area to store the boolean into
 * @param value the boolean value itself (defaults to true)
 * @return the input argument 'value'
 */
inline bool setClipsBoolean(CLIPSValue* ret, bool value = true) noexcept {
    CVSetBoolean(ret, value);
    return value;
}
/**
 * Check and see if the given CLIPS argument is of a given type and extract it
 * if it is.
 * @param env the environment to perform the check on
 * @param function the user defined function where this check is taking place
 * @param position the one-indexed position of the argument
 * @param type the type that is desired
 * @param saveTo the data object to where the argument will be stored to on
 * successful find
 * @return true if the given argument is of the correct type.
 */
bool checkThenGetArgument(void* env, const std::string& function, int position, MayaType type, DataObjectPtr saveTo) noexcept;

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
		static void setString(CLIPSValuePtr val, const std::string& str) noexcept {
			CVSetString(val, str.c_str());
		}
		static const std::string& getType() noexcept {
            return TypeToName::getSymbolicName<InternalType>();
        }
        static void setType(CLIPSValue* ret) noexcept {
            CVSetString(ret, getType().c_str());
        }
        static bool checkArgumentCount(void* env, CLIPSValuePtr ret, const std::string& operation, int inputArgCount) {
            auto aCount = baseArgumentIndex + inputArgCount;
            if (!syn::hasCorrectArgCount(env, aCount)) {
                return callErrorMessageCode3(env, ret, operation, " too many arguments provided!");
            }
            return true;
        }

		static unsigned int getAssociatedEnvironmentId(void* env) {
            return ExternalAddressRegistrar<InternalType>::getExternalAddressId(env);
        }
		static void registerWithEnvironment(void* env, externalAddressType* description) noexcept {
			ExternalAddressRegistrar<InternalType>::registerExternalAddress(env, description);
		}
		static void printAddress(void* env, const char* logicalName, void* theValue) {
			CLIPS_basePrintAddress<InternalType>(env, logicalName, theValue);
		}
		static bool deleteWrapper(void* env, void* obj) {
			if (obj != nullptr) {
				auto result = static_cast<typename ExternalAddressWrapperType<T>::TheType*>(obj);
				delete result;
			}
			return true;
		}
        static void newFunction(void* env, CLIPSValue* ret) {
            InternalType* ptr = WrappedNewCallBuilder::invokeNewFunction<InternalType>(env, ret, getFunctionErrorPrefixNew<InternalType>(), getFunctionPrefixNew<InternalType>());
            if (ptr) {
                using CorrespondingType = typename ExternalAddressWrapperType<T>::TheType;
                auto s = new CorrespondingType(ptr);
                CVSetExternalAddress(ret, s, Self::getAssociatedEnvironmentId(env));
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
        static inline bool badCallArgument(void* env, CLIPSValue* ret, int code, const std::string& msg) noexcept {
            return syn::badCallArgument<T>(env, ret, code, msg);
        }


        static bool callErrorMessage(void* env, CLIPSValue* ret, int code, const std::string& subOp, const std::string& rest) {
            std::stringstream stm;
            stm << " " << subOp << ": " << rest << std::endl;
            auto msg = stm.str();
            return badCallArgument(env, ret, code, msg);
        }

        static inline bool callErrorMessageCode3(void* env, CLIPSValue* ret, const std::string& subOp, const std::string& rest) noexcept {
            return callErrorMessage(env, ret, 3, subOp, rest);
        }
        static inline bool callErrorMessageCode3(void* env, CLIPSValuePtr ret, const std::string& subOp, const char* rest) noexcept {
            return callErrorMessageCode3(env, ret, subOp, std::string(rest));
        }
        template<typename E>
        static inline bool callErrorMessageCode3(void* env, CLIPSValuePtr ret, const std::string& subOp, const E& problem) noexcept {
            return callErrorMessageCode3(env, ret, subOp, problem.what());
        }

        static bool tryGetArgument(void* env, CLIPSValue* ret, int pos, MayaType type) noexcept {
            static bool init = true;
            static std::string funcStr;
            if (init) {
                init = false;
                funcStr = std::get<1>(syn::retrieveFunctionNames<T>("call"));
            }
            return checkThenGetArgument(env, funcStr, pos, type, ret);
        }
        static constexpr int baseArgumentIndex = 2;
        template<int index>
        static constexpr int getArgumentIndex() noexcept {
            static_assert(index >= 0, "No negative argument index");
            return baseArgumentIndex + index;
        }

        static bool tryExtractArgument(void* env, CLIPSValue* ret, CLIPSValue* storage, MayaType type, int pos, int errorCode, const std::string& msg) noexcept {
            if (!tryGetArgument(env, storage, pos, type)) {
                return badCallArgument(env, ret, errorCode, msg);
            }
            return true;
        }
        template<int index>
        static inline bool tryExtractArgument(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage, MayaType type, int errorCode, const std::string& errorMsg) noexcept {
            return tryExtractArgument(env, ret, storage, type, getArgumentIndex<index>(), errorCode, errorMsg);
        }

        static bool tryExtractFunctionName(void* env, CLIPSValue* ret, CLIPSValue* storage) noexcept {
            return tryExtractArgument<0>(env, ret, storage, MayaType::Symbol, 2, "expected a function name to call!");
        }

        static inline bool tryExtractArgument1(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage, MayaType type, const std::string& errorMsg) noexcept {
            return tryExtractArgument<1>(env, ret, storage, type, 3, errorMsg);
        }

        static inline bool tryExtractArgument2(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage, MayaType type, const std::string& errorMsg) noexcept {
            return tryExtractArgument<2>(env, ret, storage, type, 3, errorMsg);
        }

        static inline bool tryExtractArgument3(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage, MayaType type, const std::string& errorMsg) noexcept {
            return tryExtractArgument<3>(env, ret, storage, type, 3, errorMsg);
        }

        static inline bool tryExtractArgument4(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage, MayaType type, const std::string& errorMsg) noexcept {
            return tryExtractArgument<4>(env, ret, storage, type, 3, errorMsg);
        }
        template<typename Thing, typename Against>
        static bool isLegalOperation(void* env, CLIPSValuePtr ret, const std::string& op, Thing thing, Against against) {
            if (thing == against) {
                return callErrorMessageCode3(env, ret, op, " <- unknown operation requested!");
            }
            return true;
        }
        static bool isExternalAddress(void* env, DataObjectPtr ret, DataObjectPtr value) noexcept {
            if (!syn::isExternalAddress(value)) {
                return badCallArgument(env, ret, 1, "Function call expected an external address as the first argument!");
            }
            return true;
        }
	public:
		ExternalAddressWrapper(std::unique_ptr<T>&& value) : _value(std::move(value)) { }
        ExternalAddressWrapper(T* ptr) : _value(std::move(std::unique_ptr<T>(ptr))) { }
		virtual ~ExternalAddressWrapper() { }
		inline T* get() const noexcept { return _value.get(); }
        T* operator->() const noexcept { return get(); }
	protected:
		std::unique_ptr<T> _value;
};

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
        void setFirst(int type, void* value) noexcept {
            setField<1>(type, value);
        }
        void setFirst(MayaType type, void* value) noexcept {
            setFirst(static_cast<int>(type), value);
        }
        void setSecond(int type, void* value) noexcept {
            setField<2>(type, value);
        }
        void setSecond(MayaType type, void* value) noexcept {
            setSecond(static_cast<int>(type), value);
        }
    private:
        void* _rawMultifield;
};

}
#endif
