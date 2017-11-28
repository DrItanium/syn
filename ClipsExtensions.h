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
#include <functional>
#include <typeinfo>
extern "C" {
	#include "clips.h"
}

namespace clips {
inline int printRouter(void* theEnv, const std::string& logicalName, const std::string& msg) noexcept {
    return EnvPrintRouter(theEnv, logicalName.c_str(), msg.c_str());
}
inline int printRouter(void* theEnv, const char* logicalName, const std::string& msg) noexcept {
    return EnvPrintRouter(theEnv, logicalName, msg.c_str());
}
inline int printLine(void* theEnv, const char* logicalName) noexcept {
    return printRouter(theEnv, logicalName, "\n");
}
inline int printLine(void* theEnv, const std::string& logicalName) noexcept {
    return printRouter(theEnv, logicalName, "\n");
}
} // end namespace clips

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


template<typename T = int>
using ArgCountChecker = std::function<bool(T)>;
template<typename T = int>
using ArgCountModifier = std::function<T(T)>;

template<typename Operation, typename T = int>
using OperationToArgCountChecker = std::tuple<Operation, ArgCountChecker<T>>;

template<typename Operation, typename T = int>
OperationToArgCountChecker<Operation, T> associate(Operation op, ArgCountChecker<T> fn) noexcept {
	return std::make_tuple(op, fn);
}
template<typename T = int>
ArgCountChecker<T> expectExactly(T count) noexcept {
	return [count](auto against) noexcept { return count == against; };
}

template<typename Operation, typename T = int>
OperationToArgCountChecker<Operation, T> expectExactly(Operation op, T count) noexcept {
	return associate<Operation, T>(op, expectExactly<T>(count));
}

template<typename T = int>
ArgCountChecker<T> expectRangeInclusive(T min, T max) noexcept {
	return [min, max](auto against) {
		return (against >= min) && (against <= max);
	};
}
template<typename Operation, typename T = int>
OperationToArgCountChecker<Operation, T> expectRangeInclusive(Operation op, T min, T max) noexcept {
	return associate<Operation, T>(op, expectRangeInclusive<T>(min, max));
}
template<typename T = int>
ArgCountChecker<T> expectRangeExclusive(T min, T max) noexcept {
	return [min, max](auto against) { return (against > min) && (against < max); };
}

template<typename Operation, typename T = int>
OperationToArgCountChecker<Operation, T> expectRangeExclusive(Operation op, T min, T max) noexcept {
	return associate<Operation, T>(op, expectRangeExclusive<T>(min, max));
}

template<typename T = int>
ArgCountChecker<T> expectAtLeast(T min) noexcept {
	return [min](auto against) { return (against >= min); };
}
template<typename Operation, typename T = int>
OperationToArgCountChecker<Operation, T> expectAtLeast(Operation op, T count) noexcept {
	return associate<Operation, T>(op, expectAtLeast<T>(count));
}
template<typename T = int>
ArgCountChecker<T> expectAtMost(T max) noexcept {
	return [max](auto against) { return (against <= max); };
}
template<typename Operation, typename T = int>
OperationToArgCountChecker<Operation, T> expectAtMost(Operation op, T count) noexcept {
	return associate<Operation, T>(op, expectAtMost<T>(count));
}

template<typename T = int>
ArgCountChecker<T> binaryOperation() noexcept {
	return expectExactly<T>(static_cast<T>(2));
}

template<typename Operation, typename T = int>
OperationToArgCountChecker<Operation, T> binaryOperation(Operation op) noexcept {
	return associate<Operation, T>(op, binaryOperation<T>());
}


template<typename T = int>
ArgCountChecker<T> unaryOperation() noexcept {
	return expectExactly<T>(static_cast<T>(1));
}

template<typename Operation, typename T = int>
OperationToArgCountChecker<Operation, T> unaryOperation(Operation op) noexcept {
	return associate<Operation, T>(op, unaryOperation<T>());
}

template<typename T = int>
ArgCountChecker<T> trinaryOperation() noexcept {
	return expectExactly<T>(static_cast<T>(3));
}

template<typename Operation, typename T = int>
OperationToArgCountChecker<Operation, T> trinaryOperation(Operation op) noexcept {
	return associate<Operation, T>(op, trinaryOperation<T>());
}

/**
 * performs a check to see if the acquired number of arguments equals the
 * expected count with a provided offset modifier.
 * @param env the environment to check
 * @param compare the comparison function to use
 * @param modify the argcount modification function, this is optional. Pass
 * nullptr to disable modification of the arg count
 * @return true if the compare equals the actual argument code modified
 */
template<typename T = int>
bool hasCorrectArgCount(void* env, ArgCountChecker<T> compare, ArgCountModifier<T> modify = nullptr) noexcept {
	auto count = getArgCount(env);
	return compare(modify == nullptr ? count : modify(count));
}

/**
 * Return true if the given dataObjetPtr is tagged as an ExternalAddress type
 * @param value the dataObjectPtr to check
 * @return true if the given dataObjectPtr contains an external address
 */
bool isExternalAddress(DataObjectPtr value) noexcept;

CLIPSFloat extractFloat(void* env, DataObjectPtr value) noexcept;
CLIPSFloat extractFloat(void* env, DataObject& value) noexcept;
template<typename Ret>
Ret extractFloat(void* env, DataObjectPtr value) noexcept {
	return static_cast<Ret>(extractFloat(env, value));
}
template<typename Ret>
Ret extractFloat(void* env, DataObject& value) noexcept {
	return static_cast<Ret>(extractFloat(env, value));
}
CLIPSInteger extractCLIPSInteger(void* env, DataObjectPtr value) noexcept;
CLIPSInteger extractCLIPSInteger(void* env, DataObject& value) noexcept;
template<typename Ret>
Ret extractCLIPSInteger(void* env, DataObjectPtr value) noexcept {
    return static_cast<Ret>(extractCLIPSInteger(env, value));
}

template<typename Ret>
Ret extractCLIPSInteger(void* env, DataObject& value) noexcept {
    return static_cast<Ret>(extractCLIPSInteger(env, value));
}

const char* extractLexeme(void* env, DataObjectPtr value) noexcept;
const char* extractLexeme(void* env, DataObject& value) noexcept;

bool checkThenGetArgument(void* env, const std::string& function, int position, MayaType type, DataObjectPtr saveTo) noexcept;
bool tryGetArgumentAsInteger(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept;
bool tryGetArgumentAsSymbol(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept;
bool tryGetArgumentAsString(void* env, const std::string& function, int position, DataObjectPtr saveTo) noexcept;

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
 * Output an error message through clips
 * @param env the environment where the error happened
 * @param idClass the error class
 * @param idIndex the error index
 * @param msgPrefix the prefix to add
 * @param msg the message to display
 * @return bool signifying if successful error output occurred
 */
bool errorMessage(void* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept;

/**
 * Output an error message through clips
 * @param env the environment where the error happened
 * @param idClass the error class
 * @param idIndex the error index
 * @param msgPrefix the prefix to add
 * @param msg the message to display
 * @return bool signifying if successful error output occurred
 */
bool errorMessage(UDFContext* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept;

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


}
#endif
