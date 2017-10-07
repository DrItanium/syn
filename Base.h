/**
 * @file
 * Basic routines, classes, and concepts used throughout syn.
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


#ifndef _SYN_BASE_H
#define _SYN_BASE_H
#define INDIRECTOR(a, ...) PRIMITIVE_INDIRECTOR(a, __VA_ARGS__)
#define PRIMITIVE_INDIRECTOR(a, ...) a ## __VA_ARGS__
#define __RETURN_FALSE_ON_FALSE__(condition) if (!(condition)) { return false; }
#include "BaseTypes.h"
#include "Problem.h"
#include <memory>

namespace syn {

/**
 * A compile type computation for defining a bit at a given position. For
 * instance, singleBitmaskValue<byte, 7> will put 0x1 at position seven which
 * will result in 0x80.
 * @tparam T the type to make the bitmask of
 * @tparam index the position in the type to place the 1
 */
template<typename T, T index>
constexpr auto singleBitmaskValue = static_cast<T>(numeralOne<T> << index);


/**
 * A compile time computation which describes the largest value a given type
 * can store, which is usually all F's if looking at the number in hex!
 * @tparam T the type to get the largest value of
 */
template<typename T>
constexpr auto largestValue = static_cast<T>(0 + (-1));

/**
 * Retrieves the mask to extract the lower half of a type.  By default, this
 * will be zero. The backing templated variables must be specialized.
 * @tparam T the type to retrieve the lower half mask of
 * @return the lower half mask of the specified type
 */
template<typename T>
constexpr T getLowerMask() noexcept {
    return static_cast<T>(largestValue<T> >> (bitwidth<T> / 2));
}

template<> constexpr int128 getLowerMask<int128> () noexcept {  return static_cast<int128>(0xFFFFFFFFFFFFFFFF); }

/**
 * Retrieves the mask to extract the upper half of a type. By default, this
 * will be zero. The backing templated variables must be specialized.
 * @tparam T the type to retrieve the upper half mask of
 * @return the upper half mask of the specified type
 */
template<typename T>
constexpr T getUpperMask() noexcept {
    return static_cast<T>(getLowerMask<T>() << (bitwidth<T> / 2));
}

/**
 * Retrieve the shift count needed for extracting the upper half of a given
 * type. Unless specialized this will be zero
 * @tparam T the type to retrieve the shift count of
 * @return the shift count for the specified type
 */
template<typename T>
constexpr T getShiftCount() noexcept {
    return bitwidth<T> / 2;
}




/**
 * A version of singleBitmaskValue where the shift index is only known at
 * runtime.
 * @tparam T the resultant type of the single bit bitmask
 * @param index the position in the type to place the single bit
 * @return the bitmask of the given type at the specified position
 */
template<typename T>
constexpr T computeSingleBitmask(T index) noexcept {
	return static_cast<T>(numeralOne<T> << index);
}

/**
 * mask the given input with the fixed bitmask
 * @tparam T the type of all things in this function
 * @tparam bitmask the mask to apply to the input
 * @param input the value to be masked
 * @return the result of masking input with bitmask
 */
template<typename T, T bitmask>
constexpr T mask(T input) noexcept {
    return input & bitmask;
}

/**
 * Extract a section of bits out of a given input and return it as a given
 * type.
 * @tparam T the type of the input
 * @tparam F the type of the output
 * @tparam bitmask the mask to apply to the input
 * @tparam shiftcount the amount of positions to shift right
 * @param input source value to extract bits out of
 * @return input masked, shifted, and then cast to the specified type
 */
template<typename T, typename F, T bitmask, T shiftcount>
constexpr F decodeBits(T input) noexcept {
    auto result = mask<T, bitmask>(input);
    if (shiftcount != 0) {
        result >>= shiftcount;
    }
    return static_cast<F>(result);
}

/**
 * Mask a given input and return a bool based on its resultant value
 * @tparam T the type of the input
 * @tparam mask the bitmask to apply to the input
 * @param input the input to check for flag status on
 * @return input masked and cast to bool
 */
template<typename T, T mask>
constexpr bool decodeFlag(T input) noexcept {
	return decodeBits<T, bool, mask, static_cast<T>(0)>(input);
}

template<typename T, typename F, T bitmask, T shiftcount>
constexpr T encodeBits(T input, F value) noexcept {
    auto valueToInject = static_cast<T>(value);
    auto maskedValue = input & ~bitmask;
    if (shiftcount != 0) {
        valueToInject <<= shiftcount;
    }
    valueToInject &= bitmask;
    return static_cast<T>(maskedValue | valueToInject);
}

template<typename T, T mask, T shift>
constexpr T encodeFlag(T input, bool value) noexcept {
	return encodeBits<T, bool, mask, shift>(input, value);
}


template<typename T, typename F>
constexpr F decodeBits(T value, T mask, T shiftcount) noexcept {
	return static_cast<F>((value & mask) >> shiftcount);
}

template<typename T, typename F>
constexpr T encodeBits(T input, F value, T bitmask, T shiftcount) noexcept {
	return static_cast<T>((input & ~bitmask) | ((static_cast<T>(value) << shiftcount) & bitmask));
}


template<typename T, T index>
constexpr bool getBit(T value) noexcept {
    return decodeBits<T, bool, singleBitmaskValue<T, index>, index>(value);
}

template<typename T>
constexpr bool getBit(T value, T index) noexcept {
    return decodeBits<T, bool>(value, computeSingleBitmask<T>(index), index);
}

constexpr byte expandBit(bool value) noexcept {
    return value ? 0xFF : 0x00;
}

template<typename T, T index>
constexpr T setBit(T value, bool bit) noexcept {
    return syn::encodeBits<T, bool, singleBitmaskValue<T, index>, index>(value, bit);
}

template<typename T>
constexpr T setBit(T value, bool bit, T index) noexcept {
    return encodeBits<T, bool>(value, bit, computeSingleBitmask<T>(index), index);
}


/**
 * Swaps the contents of two references
 * @param a the first reference
 * @param b the second reference
 * @tparam T the type of the things to be swapped
 */
template<typename T>
inline void swap(T& a, T& b) {
    auto c = b;
    b = a;
    a = c;
}

template<typename T>
union BinaryContainer {
    T value;
    byte bytes[sizeof(T)];
};
using BinaryFloat = BinaryContainer<float>;
using BinaryDouble = BinaryContainer<double>;
using BinaryLongDouble = BinaryContainer<long double>;
template<typename T>
constexpr auto byteCount = sizeof(T);

template<typename T>
constexpr auto byteCount<BinaryContainer<T>> = sizeof(T);

/**
 * A common interface to make it possible to describe error states without
 * using exceptions. This concept is widely used with enums where the Count
 * value doubles as the error state. This makes it possible to mark functions
 * as noexcept and return an error state instead.
 *
 * @tparam T The type of the thing to describe it's error state
 */
template<typename T>
constexpr T defaultErrorState = std::is_enum<T>::value ? T::Count : static_cast<T>(0);

/**
 * Checks to see if the given input type is equal to the corresponding
 * defaultErrorState. This is meant to be used with static_assert and other
 * boolean checks. This is a safe abstraction and should be used over direct
 * equality checks!
 * @param op the thing to compare against the defaultErrorState
 * @tparam T the type of the thing to check to see if it is the error state.
 * @return boolean value signifying if the given input is the same as the
 * defaultErrorState for that type.
 */
template<typename T>
constexpr bool isErrorState(T op) noexcept {
    return op == defaultErrorState<T>;
}

/**
 * Given a specific type, check and see if the provided value is the
 * error state; if it is then throw an exception with the provided message.
 * This function provides a clean way to check for errorStates without
 * repeating the same code over and over again.
 * @param value the value to compare against the defaultErrorState of the
 * specified type
 * @param msg the message to install into the problem exception if the given
 * value is the defaultErrorState
 * @tparam T the type of the thing to compare against the defaultErrorState.
 */
template<typename T>
void throwOnErrorState(T value, const std::string& msg) {
    if (isErrorState<T>(value)) {
        throw syn::Problem(msg);
    }
}

constexpr bool fulfillsCondition(std::true_type) noexcept { return true; }
constexpr bool fulfillsCondition(std::false_type) noexcept { return false; }

template<bool fulfills>
struct ConditionFulfillment : std::integral_constant<bool, fulfills> { };


template<typename T>
constexpr bool fulfillsCondition() noexcept {
	static_assert(!std::is_integral<T>::value, "Provided type must not be an integral!");
	return fulfillsCondition(T());
}

/**
 * Construct a 128 bit unsigned integer
 */
constexpr uint128 makeuint128(uint64 lower, uint64 upper) noexcept {
    auto lowerHalf = decodeBits<uint64, uint128, getLowerMask<uint128>(), 0>(lower);
    return encodeBits<uint128, uint64, getUpperMask<uint128>(), getShiftCount<uint128>()>(lowerHalf, upper);
}
template<uint8 check>
constexpr uint8 getEndianIdent() noexcept {
    union {
        uint32 i;
        uint8 storage[sizeof(uint32)];
    } temp = { 0x01020304 };
    return temp.storage[0] == check;
}
constexpr bool isBigEndian() noexcept {
    return getEndianIdent<1>();
}
constexpr bool isLittleEndian() noexcept {
    return getEndianIdent<4>();
}

} // end namespace syn

#endif // end _SYN_BASE_H
