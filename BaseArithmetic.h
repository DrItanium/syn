/**
 * @file
 * Common arithmetic routines
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


#ifndef SYN_BASE_ARITHMETIC_H__
#define SYN_BASE_ARITHMETIC_H__
#include "BaseTypes.h"
#include "Problem.h"

#include <functional>
namespace syn {

template<typename T, typename R = bool>
constexpr R eq(T a, T b) noexcept {
	return static_cast<R>(a == b);
}

template<typename T, typename R = bool>
constexpr R neq(T a, T b) noexcept {
	return static_cast<R>(a != b);
}

template<typename T, typename R = bool>
constexpr R lt(T a, T b) noexcept {
	return static_cast<R>(a < b);
}

template<typename T, typename R = bool>
constexpr R gt(T a, T b) noexcept {
	return static_cast<R>(a > b);
}

template<typename T, typename R = bool>
constexpr R le(T a, T b) noexcept {
	return static_cast<R>(a <= b);
}

template<typename T, typename R = bool>
constexpr R ge(T a, T b) noexcept {
	return static_cast<R>(a >= b);
}

template<typename T, typename R = T>
constexpr R shiftLeft(T a, T b) noexcept {
	return static_cast<R>(a << b);
}

template<typename T, typename R = T>
constexpr R shiftRight(T a, T b) noexcept {
	return static_cast<R>(a >> b);
}

template<typename T, typename R = T>
constexpr R binaryAnd(T a, T b) noexcept {
	return static_cast<R>(a & b);
}

template<>
constexpr bool binaryAnd<bool, bool>(bool a, bool b) noexcept {
	return a && b;
}

template<typename T, typename R = T>
constexpr R binaryOr(T a, T b) noexcept {
	return static_cast<R>(a | b);
}

template<>
constexpr bool binaryOr<bool, bool>(bool a, bool b) noexcept {
	return a || b;
}


template<typename T, typename R = T>
constexpr R binaryNot(T a) noexcept {
	return static_cast<R>(~a);
}

template<>
constexpr bool binaryNot<bool, bool>(bool a) noexcept {
	return !a;
}

template<typename T, typename R = T>
constexpr R binaryXor(T a, T b) noexcept {
	return static_cast<R>(a ^ b);
}

template<typename T, typename R = T>
constexpr R binaryNand(T a, T b) noexcept {
	return static_cast<R>(~(a & b));
}

template<>
constexpr bool binaryNand(bool a, bool b) noexcept {
	return !(a && b);
}

template<typename T, typename R = T>
constexpr R binaryNor(T a, T b) noexcept {
	return ~(a | b);
}

template<>
constexpr bool binaryNor(bool a, bool b) noexcept {
	return !(a || b);
}

template<typename T, typename R = T>
constexpr R circularShiftLeft(T value, T shift) noexcept {
    constexpr auto width = largestShiftValue<T>;
    // taken from the wikipedia entry on circular shifts
    return static_cast<R>((value << shift) | (value >> ((-shift) & width)));
}

template<typename T, typename R = T>
constexpr R circularShiftRight(T value, T shift) noexcept {
    constexpr auto width = largestShiftValue<T>;
    // taken from the wikipedia entry on circular shifts
	return static_cast<R>( (value >> shift) | (value << ((-shift) & width)));
}
/**
 * Perform a multiply followed by an add. The form is (a * b) + c
 * @param a the first argument to the product
 * @param b the second argument to the product
 * @param c the second argument to the sum
 * @return the result of the multiply add
 */
template<typename T>
constexpr T multiplyAdd(T a, T b, T c) noexcept {
    return (a * b) + c;
}

template<typename T, bool includeMaximum = true, bool includeMinimum = true>
constexpr bool valueIsInRange(T value, T minimum, T maximum) noexcept {
    auto minimumCheck = includeMinimum ? value >= minimum : value > minimum;
    auto maximumCheck = includeMaximum ? value <= maximum : value < maximum;
    return minimumCheck && maximumCheck;
}

template<typename T, T minimum, T maximum, bool includeMaximum = true, bool includeMinimum = true>
constexpr bool valueIsInRange(T value) noexcept {
    auto minimumCheck = includeMinimum ? value >= minimum : value > minimum;
    auto maximumCheck = includeMaximum ? value <= maximum : value < maximum;
    return minimumCheck && maximumCheck;
}

template<typename T>
constexpr bool inRangeInclusive(T value, T minimum, T maximum) noexcept {
    return valueIsInRange<T>(value, minimum, maximum);
}

template<typename T, T minimum, T maximum>
constexpr bool inRangeInclusive(T value) noexcept {
    return valueIsInRange<T, minimum, maximum>(value);
}
template<typename T>
constexpr bool inRangeExcludingMaximum(T value, T minimum, T maximum) noexcept {
    return valueIsInRange<T, false>(value, minimum, maximum);
}

template<typename T, T min, T max>
constexpr bool inRangeExcludingMaximum(T value) noexcept {
    return valueIsInRange<T, min, max, false>(value);
}

/**
 * Check and see if the given value is the range of [0, capacity).
 * @param capacity the max size that the value can be minus 1
 * @param address the value to see range on
 * @tparam T the type of the things to check range on
 * @return true if the given value is in the range of [0, capacity)
 */
template<typename T>
constexpr bool addressInRange(T capacity, T address) noexcept {
    if (std::is_unsigned<T>::value) {
        return address < capacity;
    } else {
	    return address >= 0 && address < capacity;
    }
}

template<typename T>
constexpr T onesComplement(T value) noexcept {
    return ~value;
}

template<typename T>
constexpr T twosComplement(T value) noexcept {
    return onesComplement<T>(value) + numeralOne<T>;
}


} // end namespace syn


#endif // end SYN_BASE_ARITHMETIC_H__
