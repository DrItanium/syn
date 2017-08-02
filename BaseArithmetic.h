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
#include <climits>
namespace syn {

/**
 * Retrieve the number of bits that a given type consumes. This is different
 * from plain sizeof in that this is CHAR_BIT * the number of bytes that make
 * up the target type.
 * @tparam T the type to find the size of
 */
template<typename T>
constexpr size_t bitwidth = CHAR_BIT * sizeof(T);

template<typename T, typename R = T>
constexpr R add(T a, T b) noexcept {
	return static_cast<R>(a + b);
}

template<typename T, typename R = T>
constexpr R sub(T a, T b) noexcept {
	return static_cast<R>(a - b);
}

template<typename T, typename R = T>
constexpr R mul(T a, T b) noexcept {
	return static_cast<R>(a * b);
}

template<typename T>
using OnDivideByZero = std::function<T()>;

template<typename R>
R whenDenominatorIsZero() {
    throw syn::Problem("Denominator is zero!");
    return static_cast<R>(0);
}

template<typename R>
inline R defaultDenominatorCheck(OnDivideByZero<R> operation) {
    if (operation == nullptr) {
        throw syn::Problem("Denominator is zero!");
    } else {
        return operation();
    }
}
template<typename T, typename R = T>
inline R div(T numerator, T denominator, OnDivideByZero<R> markDivideByZero) {
    if (denominator == 0) {
        return defaultDenominatorCheck<R>(markDivideByZero);
    } else if (denominator == 1) {
        return static_cast<R>(numerator);
    } else {
        return static_cast<R>(numerator / denominator);
    }
}

template<typename T, typename R = T>
inline R div(T numerator, T denominator) {
    return div<T, R>(numerator, denominator, nullptr);
}

template<>
inline double div<double, double>(double numerator, double denominator) {
    return numerator / denominator;
}

template<>
inline float div<float, float>(float numerator, float denominator) {
    return numerator / denominator;
}

template<>
inline uint8 div<uint8, uint8>(uint8 numerator, uint8 denominator, OnDivideByZero<uint8> markDivideByZero) {
     switch(denominator) {
         case 0: return defaultDenominatorCheck<uint8>(markDivideByZero);
         case 1: return numerator;
         case 2: return numerator >> 1;
         case 4: return numerator >> 2;
         case 8: return numerator >> 3;
         case 16: return numerator >> 4;
         case 32: return numerator >> 5;
         case 64: return numerator >> 6;
         case 128: return numerator >> 7;
         default:
             return numerator / denominator;
     }
}

template<>
inline uint16 div<uint16, uint16>(uint16 numerator, uint16 denominator, OnDivideByZero<uint16> markDivideByZero) {
     switch(denominator) {
         case 0: return defaultDenominatorCheck<uint16>(markDivideByZero);
         case 1: return numerator;
         case 2: return numerator >> 1;
         case 4: return numerator >> 2;
         case 8: return numerator >> 3;
         case 16: return numerator >> 4;
         case 32: return numerator >> 5;
         case 64: return numerator >> 6;
         case 128: return numerator >> 7;
         case 256: return numerator >> 8;
         case 512: return numerator >> 9;
         case 1024: return numerator >> 10;
         case 2048: return numerator >> 11;
         case 4096: return numerator >> 12;
         case 8192: return numerator >> 13;
         case 16384: return numerator >> 14;
         case 32768: return numerator >> 15;
         default:
             return numerator / denominator;
     }
}

template<>
inline uint32 div<uint32, uint32>(uint32 numerator, uint32 denominator, OnDivideByZero<uint32> markDivideByZero) {
     switch(denominator) {
         case 0: return defaultDenominatorCheck<uint32>(markDivideByZero);
         case 1: return numerator;
         case 2: return numerator >> 1;
         case 4: return numerator >> 2;
         case 8: return numerator >> 3;
         case 16: return numerator >> 4;
         case 32: return numerator >> 5;
         case 64: return numerator >> 6;
         case 128: return numerator >> 7;
         case 256: return numerator >> 8;
         case 512: return numerator >> 9;
         case 1024: return numerator >> 10;
         case 2048: return numerator >> 11;
         case 4096: return numerator >> 12;
         case 8192: return numerator >> 13;
         case 16384: return numerator >> 14;
         case 32768: return numerator >> 15;
         case 65536: return numerator >> 16;
         case 131072: return numerator >> 17;
         case 262144: return numerator >> 18;
         case 524288: return numerator >> 19;
         case 1048576: return numerator >> 20;
         case 2097152: return numerator >> 21;
         case 4194304: return numerator >> 22;
         case 8388608: return numerator >> 23;
         case 16777216: return numerator >> 24;
         case 33554432: return numerator >> 25;
         case 67108864: return numerator >> 26;
         case 134217728: return numerator >> 27;
         case 268435456: return numerator >> 28;
         case 536870912: return numerator >> 29;
         case 1073741824: return numerator >> 30;
         case 2147483648: return numerator >> 31;
         default:
             return numerator / denominator;
     }
}

template<>
inline uint64 div<uint64, uint64>(uint64 numerator, uint64 denominator, OnDivideByZero<uint64> markDivideByZero) {
     switch(denominator) {
         case 0: return defaultDenominatorCheck<uint64>(markDivideByZero);
         case 1: return numerator;
         case 2: return numerator >> 1;
         case 4: return numerator >> 2;
         case 8: return numerator >> 3;
         case 16: return numerator >> 4;
         case 32: return numerator >> 5;
         case 64: return numerator >> 6;
         case 128: return numerator >> 7;
         case 256: return numerator >> 8;
         case 512: return numerator >> 9;
         case 1024: return numerator >> 10;
         case 2048: return numerator >> 11;
         case 4096: return numerator >> 12;
         case 8192: return numerator >> 13;
         case 16384: return numerator >> 14;
         case 32768: return numerator >> 15;
         case 65536: return numerator >> 16;
         case 131072: return numerator >> 17;
         case 262144: return numerator >> 18;
         case 524288: return numerator >> 19;
         case 1048576: return numerator >> 20;
         case 2097152: return numerator >> 21;
         case 4194304: return numerator >> 22;
         case 8388608: return numerator >> 23;
         case 16777216: return numerator >> 24;
         case 33554432: return numerator >> 25;
         case 67108864: return numerator >> 26;
         case 134217728: return numerator >> 27;
         case 268435456: return numerator >> 28;
         case 536870912: return numerator >> 29;
         case 1073741824: return numerator >> 30;
         case 2147483648: return numerator >> 31;
         case 4294967296: return numerator >> 32;
         case 8589934592: return numerator >> 33;
         case 17179869184: return numerator >> 34;
         case 34359738368: return numerator >> 35;
         case 68719476736: return numerator >> 36;
         case 137438953472: return numerator >> 37;
         case 274877906944: return numerator >> 38;
         case 549755813888: return numerator >> 39;
         case 1099511627776: return numerator >> 40;
         case 2199023255552: return numerator >> 41;
         case 4398046511104: return numerator >> 42;
         case 8796093022208: return numerator >> 43;
         case 17592186044416: return numerator >> 44;
         case 35184372088832: return numerator >> 45;
         case 70368744177664: return numerator >> 46;
         case 140737488355328: return numerator >> 47;
         case 281474976710656: return numerator >> 48;
         case 562949953421312: return numerator >> 49;
         case 1125899906842624: return numerator >> 50;
         case 2251799813685248: return numerator >> 51;
         case 4503599627370496: return numerator >> 52;
         case 9007199254740992: return numerator >> 53;
         case 18014398509481984: return numerator >> 54;
         case 36028797018963968: return numerator >> 55;
         case 72057594037927936: return numerator >> 56;
         case 144115188075855872: return numerator >> 57;
         case 288230376151711744: return numerator >> 58;
         case 576460752303423488: return numerator >> 59;
         case 1152921504606846976: return numerator >> 60;
         case 2305843009213693952: return numerator >> 61;
         case 4611686018427387904: return numerator >> 62;
         case 9223372036854775808u: return numerator >> 63;
         default:
             return numerator / denominator;
     }
}



template<typename T, typename R = T>
inline R rem(T numerator, T denominator, OnDivideByZero<R> markDivideByZero) {
	if (denominator == 0) {
        return defaultDenominatorCheck<R>(markDivideByZero);
    } else if (denominator == 1) {
        return static_cast<R>(0);
	} else {
		return static_cast<R>(numerator % denominator);
	}
}

template<typename T, typename R = T>
inline R rem(T numerator, T denominator) {
    return rem<T, R>(numerator, denominator, nullptr);
}

template<>
inline uint8 rem<uint8, uint8>(uint8 numerator, uint8 denominator, OnDivideByZero<uint8> markDivideByZero) {
    switch(denominator) {
        case 0: return defaultDenominatorCheck<uint8>(markDivideByZero);
        case 1: return 0;
        case 2: return numerator & 1;
        case 4: return numerator & 3;
        case 8: return numerator & 7;
        case 16: return numerator & 15;
        case 32: return numerator & 31;
        case 64: return numerator & 63;
        case 128: return numerator & 127;
        default:
            return numerator % denominator;
    }
}


template<>
inline uint16 rem<uint16, uint16>(uint16 numerator, uint16 denominator, OnDivideByZero<uint16> markDivideByZero) {
    switch(denominator) {
        case 0: return defaultDenominatorCheck<uint16>(markDivideByZero);
        case 1: return 0;
        case 2: return numerator & 1;
        case 4: return numerator & 3;
        case 8: return numerator & 7;
        case 16: return numerator & 15;
        case 32: return numerator & 31;
        case 64: return numerator & 63;
        case 128: return numerator & 127;
        case 256: return numerator & 255;
        case 512: return numerator & 511;
        case 1024: return numerator & 1023;
        case 2048: return numerator & 2047;
        case 4096: return numerator & 4095;
        case 8192: return numerator & 8191;
        case 16384: return numerator & 16383;
        case 32768: return numerator & 32767;
        default:
            return numerator % denominator;
    }
}

template<>
inline uint32 rem<uint32, uint32>(uint32 numerator, uint32 denominator, OnDivideByZero<uint32> markDivideByZero) {
    switch(denominator) {
        case 0: return defaultDenominatorCheck<uint32>(markDivideByZero);
        case 1: return 0;
        case 2: return numerator & 1;
        case 4: return numerator & 3;
        case 8: return numerator & 7;
        case 16: return numerator & 15;
        case 32: return numerator & 31;
        case 64: return numerator & 63;
        case 128: return numerator & 127;
        case 256: return numerator & 255;
        case 512: return numerator & 511;
        case 1024: return numerator & 1023;
        case 2048: return numerator & 2047;
        case 4096: return numerator & 4095;
        case 8192: return numerator & 8191;
        case 16384: return numerator & 16383;
        case 32768: return numerator & 32767;
        case 65536: return numerator & 65535;
        case 131072: return numerator & 131071;
        case 262144: return numerator & 262143;
        case 524288: return numerator & 524287;
        case 1048576: return numerator & 1048575;
        case 2097152: return numerator & 2097151;
        case 4194304: return numerator & 4194303;
        case 8388608: return numerator & 8388607;
        case 16777216: return numerator & 16777215;
        case 33554432: return numerator & 33554431;
        case 67108864: return numerator & 67108863;
        case 134217728: return numerator & 134217727;
        case 268435456: return numerator & 268435455;
        case 536870912: return numerator & 536870911;
        case 1073741824: return numerator & 1073741823;
        case 2147483648: return numerator & 2147483647;
        default:
            return numerator % denominator;
    }
}
template<>
inline uint64 rem<uint64, uint64>(uint64 numerator, uint64 denominator, OnDivideByZero<uint64> markDivideByZero) {
    switch(denominator) {
        case 0: return defaultDenominatorCheck<uint64>(markDivideByZero);
        case 1: return 0;
        case 2: return numerator & 1;
        case 4: return numerator & 3;
        case 8: return numerator & 7;
        case 16: return numerator & 15;
        case 32: return numerator & 31;
        case 64: return numerator & 63;
        case 128: return numerator & 127;
        case 256: return numerator & 255;
        case 512: return numerator & 511;
        case 1024: return numerator & 1023;
        case 2048: return numerator & 2047;
        case 4096: return numerator & 4095;
        case 8192: return numerator & 8191;
        case 16384: return numerator & 16383;
        case 32768: return numerator & 32767;
        case 65536: return numerator & 65535;
        case 131072: return numerator & 131071;
        case 262144: return numerator & 262143;
        case 524288: return numerator & 524287;
        case 1048576: return numerator & 1048575;
        case 2097152: return numerator & 2097151;
        case 4194304: return numerator & 4194303;
        case 8388608: return numerator & 8388607;
        case 16777216: return numerator & 16777215;
        case 33554432: return numerator & 33554431;
        case 67108864: return numerator & 67108863;
        case 134217728: return numerator & 134217727;
        case 268435456: return numerator & 268435455;
        case 536870912: return numerator & 536870911;
        case 1073741824: return numerator & 1073741823;
        case 2147483648: return numerator & 2147483647;
        case 4294967296: return numerator & 4294967295;
        case 8589934592: return numerator & 8589934591;
        case 17179869184: return numerator & 17179869183;
        case 34359738368: return numerator & 34359738367;
        case 68719476736: return numerator & 68719476735;
        case 137438953472: return numerator & 137438953471;
        case 274877906944: return numerator & 274877906943;
        case 549755813888: return numerator & 549755813887;
        case 1099511627776: return numerator & 1099511627775;
        case 2199023255552: return numerator & 2199023255551;
        case 4398046511104: return numerator & 4398046511103;
        case 8796093022208: return numerator & 8796093022207;
        case 17592186044416: return numerator & 17592186044415;
        case 35184372088832: return numerator & 35184372088831;
        case 70368744177664: return numerator & 70368744177663;
        case 140737488355328: return numerator & 140737488355327;
        case 281474976710656: return numerator & 281474976710655;
        case 562949953421312: return numerator & 562949953421311;
        case 1125899906842624: return numerator & 1125899906842623;
        case 2251799813685248: return numerator & 2251799813685247;
        case 4503599627370496: return numerator & 4503599627370495;
        case 9007199254740992: return numerator & 9007199254740991;
        case 18014398509481984: return numerator & 18014398509481983;
        case 36028797018963968: return numerator & 36028797018963967;
        case 72057594037927936: return numerator & 72057594037927935;
        case 144115188075855872: return numerator & 144115188075855871;
        case 288230376151711744: return numerator & 288230376151711743;
        case 576460752303423488: return numerator & 576460752303423487;
        case 1152921504606846976: return numerator & 1152921504606846975;
        case 2305843009213693952: return numerator & 2305843009213693951;
        case 4611686018427387904: return numerator & 4611686018427387903;
        case 9223372036854775808u: return numerator & 9223372036854775807;
        default:
            return numerator % denominator;
    }
}
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
    constexpr T width = bitwidth<T> - 1;
    // taken from the wikipedia entry on circular shifts
    return static_cast<R>((value << shift) | (value >> ((-shift) & width)));
}

template<typename T, typename R = T>
constexpr R circularShiftRight(T value, T shift) noexcept {
    constexpr T width = bitwidth<T> - 1;
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
 * Checks to see if the given value is even.
 * @param value the value to check evenness against
 * @return boolean signifying that the given input is even.
 * @tparam T the type of the thing to check to see if it is even.
 */
template<typename T>
constexpr bool isEven(T value) noexcept {
    return (value & 1) == 0;
}

/**
 * Checks to see if the given value is odd.
 * @param value the value to check oddness against
 * @return boolean signifying that the given input is odd.
 * @tparam T the type of the thing to check to see if it is odd.
 */
template<typename T>
constexpr bool isOdd(T value) noexcept {
	return !isEven<T>(value);
}

} // end namespace syn

#endif // end SYN_BASE_ARITHMETIC_H__
