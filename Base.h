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
constexpr auto largestValue = static_cast<T>(-1);


    /**
     * Describes the upper and lower halves of a type
     */
    namespace UpperLowerPair {
        template<typename T>
        struct TypeData {
            TypeData() = delete;
            TypeData(const TypeData&) = delete;
            TypeData(TypeData&&) = delete;
            ~TypeData() = delete;
        };
        template<typename T>
        constexpr auto shiftCount = bitwidth<T> / 2;
        template<typename T>
        constexpr auto upperMask = static_cast<T>(largestValue<T> << shiftCount<T>);
        template<typename T>
        constexpr auto lowerMask = static_cast<T>(largestValue<T> >> shiftCount<T>);
        template<> constexpr auto lowerMask<int128> = static_cast<int128>(0xFFFFFFFFFFFFFFFF);
        template<> constexpr auto upperMask<int128> = lowerMask<int128> << shiftCount<int128>;
        // negative shifts are not allowed
        template<> constexpr auto lowerMask<int64>  = static_cast<int64>(0x00000000FFFFFFFF);
        template<> constexpr auto upperMask<int64>  = static_cast<int64>(0xFFFFFFFF00000000);
        template<> constexpr auto lowerMask<int32>  = static_cast<int32>(0x0000FFFF);
        template<> constexpr auto upperMask<int32>  = static_cast<int32>(0xFFFF0000);
        template<> constexpr auto upperMask<int16>  = static_cast<int16>(0xFF00);
        template<> constexpr auto lowerMask<int16>  = static_cast<int16>(0x00FF);
        template<> constexpr auto upperMask<int8>   = static_cast<int8>(0xF0);
        template<> constexpr auto lowerMask<int8>   = static_cast<int8>(0x0F);
        // int128 needs to be handled specially!
    } // end namespace UpperLowerPair
    #define DefUpperLowerPair(type, halfType, up, low, shift) \
    namespace UpperLowerPair { \
        template<> struct TypeData<type> { \
            TypeData() = delete; \
            TypeData(const TypeData&) = delete; \
            TypeData(TypeData&&) = delete; \
            ~TypeData() = delete; \
            using HalfType = halfType; \
            using DataType = type; \
        }; \
        static_assert(static_cast<type>(up) == upperMask < type > , "Upper mask is wrong!"); \
        static_assert(static_cast<type>(low) == lowerMask < type > , "Lower mask is wrong!"); \
        static_assert(static_cast<type>(shift) == shiftCount< type > , "shift count is wrong!"); \
    }

    DefUpperLowerPair(uint8, uint8, 0xF0, 0x0F, 4);
    DefUpperLowerPair(int8, int8, 0xF0, 0x0F, 4);
    DefUpperLowerPair(uint16, uint8, 0xFF00, 0x00FF, 8);
    DefUpperLowerPair(int16, int8, 0xFF00, 0x00FF, 8);
    DefUpperLowerPair(uint32, uint16, 0xFFFF0000, 0x0000FFFF, 16);
    DefUpperLowerPair(int32, int16, 0xFFFF0000, 0x0000FFFF, 16);
    DefUpperLowerPair(int64, int32, 0xFFFFFFFF00000000, 0x00000000FFFFFFFF, 32);
    DefUpperLowerPair(uint64, uint32, 0xFFFFFFFF00000000, 0x00000000FFFFFFFF, 32);
    DefUpperLowerPair(uint128, uint64, (largestValue<uint128> << 64),
                                       (largestValue<uint128> >> 64), 64);
    DefUpperLowerPair(int128, int64, (static_cast<int128>(0xFFFFFFFFFFFFFFFF)<< 64),
                                     (static_cast<int128>(0xFFFFFFFFFFFFFFFF)), 64);

#undef DefUpperLowerPair

/**
 * If the given type T was halved, what would its type be?
 * @tparam T the type to be halved
 */
template<typename T>
using HalfType = typename UpperLowerPair::TypeData<T>::HalfType;

/**
 * If the given type T was quartered, what would its type be?
 * @tparam T the type to be quartered
 */
template<typename T>
using QuarterType = HalfType<HalfType<T>>;

/**
 * If the given type was split into eight parts, what would its type be?
 * @tparam T the type to be split into eight parts
 */
template<typename T>
using EighthType = HalfType<QuarterType<T>>;

/**
 * Retrieves the mask to extract the upper half of a type. By default, this
 * will be zero. The backing templated variables must be specialized.
 * @tparam T the type to retrieve the upper half mask of
 * @return the upper half mask of the specified type
 */
template<typename T>
constexpr T getUpperMask() noexcept {
    return UpperLowerPair::upperMask<T>;
}

/**
 * Retrieves the mask to extract the lower half of a type.  By default, this
 * will be zero. The backing templated variables must be specialized.
 * @tparam T the type to retrieve the lower half mask of
 * @return the lower half mask of the specified type
 */
template<typename T>
constexpr T getLowerMask() noexcept {
    return UpperLowerPair::lowerMask<T>;
}

/**
 * Retrieve the shift count needed for extracting the upper half of a given
 * type. Unless specialized this will be zero
 * @tparam T the type to retrieve the shift count of
 * @return the shift count for the specified type
 */
template<typename T>
constexpr T getShiftCount() noexcept {
    return UpperLowerPair::shiftCount<T>;
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
template<> constexpr uint8 mask<uint8, largestValue<uint8>>(uint8 value) noexcept { return value; }
template<> constexpr uint8 mask<uint8, 0>(uint8 value) noexcept { return 0; }

template<> constexpr uint16 mask<uint16, largestValue<uint16>>(uint16 value) noexcept { return value; }
template<> constexpr uint16 mask<uint16, 0>(uint16 value) noexcept { return 0; }

template<> constexpr uint32 mask<uint32, largestValue<uint32>>(uint32 value) noexcept { return value; }
template<> constexpr uint32 mask<uint32, 0>(uint32 value) noexcept { return 0; }

template<> constexpr uint64 mask<uint64, largestValue<uint64>>(uint64 value) noexcept { return value; }
template<> constexpr uint64 mask<uint64, 0>(uint64 value) noexcept { return 0; }

template<> constexpr uint128 mask<uint128, largestValue<uint128>>(uint128 value) noexcept { return value; }
template<> constexpr uint128 mask<uint128, 0>(uint128 value) noexcept { return 0; }


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


template<> constexpr uint8 decodeBits<uint8, uint8, largestValue<uint8>, 0>(uint8 input) noexcept { return input; }
template<> constexpr uint8 decodeBits<uint8, uint8, 0, 0>(uint8 input) noexcept { return 0; }
template<> constexpr uint16 decodeBits<uint16, uint16, largestValue<uint16>, 0>(uint16 input) noexcept { return input; }
template<> constexpr uint16 decodeBits<uint16, uint16, 0, 0>(uint16 input) noexcept { return 0; }
template<> constexpr uint32 decodeBits<uint32, uint32, largestValue<uint32>, 0>(uint32 input) noexcept { return input; }
template<> constexpr uint32 decodeBits<uint32, uint32, 0, 0>(uint32 input) noexcept { return 0; }
template<> constexpr uint64 decodeBits<uint64, uint64, largestValue<uint64>, 0>(uint64 input) noexcept { return input; }
template<> constexpr uint64 decodeBits<uint64, uint64, 0, 0>(uint64 input) noexcept { return 0; }
template<> constexpr uint128 decodeBits<uint128, uint128, largestValue<uint128>, 0>(uint128 input) noexcept { return input; }
template<> constexpr uint128 decodeBits<uint128, uint128, 0, 0>(uint128 input) noexcept { return 0; }

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
template<> constexpr uint8 encodeBits<uint8, uint8, largestValue<uint8>, 0>(uint8 input, uint8 value) noexcept { return value; }
template<> constexpr uint8 encodeBits<uint8, uint8, 0, 0>(uint8 input, uint8 value) noexcept { return input; }
template<> constexpr uint16 encodeBits<uint16, uint16, largestValue<uint16>, 0>(uint16 input, uint16 value) noexcept { return value; }
template<> constexpr uint16 encodeBits<uint16, uint16, 0, 0>(uint16 input, uint16 value) noexcept { return input; }
template<> constexpr uint32 encodeBits<uint32, uint32, largestValue<uint32>, 0>(uint32 input, uint32 value) noexcept { return value; }
template<> constexpr uint32 encodeBits<uint32, uint32, 0, 0>(uint32 input, uint32 value) noexcept { return input; }
template<> constexpr uint64 encodeBits<uint64, uint64, largestValue<uint64>, 0>(uint64 input, uint64 value) noexcept { return value; }
template<> constexpr uint64 encodeBits<uint64, uint64, 0, 0>(uint64 input, uint64 value) noexcept { return input; }
template<> constexpr uint128 encodeBits<uint128, uint128, largestValue<uint128>, 0>(uint128 input, uint128 value) noexcept { return value; }
template<> constexpr uint128 encodeBits<uint128, uint128, 0, 0>(uint128 input, uint128 value) noexcept { return input; }

template<typename T, T mask, T shift>
constexpr T encodeFlag(T input, bool value) noexcept {
	return encodeBits<T, bool, mask, shift>(input, value);
}

template<typename T>
constexpr T encodeValueLE(HalfType<T> lower, HalfType<T> upper) noexcept {
    // this will break on int4 and such ;)
    return encodeBits<T, HalfType<T>, getUpperMask<T>(), getShiftCount<T>()>( encodeBits<T, HalfType<T>, getLowerMask<T>(), 0>(0, lower), upper);
}
template<typename T>
constexpr T encodeValueLE(QuarterType<T> lowest, QuarterType<T> upperLower, QuarterType<T> lowerUpper, QuarterType<T> upperMost) noexcept {
    return encodeValueLE<T>(encodeValueLE<HalfType<T>>(lowest, upperLower), encodeValueLE<HalfType<T>>(lowerUpper, upperMost));
}

template<typename T>
constexpr T encodeValueLE(EighthType<T> a, EighthType<T> b, EighthType<T> c, EighthType<T> d, EighthType<T> e, EighthType<T> f, EighthType<T> g, EighthType<T> h) noexcept {
    return encodeValueLE<T>(
            encodeValueLE<QuarterType<T>>(a, b),
            encodeValueLE<QuarterType<T>>(c, d),
            encodeValueLE<QuarterType<T>>(e, f),
            encodeValueLE<QuarterType<T>>(g, h));
}

constexpr uint16 encodeUint16LE(byte a, byte b) noexcept {
    return encodeValueLE<uint16>(a, b);
}
constexpr int16 encodeInt16LE(byte a, byte b) noexcept {
    return encodeValueLE<int16>(a, b);
}
constexpr uint32 encodeUint32LE(byte a, byte b, byte c, byte d)  noexcept {
    return encodeValueLE<uint32>(a, b, c, d);
}
constexpr uint16 encodeUint16LE(byte* buf)  noexcept {
	return encodeUint16LE(buf[0], buf[1]);
}
constexpr uint32 encodeUint32LE(byte* buf)  noexcept {
	return encodeUint32LE(buf[0], buf[1], buf[2], buf[3]);
}
constexpr int32 encodeInt32LE(byte lowest, byte upperLower, byte lowerUpper, byte upperMost)  noexcept {
    return encodeValueLE<int32>(lowest, upperLower, lowerUpper, upperMost);
}

constexpr uint32 encodeUint32LE(uint16 lower, uint16 upper) noexcept {
    return encodeValueLE<uint32>(lower, upper);
}

constexpr int32 encodeInt32LE(int16 lower, int16 upper) noexcept {
    return encodeValueLE<int32>(lower, upper);
}


constexpr uint64 encodeUint64LE(uint32 lower, uint32 upper) noexcept {
    return encodeValueLE<uint64>(lower, upper);
}
constexpr uint64 encodeUint64LE(byte a, byte b, byte c, byte d, byte e, byte f, byte g, byte h) noexcept {
    return encodeValueLE<uint64>(a, b, c, d, e, f, g, h);
}

constexpr uint64 encodeUint64LE(uint16 a, uint16 b, uint16 c, uint16 d) noexcept {
    return encodeValueLE<uint64>(a, b, c, d);

}

constexpr uint64 encodeUint64LE(byte* buf) noexcept {
	return encodeUint64LE(buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]);
}

constexpr int64 encodeInt64LE(uint32 lower, uint32 upper) noexcept {
    return encodeValueLE<int64>(lower, upper);
}
constexpr int64 encodeInt64LE(byte a, byte b, byte c, byte d, byte e, byte f, byte g, byte h) noexcept {
    return encodeValueLE<int64>(a, b, c, d, e, f, g, h);
}

constexpr int64 encodeInt64LE(uint16 a, uint16 b, uint16 c, uint16 d) noexcept {
    return encodeValueLE<int64>(a, b, c, d);
}

constexpr int64 encodeInt64LE(byte* buf) noexcept {
	return encodeInt64LE(buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]);
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

template<> constexpr bool getBit<byte, 0>(byte value) noexcept { return (singleBitmaskValue<byte, 0> & value) != 0; }
template<> constexpr bool getBit<byte, 1>(byte value) noexcept { return (singleBitmaskValue<byte, 1> & value) != 0; }
template<> constexpr bool getBit<byte, 2>(byte value) noexcept { return (singleBitmaskValue<byte, 2> & value) != 0; }
template<> constexpr bool getBit<byte, 3>(byte value) noexcept { return (singleBitmaskValue<byte, 3> & value) != 0; }

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

constexpr uint32 expandUInt32LE(bool lowest, bool lowerUpper, bool upperLower, bool upperMost) noexcept {
    return encodeUint32LE(expandBit(lowest), expandBit(lowerUpper), expandBit(upperLower), expandBit(upperMost));
}

constexpr uint16 expandUInt16LE(bool lower, bool upper) noexcept {
    return encodeUint16LE(expandBit(lower), expandBit(upper));
}

constexpr uint64 expandUInt64LE(bool b0, bool b1, bool b2, bool b3, bool b4, bool b5, bool b6, bool b7) noexcept {
    return encodeUint64LE(expandUInt32LE(b0, b1, b2, b3), expandUInt32LE(b4, b5, b6, b7));
}

template<typename T>
constexpr typename UpperLowerPair::TypeData<T>::HalfType getUpperHalf(T value) noexcept {
    using InputType = T;
    using DataPair = UpperLowerPair::TypeData<T>;
    using OutputType = typename DataPair::HalfType;
    return syn::decodeBits<InputType, OutputType, UpperLowerPair::upperMask<T>, UpperLowerPair::shiftCount<T>>(value);
}
template<typename T>
constexpr typename UpperLowerPair::TypeData<T>::HalfType getLowerHalf(T value) noexcept {
    using InputType = T;
    using DataPair = UpperLowerPair::TypeData<T>;
    using OutputType = typename DataPair::HalfType;
    return syn::decodeBits<InputType, OutputType, UpperLowerPair::lowerMask<T>, 0>(value);
}
template<> constexpr byte getLowerHalf<uint16>(uint16 value) noexcept { return static_cast<byte>(value); }
template<> constexpr uint16 getLowerHalf<uint32>(uint32 value) noexcept { return static_cast<uint16>(value); }
template<> constexpr uint32 getLowerHalf<uint64>(uint64 value) noexcept { return static_cast<uint32>(value); }

template<typename T>
constexpr T setLowerHalf(T value, typename UpperLowerPair::TypeData<T>::HalfType lower) noexcept {
    using DataPair = UpperLowerPair::TypeData<T>;
    return syn::encodeBits<T, typename DataPair::HalfType, UpperLowerPair::lowerMask<T>, 0>(value, lower);
}
template<typename T>
constexpr T setUpperHalf(T value, typename UpperLowerPair::TypeData<T>::HalfType upper) noexcept {
    using DataPair = UpperLowerPair::TypeData<T>;
    return syn::encodeBits<T, typename DataPair::HalfType, UpperLowerPair::lowerMask<T>, UpperLowerPair::shiftCount<T>>(value, upper);
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
 * Retrieves a byte from std::cin, casts it to the specified type, and returns
 * it.
 * @tparam T the type to cast the input byte to
 * @return the extracted byte cast to the specified type
 */
template<typename T>
T getc() noexcept {
    byte value = 0;
    std::cin >> std::noskipws >> value;
    return static_cast<T>(value);
}

/**
 * Simple wrapper over outputting characters (or whatever) to std::cout
 * @tparam T the type of the input
 * @param value the value to output to standard out
 */
template<typename T>
void putc(T value) noexcept {
    std::cout << static_cast<char>(value);
}

/**
 * A common interface to make it possible to describe error states without
 * using exceptions. This concept is widely used with enums where the Count
 * value doubles as the error state. This makes it possible to mark functions
 * as noexcept and return an error state instead.
 *
 * @tparam T The type of the thing to describe it's error state
 */
template<typename T>
constexpr T defaultErrorState = T::Count;

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

}
#endif // end _SYN_BASE_H
