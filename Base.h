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


#ifndef _IRIS_BASE_H
#define _IRIS_BASE_H
#define INDIRECTOR(a, ...) PRIMITIVE_INDIRECTOR(a, __VA_ARGS__)
#define PRIMITIVE_INDIRECTOR(a, ...) a ## __VA_ARGS__
#include <cstdint>
#include "Problem.h"
#include <map>
#include <memory>
#include <climits>

using int8 = int8_t;
using uint8 = uint8_t;
using byte = uint8_t;
using int16 = int16_t;
using uint16 = uint16_t;
using int32 = int32_t;
using uint32 = uint32_t;
using int64 = int64_t;
using uint64 = uint64_t;

namespace syn {

	constexpr int32 fields[8] = {
		0,
		8,
		16,
		24,
		32,
		40,
		48,
		56
	};

	template<typename T, int32 index>
	struct FieldData  {
        FieldData() = delete;
        FieldData(const FieldData&) = delete;
        FieldData(FieldData&&) = delete;
        ~FieldData() = delete;
    };

#define DefFieldData(type, index, mask) \
	template<> \
	struct FieldData<type, index> { \
        FieldData() = delete; \
        FieldData(const FieldData&) = delete; \
        FieldData(FieldData&&) = delete; \
        ~FieldData() = delete; \
        using DataType = type; \
	    static constexpr DataType Value = mask ;  \
	    static constexpr DataType FieldIndex = static_cast<DataType>(fields[index]); \
    }
	DefFieldData(int16, 0, 0x00FF);
	DefFieldData(int16, 1, static_cast<int16>(0xFF00));

	DefFieldData(int32, 0, 0x000000FF);
	DefFieldData(int32, 1, 0x0000FF00);
	DefFieldData(int32, 2, 0x00FF0000);
	DefFieldData(int32, 3, static_cast<int32>(0xFF000000));

	DefFieldData(int64,  0, 0x00000000000000FF);
	DefFieldData(int64,  1, 0x000000000000FF00);
	DefFieldData(int64,  2, 0x0000000000FF0000);
	DefFieldData(int64,  3, 0x00000000FF000000);
	DefFieldData(int64,  4, 0x000000FF00000000);
	DefFieldData(int64,  5, 0x0000FF0000000000);
	DefFieldData(int64,  6, 0x00FF000000000000);
	DefFieldData(int64,  7, static_cast<int64>(0xFF00000000000000));

	DefFieldData(uint16, 0, 0x00FF);
	DefFieldData(uint16, 1, 0xFF00);

	DefFieldData(uint32, 0, 0x000000FF);
	DefFieldData(uint32, 1, 0x0000FF00);
	DefFieldData(uint32, 2, 0x00FF0000);
	DefFieldData(uint32, 3, 0xFF000000);

	DefFieldData(uint64, 0, 0x00000000000000FF);
	DefFieldData(uint64, 1, 0x000000000000FF00);
	DefFieldData(uint64, 2, 0x0000000000FF0000);
	DefFieldData(uint64, 3, 0x00000000FF000000);
	DefFieldData(uint64, 4, 0x000000FF00000000);
	DefFieldData(uint64, 5, 0x0000FF0000000000);
	DefFieldData(uint64, 6, 0x00FF000000000000);
	DefFieldData(uint64, 7, 0xFF00000000000000);
#undef DefFieldData

    template<typename T>
    struct UpperLowerPair { };
    #define DefUpperLowerPair(type, halfType, up, low, shift) \
    template<> \
    struct UpperLowerPair<type> { \
        UpperLowerPair() = delete; \
        UpperLowerPair(const UpperLowerPair&) = delete; \
        UpperLowerPair(UpperLowerPair&&) = delete; \
        ~UpperLowerPair() = delete; \
        using HalfType = halfType; \
        using DataType = type; \
        static constexpr DataType upperMask = static_cast<type>(up); \
        static constexpr DataType lowerMask = static_cast<type>(low); \
        static constexpr DataType shiftCount = static_cast<type>(shift); \
    }
    DefUpperLowerPair(uint8, uint8, 0xF0, 0x0F, 4);
    DefUpperLowerPair(int8, int8, 0xF0, 0x0F, 4);
    DefUpperLowerPair(uint16, byte, 0xFF00, 0x00FF, 8);
    DefUpperLowerPair(int16, int8, 0xFF00, 0x00FF, 8);
    DefUpperLowerPair(uint32, uint16, 0xFFFF0000, 0x0000FFFF, 16);
    DefUpperLowerPair(int32, int16, 0xFFFF0000, 0x0000FFFF, 16);
    DefUpperLowerPair(int64, int32, 0xFFFFFFFF00000000, 0x00000000FFFFFFFF, 32);
    DefUpperLowerPair(uint64, uint32, 0xFFFFFFFF00000000, 0x00000000FFFFFFFF, 32);
#undef DefUpperLowerPair

template<typename T>
using HalfType = typename UpperLowerPair<T>::HalfType;

template<typename T>
using QuarterType = HalfType<HalfType<T>>;

template<typename T>
using EighthType = HalfType<QuarterType<T>>;

template<typename T>
constexpr T getUpperMask() noexcept {
    return UpperLowerPair<T>::upperMask;
}
template<typename T>
constexpr T getLowerMask() noexcept {
    return UpperLowerPair<T>::lowerMask;
}

template<typename T>
constexpr T getShiftCount() noexcept {
    return UpperLowerPair<T>::shiftCount;
}


template<typename T, typename F, T bitmask, T shiftcount>
constexpr F decodeBits(T input) noexcept {
	return static_cast<F>((input & bitmask) >> shiftcount);
}

template<typename T, typename F, int field>
constexpr F decodeField(T input) noexcept {
	return decodeBits<T, F, FieldData<T, field>::Value, FieldData<T, field>::FieldIndex>(input);
}

template<typename T, T mask>
constexpr bool decodeFlag(T input) noexcept {
	return decodeBits<T, bool, mask, static_cast<T>(0)>(input);
}

template<typename T, typename F, T bitmask, T shiftcount>
constexpr T encodeBits(T input, F value) noexcept {
	return static_cast<T>((input & ~bitmask) | ((static_cast<T>(value) << shiftcount) & bitmask));
}

template<typename T, T mask, T shift>
constexpr T encodeFlag(T input, bool value) noexcept {
	return encodeBits<T, bool, mask, shift>(input, value);
}

template<typename T, typename F, int field>
constexpr T encodeField(T input, F value) noexcept {
	return encodeBits<T, F, FieldData<T, field>::Value, FieldData<T, field>::FieldIndex>(input, value);
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

template<typename T>
inline void decode2byteQuantityLE(T value, byte storage[sizeof(T)], int offset = 0) noexcept {
	static_assert(sizeof(T) >= 2, "Provided type is too small to decode a 2 byte quantity into!");
	using Number = T;
	using Field = byte;
	storage[offset + 0] = decodeField<Number, Field, 0>(value);
	storage[offset + 1] = decodeField<Number, Field, 1>(value);
}

template<typename T>
inline void decode4byteQuantityLE(T value, byte storage[sizeof(T)], int offset = 0) noexcept {
	static_assert(sizeof(T) >= 4, "Provided type is too small to decode a 4 byte quantity into!");
	using Number = T;
	using Field = byte;
	storage[offset + 0] = decodeField<Number, Field, 0>(value);
	storage[offset + 1] = decodeField<Number, Field, 1>(value);
	storage[offset + 2] = decodeField<Number, Field, 2>(value);
	storage[offset + 3] = decodeField<Number, Field, 3>(value);
}

template<typename T>
inline void decode8byteQuantityLE(T value, byte storage[sizeof(T)], int offset = 0) noexcept {
	static_assert(sizeof(T) >= 8, "Provided type is too small to decode an 8 byte quantity into!");
	using Number = T;
	using Field = byte;
	storage[offset+0] = decodeField<Number, Field, 0>(value);
	storage[offset+1] = decodeField<Number, Field, 1>(value);
	storage[offset+2] = decodeField<Number, Field, 2>(value);
	storage[offset+3] = decodeField<Number, Field, 3>(value);
	storage[offset+4] = decodeField<Number, Field, 4>(value);
	storage[offset+5] = decodeField<Number, Field, 5>(value);
	storage[offset+6] = decodeField<Number, Field, 6>(value);
	storage[offset+7] = decodeField<Number, Field, 7>(value);
}
inline void decodeUint32LE(uint32 value, byte storage[sizeof(uint32)], int offset = 0) noexcept {
	decode4byteQuantityLE<uint32>(value, storage, offset);
}

inline void decodeUint16LE(uint16 value, byte storage[sizeof(uint16)], int offset = 0) noexcept {
	decode2byteQuantityLE<uint16>(value, storage, offset);
}
inline void decodeInt32LE(int32 value, byte storage[sizeof(int32)], int offset = 0) noexcept {
	decode4byteQuantityLE<int32>(value, storage, offset);

}

inline void decodeInt16LE(int16 value, byte storage[sizeof(int16)], int offset = 0) noexcept {
	decode2byteQuantityLE<int16>(value, storage, offset);
}
inline void decodeUint64LE(uint64 value, byte storage[sizeof(uint64)], int offset = 0) noexcept {
	decode8byteQuantityLE<uint64>(value, storage, offset);
}

inline void decodeInt64LE(int64 value, byte storage[sizeof(int64)], int offset = 0) noexcept {
	decode8byteQuantityLE<int64>(value, storage, offset);
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

template<typename T>
constexpr T add(T a, T b) noexcept {
	return a + b;
}

template<typename T>
constexpr T sub(T a, T b) noexcept {
	return a - b;
}

template<typename T>
constexpr T mul(T a, T b) noexcept {
	return a * b;
}

template<typename T>
inline T div(T numerator, T denominator) {
	if (denominator == 0) {
		throw syn::Problem("Denominator is zero");
	} else {
		return numerator / denominator;
	}
}

template<typename T>
inline T rem(T numerator, T denominator) {
	if (denominator == 0) {
		throw syn::Problem("Denominator is zero");
	} else {
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
    constexpr T width = (CHAR_BIT * sizeof(T)) - 1;
    // taken from the wikipedia entry on circular shifts
    return static_cast<R>((value << shift) | (value >> ((-shift) & width)));
}

template<typename T, typename R = T>
constexpr R circularShiftRight(T value, T shift) noexcept {
    constexpr T width = (CHAR_BIT * sizeof(T)) - 1;
    // taken from the wikipedia entry on circular shifts
	return static_cast<R>( (value >> shift) | (value << ((-shift) & width)));
}

template<typename T, T index>
constexpr bool getBit(T value) noexcept {
    return decodeBits<T, bool, 1 << index, index>(value);
}

constexpr byte expandBit(bool value) noexcept {
    return value ? 0xFF : 0x00;
}

template<typename T, T index>
constexpr T setBit(T value, bool bit) noexcept {
    return syn::encodeBits<T, bool, 1 << index, index>(value, bit);
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
constexpr typename UpperLowerPair<T>::HalfType getUpperHalf(T value) noexcept {
    using InputType = T;
    using DataPair = UpperLowerPair<T>;
    using OutputType = typename DataPair::HalfType;
    return syn::decodeBits<InputType, OutputType, DataPair::upperMask, DataPair::shiftCount>(value);
}
template<typename T>
constexpr typename UpperLowerPair<T>::HalfType getLowerHalf(T value) noexcept {
    using InputType = T;
    using DataPair = UpperLowerPair<T>;
    using OutputType = typename DataPair::HalfType;
    return syn::decodeBits<InputType, OutputType, DataPair::lowerMask, 0>(value);
}

template<typename T>
constexpr T setLowerHalf(T value, typename UpperLowerPair<T>::HalfType lower) noexcept {
    using DataPair = UpperLowerPair<T>;
    return syn::encodeBits<T, typename DataPair::HalfType, DataPair::lowerMask, 0>(value, lower);
}
template<typename T>
constexpr T setUpperHalf(T value, typename UpperLowerPair<T>::HalfType upper) noexcept {
    using DataPair = UpperLowerPair<T>;
    return syn::encodeBits<T, typename DataPair::HalfType, DataPair::lowerMask, DataPair::shiftCount>(value, upper);
}

template<typename T>
constexpr size_t bitwidth() noexcept {
	return CHAR_BIT * sizeof(T);
}

template<typename T>
inline void swap(T& a, T& b) {
    auto c = b;
    b = a;
    a = c;
}

template<typename T>
constexpr bool inRangeInclusive(T value, T minimum, T maximum) noexcept {
	return value >= minimum && value <= maximum;
}

template<typename T>
constexpr bool inRangeExcludingMaximum(T value, T minimum, T maximum) noexcept {
	return value >= minimum && value < maximum;
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
T getc() noexcept {
    byte value = 0;
    std::cin >> std::noskipws >> value;
    return static_cast<T>(value);
}

template<typename T>
void putc(T value) noexcept {
    std::cout << static_cast<char>(value);
}

}
#endif
