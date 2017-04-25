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

    namespace FieldData {
        constexpr uint8 fields[8] = {
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
        constexpr auto Value = static_cast<T>(0);

        template<typename T, int32 index>
        constexpr auto FieldIndex = static_cast<T>(fields[index]);
    } // end namespace FieldData


#define DefFieldData(type, index, mask) \
    namespace FieldData { \
        template<> constexpr auto Value<type, index>  = static_cast<type>(mask); \
    }

#define DefSignedAndUnsignedFieldData(type, index, mask) \
    DefFieldData(type, index, mask) \
    DefFieldData( u ## type, index, mask)

    DefSignedAndUnsignedFieldData(int16, 0, 0x00FF);
    DefSignedAndUnsignedFieldData(int16, 1, 0xFF00);

	DefSignedAndUnsignedFieldData(int32, 0, 0x000000FF);
	DefSignedAndUnsignedFieldData(int32, 1, 0x0000FF00);
	DefSignedAndUnsignedFieldData(int32, 2, 0x00FF0000);
	DefSignedAndUnsignedFieldData(int32, 3, 0xFF000000);

	DefSignedAndUnsignedFieldData(int64,  0, 0x00000000000000FF);
	DefSignedAndUnsignedFieldData(int64,  1, 0x000000000000FF00);
	DefSignedAndUnsignedFieldData(int64,  2, 0x0000000000FF0000);
	DefSignedAndUnsignedFieldData(int64,  3, 0x00000000FF000000);
	DefSignedAndUnsignedFieldData(int64,  4, 0x000000FF00000000);
	DefSignedAndUnsignedFieldData(int64,  5, 0x0000FF0000000000);
	DefSignedAndUnsignedFieldData(int64,  6, 0x00FF000000000000);
	DefSignedAndUnsignedFieldData(int64,  7, 0xFF00000000000000);

#undef DefSignedAndUnsignedFieldData
#undef DefFieldData

    namespace UpperLowerPair {
        template<typename T>
        struct TypeData {
            TypeData() = delete;
            TypeData(const TypeData&) = delete;
            TypeData(TypeData&&) = delete;
            ~TypeData() = delete;
        };
        template<typename T>
        constexpr auto upperMask = static_cast<T>(0);
        template<typename T>
        constexpr auto lowerMask = static_cast<T>(0);
        template<typename T>
        constexpr auto shiftCount = static_cast<T>(0);
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
        template<> constexpr auto upperMask<type> = static_cast< type > ( up ); \
        template<> constexpr auto lowerMask<type> = static_cast< type > ( low ) ; \
        template<> constexpr auto shiftCount<type> = static_cast< type > ( shift ); \
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
using HalfType = typename UpperLowerPair::TypeData<T>::HalfType;

template<typename T>
using QuarterType = HalfType<HalfType<T>>;

template<typename T>
using EighthType = HalfType<QuarterType<T>>;

template<typename T>
constexpr T getUpperMask() noexcept {
    return UpperLowerPair::upperMask<T>;
}
template<typename T>
constexpr T getLowerMask() noexcept {
    return UpperLowerPair::lowerMask<T>;
}

template<typename T>
constexpr T getShiftCount() noexcept {
    return UpperLowerPair::shiftCount<T>;
}

template<typename T>
constexpr size_t bitwidth = CHAR_BIT * sizeof(T);


template<typename T, T index>
constexpr auto singleBitmaskValue = static_cast<T>(0x1 << index);

template<typename T>
constexpr T computeSingleBitmask(T index) noexcept {
	return static_cast<T>(1 << index);
}

template<typename T, T bitmask>
constexpr T mask(T input) noexcept {
    return input & bitmask;
}

template<> constexpr uint8 mask<uint8, 0xFF>(uint8 value) noexcept { return value; }
template<> constexpr uint8 mask<uint8, 0>(uint8 value) noexcept { return 0; }

template<> constexpr uint16 mask<uint16, 0xFFFF>(uint16 value) noexcept { return value; }
template<> constexpr uint16 mask<uint16, 0>(uint16 value) noexcept { return 0; }

template<> constexpr uint32 mask<uint32, 0xFFFFFFFF>(uint32 value) noexcept { return value; }
template<> constexpr uint32 mask<uint32, 0>(uint32 value) noexcept { return 0; }

template<> constexpr uint64 mask<uint64, 0xFFFFFFFFFFFFFFFF>(uint64 value) noexcept { return value; }
template<> constexpr uint64 mask<uint64, 0>(uint64 value) noexcept { return 0; }

template<typename T, typename F, T bitmask, T shiftcount>
constexpr F decodeBits(T input) noexcept {
    auto result = mask<T, bitmask>(input);
    if (shiftcount != 0) {
        result >>= shiftcount;
    }
    return static_cast<F>(result);
}

template<> constexpr uint8 decodeBits<uint8, uint8, 0xFF, 0>(uint8 input) noexcept { return input; }
template<> constexpr uint8 decodeBits<uint8, uint8, 0, 0>(uint8 input) noexcept { return 0; }
template<> constexpr uint16 decodeBits<uint16, uint16, 0xFFFF, 0>(uint16 input) noexcept { return input; }
template<> constexpr uint16 decodeBits<uint16, uint16, 0, 0>(uint16 input) noexcept { return 0; }
template<> constexpr uint32 decodeBits<uint32, uint32, 0xFFFFFFFF, 0>(uint32 input) noexcept { return input; }
template<> constexpr uint32 decodeBits<uint32, uint32, 0, 0>(uint32 input) noexcept { return 0; }
template<> constexpr uint64 decodeBits<uint64, uint64, 0xFFFFFFFFFFFFFFFF, 0>(uint64 input) noexcept { return input; }
template<> constexpr uint64 decodeBits<uint64, uint64, 0, 0>(uint64 input) noexcept { return 0; }

template<typename T, typename F, int field>
constexpr F decodeField(T input) noexcept {
	return decodeBits<T, F, FieldData::Value<T, field>, FieldData::FieldIndex<T, field>>(input);
}

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
template<> constexpr uint8 encodeBits<uint8, uint8, 0xFF, 0>(uint8 input, uint8 value) noexcept { return value; }
template<> constexpr uint16 encodeBits<uint16, uint16, 0xFFFF, 0>(uint16 input, uint16 value) noexcept { return value; }
template<> constexpr uint32 encodeBits<uint32, uint32, 0xFFFFFFFF, 0>(uint32 input, uint32 value) noexcept { return value; }
template<> constexpr uint64 encodeBits<uint64, uint64, 0xFFFFFFFFFFFFFFFF, 0>(uint64 input, uint64 value) noexcept { return value; }
template<> constexpr uint8 encodeBits<uint8, uint8, 0, 0>(uint8 input, uint8 value) noexcept { return input; }
template<> constexpr uint16 encodeBits<uint16, uint16, 0, 0>(uint16 input, uint16 value) noexcept { return input; }
template<> constexpr uint32 encodeBits<uint32, uint32, 0, 0>(uint32 input, uint32 value) noexcept { return input; }
template<> constexpr uint64 encodeBits<uint64, uint64, 0, 0>(uint64 input, uint64 value) noexcept { return input; }

template<typename T, T mask, T shift>
constexpr T encodeFlag(T input, bool value) noexcept {
	return encodeBits<T, bool, mask, shift>(input, value);
}

template<typename T, typename F, int field>
constexpr T encodeField(T input, F value) noexcept {
	return encodeBits<T, F, FieldData::Value<T, field>, FieldData::FieldIndex<T, field>>(input, value);
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
inline uint8_t div<uint8_t, uint8_t>(uint8_t numerator, uint8_t denominator, OnDivideByZero<uint8_t> markDivideByZero) {
     switch(denominator) {
         case 0: return defaultDenominatorCheck<uint8_t>(markDivideByZero);
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
inline uint16_t div<uint16_t, uint16_t>(uint16_t numerator, uint16_t denominator, OnDivideByZero<uint16_t> markDivideByZero) {
     switch(denominator) {
         case 0: return defaultDenominatorCheck<uint16_t>(markDivideByZero);
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
inline uint32_t div<uint32_t, uint32_t>(uint32_t numerator, uint32_t denominator, OnDivideByZero<uint32_t> markDivideByZero) {
     switch(denominator) {
         case 0: return defaultDenominatorCheck<uint32_t>(markDivideByZero);
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
inline uint64_t div<uint64_t, uint64_t>(uint64_t numerator, uint64_t denominator, OnDivideByZero<uint64_t> markDivideByZero) {
     switch(denominator) {
         case 0: return defaultDenominatorCheck<uint64_t>(markDivideByZero);
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
inline uint8_t rem<uint8_t, uint8_t>(uint8_t numerator, uint8_t denominator, OnDivideByZero<uint8_t> markDivideByZero) {
    switch(denominator) {
        case 0: return defaultDenominatorCheck<uint8_t>(markDivideByZero);
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
inline uint16_t rem<uint16_t, uint16_t>(uint16_t numerator, uint16_t denominator, OnDivideByZero<uint16_t> markDivideByZero) {
    switch(denominator) {
        case 0: return defaultDenominatorCheck<uint16_t>(markDivideByZero);
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
inline uint32_t rem<uint32_t, uint32_t>(uint32_t numerator, uint32_t denominator, OnDivideByZero<uint32_t> markDivideByZero) {
    switch(denominator) {
        case 0: return defaultDenominatorCheck<uint32_t>(markDivideByZero);
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
inline uint64_t rem<uint64_t, uint64_t>(uint64_t numerator, uint64_t denominator, OnDivideByZero<uint64_t> markDivideByZero) {
    switch(denominator) {
        case 0: return defaultDenominatorCheck<uint64_t>(markDivideByZero);
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

template<typename T>
constexpr T multiplyAdd(T a, T b, T c) noexcept {
    return (a * b) + c;
}
template<typename T>
constexpr T defaultErrorState = T::Count;

template<typename T>
constexpr bool isErrorState(T op) noexcept {
    return op == defaultErrorState<T>;
}

template<typename T>
constexpr void throwOnErrorState(T value, const std::string& msg) noexcept {
    if (isErrorState<T>(value)) {
        throw syn::Problem(msg);
    }
}

}
#endif
