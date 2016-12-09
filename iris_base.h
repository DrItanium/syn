#ifndef _IRIS_BASE_H
#define _IRIS_BASE_H
#define INDIRECTOR(a, ...) PRIMITIVE_INDIRECTOR(a, __VA_ARGS__)
#define PRIMITIVE_INDIRECTOR(a, ...) a ## __VA_ARGS__
#include <cstdint>
#include "Problem.h"
#include <map>
#include <memory>

using int8 = int8_t;
using uint8 = uint8_t;
using byte = uint8_t;
using int16 = int16_t;
using uint16 = uint16_t;
using int32 = int32_t;
using uint32 = uint32_t;
using int64 = int64_t;
using uint64 = uint64_t;

namespace iris {

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
	template<typename T, int index>
	struct FieldData  { };
#define DefFieldData(type, index, mask) \
	template<> \
	struct FieldData<type, index> { \
	static constexpr type Value = mask ;  \
	static constexpr type FieldIndex = static_cast<type>(fields[index]); \
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
        using HalfType = halfType; \
        static constexpr type upperMask = static_cast<type>(up); \
        static constexpr type lowerMask = static_cast<type>(low); \
        static constexpr type shiftCount = static_cast<type>(shift); \
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
inline constexpr T getUpperMask() noexcept {
    return UpperLowerPair<T>::upperMask;
}
template<typename T>
inline constexpr T getLowerMask() noexcept {
    return UpperLowerPair<T>::lowerMask;
}

template<typename T>
inline constexpr T getShiftCount() noexcept {
    return UpperLowerPair<T>::shiftCount;
}


template<typename T, typename F, T bitmask, T shiftcount>
constexpr inline F decodeBits(T input) noexcept {
	return static_cast<F>((input & bitmask) >> shiftcount);
}

template<typename T, typename F, int field>
constexpr inline F decodeField(T input) noexcept {
	return decodeBits<T, F, FieldData<T, field>::Value, FieldData<T, field>::FieldIndex>(input);
}

template<typename T, T mask>
constexpr inline bool decodeFlag(T input) noexcept {
	return decodeBits<T, bool, mask, static_cast<T>(0)>(input);
}

template<typename T, typename F, T bitmask, T shiftcount>
constexpr inline T encodeBits(T input, F value) noexcept {
	return static_cast<T>((input & ~bitmask) | (static_cast<T>(value) << shiftcount));
}

template<typename T, T mask, T shift>
constexpr inline T encodeFlag(T input, bool value) noexcept {
	return encodeBits<T, bool, mask, shift>(input, value);
}

template<typename T, typename F, int field>
constexpr inline T encodeField(T input, F value) noexcept {
	return encodeBits<T, F, FieldData<T, field>::Value, FieldData<T, field>::FieldIndex>(input, value);
}

template<typename T>
inline constexpr T encodeValueLE(HalfType<T> lower, HalfType<T> upper) noexcept {
    // this will break on int4 and such ;)
    return encodeBits<T, HalfType<T>, getUpperMask<T>(), getShiftCount<T>()>( encodeBits<T, HalfType<T>, getLowerMask<T>(), 0>(0, lower), upper);
}
template<typename T>
inline constexpr T encodeValueLE(QuarterType<T> lowest, QuarterType<T> upperLower, QuarterType<T> lowerUpper, QuarterType<T> upperMost) noexcept {
    return encodeValueLE<T>(encodeValueLE<HalfType<T>>(lowest, upperLower), encodeValueLE<HalfType<T>>(lowerUpper, upperMost));
}

template<typename T>
inline constexpr T encodeValueLE(HalfType<T>* buf) noexcept {
    return encodeValueLE<T>(buf[0], buf[1]);
}

template<typename T>
inline constexpr T encodeValueLE(QuarterType<T>* buf) noexcept {
    return encodeValueLE<T>(buf[0], buf[1], buf[2], buf[3]);
}

inline constexpr uint16 encodeUint16LE(byte a, byte b) noexcept {
    return encodeValueLE<uint16>(a, b);
}
inline constexpr int16 encodeInt16LE(byte a, byte b) noexcept {
    using Number = int16;
    using Field = byte;
    return encodeField<Number, Field, 1>(encodeField<Number, Field, 0>(0, a), b);
}
inline constexpr uint32 encodeUint32LE(byte a, byte b, byte c, byte d)  noexcept {
    using Number = uint32;
    using Field = byte;
	return encodeField<Number, Field, 3>( encodeField<Number, Field, 2>( encodeField<Number, Field, 1>( encodeField<Number, Field, 0>(0, a), b), c), d);
}
inline constexpr uint16 encodeUint16LE(byte* buf)  noexcept {
	return encodeUint16LE(buf[0], buf[1]);
}
inline constexpr uint32 encodeUint32LE(byte* buf)  noexcept {
	return encodeUint32LE(buf[0], buf[1], buf[2], buf[3]);
}
inline constexpr int32 encodeInt32LE(byte lowest, byte upperLower, byte lowerUpper, byte upperMost)  noexcept {
    using Number = int32;
    using Field = byte;
	return encodeField<Number, Field, 3>( encodeField<Number, Field, 2>( encodeField<Number, Field, 1>( encodeField<Number, Field, 0>(0, lowest), upperLower), lowerUpper), upperMost);
}

inline constexpr uint32 encodeUint32LE(uint16 lower, uint16 upper) noexcept {
	return encodeBits<uint32, uint16, getUpperMask<uint32>(), getShiftCount<uint32>()>(encodeBits<uint32, uint16, getLowerMask<uint32>(), getShiftCount<uint32>()>(0, lower), upper);
}

inline constexpr int32 encodeInt32LE(int16 lower, int16 upper) noexcept {
	return encodeBits<int32, int16, static_cast<int32>(0xFFFF0000), 16>(encodeBits<int32, int16, 0x0000FFFF, 0>(0, lower), upper);
}

inline void decodeUint32LE(uint32 value, byte storage[sizeof(uint32)]) noexcept {
	using Number = uint32;
	using Field = byte;
	storage[0] = decodeField<Number, Field, 0>(value);
	storage[1] = decodeField<Number, Field, 1>(value);
	storage[2] = decodeField<Number, Field, 2>(value);
	storage[3] = decodeField<Number, Field, 3>(value);
}

inline void decodeUint16LE(uint16 value, byte storage[sizeof(uint16)]) noexcept {
	using Number = uint16;
	using Field = byte;
	storage[0] = decodeField<Number, Field, 0>(value);
	storage[1] = decodeField<Number, Field, 1>(value);
}
inline void decodeInt32LE(int32 value, byte storage[sizeof(int32)]) noexcept {
		using Number = int32;
		using Field = byte;
		storage[0] = decodeField<Number, Field, 0>(value);
		storage[1] = decodeField<Number, Field, 1>(value);
		storage[2] = decodeField<Number, Field, 2>(value);
		storage[3] = decodeField<Number, Field, 3>(value);

}

inline void decodeInt16LE(int16 value, byte storage[sizeof(int16)]) noexcept {
		using Number = int16;
		using Field = byte;
		storage[0] = decodeField<Number, Field, 0>(value);
		storage[1] = decodeField<Number, Field, 1>(value);
}

inline void decodeUint64LE(uint64 value, byte storage[sizeof(uint64)]) noexcept {
	using Number = uint64;
	using Field = byte;
	storage[0] = decodeField<Number, Field, 0>(value);
	storage[1] = decodeField<Number, Field, 1>(value);
	storage[2] = decodeField<Number, Field, 2>(value);
	storage[3] = decodeField<Number, Field, 3>(value);
	storage[4] = decodeField<Number, Field, 4>(value);
	storage[5] = decodeField<Number, Field, 5>(value);
	storage[6] = decodeField<Number, Field, 6>(value);
	storage[7] = decodeField<Number, Field, 7>(value);
}

inline void decodeInt64LE(int64 value, byte storage[sizeof(int64)]) noexcept {
	using Number = int64;
	using Field = byte;
	storage[0] = decodeField<Number, Field, 0>(value);
	storage[1] = decodeField<Number, Field, 1>(value);
	storage[2] = decodeField<Number, Field, 2>(value);
	storage[3] = decodeField<Number, Field, 3>(value);
	storage[4] = decodeField<Number, Field, 4>(value);
	storage[5] = decodeField<Number, Field, 5>(value);
	storage[6] = decodeField<Number, Field, 6>(value);
	storage[7] = decodeField<Number, Field, 7>(value);
}


inline constexpr uint64 encodeUint64LE(uint32 lower, uint32 upper) noexcept {
	return encodeBits<uint64, uint32,  0xFFFFFFFF00000000, 32>(encodeBits<uint64, uint32, 0x00000000FFFFFFFF, 0>(0, lower), upper);
}
inline constexpr uint64 encodeUint64LE(byte a, byte b, byte c, byte d, byte e, byte f, byte g, byte h) noexcept {
	return encodeUint64LE(encodeUint32LE(a, b, c, d), encodeUint32LE(e, f, g, h));
}

inline constexpr uint64 encodeUint64LE(uint16 a, uint16 b, uint16 c, uint16 d) noexcept {
	return encodeUint64LE(encodeUint32LE(a, b), encodeUint32LE(c, d));
}

inline constexpr uint64 encodeUint64LE(byte* buf) noexcept {
	return encodeUint64LE(buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]);
}

inline constexpr int64 encodeInt64LE(uint32 lower, uint32 upper) noexcept {
	return encodeBits<int64, uint32,  static_cast<int64>(0xFFFFFFFF00000000), 32>(encodeBits<int64, uint32, 0x00000000FFFFFFFF, 0>(0, lower), upper);
}
inline constexpr int64 encodeInt64LE(byte a, byte b, byte c, byte d, byte e, byte f, byte g, byte h) noexcept {
	return encodeInt64LE(encodeInt32LE(a, b, c, d), encodeInt32LE(e, f, g, h));
}

inline constexpr int64 encodeInt64LE(uint16 a, uint16 b, uint16 c, uint16 d) noexcept {
	return encodeInt64LE(encodeInt32LE(a, b), encodeInt32LE(c, d));
}

inline constexpr int64 encodeInt64LE(byte* buf) noexcept {
	return encodeInt64LE(buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]);
}

template<typename T, typename F>
inline constexpr F decodeBits(T value, T mask, T shiftcount) {
	return static_cast<F>((value & mask) >> shiftcount);
}

template<typename T, typename F>
inline constexpr T encodeBits(T input, F value, T bitmask, T shiftcount) {
	return static_cast<T>((input & ~bitmask) | (static_cast<T>(value) << shiftcount));
}

template<typename T>
inline constexpr T add(T a, T b) noexcept {
	return a + b;
}

template<typename T>
inline constexpr T sub(T a, T b) noexcept {
	return a - b;
}

template<typename T>
inline constexpr T mul(T a, T b) noexcept {
	return a * b;
}

template<typename T>
inline T div(T numerator, T denominator) {
	if (denominator == 0) {
		throw iris::Problem("Denominator is zero");
	} else {
		return numerator / denominator;
	}
}

template<typename T>
inline T rem(T numerator, T denominator) {
	if (denominator == 0) {
		throw iris::Problem("Denominator is zero");
	} else {
		return numerator % denominator;
	}
}
template<typename T>
inline constexpr bool eq(T a, T b) noexcept {
	return a == b;
}

template<typename T>
inline constexpr bool neq(T a, T b) noexcept {
	return a != b;
}

template<typename T>
inline constexpr bool lt(T a, T b) noexcept {
	return a < b;
}

template<typename T>
inline constexpr bool gt(T a, T b) noexcept {
	return a > b;
}

template<typename T>
inline constexpr bool le(T a, T b) noexcept {
	return a <= b;
}

template<typename T>
inline constexpr bool ge(T a, T b) noexcept {
	return a >= b;
}

template<typename T>
inline constexpr T shiftLeft(T a, T b) noexcept {
	return a << b;
}

template<typename T>
inline constexpr T shiftRight(T a, T b) noexcept {
	return a >> b;
}

template<typename T>
inline constexpr T binaryAnd(T a, T b) noexcept {
	return a & b;
}

template<typename T>
inline constexpr T binaryOr(T a, T b) noexcept {
	return a | b;
}

template<typename T>
inline constexpr T binaryNot(T a) noexcept {
	return ~a;
}

template<typename T>
inline constexpr T binaryXor(T a, T b) noexcept {
	return a ^ b;
}

template<typename T>
inline constexpr T binaryNand(T a, T b) noexcept {
	return ~(a & b);
}
template<typename T, T index>
inline constexpr bool getBit(T value) noexcept {
    return decodeBits<T, bool, 1 << index, index>(value);
}

inline constexpr byte expandBit(bool value) {
    return value ? 0xFF : 0x00;
}

template<typename T, T index>
inline constexpr T setBit(T value, bool bit) {
    return iris::encodeBits<T, bool, 1 << index, index>(value, bit);
}
inline constexpr uint32 expandUInt32LE(bool lowest, bool lowerUpper, bool upperLower, bool upperMost) noexcept {
    return encodeUint32LE(expandBit(lowest), expandBit(lowerUpper), expandBit(upperLower), expandBit(upperMost));
}
inline constexpr uint16 expandUInt16LE(bool lower, bool upper) noexcept {
    return encodeUint16LE(expandBit(lower), expandBit(upper));
}

inline constexpr uint64 expandUInt64LE(bool b0, bool b1, bool b2, bool b3, bool b4, bool b5, bool b6, bool b7) noexcept {
    return encodeUint64LE(expandUInt32LE(b0, b1, b2, b3), expandUInt32LE(b4, b5, b6, b7));
}

template<typename T>
inline constexpr typename UpperLowerPair<T>::HalfType getUpperHalf(T value) noexcept {
    using InputType = T;
    using DataPair = UpperLowerPair<T>;
    using OutputType = typename DataPair::HalfType;
    return iris::decodeBits<InputType, OutputType, DataPair::upperMask, DataPair::shiftCount>(value);
}
template<typename T>
inline constexpr typename UpperLowerPair<T>::HalfType getLowerHalf(T value) noexcept {
    using InputType = T;
    using DataPair = UpperLowerPair<T>;
    using OutputType = typename DataPair::HalfType;
    return iris::decodeBits<InputType, OutputType, DataPair::lowerMask, 0>(value);
}

template<typename T>
inline constexpr T setLowerHalf(T value, typename UpperLowerPair<T>::HalfType lower) noexcept {
    using DataPair = UpperLowerPair<T>;
    return iris::encodeBits<T, typename DataPair::HalfType, DataPair::lowerMask, 0>(value, lower);
}
template<typename T>
inline constexpr T setUpperHalf(T value, typename UpperLowerPair<T>::HalfType upper) noexcept {
    using DataPair = UpperLowerPair<T>;
    return iris::encodeBits<T, typename DataPair::HalfType, DataPair::lowerMask, DataPair::shiftCount>(value, upper);
}
/*
inline constexpr byte getUpperHalf(byte value) noexcept {
    return iris::decodeBits<byte, byte, upperByteHalf, 4>(value);
}
inline constexpr byte getLowerHalf(byte value) noexcept {
    return iris::decodeBits<byte, byte, lowerByteHalf, 0>(value);
}
inline constexpr uint16 getUpperHalf(uint16 value) noexcept {
    using N = uint16;
    return iris::decodeBits<N, N, upperUint16Half, 8>(value);
}
inline constexpr uint16 getLowerHalf(uint16 value) noexcept {
    using N = uint16;
    return iris::decodeBits<N, N, lowerUint16Half, 0>(value);
}

inline constexpr uint32 getUpperHalf(uint32 value) noexcept {
    using N = uint32;
    return iris::decodeBits<N, N, upperUint32Half, 16>(value);
}
inline constexpr uint32 getLowerHalf(uint32 value) noexcept {
    using N = uint32;
    return iris::decodeBits<N, N, lowerUint32Half, 0>(value);
}

inline constexpr int32 getUpperHalf(int32 value) noexcept {
    using N = int32;
    return iris::decodeBits<N, N, upperInt32Half, 16>(value);
}
inline constexpr int32 getLowerHalf(int32 value) noexcept {
    using N = int32;
    return iris::decodeBits<N, N, lowerInt32Half, 0>(value);
}

inline constexpr uint32 setUpperHalf(uint32 value, uint16 upper) noexcept {
    return iris::encodeBits<uint32, uint16, upperUint32Half, 16>(value, upper);
}
inline constexpr uint32 setLowerHalf(uint32 value, uint16 lower) noexcept {
    return iris::encodeBits<uint32, uint16, lowerUint32Half, 0>(value, lower);
}

inline constexpr int32 setUpperHalf(int32 value, int16 upper) noexcept {
    return iris::encodeBits<int32, int16, upperInt32Half, 16>(value, upper);
}
inline constexpr int32 setLowerHalf(int32 value, int16 lower) noexcept {
    return iris::encodeBits<int32, int16, lowerInt32Half, 0>(value, lower);
}

inline constexpr uint16 setUpperHalf(uint16 value, byte upper) noexcept {
    return iris::encodeBits<uint16, byte, upperUint16Half, 8>(value, upper);
}
inline constexpr uint16 setLowerHalf(uint16 value, byte lower) noexcept {
    return iris::encodeBits<uint16, byte, lowerUint16Half, 0>(value, lower);
}

inline constexpr int64 getUpperHalf(int64 value) noexcept {
    using N = int64;
    return iris::decodeBits<N, N, upperInt64Half, 32>(value);
}
inline constexpr int64 getLowerHalf(int64 value) noexcept {
    using N = int64;
    return iris::decodeBits<N, N, lowerInt64Half, 0>(value);
}

inline constexpr int64 setUpperHalf(int64 value, int32 upper) noexcept {
    return iris::encodeBits<int64, int32, upperInt64Half, 32>(value, upper);
}
inline constexpr int64 setLowerHalf(int64 value, int32 lower) noexcept {
    return iris::encodeBits<int64, int32, lowerInt64Half, 0>(value, lower);
}

inline constexpr uint64 getUpperHalf(uint64 value) noexcept {
    using N = uint64;
    return iris::decodeBits<N, N, upperUint64Half, 32>(value);
}
inline constexpr uint64 getLowerHalf(uint64 value) noexcept {
    using N = uint64;
    return iris::decodeBits<N, N, lowerUint64Half, 0>(value);
}

inline constexpr uint64 setUpperHalf(uint64 value, uint32 upper) noexcept {
    return iris::encodeBits<uint64, uint32, upperUint64Half, 32>(value, upper);
}
inline constexpr uint64 setLowerHalf(uint64 value, uint32 lower) noexcept {
    return iris::encodeBits<uint64, uint32, lowerUint64Half, 0>(value, lower);
}

*/



template<typename T>
inline void swap(T& a, T& b) {
    auto c = b;
    b = a;
    a = c;
}

}
#endif
