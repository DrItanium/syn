#ifndef _IRIS_BASE_H
#define _IRIS_BASE_H
#define INDIRECTOR(a, ...) PRIMITIVE_INDIRECTOR(a, __VA_ARGS__)
#define PRIMITIVE_INDIRECTOR(a, ...) a ## __VA_ARGS__
#include <cstdint>
#include "Problem.h"
typedef uint8_t byte;
typedef uint16_t uint16;
typedef uint32_t uint32;
namespace iris {

	constexpr int32_t fields[8] = {
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
	DefFieldData(int16_t, 0, 0x00FF);
	DefFieldData(int16_t, 1, int16_t(0xFF00));

	DefFieldData(int32_t, 0, 0x000000FF);
	DefFieldData(int32_t, 1, 0x0000FF00);
	DefFieldData(int32_t, 2, 0x00FF0000);
	DefFieldData(int32_t, 3, int32_t(0xFF000000));

	DefFieldData(int64_t,  0, 0x00000000000000FF);
	DefFieldData(int64_t,  1, 0x000000000000FF00);
	DefFieldData(int64_t,  2, 0x0000000000FF0000);
	DefFieldData(int64_t,  3, 0x00000000FF000000);
	DefFieldData(int64_t,  4, 0x000000FF00000000);
	DefFieldData(int64_t,  5, 0x0000FF0000000000);
	DefFieldData(int64_t,  6, 0x00FF000000000000);
	DefFieldData(int64_t,  7, int64_t(0xFF00000000000000));

	DefFieldData(uint16_t, 0, 0x00FF);
	DefFieldData(uint16_t, 1, 0xFF00);

	DefFieldData(uint32_t, 0, 0x000000FF);
	DefFieldData(uint32_t, 1, 0x0000FF00);
	DefFieldData(uint32_t, 2, 0x00FF0000);
	DefFieldData(uint32_t, 3, 0xFF000000);

	DefFieldData(uint64_t, 0, 0x00000000000000FF);
	DefFieldData(uint64_t, 1, 0x000000000000FF00);
	DefFieldData(uint64_t, 2, 0x0000000000FF0000);
	DefFieldData(uint64_t, 3, 0x00000000FF000000);
	DefFieldData(uint64_t, 4, 0x000000FF00000000);
	DefFieldData(uint64_t, 5, 0x0000FF0000000000);
	DefFieldData(uint64_t, 6, 0x00FF000000000000);
	DefFieldData(uint64_t, 7, 0xFF00000000000000);
#undef DefFieldData


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

inline constexpr uint16_t encodeUint16LE(byte a, byte b) noexcept {
    using Number = uint16_t;
    using Field = byte;
    return encodeField<Number, Field, 1>(encodeField<Number, Field, 0>(0, a), b);
}
inline constexpr int16_t encodeInt16LE(byte a, byte b) noexcept {
    typedef int16_t Number;
    typedef byte Field;
    return encodeField<Number, Field, 1>(encodeField<Number, Field, 0>(0, a), b);
}
inline constexpr uint32_t encodeUint32LE(byte a, byte b, byte c, byte d)  noexcept {
	typedef uint32_t Number;
	typedef byte Field;
	return encodeField<Number, Field, 3>( encodeField<Number, Field, 2>( encodeField<Number, Field, 1>( encodeField<Number, Field, 0>(0, a), b), c), d);
}
inline constexpr uint16_t encodeUint16LE(byte* buf)  noexcept {
	return encodeUint16LE(buf[0], buf[1]);
}
inline constexpr uint32_t encodeUint32LE(byte* buf)  noexcept {
	return encodeUint32LE(buf[0], buf[1], buf[2], buf[3]);
}
inline constexpr int32_t encodeInt32LE(byte lowest, byte upperLower, byte lowerUpper, byte upperMost)  noexcept {
    using Number = int32_t;
    using Field = byte;
	return encodeField<Number, Field, 3>( encodeField<Number, Field, 2>( encodeField<Number, Field, 1>( encodeField<Number, Field, 0>(0, lowest), upperLower), lowerUpper), upperMost);
}

inline constexpr uint32_t encodeUint32LE(uint16_t lower, uint16_t upper) noexcept {
	return encodeBits<uint32_t, uint16_t, 0xFFFF0000, 16>(encodeBits<uint32_t, uint16_t, 0x0000FFFF, 0>(0, lower), upper);
}

inline constexpr int32_t encodeInt32LE(int16_t lower, int16_t upper) noexcept {
	return encodeBits<int32_t, int16_t, static_cast<int32_t>(0xFFFF0000), 16>(encodeBits<int32_t, int16_t, 0x0000FFFF, 0>(0, lower), upper);
}

inline void decodeUint32LE(uint32_t value, byte storage[sizeof(uint32_t)]) noexcept {
	using Number = uint32_t;
	using Field = byte;
	storage[0] = decodeField<Number, Field, 0>(value);
	storage[1] = decodeField<Number, Field, 1>(value);
	storage[2] = decodeField<Number, Field, 2>(value);
	storage[3] = decodeField<Number, Field, 3>(value);
}

inline void decodeUint16LE(uint16_t value, byte storage[sizeof(uint16_t)]) noexcept {
	using Number = uint16_t;
	using Field = byte;
	storage[0] = decodeField<Number, Field, 0>(value);
	storage[1] = decodeField<Number, Field, 1>(value);
}
inline void decodeInt32LE(int32_t value, byte storage[sizeof(int32_t)]) noexcept {
		using Number = int32_t;
		using Field = byte;
		storage[0] = decodeField<Number, Field, 0>(value);
		storage[1] = decodeField<Number, Field, 1>(value);
		storage[2] = decodeField<Number, Field, 2>(value);
		storage[3] = decodeField<Number, Field, 3>(value);

}

inline void decodeInt16LE(int16_t value, byte storage[sizeof(int16_t)]) noexcept {
		using Number = int16_t;
		using Field = byte;
		storage[0] = decodeField<Number, Field, 0>(value);
		storage[1] = decodeField<Number, Field, 1>(value);
}

inline void decodeUint64LE(uint64_t value, byte storage[sizeof(uint64_t)]) noexcept {
	using Number = int64_t;
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

inline constexpr uint64_t encodeUint64LE(uint32_t lower, uint32_t upper) noexcept {
	return encodeBits<uint64_t, uint32_t,  0xFFFFFFFF00000000, 32>(encodeBits<uint64_t, uint32_t, 0x00000000FFFFFFFF, 0>(0, lower), upper);
}
inline constexpr uint64_t encodeUint64LE(byte a, byte b, byte c, byte d, byte e, byte f, byte g, byte h) noexcept {
	return encodeUint64LE(encodeUint32LE(a, b, c, d), encodeUint32LE(e, f, g, h));
}

inline constexpr uint64_t encodeUint64LE(uint16_t a, uint16_t b, uint16_t c, uint16_t d) noexcept {
	return encodeUint64LE(encodeUint32LE(a, b), encodeUint32LE(c, d));
}

template<typename T, typename F>
inline constexpr F decodeBits(T value, T mask, T shiftcount) {
	return static_cast<F>((value & mask) >> shiftcount);
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
inline constexpr bool getBit(T value) {
    return decodeBits<T, bool, 1 << index, index>(value);
}

inline constexpr byte expandBit(bool value) {
    return value ? 0xFF : 0x00;
}

template<typename T, T index>
inline constexpr T setBit(T value, bool bit) {
    return iris::encodeBits<T, bool, 1 << index, index>(value, bit);
}

inline constexpr uint32_t expandUInt32LE(bool lowest, bool lowerUpper, bool upperLower, bool upperMost) noexcept {
    return encodeUint32LE(expandBit(lowest), expandBit(lowerUpper), expandBit(upperLower), expandBit(upperMost));
}
inline constexpr uint16_t expandUInt16LE(bool lower, bool upper) noexcept {
    return encodeUint16LE(expandBit(lower), expandBit(upper));
}

constexpr byte upperByteHalf = 0xF0;
constexpr byte lowerByteHalf = 0x0F;
constexpr uint16_t upperUint16Half = 0xFF00;
constexpr uint16_t lowerUint16Half = 0x00FF;
constexpr uint32_t upperUint32Half = 0xFFFF0000;
constexpr uint32_t lowerUint32Half = 0x0000FFFF;

inline constexpr byte getUpperHalf(byte value) noexcept {
    return iris::decodeBits<byte, byte, upperByteHalf, 4>(value);
}
inline constexpr byte getLowerHalf(byte value) noexcept {
    return iris::decodeBits<byte, byte, lowerByteHalf, 0>(value);
}
inline constexpr uint16_t getUpperHalf(uint16_t value) noexcept {
    using N = uint16_t;
    return iris::decodeBits<N, N, upperUint16Half, 8>(value);
}
inline constexpr uint16_t getLowerHalf(uint16_t value) noexcept {
    using N = uint16_t;
    return iris::decodeBits<N, N, lowerUint16Half, 0>(value);
}

inline constexpr uint32_t getUpperHalf(uint32_t value) noexcept {
    using N = uint32_t;
    return iris::decodeBits<N, N, upperUint32Half, 16>(value);
}
inline constexpr uint32_t getLowerHalf(uint32_t value) noexcept {
    using N = uint32_t;
    return iris::decodeBits<N, N, lowerUint32Half, 0>(value);
}



}
#endif
