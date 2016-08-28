#ifndef _IRIS_BASE_H
#define _IRIS_BASE_H
#define INDIRECTOR(a, ...) PRIMITIVE_INDIRECTOR(a, __VA_ARGS__)
#define PRIMITIVE_INDIRECTOR(a, ...) a ## __VA_ARGS__
#include <cstdint>
#include "Problem.h"
typedef uint8_t byte;
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

template<typename T, typename F, T bitmask, T shiftcount>
constexpr inline T encodeBits(T input, F value) noexcept {
	return static_cast<T>((input & ~bitmask) | (static_cast<T>(value) << shiftcount));
}

template<typename T, typename F, int field>
constexpr inline T encodeField(T input, F value) noexcept {
	return encodeBits<T, F, FieldData<T, field>::Value, FieldData<T, field>::FieldIndex>(input, value);
}

constexpr uint16_t encodeUint16LE(byte a, byte b) noexcept {
		using Number = uint16_t;
		using Field = byte;
		return encodeField<Number, Field, 1>(encodeField<Number, Field, 0>(0, a), b);
}
constexpr int16_t encodeInt16LE(byte a, byte b) noexcept {
		typedef int16_t Number;
		typedef byte Field;
		return encodeField<Number, Field, 1>(encodeField<Number, Field, 0>(0, a), b);
}
constexpr uint32_t encodeUint32LE(byte a, byte b, byte c, byte d)  noexcept {
	typedef uint32_t Number;
	typedef byte Field;
	return encodeField<Number, Field, 3>( encodeField<Number, Field, 2>( encodeField<Number, Field, 1>( encodeField<Number, Field, 0>(0, a), b), c), d);
}
constexpr uint16_t encodeUint16LE(byte* buf)  noexcept {
	return encodeUint16LE(buf[0], buf[1]);
}
constexpr uint32_t encodeUint32LE(byte* buf)  noexcept {
	return encodeUint32LE(buf[0], buf[1], buf[2], buf[3]);
}
constexpr int32_t encodeInt32LE(byte a, byte b, byte c, byte d)  noexcept {
	typedef int32_t Number;
	typedef byte Field;
	return encodeField<Number, Field, 3>( encodeField<Number, Field, 2>( encodeField<Number, Field, 1>( encodeField<Number, Field, 0>(0, a), b), c), d);
}
void decodeUint32LE(uint32_t value, byte storage[sizeof(uint32_t)]) noexcept;
void decodeUint16LE(uint16_t value, byte storage[sizeof(uint16_t)]) noexcept;
void decodeInt32LE(int32_t value, byte storage[sizeof(int32_t)]) noexcept;
void decodeInt16LE(int16_t value, byte storage[sizeof(int16_t)]) noexcept;

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
inline constexpr T div(T numerator, T denominator) {
	if (denominator == 0) {
		throw iris::Problem("Denominator is zero");
	} else {
		return numerator / denominator;
	}
}

template<typename T>
inline constexpr T rem(T numerator, T denominator) {
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

}
#endif
