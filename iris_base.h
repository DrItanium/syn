#ifndef _IRIS_BASE_H
#define _IRIS_BASE_H
#define INDIRECTOR(a, ...) PRIMITIVE_INDIRECTOR(a, __VA_ARGS__)
#define PRIMITIVE_INDIRECTOR(a, ...) a ## __VA_ARGS__
#include <cstdint>
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
inline F decodeBits(T input) {
   return (F)((input & bitmask) >> shiftcount);
}

template<typename T, typename F, int field> 
inline F decodeField(T input) {
	return decodeBits<T, F, FieldData<T, field>::Value, FieldData<T, field>::FieldIndex>(input);
}

template<typename T, typename F, T bitmask, T shiftcount>
inline T encodeBits(T input, F value) {
	return (T)((input & ~bitmask) | (value << shiftcount));
}

template<typename T, typename F, int field>
inline T encodeField(T input, F value) {
	return encodeBits<T, F, FieldData<T, field>::Value, FieldData<T, field>::FieldIndex>(input, value);
}

uint16_t encodeUint16LE(byte a, byte b);
int16_t encodeInt16LE(byte a, byte b);
uint32_t encodeUint32LE(byte a, byte b, byte c, byte d);
int32_t encodeInt32LE(byte a, byte b, byte c, byte d);
void decodeUint32LE(uint32_t value, byte storage[sizeof(uint32_t)]);
void decodeUint16LE(uint16_t value, byte storage[sizeof(uint16_t)]);
void decodeInt32LE(int32_t value, byte storage[sizeof(int32_t)]);
void decodeInt16LE(int16_t value, byte storage[sizeof(int16_t)]);

}
#endif
