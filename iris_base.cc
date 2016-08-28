#include "iris_base.h"

namespace iris {

	uint16_t encodeUint16LE(byte a, byte b)  noexcept {
		typedef uint16_t Number;
		typedef byte Field;
		return encodeField<Number, Field, 1>(encodeField<Number, Field, 0>(0, a), b);
	}
	int16_t encodeInt16LE(byte a, byte b)  noexcept {
		typedef int16_t Number;
		typedef byte Field;
		return encodeField<Number, Field, 1>(encodeField<Number, Field, 0>(0, a), b);
	}
	uint32_t encodeUint32LE(byte a, byte b, byte c, byte d)  noexcept {
		typedef uint32_t Number;
		typedef byte Field;
		return encodeField<Number, Field, 3>( encodeField<Number, Field, 2>( encodeField<Number, Field, 1>( encodeField<Number, Field, 0>(0, a), b), c), d);
	}
	uint16_t encodeUint16LE(byte* buf)  noexcept {
		return encodeUint16LE(buf[0], buf[1]);
	}
	uint32_t encodeUint32LE(byte* buf)  noexcept {
		return encodeUint32LE(buf[0], buf[1], buf[2], buf[3]);
	}
	int32_t encodeInt32LE(byte a, byte b, byte c, byte d)  noexcept {
		typedef int32_t Number;
		typedef byte Field;
		return encodeField<Number, Field, 3>( encodeField<Number, Field, 2>( encodeField<Number, Field, 1>( encodeField<Number, Field, 0>(0, a), b), c), d);
	}

	void decodeUint32LE(uint32_t value, byte* storage)  noexcept {
		typedef uint32_t Number;
		typedef byte Field;
		storage[0] = decodeField<Number, Field, 0>(value);
		storage[1] = decodeField<Number, Field, 1>(value);
		storage[2] = decodeField<Number, Field, 2>(value);
		storage[3] = decodeField<Number, Field, 3>(value);
	}
	void decodeUint16LE(uint16_t value, byte* storage)  noexcept {
		typedef uint16_t Number;
		typedef byte Field;
		storage[0] = decodeField<Number, Field, 0>(value);
		storage[1] = decodeField<Number, Field, 1>(value);
	}

	void decodeInt32LE(int32_t value, byte* storage)  noexcept {
		typedef int32_t Number;
		typedef byte Field;
		storage[0] = decodeField<Number, Field, 0>(value);
		storage[1] = decodeField<Number, Field, 1>(value);
		storage[2] = decodeField<Number, Field, 2>(value);
		storage[3] = decodeField<Number, Field, 3>(value);
	}

	void decodeInt16LE(int16_t value, byte* storage)  noexcept {
		typedef int16_t Number;
		typedef byte Field;
		storage[0] = decodeField<Number, Field, 0>(value);
		storage[1] = decodeField<Number, Field, 1>(value);
	}


}
