#include "iris_base.h"

namespace iris {


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
