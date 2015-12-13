#include "target/iris16/iris.h"
namespace iris16 {
	word encodeWord(byte a, byte b) {
		return iris::encodeBits<word, byte, word, 0xFF00, 8>(iris::encodeBits<word, byte, word, 0x00FF>(word(0), a), b);
	}
	dword encodeDword(byte a, byte b, byte c, byte d) {
		return iris::encodeBits<word, byte, word, 0xFF000000, 24>(iris::encodeBits<word, byte, word, 0x00FF0000, 16>( iris::encodeBits<word, byte, word, 0x0000FF00, 8>( iris::encodeBits<word, byte, word, 0x000000FF>(word(0), a), b), c), d);
	}

}
