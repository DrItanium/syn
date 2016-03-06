#include "iris.h"

namespace iris16 {
	DecodedInstruction::DecodedInstruction() { }

	void DecodedInstruction::decode(raw_instruction input) {
#define X(title, mask, shift, type, is_register, post) \
		_ ## post = iris::decodeBits<raw_instruction, type, mask, shift>(input);
#include "instruction.def"
#undef X
	}
}
