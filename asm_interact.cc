#include "asm_interact.h"

namespace iris {
	void assemble(const std::string& target, FILE* input, std::ostream* output) {
		assemblers->get(target)(input, output);
	}
}
