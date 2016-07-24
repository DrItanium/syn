#include "asm_interact.h"

namespace iris {
	void parseAssembly(Architecture arch, FILE* input, FILE* output);
	void parseAssembly(const std::string& target, FILE* input, FILE* output) {
		parseAssembly(getArchitectureFromString(target), input, output);
	}
	void parseAssembly(Architecture arch, FILE* input, FILE* output) {
		switch(arch) {
#define X(en, str, instance) \
			case Architecture:: en \
				return assemble<Architecture:: en>(input, output);
#include "architecture_registrations.def"
#undef X
			default:
				throw Problem("Provided unknown assembly target to parse");
		}
	}
}
