#include "asm_interact.h"

namespace iris {
	void parseAssembly(Architecture arch, FILE* input, std::ostream* output);
	void parseAssembly(const std::string& target, FILE* input, std::ostream* output) {
		parseAssembly(getArchitectureFromString(target), input, output);
	}
	void parseAssembly(Architecture arch, FILE* input, std::ostream* output) {
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
