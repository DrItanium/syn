#include "asm_interact.h"
#include "iris18.h"
#include "iris16.h"
#include "iris17.h"
#include "iris19.h"

#include <map>

namespace iris {
	static std::map<std::string, std::function<void(FILE*, std::ostream*)>> assemblers = {
		{ "iris19", iris19::assemble },
		{ "iris18", iris18::assemble },
		{ "iris17", iris17::assemble },
		{ "iris16", iris16::assemble },
	};
	void assemble(const std::string& name, FILE* input, std::ostream* output) {
		auto loc = assemblers.find(name);
		if (loc != assemblers.end()) {
			loc->second(input, output);
		} else {
			std::stringstream stream;
			stream << "Tried to assemble code for non-existent core: " << name << "!!!";
			throw iris::Problem(stream.str());
		}
	}

	void forEachAssembler(std::function<void(const std::string&)> fn) {
		for(auto const& entry : assemblers) {
			fn(entry.first);
		}

	}
}
