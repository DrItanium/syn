#include "asm_interact.h"
#include "iris.h"
#include "iris18.h"
#include "iris20.h"

#include <map>

namespace stdiris {
	static std::map<std::string, std::function<void(FILE*, std::ostream*)>> assemblers = {
        { "iris20", [](auto a, auto b) { throw stdiris::Problem("Assembler is done through clips!"); }, },
		{ "iris18", iris18::assemble },
		{ "iris", iris::assemble },
	};
	void assemble(const std::string& name, FILE* input, std::ostream* output) {
		auto loc = assemblers.find(name);
		if (loc != assemblers.end()) {
			loc->second(input, output);
		} else {
			std::stringstream stream;
			stream << "Tried to assemble code for non-existent core: " << name << "!!!";
			throw stdiris::Problem(stream.str());
		}
	}

	void forEachAssembler(std::function<void(const std::string&)> fn) {
		for(auto const& entry : assemblers) {
			fn(entry.first);
		}

	}
}
