#include "AssemblerRegistrar.h"
#include "iris.h"
#include "cisc0.h"
#include "molecule.h"

#include <map>

namespace syn {
	void AssemblerRegistrar::assemble(const std::string& name, const std::string& inputFilePath, FILE* input, std::ostream* output) {
		auto loc = assemblers.find(name);
		if (loc != assemblers.end()) {
			loc->second(inputFilePath, input, output);
		} else {
			std::stringstream stream;
			stream << "Tried to assemble code for non-existent core: " << name << "!!!";
			throw syn::Problem(stream.str());
		}
	}

	void AssemblerRegistrar::forEachAssembler(AssemblerRegistrar::OnEachEntry fn) {
		for(auto const& entry : assemblers) {
			fn(entry.first);
		}
	}
	void AssemblerRegistrar::addToRegistry(const std::string& name, AssemblerRegistrar::Operation op) {
		assemblers.emplace(name, op);
	}

	AssemblerRegistrar::AssemblerRegistrar() { }
	AssemblerRegistrar::~AssemblerRegistrar() { }

	AssemblerRegistrar assemblerRegistry;
}