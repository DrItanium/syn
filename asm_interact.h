#ifndef IRIS_ASM_INTERACT_H
#define IRIS_ASM_INTERACT_H
#include <string>
#include <cstdio>
#include "architecture.h"
#include "Problem.h"
namespace iris {
	void parseAssembly(const std::string& target, FILE* input, FILE* output);
	template<Architecture arch>
	void assemble(FILE* input, FILE* output) {
		throw Problem("Unimplemented parser for target "+ getStringFromArchitecture(arch));
	}
}

#endif // end IRIS_ASM_INTERACT_H
