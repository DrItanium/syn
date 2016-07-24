#ifndef IRIS_ASM_INTERACT_H
#define IRIS_ASM_INTERACT_H
#include <string>
#include <cstdio>
#include "architecture.h"
#include "Problem.h"
#include <iostream>
namespace iris {
	void parseAssembly(const std::string& target, FILE* input, std::ostream* output);
	template<Architecture arch>
	void assemble(FILE* input, std::ostream& output) {
		throw Problem("Unimplemented parser for target "+ getStringFromArchitecture(arch));
	}
}

#endif // end IRIS_ASM_INTERACT_H
