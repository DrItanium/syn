#ifndef IRIS_ASM_INTERACT_H
#define IRIS_ASM_INTERACT_H
#include <string>
#include <cstdio>
#include "Problem.h"
#include <iostream>
namespace iris {
	void assemble(const std::string& target, FILE* input, std::ostream* output);
}

#endif // end IRIS_ASM_INTERACT_H
