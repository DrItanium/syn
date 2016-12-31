#ifndef IRIS_ASM_INTERACT_H
#define IRIS_ASM_INTERACT_H
#include <string>
#include <cstdio>
#include <iostream>
#include <functional>
namespace stdiris {
	void assemble(const std::string& target, FILE* input, std::ostream* output);
	void forEachAssembler(std::function<void(const std::string&)> fn);
}

#endif // end IRIS_ASM_INTERACT_H
