#ifndef IRIS_ASM_INTERACT_H
#define IRIS_ASM_INTERACT_H
#include <string>
#include <cstdio>
#include <iostream>
#include <functional>
#include "Singleton.h"
#include "Registrar.h"
namespace iris {
	using AssembleFunction = std::function<void(FILE*, std::ostream*)>;
	using AssemblerRegistrar = Registrar<AssembleFunction>;
	static Singleton<AssemblerRegistrar> assemblers;
	void assemble(const std::string& target, FILE* input, std::ostream* output);

	using RegisterAssembler = RegisterAction<AssemblerRegistrar>;
}

#endif // end IRIS_ASM_INTERACT_H
