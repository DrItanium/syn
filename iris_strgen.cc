/* strgen.cc - convert a string into an data decl */
#include <iostream>
#include <fstream>
#include <string>
#include "architecture.h"
#include "Problem.h"
#include "strgen.h"

void outputString(iris::Architecture arch) {
	char c = 0;
	std::cin >> std::noskipws >> c;
	while (!std::cin.eof()) {
		iris::constructWord(arch, std::cout, c);
		std::cin >> std::noskipws >> c;
	}
	iris::constructWord(arch, std::cout, 0);
}
int main(int argc, char* argv[]) {
	outputString(iris::Architecture::iris16);
	return 0;
}

namespace iris {
	void constructWord(Architecture target, std::ostream& out, char c) {
		switch (target) {
#define X(arch, om, nom) case Architecture:: arch : arch :: getWordDescription(out); break;
#include "architecture_registrations.def"
#undef X
			default:
				std::cerr << "Unknown architecture defined!" << std::endl;
				throw 0;
		}
		out << " 0x" << std::hex << (int)c << std::endl;
	}
}
