/* strgen.cc - convert a string into an data decl */
#include <iostream>

int main(int argc, char* argv[]) {
	char c = 0;
	std::cin >> c;
	while (!std::cin.eof()) {
		std::cout << "@declare 0x" << std::hex << (int)c << std::endl;
		std::cin >> c;
	}
	std::cout << "@declare 0x00" << std::endl;
	return 0;
}
