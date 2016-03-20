/* sim.c - the iris simulator */
#include "iris64.h"
#include <iostream>
#include <fstream>
#include <string>

std::istream* input = 0;
bool close = false;
iris64::Core core(iris64::ArchitectureConstants::AddressMax);
static void usage(char* arg0);

int main(int argc, char* argv[]) {
	bool errorfree = true;
	int last = argc - 1, 
		i = 0;
	if(argc > 1) {
		for(i = 1; errorfree && (i < last); ++i) {
			std::string tmpline(argv[i]);
			if(tmpline.size() == 2 && tmpline[0] == '-') {
				switch(tmpline[1]) {
					case 'h':
					default:
						errorfree = 0;
						break;
				}
			} else {
				errorfree = 0;
				break;
			}
		}
		if(errorfree) {
			if(i == last) {
				std::string line(argv[last]);
				if(line.size() == 1 && line[0] == '-') {
					input = &std::cin;
					close = false;
				} else if (line.size() >= 1) {
					input = new std::ifstream(line.c_str(), std::ifstream::in | std::ifstream::binary);
					close = true;
				}
			} else {
				std::cerr << "no file provided" << std::endl;
			}
		}
	}
	if(input) {
		core.initialize();
		core.installprogram(*input);
		core.run();
		core.shutdown();
		if (close) {
			static_cast<std::ifstream*>(input)->close();
			delete input;
		}
	} else {
		usage(argv[0]);
	}
}

void usage(char* arg0) {
	std::cerr << "usage: " << arg0 << " -h | [file | -]" << std::endl;
}

