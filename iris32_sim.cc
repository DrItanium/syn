/* sim.c - the iris simulator */
#include "iris32.h"
#include <iostream>
#include <fstream>
#include <string>

std::istream* input = 0;
bool close = false;
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
		iris32::ExecState t0, t1;
		t1.gpr[iris32::ArchitectureConstants::InstructionPointerIndex] = (iris32::ArchitectureConstants::AddressMax / 2);
		iris32::Core core(iris32::ArchitectureConstants::AddressMax, std::move(t0), std::move(t1));
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

