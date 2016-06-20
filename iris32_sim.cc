/* sim.c - the iris simulator */
#include "iris32.h"
#include <iostream>
#include <fstream>
#include <string>

std::istream* input = nullptr;
bool close = false,
	 debug = false;
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
					case 'd':
						debug = true;
						break;
					case 'h':
					default:
						errorfree = false;
						break;
				}
			} else {
				errorfree = false;
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
		// eight threads of execution that are interleaved
		//iris32::ExecState t0, t1, t2, t3, t4, t5, t6, t7;
		//// make sure they all start at the same position
		//t0.gpr[iris32::ArchitectureConstants::ThreadIndex] = 0;
		//t1.gpr[iris32::ArchitectureConstants::ThreadIndex] = 1;
		//t2.gpr[iris32::ArchitectureConstants::ThreadIndex] = 2;
		//t3.gpr[iris32::ArchitectureConstants::ThreadIndex] = 3;
		//t4.gpr[iris32::ArchitectureConstants::ThreadIndex] = 4;
		//t5.gpr[iris32::ArchitectureConstants::ThreadIndex] = 5;
		//t6.gpr[iris32::ArchitectureConstants::ThreadIndex] = 6;
		//t7.gpr[iris32::ArchitectureConstants::ThreadIndex] = 7;
		//iris32::Core core(iris32::ArchitectureConstants::AddressMax, { &t0, &t1, &t2, &t3, &t4, &t5, &t6, &t7 });
		iris32::ExecState t0;
		iris32::Core core(iris32::ArchitectureConstants::AddressMax, { &t0 });
		if (debug) {
			core.toggleDebug();
		}
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
	std::cerr << "usage: " << arg0 << " -h | [-d] [file | -]" << std::endl;
}

