#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>

#include "Problem.h"
#include "asm_interact.h"

void usage(char* arg0);

int main(int argc, char* argv[]) {
	FILE* input = nullptr;
	std::string line("v.obj");
	std::string target;

	std::ostream* output = nullptr;
	auto closeOutput = false,
		 closeInput = false,
		 errorfree = true;
	int last = argc - 1,
		i = 0;
	/* make sure these are properly initialized */
	last = argc - 1;
	errorfree = 1;
	i = 0;
	if(argc > 1) {
		for(i = 1; errorfree && (i < last); ++i) {
			std::string tmpline(argv[i]);
			if(tmpline.size() == 2 && tmpline[0] == '-') {
				switch(tmpline[1]) {
					case 't':
						++i;
						target = argv[i];
						break;
					case 'o':
						++i;
						line = argv[i];
						break;
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
				std::string tline(argv[last]);
				if(tline.size() == 1 && tline[0] == '-') {
					input = stdin;
					closeInput = false;
				} else if (tline.size() >= 1) {
					if ((input = fopen(tline.c_str(), "r")) != NULL) {
						closeInput = true;
					} else {
						std::cerr << "Couldn't open " << tline << " for reading!" << std::endl;
						exit(1);
					}
				}
				/* open the output */
				if(line.size() == 1 && line[0] == '-') {
					output = &std::cout; 
					closeOutput = false;
				} else {
					output = new std::ofstream(line.c_str(), std::ofstream::out | std::ofstream::binary);
					closeOutput = true;
				}
			} else {
				std::cerr << "no file provided" << std::endl;
			}
		} else if (target.empty()) {
			std::cerr << "No target provided!" << std::endl;
		}
	} else {
		usage(argv[0]);
		return 0;
	}
	if(output && input) {
		try {
			iris::assemble(target, input, output);
			if (closeOutput) {
				static_cast<std::ofstream*>(output)->close();
				delete output;
				output = 0;
			}
			if(closeInput) {
				fclose(input);
				input = 0;
			}
		} catch (iris::Problem pb) {
			std::cerr << pb.what() << std::endl;
			return 1;
		}
	} else {
		usage(argv[0]);
		return 0;
	}
}

void usage(char* arg0) {
	std::cerr << "usage: " << arg0 << " -t <target> [-o <file>] <file>" << std::endl;
	std::cerr << "Supported Targets:" << std::endl;
	iris::forEachAssembler([](const std::string& name) { std::cerr << "\t" << name << std::endl; });
}
