/* img.c - the iris image creator */
#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include <fstream>
#include "iris16.h"


static void usage(char* arg0);
static void execute(std::istream& file);
iris16::Core proc;
bool debug = false;

int main(int argc, char* argv[]) {
	std::string line("v.img");
	std::istream* input = 0;
	std::ostream* output = 0;
	bool closeOutput = false,
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
					case 'o':
						++i;
						line = argv[i];
						break;
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
				std::string tline(argv[last]);
				if(tline.size() == 1 && tline[0] == '-') {
					input = &std::cin;
					closeInput = false;
				} else if (tline.size() >= 1) {
					input = new std::ifstream(tline.c_str(), std::ifstream::binary | std::ifstream::in);
					if (input->good()) {
						closeInput = true;
					} else {
						std::cerr << "couldn't open " << tline << " for reading!" << std::endl;
						exit(1);
					}
				}
				/* open the output */
				if(line.size() == 1 && line[0] == '-') {
					output = &std::cout; 
					closeOutput = false;
				} else {
					output = new std::ofstream(line.c_str(), std::ofstream::out | std::ofstream::binary);
					if (output->good()) {
						closeOutput = true;
					} else {
						std::cerr << "couldn't open " << line << " for writing!" << std::endl;
						exit(1);
					}
				}
			} else {
				std::cerr << "no file provided" << std::endl;
			}
		} else {
			usage(argv[0]);
		}
	}

	if(output && input) {
		proc.initialize();
		execute(*input);
		proc.dump(*output);
		proc.shutdown();
		if (closeInput) {
			static_cast<std::ifstream*>(input)->close();
			delete input;
			input = 0;
		}
		if (closeOutput) {
			static_cast<std::ofstream*>(output)->close();
			delete output;
			output = 0;
		}
	} else {
		usage(argv[0]);
	}
}

void usage(char* arg0) {
	std::cerr << "usage: " << arg0 << " [-d] [-o <file>] <file>" << std::endl;
}
