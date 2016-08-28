/* img.c - the iris image creator */
#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include <fstream>
#include "Core.h"
#include "sim_registration.h"
#include "Problem.h"

static void usage(char* arg0);
iris::Core* core = nullptr;
bool debug = false;

int main(int argc, char* argv[]) {
	std::string line("v.img");
	std::string target;
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
					case 't':
						++i;
						target = argv[i];
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
	if(target.empty()) {
		std::cerr << "No target provided!" << std::endl;
		usage(argv[0]);
		return 2;
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
	try {
		core = iris::getCore(target);
		if (!core) {
			throw iris::Problem("core initialization for target '" + target + "' failed!");
		} else {
			if(output && input) {
				if (debug) {
					core->toggleDebug();
				}
				core->initialize();
				core->link(*input);
				core->dump(*output);
				core->shutdown();
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
	} catch(iris::Problem p) {
		std::cerr << "PROBLEM: " << p.what() << std::endl;
		return 1;
	}
}

void usage(char* arg0) {
	std::cerr << "usage: " << arg0 << " -t <target> [-d] [-o <file>] <file>" << std::endl;
	std::cerr << "Supported Targets:" << std::endl;
#define X(blah, str, om) std::cerr << "\t" << str << std::endl;
#include "def/architecture_registrations.def"
#undef X
}
