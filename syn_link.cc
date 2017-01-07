/* img.c - the iris image creator */
#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include <fstream>
#include "Core.h"
#include "CoreRegistrar.h"
#include "Problem.h"

void usage(char* arg0);

int main(int argc, char* argv[]) {
    syn::Core* core = nullptr;
    auto debug = false;
	std::string line("v.img");
	std::string target;
	std::istream* input = nullptr;
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
                        usage(argv[0]);
                        return 0;
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
		core = syn::registry.getCore(target);
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
            delete core;
        } else {
            usage(argv[0]);
        }
	} catch(syn::Problem p) {
		std::cerr << "PROBLEM: " << p.what() << std::endl;
		return 1;
	}
}

void usage(char* arg0) {
	std::cerr << "usage: " << arg0 << " -t <target> [-d] [-o <file>] <file>" << std::endl;
	std::cerr << "Supported Targets:" << std::endl;
    syn::registry.printEachCoreName(std::cerr);
}
