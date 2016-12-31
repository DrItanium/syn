/* sim.c - the iris simulator */
#include <iostream>
#include <fstream>
#include <string>
#include "Problem.h"
#include "Core.h"
#include "sim_registration.h"
#include "iris18.h"
#include "iris.h"

std::istream* input = nullptr;
auto close = false;
auto debug = false;
stdiris::Core* core = nullptr;
static void usage(char* arg0);
std::string target;
int main(int argc, char* argv[]) {
	auto errorfree = true;
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
				usage(argv[0]);
				return 1;
			}
		}
	}
	if(target.empty()) {
		std::cerr << "No target provided!" << std::endl;
		usage(argv[0]);
		return 2;
	}
	// now check the target:
	try {
		core = stdiris::getCore(target);
		if (!core) {
			std::cerr << "PROBLEM: core initialization for target '" << target << "' failed!" << std::endl;
			return 1;
		} else {
			if(input) {
				if (debug) {
					core->toggleDebug();
				}
				core->initialize();
				core->installprogram(*input);
				core->run();
				core->shutdown();
				if (close) {
					static_cast<std::ifstream*>(input)->close();
					delete input;
				}
			} else {
				usage(argv[0]);
			}
		}
	} catch(stdiris::Problem p) {
		std::cerr << "PROBLEM: " << p.what() << std::endl;
		return 1;
	}
}

void usage(char* arg0) {
	std::cerr << "usage: " << arg0 << " -h | -d -t <type> [file | -]" << std::endl;
	std::cerr << "Supported Targets:" << std::endl;
	stdiris::forEachCoreName([](const std::string& name) { std::cerr << "\t" << name << std::endl; });
}
