/* img.c - the iris image creator */
#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include <fstream>
#include "iris32.h"


enum class Segment  {
	Code, 
	Data,
	Count,
};

static void usage(char* arg0);
static void execute(iris32::Core& core, std::istream& file);
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
		iris32::ExecState t0, t1;
		iris32::Core proc(iris32::ArchitectureConstants::AddressMax, std::move(t0), std::move(t1));
		proc.initialize();
		execute(proc, *input);
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
void execute(iris32::Core& core, std::istream& input) {
	char buf[8] = { 0 };
	for(int lineNumber = 0; input.good(); ++lineNumber) {
		input.read(buf, 8);
		if (input.gcount() < 8 && input.gcount() > 0) {
			std::cerr << "panic: unaligned object file found!" << std::endl;
			exit(1);
		} else if (input.gcount() == 0) {
			if (input.eof()) {
				break;
			} else {
				std::cerr << "panic: something bad happened while reading input file!" << std::endl;
				exit(1);
			}
		}
		word address = iris32::encodeWord(buf[0], buf[1], buf[2], buf[3]),
			 value = iris32::encodeWord(buf[4], buf[5], buf[6], buf[7]);
		if (debug) {
			std::cerr << "current address = 0x" << std::hex << address << "\n\tvalue = 0x" << std::hex << value << std::endl;
		}
		core.write(address, value);
	}
}
