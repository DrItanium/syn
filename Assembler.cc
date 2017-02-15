/*
 * syn
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>

#include "Problem.h"
#include "AssemblerRegistrar.h"

void usage(char* arg0);

int main(int argc, char* argv[]) {
	FILE* input = nullptr;
	std::string line("v.obj");
	std::string target;
	std::string inputFile("stdin");

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
		if(errorfree) {
			if(i == last) {
				std::string tline(argv[last]);
				if(tline.size() == 1 && tline[0] == '-') {
					input = stdin;
					closeInput = false;
				} else if (tline.size() >= 1) {
					if ((input = fopen(tline.c_str(), "r")) != NULL) {
						closeInput = true;
						inputFile = tline;
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
			syn::assemblerRegistry.assemble(target, inputFile, input, output);
			if (closeOutput) {
				static_cast<std::ofstream*>(output)->close();
				delete output;
				output = 0;
			}
			if(closeInput) {
				fclose(input);
				input = 0;
			}
		} catch (syn::Problem pb) {
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
	syn::assemblerRegistry.forEachAssembler([](const std::string& name) { std::cerr << "\t" << name << std::endl; });
}
