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


/* sim.c - the iris simulator */
#include <iostream>
#include <fstream>
#include <string>
#include "Problem.h"
#include "Core.h"
#include "CoreRegistrar.h"

void usage(char* arg0);
int main(int argc, char* argv[]) {
    syn::Core* core = nullptr;
    auto debug = false;
    auto close = false;
    std::istream* input = nullptr;
    std::string target;
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
        core = syn::registry.getCore(target);
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
    std::cerr << "usage: " << arg0 << " -h | -d -t <type> [file | -]" << std::endl;
    std::cerr << "Supported Targets:" << std::endl;
    syn::registry.printEachCoreName(std::cerr);
}
