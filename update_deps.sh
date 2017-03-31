#!/bin/bash

make clean
make
g++ -std=c++14 -MM -Imisc/maya -Imisc/pegtl *.cc > deps.make
