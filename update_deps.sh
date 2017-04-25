#!/bin/bash

make nuke
make
g++ -std=c++14 -MM -Imisc/maya -Imisc/pegtl *.cc > deps.make
