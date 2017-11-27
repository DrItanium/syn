#!/bin/bash

make nuke
make
g++ -std=c++14 -MM *.cc *.c > deps.make
