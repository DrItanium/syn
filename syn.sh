#!/bin/bash

rlwrap -pPurple --remember -c -f misc/completions/maya.rlwrap -f misc/completions/syn.rlwrap -m -M .clp ./syn $@
