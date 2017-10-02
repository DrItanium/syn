#!/bin/bash

#rlwrap -pPurple --remember -c -f misc/completions/maya.rlwrap -f misc/completions/syn.rlwrap -m -M .clp ./syn $@
# the completions don't work right since CLIPS can use hyphens :(
rlwrap -pPurple -c -m -M .clp ./syn $@
