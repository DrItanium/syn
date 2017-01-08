#!/bin/bash

m4 -Itarget/iris/m4 target/iris/m4/core.m4 target/iris/m4/macro_functions.m4 target/iris/m4/extended_functions.m4 $@
