#!/bin/bash

./iris_repl -f2 lib/cortex.clp -f2 lib/lisp.clp -f2 lib/lower.clp -f2 cmd/parse-lisp.clp $@ -f2 lib/set-main-module.clp -f2 lib/reset.clp
