(defmodule MAIN
           (import cortex
                   ?ALL)
           (import lisp-parse
                   ?ALL))
(deffunction MAIN::parse-file
             (?path)
             (assert (parse-request (path ?path)))
             (focus lisp-parse)
             (run)
             TRUE)


