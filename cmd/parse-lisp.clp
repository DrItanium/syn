(defmodule MAIN
           (import cortex
                   ?ALL)
           (import lisp-parse
                   ?ALL)
           (import lisp->intermediary
                   ?ALL))
(deffunction MAIN::parse-file
             (?path)
             (assert (parse-request (path ?path)))
             (focus lisp-parse
                    lisp->intermediary)
             (run)
             TRUE)


