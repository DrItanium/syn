(defmodule MAIN
           (import cortex
                   ?ALL)
           (import lisp-parse
                   ?ALL)
           (import lower
                   ?ALL))
(deffunction MAIN::parse-file
             (?path)
             (assert (parse-request (path ?path)))
             (focus lisp-parse
                    lower)
             (run)
             TRUE)


