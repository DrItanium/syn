; define encode and decode C++ functions
(defmodule MAIN
           (import cortex
                   ?ALL)
           (import lisp-parse
                   ?ALL))

(defrule MAIN::identify-deffield
         (object (is-a list)
                 (contents deffield $?)
                 (name ?field))
         =>
         (assert (fact (description is-a)
                       (data deffield)
                       (target ?field))))

