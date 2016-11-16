; lower from lisp structures to a simpler format

(defmodule lisp->intermediary
           (import cortex
                   ?ALL)
           (import lisp-parse
                   ?ALL)
           (export ?ALL))

;(defrule lisp->intermediary::has-operation
;         (object (is-a list)
;                 (contents ?operation&:(symbolp ?operation) $?)
;                 (name ?value))
;         =>
;         (assert (fact (target ?value)
;                       (description operation-is)
;                       (data ?operation))))


;(defrule lisp->intermediary::mark-top-level
;         (object (is-a file)
;                 (name ?file))
;         (object (is-a node)
;                 (parent ?file)
;                 (name ?node))
;         =>
;         (assert (fact (target ?node)
;                       (description flag)
;                       (data top-level))))

(defrule lisp->intermediary::is-arithmetic-operation
         (object (is-a list)
                 (name ?value)
                 (contents +|-|*|/|% $?))
         =>
         (assert (fact (target ?value)
                       (description operation-class)
                       (data arithmetic))))

(deffunction lisp->intermediary::sym-eq
             ()
             (sym-cat "="))
(defrule lisp->intermediary::is-comparision-operator
         (object (is-a list)
                 (name ?value)
                 (contents =(sym-eq)|<>|!=|eq|neq|>|<|>=|<= $?))
         =>
         (assert (fact (target ?value)
                       (description operation-class)
                       (data comparison))))

;(defrule lisp->intermediary::operation-is-binary
;         (object (is-a list)
;                 (name ?value)
;                 (contents ? ? ?))
;         =>
;         (assert (fact (target ?value)
;                       (description flag)
;                       (data binary-op))))
;(defrule lisp->intermediary::operation-is-unary
;         (object (is-a list)
;                 (name ?value)
;                 (contents ? ?))
;         =>
;         (assert (fact (target ?value)
;                       (description flag)
;                       (data unary-op))))
;
;(defrule lisp->intermediary::operation-has-no-args
;         (object (is-a list)
;                 (name ?value)
;                 (contents ?))
;         =>
;         (assert (fact (target ?value)
;                       (description flag)
;                       (data no-args))))
;
(defrule lisp->intermediary::operation-is-a-bind
         (object (is-a list)
                 (name ?value)
                 (contents bind
                           ?name
                           $?contents))
         (object (is-a variable)
                 (name ?name)
                 (value ?title))
         =>
         (assert (fact (description is-a)
                       (data bind-operation)
                       (target ?value))
                 (fact (description named)
                       (data ?title)
                       (target ?value))
                 (fact (description contents)
                       (data ?contents)
                       (target ?value))))
