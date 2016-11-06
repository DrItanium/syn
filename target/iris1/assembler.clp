(defmodule MAIN
           (import cortex
                   ?ALL)
           (import iris1
                   ?ALL)
           (import parsing
                   ?ALL)
           (export ?ALL))
(defclass MAIN::operation
  (is-a statement)
  (slot operation
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::has-destination
  (is-a USER)
  (slot destination
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::has-source0
  (is-a USER)
  (slot source0
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::has-source1
  (is-a USER)
  (slot source1
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::if-statement
  (is-a operation)
  (slot operation
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default if))
  (slot condition
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot then
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot else
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::unary-operation
  (is-a operation
        has-destination
        has-source0))
(defclass MAIN::binary-operation
  (is-a operation
        has-destination
        has-source0
        has-source1))

(defrule MAIN::parse-if-command
         (line (exploded-line if ?cond then ?on-true else ?on-false)
               (line-index ?index)
               (parent ?parent))
         =>
         (make-instance of if-statement
                        (parent ?parent)
                        (index ?index)
                        (condition ?cond)
                        (then ?on-true)
                        (else ?on-false)))

(defrule MAIN::parse-unary-operation
         (line (exploded-line ?op ?dest ?src)
               (line-index ?index)
               (parent ?parent))
         =>
         (make-instance of unary-operation
                        (index ?index)
                        (parent ?parent)
                        (operation load)
                        (destination ?dest)
                        (source0 ?src)))

(defrule MAIN::parse-binary-operation
         (line (exploded-line ?op ?dest ?src0 ?src1)
               (line-index ?index)
               (parent ?parent))
         =>
         (make-instance of binary-operation
                        (parent ?parent)
                        (index ?index)
                        (operation ?op)
                        (destination ?dest)
                        (source0 ?src0)
                        (source1 ?src1)))


(defrule MAIN::parse-data-declaration
         (line (exploded-line @declare ?value)
               (line-index ?i)
               (parent ?p))
         =>
         (make-instance of data
                        (parent ?p)
                        (index ?i)
                        (title @declare)
                        (value ?value)))

(defrule MAIN::identify-register-alias-in-source1
         ?f <- (object (is-a has-source1)
                       (source1 ?value&:(not (registerp ?value))))
         (object (is-a register-alias)
                 (title ?value)
                 (name ?alias))
         =>
         (modify-instance ?f
                          (source1 ?alias)))

(defrule MAIN::identify-label-in-source1
         ?f <- (object (is-a has-source1)
                       (source1 ?value&:(not (registerp ?value))))
         (object (is-a label)
                 (title ?value)
                 (name ?alias))
         =>
         (modify-instance ?f
                          (source1 ?alias)))

(defrule MAIN::identify-register-alias-in-source0
         ?f <- (object (is-a has-source0)
                       (source0 ?value&:(not (registerp ?value))))
         (object (is-a register-alias)
                 (title ?value)
                 (name ?alias))
         =>
         (modify-instance ?f
                          (source0 ?alias)))

(defrule MAIN::identify-register-alias-in-destination
         ?f <- (object (is-a has-destination)
                       (destination ?value&:(not (registerp ?value))))
         (object (is-a register-alias)
                 (title ?value)
                 (name ?alias))
         =>
         (modify-instance ?f 
                          (destination ?alias)))
