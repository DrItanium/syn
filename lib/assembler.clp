; syn
; Copyright (c) 2013-2017, Joshua Scoggins and Contributors
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


; assembler parse and lex
(defmodule assembler
           (import cortex
                   ?ALL)
           (export ?ALL))
(defgeneric assembler::registerp
            "Is the given symbol a register?")
(defgeneric assembler::register-to-index
            "convert the given register to an index value")
(defgeneric assembler::index-to-register
            "convert the given index to a register symbol")
(defgeneric assembler::bitmaskp
            "is the given value a bitmask?")
(defgeneric assembler::binaryp
            "is the given symbol a binary number?")
(defgeneric assembler::hexp
            "is the given symbol a hexadecimal number?")

(defmethod assembler::binaryp
  ((?value LEXEME))
  (has-prefix ?value
              0b))
(defmethod assembler::binaryp
  (?value)
  FALSE)

(defmethod assembler::hexp
  ((?value LEXEME))
  (has-prefix ?value
              0x))
(defmethod assembler::hexp
  (?value)
  FALSE)


(defmethod assembler::bitmaskp
  ((?value LEXEME))
  (has-prefix ?value
              0m))

(defmethod assembler::bitmaskp
  (?value)
  FALSE)

(defmethod assembler::registerp
  ((?sym SYMBOL))
  (and (has-prefix ?sym r)
       (integerp
         (string-to-field
           (sub-string 2 (length$ ?sym)
                       ?sym)))))
(defmethod assembler::registerp
  ((?sym INTEGER))
  FALSE)

(defmethod assembler::register-to-index
  ((?sym SYMBOL
         (registerp ?sym)))
  (string-to-field
    (sub-string 2 (length$ ?sym)
                ?sym)))

(defmethod assembler::index-to-register
  ((?index INTEGER
           (>= ?index 0)))
  (sym-cat r
           ?index))
(defclass assembler::statement
  (is-a indexed-thing-with-children
        has-title))
(defclass assembler::operand
  (is-a thing
        has-value))
(defclass assembler::immediate-operand
  (is-a operand))

(defclass assembler::bitmask-operand
 (is-a operand))
(defclass assembler::stack-operand
 (is-a operand))
(defclass assembler::indirect-operand
 (is-a operand))


(defclass assembler::instruction
  (is-a statement)
  (slot group
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot operation
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass assembler::register-alias
  (is-a statement)
  (slot actual-register
        (type SYMBOL
              INSTANCE)
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmethod assembler::register-to-index
  ((?register register-alias))
  (register-to-index (send ?register
                           get-actual-register)))
(defmethod assembler::registerp
  ((?register register-alias))
  (registerp (send ?register
                   get-actual-register)))


(defclass assembler::label
  (is-a statement)
  (slot address
        (type INTEGER
              SYMBOL
              INSTANCE)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default-dynamic undefined)))

(defclass assembler::data
  (is-a statement)
  (slot value
        (type INTEGER
              SYMBOL
              INSTANCE)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass assembler::scope
  (is-a statement
        indexed-thing-with-children))

(defclass assembler::section
  (is-a scope
        has-title))


(deftemplate assembler::line
             "A single line when dealing with an assembler syntax which is line by line"
             (slot parent
                   (type SYMBOL)
                   (default ?NONE))
             (slot raw-line
                   (type STRING)
                   (default ?NONE))
             (slot line-index
                   (type INTEGER)
                   (default ?NONE))
             (multislot exploded-line))

(deftemplate assembler::file-information
             (slot path
                   (type LEXEME)
                   (default ?NONE))
             (slot router
                   (type SYMBOL)
                   (default-dynamic nil))
             (slot count
                   (type INTEGER))
             (slot current-line
                   (type LEXEME)))

(defrule assembler::open-file
         ?f <- (file-information (router nil)
                                 (path ?path))
         =>
         (bind ?file
               (gensym*))
         (if (open ?path ?file "r") then
           (modify ?f
                   (router ?file)
                   (current-line (readline ?file)))
           else
           (printout werror "Couldn't open " ?path " for reading!" crlf)
           (halt)))

(defrule assembler::readline
         ?f <- (file-information (current-line ?line&~EOF)
                                 (count ?index)
                                 (router ?router&~nil))
         =>
         (assert (line (parent ?router)
                       (raw-line ?line)
                       (exploded-line (explode$ ?line))
                       (line-index ?index)))
         (modify ?f
                 (current-line (readline ?router))
                 (count (+ ?index 1))))
(defrule assembler::done
         (file-information (current-line EOF)
                           (router ?router&~nil))
         =>
         (close ?router))


(deffunction assembler::parse-file
             (?path)
             (assert (file-information (path ?path)))
             (focus assembler)
             (run))

(defrule assembler::identify-labels
         (declare (salience 1))
         ?f <- (line (exploded-line @label ?name)
                     (parent ?parent))
         =>
         (retract ?f)
         (make-instance of label
                        (parent ?parent)
                        (title ?name)))

(defrule assembler::identify-register-alias
         (declare (salience 1))
         ?f <- (line (exploded-line @alias ?name ?register)
                     (line-index ?index)
                     (parent ?parent))
         =>
         (retract ?f)
         (make-instance of register-alias
                        (title ?name)
                        (index ?index)
                        (actual-register ?register)
                        (parent ?parent)))

(defrule assembler::identify-instruction
         ?f <- (line (exploded-line ?group ?operation $?rest)
                     (line-index ?index)
                     (parent ?parent))
         =>
         (retract ?f)
         (make-instance of instruction
                        (parent ?parent)
                        (index ?index)
                        (group ?group)
                        (operation ?operation)
                        (children $?rest)))

(defrule assembler::mark-immediate-operand
         ?f <- (object (is-a instruction)
                       (children $?a immediate ?value $?rest)
                       (name ?instruction))
         =>
         (modify-instance ?f
                          (children $?a (make-instance of immediate-operand
                                                       (parent ?instruction)
                                                       (value ?value))
                                    $?rest)))

(defrule assembler::identify-bitmask-entries
         ?f <- (object (is-a instruction)
                       (children $?a
                                 ?bitmask&:(bitmaskp ?bitmask)
                                 $?b)
                       (name ?inst))
         =>
         (modify-instance ?f
                          (children ?a (make-instance of bitmask-operand
                                                      (parent ?inst)
                                                      (value ?bitmask))
                                    ?b)))

(defrule assembler::identify-stack-operation
         ?f <- (object (is-a instruction)
                       (children $?a
                                 stack ?value&:(registerp ?value)
                                 $?b)
                       (name ?inst))
         =>
         (modify-instance ?f
                          (children $?a
                                    (make-instance of stack-operand
                                                   (parent ?inst)
                                                   (value ?value))
                                    $?b)))

(defrule assembler::identify-indirect-operation
         ?f <- (object (is-a instruction)
                       (children $?a
                                 indirect ?value&:(registerp ?value)
                                 $?b)
                       (name ?inst))
         =>
         (modify-instance ?f
                          (children $?a
                                    (make-instance of indirect-operand
                                                   (parent ?inst)
                                                   (value ?value))
                                    $?b)))

(defrule assembler::translate-halve-macro
         ?f <- (object (is-a instruction)
                       (group halve)
                       (operation ?register))
         =>
         (modify-instance ?f
                          (group arithmetic)
                          (operation div)
                          (children ?register ?register immediate 2)))

(defrule assembler::translate-increment-macro
         ?f <- (object (is-a instruction)
                       (group increment)
                       (operation ?register))
         =>
         (modify-instance ?f
                          (group arithmetic)
                          (operation add)
                          (children ?register ?register immediate 1)))

(defrule assembler::translate-decrement-macro
         ?f <- (object (is-a instruction)
                       (group decrement)
                       (operation ?register))
         =>
         (modify-instance ?f
                          (group arithmetic)
                          (operation sub)
                          (children ?register ?register immediate 1)))

(defrule assembler::translate-zero-macro
         ?f <- (object (is-a instruction)
                       (group zero)
                       (operation ?register))
         =>
         (modify-instance ?f
                          (group arithmetic)
                          (operation sub)
                          (children ?register ?register ?register)))
(defrule assembler::translate-double-macro
         ?f <- (object (is-a instruction)
                       (group double)
                       (operation ?register))
         =>
         (modify-instance ?f
                          (group arithmetic)
                          (operation mul)
                          (children ?register ?register immediate 2)))
