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


; Copyright (c) 2015-2016 Joshua Scoggins
;
; This software is provided 'as-is', without any express or implied
; warranty. In no event will the authors be held liable for any damages
; arising from the use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must not
;    claim that you wrote the original software. If you use this software
;    in a product, an acknowledgement in the product documentation would be
;    appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must not be
;    misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source distribution.
(defmodule lisp-parse
           (import cortex
                   ?ALL)
           (export ?ALL))

(deftemplate lisp-parse::parse-request
             (slot path
                   (type LEXEME)
                   (default ?NONE)))
(defgeneric lisp-parse::no-strings-in-list
            "checks to see if the given list is free of strings")
(defgeneric lisp-parse::no-primitive-strings-in-list
            "checks to see if the given list is free of primitive strings")
(deffunction lisp-parse::string-classp
             (?value)
             (eq (class ?value)
                 string))
(defmethod lisp-parse::no-primitive-strings-in-list
  ((?list MULTIFIELD))
  (not-exists stringp
              (expand$ ?list)))
(defmethod lisp-parse::no-primitive-strings-in-list
  ($?list)
  (no-primitive-strings-in-list ?list))
(defmethod lisp-parse::no-strings-in-list
  ((?list MULTIFIELD))
  (not-exists string-classp
              (expand$ ?list)))
(defmethod lisp-parse::no-strings-in-list
  ($?list)
  (not-exists string-classp
              $?list))

(defclass lisp-parse::file
  (is-a thing)
  (slot parent
        (source composite)
        (storage shared)
        (access initialize-only)
        (default FALSE))
  (slot path
        (visibility public)
        (storage local)
        (type LEXEME)
        (default ?NONE))
  (slot router
        (visibility public)
        (storage local)
        (type SYMBOL)
        (default ?NONE))
  (multislot top
             (visibility public)
             (storage local)
             (type SYMBOL
                   INSTANCE-NAME))
  (multislot elements
             (visibility public)
             (storage local))
  (message-handler parent-is primary))

(defmessage-handler lisp-parse::file parent-is primary
                    (?parent)
                    (eq (instance-name ?self)
                        ?parent))



(defclass lisp-parse::node
  (is-a thing))

(defclass lisp-parse::has-comment
  (is-a USER)
  (slot comment
        (visibility public)
        (type STRING)))

(defclass lisp-parse::has-local-binds
  (is-a USER)
  (multislot local-binds
             (visibility public)))

(defclass lisp-parse::has-arguments
  (is-a USER)
  (multislot arguments
             (visibility public)))

(defclass lisp-parse::has-body
  (is-a USER)
  (multislot body
             (visibility public)))

(defclass lisp-parse::composite-node
  "A node that is made up of other nodes. This is separate from a list!"
  (is-a node
        has-contents)
  (multislot contents
             (source composite)
             (default ?NONE)))
(defclass lisp-parse::scalar-node
  (is-a node
        has-value)
  (slot value
        (source composite)
        (default ?NONE)))

(defclass lisp-parse::typed-scalar-node
  (is-a scalar-node)
  (slot type
        (type SYMBOL)
        (default ?NONE)))

(defclass lisp-parse::string
  "Strings need to be wrapped in their own nodes"
  (is-a scalar-node)
  (slot value
        (type STRING)
        (source composite)
        (storage local)))
(defclass lisp-parse::variable (is-a scalar-node))
(defclass lisp-parse::global-variable (is-a variable))
(defclass lisp-parse::singlefield-global-variable (is-a global-variable))
(defclass lisp-parse::multifield-global-variable (is-a global-variable))
(defclass lisp-parse::local-variable (is-a variable))
(defclass lisp-parse::multifield-variable (is-a local-variable))
(defclass lisp-parse::singlefield-variable (is-a local-variable))
(defclass lisp-parse::constraint (is-a scalar-node))
(defclass lisp-parse::not-constraint (is-a constraint))
(defclass lisp-parse::and-constraint (is-a constraint))
(defclass lisp-parse::or-constraint (is-a constraint))
(defclass lisp-parse::wildcard (is-a scalar-node))
(defclass lisp-parse::multifield-wildcard (is-a wildcard))
(defclass lisp-parse::singlefield-wildcard (is-a wildcard))



(defclass lisp-parse::list
  (is-a node
        has-contents))

(defclass lisp-parse::reference
  "An indirect reference to something else, useful for deffunctions and arguments"
  (is-a scalar-node)
  (slot expand
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)))
;------------------------------------------------------------------------------
(defrule lisp-parse::open-file
         ?f <- (parse-request (path ?path))
         =>
         (retract ?f)
         (bind ?name
               (gensym*))
         (if (open ?path
                   ?name
                   "r") then
           (make-instance ?name of file
                          (path ?path)
                          (router ?name)
                          (elements (next-token ?name))
                          (top (symbol-to-instance-name ?name)))
           else
           (printout werror
                     "couldn't open " ?path crlf)))

(defrule lisp-parse::new-top-level
         (declare (salience 2))
         (object (is-a file)
                 (elements LPAREN ?)
                 (top ?file)
                 (name ?file)
                 (router ?router))
         =>
         (slot-insert$ ?file
                       top
                       1
                       (make-instance of list
                                      (parent ?file)))
         (slot-replace$ ?file
                        elements
                        1 2
                        (next-token ?router)))

(defrule lisp-parse::new-list
         (declare (salience 3))
         ?f <- (object (is-a file)
                       (elements LPAREN ?)
                       (top ?top $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top))
         =>
         (slot-insert$ ?f
                       top
                       1
                       (make-instance of list
                                      (parent ?top)))
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r)))

(defrule lisp-parse::end-list
         (declare (salience 3))
         ?f <- (object (is-a file)
                       (elements RPAREN ?)
                       (router ?r)
                       (top ?curr ?parent $?rest))
         (object (is-a list)
                 (name ?parent)
                 (contents $?contents))
         =>
         (slot-insert$ ?parent
                       contents
                       (+ (length$ ?contents) 1)
                       ?curr)
         (modify-instance ?f
                          (elements (next-token ?r))
                          (top ?parent
                               ?rest)))
(defrule lisp-parse::end-list:top-level
         (declare (salience 3))
         ?f <- (object (is-a file)
                       (elements RPAREN ?)
                       (router ?r)
                       (top ? ?parent)
                       (name ?parent))
         =>
         (slot-delete$ ?f
                       top
                       1 1)
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r)))


(defrule lisp-parse::parse-special-element
         (declare (salience 1))
         ?f <- (object (is-a file)
                       (elements ?type
                                 ?value)
                       (top ?top $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (modify-instance ?f
                          (elements (next-token ?r)))
         (modify-instance ?top
                          (contents ?contents
                                    (make-instance of ?type
                                                   (parent ?top)
                                                   (value ?value)))))

(defrule lisp-parse::warn:parse-special-element-outside-list
         (declare (salience 1))
         ?f <- (object (is-a file)
                       (elements ?type ?value)
                       (router ?r)
                       (top ?file)
                       (name ?file))
         =>
         (printout werror
                   "WARNING: Found a special tag outside a list!" crlf)
         (modify-instance ?f
                          (elements (next-token ?r)))
         (make-instance of ?type
                        (parent ?file)
                        (value ?value)))

(defrule lisp-parse::parse-string
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements ?value&:(stringp ?value))
                       (top ?top $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (modify-instance ?f
                          (elements (next-token ?r)))
         (slot-insert$ ?top
                       contents
                       (+ (length$ ?contents) 1)
                       (make-instance of string
                                      (parent ?top)
                                      (value ?value))))

(defrule lisp-parse::parse-string-outside-list
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements ?value&:(stringp ?value))
                       (name ?file)
                       (top ?file)
                       (router ?r))
         =>
         (printout werror
                   "WARNING: Found a string outside a list!" crlf)
         (modify-instance ?f
                          (elements (next-token ?r)))
         (make-instance of string
                        (parent ?file)
                        (value ?value)))

(defrule lisp-parse::parse-normal-element
         (declare (salience 1))
         ?f <- (object (is-a file)
                       (elements ?value)
                       (top ?top $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))

         =>
         (slot-insert$ ?top
                       contents
                       (+ (length$ ?contents) 1)
                       ?value)
         (slot-replace$ ?f
                        elements
                        1 1
                        (next-token ?r)))


(defrule lisp-parse::warn:parse-normal-element-outside-list
         (declare (salience 1))
         ?f <- (object (is-a file)
                       (elements ?value)
                       (name ?file)
                       (top ?file)
                       (router ?r))
         =>
         (format werror
                 "WARNING: Found a %s (%s) outside a list!%n"
                 (class ?value)
                 ?value)
         (modify-instance ?f
                          (elements (next-token ?r)))
         (make-instance of typed-scalar-node
                        (parent ?file)
                        (type (class ?value))
                        (value ?value)))


(defrule lisp-parse::error:end-list-without-beginning
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements RPAREN ?)
                       (name ?file)
                       (top ?file)
                       (path ?path))
         =>
         (printout werror
                   "ERROR: " ?file crlf
                   tab "PATH: " ?path crlf
                   tab "found a ) outside an actual list!" crlf)
         (halt))

(defrule lisp-parse::finished-completely
         (declare (salience 3))
         ?f <- (object (is-a file)
                       (elements STOP ?)
                       (router ?name))
         =>
         (close ?name)
         (modify-instance ?f
                          (elements)))

(defrule lisp-parse::error:bottom-of-top-should-be-self-referential
         (declare (salience 10000))
         ?f <- (object (is-a file)
                       (name ?file)
                       (top ?x&~?file)
                       (path ?path))
         =>
         (printout werror
                   "ERROR: " ?file crlf
                   tab "PATH: " ?path crlf
                   tab "The bottom of the parsing stack is not the file itself. Some rule has broken the parser" crlf)
         (halt))

(defrule lisp-parse::error:top-is-empty
         (declare (salience 10000))
         ?f <- (object (is-a file)
                       (top)
                       (path ?file))
         =>
         (printout werror
                   "ERROR: " ?file crlf
                   tab "The parsing stack is empty. Some rule has broken the parser" crlf)
         (halt))
;-----------------------------------------------------------------------------
; Auto generated rules for special symbols
;-----------------------------------------------------------------------------
(defrule lisp-parse::construct-special-instance:or-constraint
         "convert a symbol of type OR_CONSTRAINT to class of type or-constraint"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements OR_CONSTRAINT
                                 ?value)
                       (top ?top
                            $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (slot-insert$ ?top
                       contents
                       (+ (length$ ?contents) 1)
                       (make-instance of or-constraint
                                      (parent ?top)
                                      (value ?value)))
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r)))


(defrule lisp-parse::construct-special-instance-outside-list:or-constraint
         "convert a symbol of type OR_CONSTRAINT to class of type or-constraint"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements OR_CONSTRAINT
                                 ?value)
                       (top ?file)
                       (name ?file)
                       (router ?r))
         =>
         (printout werror
                   "WARNING: Found a special tag outside a list!" crlf)
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r))
         (make-instance of or-constraint
                        (parent ?file)
                        (value ?value)))

(defrule lisp-parse::construct-special-instance:and-constraint
         "convert a symbol of type AND_CONSTRAINT to class of type and-constraint"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements AND_CONSTRAINT
                                 ?value)
                       (top ?top
                            $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (slot-insert$ ?top
                       contents
                       (+ (length$ ?contents) 1)
                       (make-instance of and-constraint
                                      (parent ?top)
                                      (value ?value)))
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r)))


(defrule lisp-parse::construct-special-instance-outside-list:and-constraint
         "convert a symbol of type AND_CONSTRAINT to class of type and-constraint"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements AND_CONSTRAINT
                                 ?value)
                       (top ?file)
                       (name ?file)
                       (router ?r))
         =>
         (printout werror
                   "WARNING: Found a special tag outside a list!" crlf)
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r))
         (make-instance of and-constraint
                        (parent ?file)
                        (value ?value)))

(defrule lisp-parse::construct-special-instance:not-constraint
         "convert a symbol of type NOT_CONSTRAINT to class of type not-constraint"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements NOT_CONSTRAINT
                                 ?value)
                       (top ?top
                            $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (slot-insert$ ?top
                       contents
                       (+ (length$ ?contents) 1)
                       (make-instance of not-constraint
                                      (parent ?top)
                                      (value ?value)))
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r)))


(defrule lisp-parse::construct-special-instance-outside-list:not-constraint
         "convert a symbol of type NOT_CONSTRAINT to class of type not-constraint"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements NOT_CONSTRAINT
                                 ?value)
                       (top ?file)
                       (name ?file)
                       (router ?r))
         =>
         (printout werror
                   "WARNING: Found a special tag outside a list!" crlf)
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r))
         (make-instance of not-constraint
                        (parent ?file)
                        (value ?value)))

(defrule lisp-parse::construct-special-instance:multifield-wildcard
         "convert a symbol of type MF_WILDCARD to class of type multifield-wildcard"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements MF_WILDCARD
                                 ?value)
                       (top ?top
                            $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (slot-insert$ ?top
                       contents
                       (+ (length$ ?contents) 1)
                       (make-instance of multifield-wildcard
                                      (parent ?top)
                                      (value ?value)))
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r)))


(defrule lisp-parse::construct-special-instance-outside-list:multifield-wildcard
         "convert a symbol of type MF_WILDCARD to class of type multifield-wildcard"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements MF_WILDCARD
                                 ?value)
                       (top ?file)
                       (name ?file)
                       (router ?r))
         =>
         (printout werror
                   "WARNING: Found a special tag outside a list!" crlf)
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r))
         (make-instance of multifield-wildcard
                        (parent ?file)
                        (value ?value)))

(defrule lisp-parse::construct-special-instance:singlefield-wildcard
         "convert a symbol of type SF_WILDCARD to class of type singlefield-wildcard"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements SF_WILDCARD
                                 ?value)
                       (top ?top
                            $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (slot-insert$ ?top
                       contents
                       (+ (length$ ?contents) 1)
                       (make-instance of singlefield-wildcard
                                      (parent ?top)
                                      (value ?value)))
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r)))


(defrule lisp-parse::construct-special-instance-outside-list:singlefield-wildcard
         "convert a symbol of type SF_WILDCARD to class of type singlefield-wildcard"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements SF_WILDCARD
                                 ?value)
                       (top ?file)
                       (name ?file)
                       (router ?r))
         =>
         (printout werror
                   "WARNING: Found a special tag outside a list!" crlf)
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r))
         (make-instance of singlefield-wildcard
                        (parent ?file)
                        (value ?value)))

(defrule lisp-parse::construct-special-instance:multifield-variable
         "convert a symbol of type MF_VARIABLE to class of type multifield-variable"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements MF_VARIABLE
                                 ?value)
                       (top ?top
                            $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (slot-insert$ ?top
                       contents
                       (+ (length$ ?contents) 1)
                       (make-instance of multifield-variable
                                      (parent ?top)
                                      (value ?value)))
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r)))


(defrule lisp-parse::construct-special-instance-outside-list:multifield-variable
         "convert a symbol of type MF_VARIABLE to class of type multifield-variable"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements MF_VARIABLE
                                 ?value)
                       (top ?file)
                       (name ?file)
                       (router ?r))
         =>
         (printout werror
                   "WARNING: Found a special tag outside a list!" crlf)
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r))
         (make-instance of multifield-variable
                        (parent ?file)
                        (value ?value)))

(defrule lisp-parse::construct-special-instance:singlefield-variable
         "convert a symbol of type SF_VARIABLE to class of type singlefield-variable"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements SF_VARIABLE
                                 ?value)
                       (top ?top
                            $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (slot-insert$ ?top
                       contents
                       (+ (length$ ?contents) 1)
                       (make-instance of singlefield-variable
                                      (parent ?top)
                                      (value ?value)))
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r)))


(defrule lisp-parse::construct-special-instance-outside-list:singlefield-variable
         "convert a symbol of type SF_VARIABLE to class of type singlefield-variable"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements SF_VARIABLE
                                 ?value)
                       (top ?file)
                       (name ?file)
                       (router ?r))
         =>
         (printout werror
                   "WARNING: Found a special tag outside a list!" crlf)
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r))
         (make-instance of singlefield-variable
                        (parent ?file)
                        (value ?value)))

(defrule lisp-parse::construct-special-instance:multifield-global-variable
         "convert a symbol of type MF_GBL_VARIABLE to class of type multifield-global-variable"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements MF_GBL_VARIABLE
                                 ?value)
                       (top ?top
                            $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (slot-insert$ ?top
                       contents
                       (+ (length$ ?contents) 1)
                       (make-instance of multifield-global-variable
                                      (parent ?top)
                                      (value ?value)))
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r)))


(defrule lisp-parse::construct-special-instance-outside-list:multifield-global-variable
         "convert a symbol of type MF_GBL_VARIABLE to class of type multifield-global-variable"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements MF_GBL_VARIABLE
                                 ?value)
                       (top ?file)
                       (name ?file)
                       (router ?r))
         =>
         (printout werror
                   "WARNING: Found a special tag outside a list!" crlf)
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r))
         (make-instance of multifield-global-variable
                        (parent ?file)
                        (value ?value)))

(defrule lisp-parse::construct-special-instance:singlefield-global-variable
         "convert a symbol of type GBL_VARIABLE to class of type singlefield-global-variable"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements GBL_VARIABLE
                                 ?value)
                       (top ?top
                            $?)
                       (router ?r))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (slot-insert$ ?top
                       contents
                       (+ (length$ ?contents) 1)
                       (make-instance of singlefield-global-variable
                                      (parent ?top)
                                      (value ?value)))
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r)))


(defrule lisp-parse::construct-special-instance-outside-list:singlefield-global-variable
         "convert a symbol of type GBL_VARIABLE to class of type singlefield-global-variable"
         (declare (salience 2))
         ?f <- (object (is-a file)
                       (elements GBL_VARIABLE
                                 ?value)
                       (top ?file)
                       (name ?file)
                       (router ?r))
         =>
         (printout werror
                   "WARNING: Found a special tag outside a list!" crlf)
         (slot-replace$ ?f
                        elements
                        1 2
                        (next-token ?r))
         (make-instance of singlefield-global-variable
                        (parent ?file)
                        (value ?value)))


