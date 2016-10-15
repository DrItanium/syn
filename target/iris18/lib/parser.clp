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

(defglobal MAIN 
           ?*priority:first* = 10000
           ?*priority:three* = 3
           ?*priority:two* = 2
           ?*priority:one* = 1
           ?*priority:last* = -9999
           ?*priority:dead-last* = -10000)

(deftemplate stage
             (slot current
                   (type SYMBOL)
                   (default ?NONE))
             (multislot rest
                        (type SYMBOL)))
(defrule next-stage
         (declare (salience ?*priority:last*))
         ?f <- (stage (rest ?next $?rest))
         =>
         (modify ?f (current ?next)
                 (rest ?rest)))
(defrule done-with-stages
         (declare (salience ?*priority:dead-last*))
         ?f <- (stage (rest))
         =>
         (retract ?f))
(deffacts init-stages
          (stage (current load)
                 (rest lex
                       parse
                       associate
                       optimize)))
(defgeneric no-strings-in-list
            "checks to see if the given list is free of strings")
(defgeneric no-primitive-strings-in-list
            "checks to see if the given list is free of primitive strings")
(deffunction string-classp
             (?value)
             (eq (class ?value)
                 string))
(defmethod no-primitive-strings-in-list
  ((?list MULTIFIELD))
  (not-exists stringp
              (expand$ ?list)))
(defmethod no-primitive-strings-in-list
  ($?list)
  (no-primitive-strings-in-list ?list))
(defmethod no-strings-in-list
  ((?list MULTIFIELD))
  (not-exists string-classp
              (expand$ ?list)))
(defmethod no-strings-in-list
  ($?list)
  (not-exists string-classp
              $?list))

(defclass file 
  (is-a USER)
  (slot file
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

(defmessage-handler file parent-is primary
                    (?parent)
                    (eq (instance-name ?self)
                        ?parent))



(defclass node
  (is-a USER)
  (slot parent 
        (type SYMBOL
              INSTANCE-NAME)
        (default ?NONE))
  (message-handler parent-is primary))

(defmessage-handler node parent-is primary
                    (?parent)
                    (or (eq ?self:parent
                            ?parent)
                        (send ?self:parent 
                              parent-is 
                              ?parent)))

(defclass has-comment
  (is-a USER)
  (slot comment
        (visibility public)
        (type STRING)))
(defclass has-local-binds
  (is-a USER)
  (multislot local-binds
             (visibility public)))

(defclass has-arguments
  (is-a USER)
  (multislot arguments
             (visibility public)))

(defclass has-body
  (is-a USER)
  (multislot body
             (visibility public)))

(defclass exec-fragment 
  "An amalgamation of several different types"
  (is-a node
        has-body
        has-local-binds
        has-arguments))


(defclass composite-node
  "A node that is made up of other nodes. This is separate from a list!"
  (is-a node)
  (multislot contents
             (visibility public)
             (default ?NONE)))
(defclass scalar-node
  (is-a node)
  (slot value
        (visibility public)
        (default ?NONE)))

(defclass typed-scalar-node
  (is-a scalar-node)
  (slot type
        (type SYMBOL)
        (default ?NONE)))

(defclass string
  "Strings need to be wrapped in their own nodes"
  (is-a scalar-node)
  (slot value
        (type STRING)
        (source composite)
        (storage local)))
(defclass variable (is-a scalar-node))
(defclass global-variable (is-a variable))
(defclass singlefield-global-variable (is-a global-variable))
(defclass multifield-global-variable (is-a global-variable))

(defclass local-variable (is-a variable))
(defclass multifield-variable (is-a local-variable))
(defclass singlefield-variable (is-a local-variable))

(defclass constraint (is-a scalar-node))
(defclass not-constraint (is-a constraint))
(defclass and-constraint (is-a constraint))
(defclass or-constraint (is-a constraint))

(defclass wildcard (is-a scalar-node))
(defclass multifield-wildcard (is-a wildcard))
(defclass singlefield-wildcard (is-a wildcard))



(defclass list
  (is-a node)
  (multislot contents
             (visibility public)))

(defclass reference
  "An indirect reference to something else, useful for deffunctions and arguments"
  (is-a scalar-node)
  (slot expand
        (type SYMBOL)
        (allowed-symbols FALSE TRUE)))
;------------------------------------------------------------------------------
(defrule open-file
         (stage (current load))
         ?f <- (open ?path)
         =>
         (retract ?f)
         (bind ?name 
               (gensym*))
         (if (open ?path 
                   ?name 
                   "r") then
           (make-instance ?name of file
                          (file ?path)
                          (router ?name)
                          (elements (next-token ?name))
                          (top (symbol-to-instance-name ?name)))
           else
           (printout werror 
                     "couldn't open " ?path crlf)))

(defrule new-top-level
         (declare (salience 2))
         (stage (current lex))
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

(defrule new-list
         (declare (salience 3))
         (stage (current lex))
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

(defrule end-list
         (declare (salience 3))
         (stage (current lex))
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
(defrule end-list:top-level
         (declare (salience 3))
         (stage (current lex))
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


(defrule parse-special-element
         (declare (salience 1))
         (stage (current lex))
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

(defrule warn:parse-special-element-outside-list
         (declare (salience 1))
         (stage (current lex))
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

(defrule parse-string
         (declare (salience 2))
         (stage (current lex))
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

(defrule parse-string-outside-list
         (declare (salience 2))
         (stage (current lex))
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

(defrule parse-normal-element
         (declare (salience 1))
         (stage (current lex))
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


(defrule warn:parse-normal-element-outside-list
         (declare (salience 1))
         (stage (current lex))
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


(defrule error:end-list-without-beginning
         (declare (salience 2))
         (stage (current lex))
         ?f <- (object (is-a file)
                       (elements RPAREN ?)
                       (name ?file)
                       (top ?file)
                       (file ?path))
         =>
         (printout werror
                   "ERROR: " ?file crlf
                   tab "PATH: " ?path crlf
                   tab "found a ) outside an actual list!" crlf)
         (halt))

(defrule finished-completely
         (declare (salience 3))
         (stage (current lex))
         ?f <- (object (is-a file)
                       (elements STOP ?)
                       (router ?name))
         =>
         (close ?name)
         (modify-instance ?f 
                          (elements)))

(defrule error:bottom-of-top-should-be-self-referential
         (declare (salience 10000))
         (stage (current lex))
         ?f <- (object (is-a file)
                       (name ?file)
                       (top ?x&~?file)
                       (file ?path))
         =>
         (printout werror
                   "ERROR: " ?file crlf
                   tab "PATH: " ?path crlf
                   tab "The bottom of the parsing stack is not the file itself. Some rule has broken the parser" crlf)
         (halt))

(defrule error:top-is-empty
         (declare (salience 10000))
         (stage (current lex))
         ?f <- (object (is-a file)
                       (top)
                       (file ?file))
         =>
         (printout werror
                   "ERROR: " ?file crlf
                   tab "The parsing stack is empty. Some rule has broken the parser" crlf)
         (halt))
;-----------------------------------------------------------------------------
; Auto generated rules for special symbols
;-----------------------------------------------------------------------------

(defrule construct-special-instance:or-constraint
         "convert a symbol of type OR_CONSTRAINT to class of type or-constraint"
         (declare (salience 2))
         (stage (current lex))
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


(defrule construct-special-instance-outside-list:or-constraint
         "convert a symbol of type OR_CONSTRAINT to class of type or-constraint"
         (declare (salience 2))
         (stage (current lex))
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

(defrule construct-special-instance:and-constraint
         "convert a symbol of type AND_CONSTRAINT to class of type and-constraint"
         (declare (salience 2))
         (stage (current lex))
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


(defrule construct-special-instance-outside-list:and-constraint
         "convert a symbol of type AND_CONSTRAINT to class of type and-constraint"
         (declare (salience 2))
         (stage (current lex))
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

(defrule construct-special-instance:not-constraint
         "convert a symbol of type NOT_CONSTRAINT to class of type not-constraint"
         (declare (salience 2))
         (stage (current lex))
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


(defrule construct-special-instance-outside-list:not-constraint
         "convert a symbol of type NOT_CONSTRAINT to class of type not-constraint"
         (declare (salience 2))
         (stage (current lex))
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

(defrule construct-special-instance:multifield-wildcard
         "convert a symbol of type MF_WILDCARD to class of type multifield-wildcard"
         (declare (salience 2))
         (stage (current lex))
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


(defrule construct-special-instance-outside-list:multifield-wildcard
         "convert a symbol of type MF_WILDCARD to class of type multifield-wildcard"
         (declare (salience 2))
         (stage (current lex))
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

(defrule construct-special-instance:singlefield-wildcard
         "convert a symbol of type SF_WILDCARD to class of type singlefield-wildcard"
         (declare (salience 2))
         (stage (current lex))
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


(defrule construct-special-instance-outside-list:singlefield-wildcard
         "convert a symbol of type SF_WILDCARD to class of type singlefield-wildcard"
         (declare (salience 2))
         (stage (current lex))
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

(defrule construct-special-instance:multifield-variable
         "convert a symbol of type MF_VARIABLE to class of type multifield-variable"
         (declare (salience 2))
         (stage (current lex))
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


(defrule construct-special-instance-outside-list:multifield-variable
         "convert a symbol of type MF_VARIABLE to class of type multifield-variable"
         (declare (salience 2))
         (stage (current lex))
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

(defrule construct-special-instance:singlefield-variable
         "convert a symbol of type SF_VARIABLE to class of type singlefield-variable"
         (declare (salience 2))
         (stage (current lex))
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


(defrule construct-special-instance-outside-list:singlefield-variable
         "convert a symbol of type SF_VARIABLE to class of type singlefield-variable"
         (declare (salience 2))
         (stage (current lex))
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

(defrule construct-special-instance:multifield-global-variable
         "convert a symbol of type MF_GBL_VARIABLE to class of type multifield-global-variable"
         (declare (salience 2))
         (stage (current lex))
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


(defrule construct-special-instance-outside-list:multifield-global-variable
         "convert a symbol of type MF_GBL_VARIABLE to class of type multifield-global-variable"
         (declare (salience 2))
         (stage (current lex))
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

(defrule construct-special-instance:singlefield-global-variable
         "convert a symbol of type GBL_VARIABLE to class of type singlefield-global-variable"
         (declare (salience 2))
         (stage (current lex))
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


(defrule construct-special-instance-outside-list:singlefield-global-variable
         "convert a symbol of type GBL_VARIABLE to class of type singlefield-global-variable"
         (declare (salience 2))
         (stage (current lex))
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


(defclass match
  (is-a node)
  (slot binding
        (type LEXEME
              INSTANCE-NAME))
  (multislot contents))
(defclass defrule
  (is-a node
        has-comment)
  (slot rule-name
        (type SYMBOL)
        (default ?NONE))
  (slot salience
        (type INTEGER
              INSTANCE-NAME)
        (range -10000
               10000)
        (default-dynamic 0))
  (slot auto-focus
        (type SYMBOL
              INSTANCE-NAME)
        (allowed-symbols FALSE
                         TRUE))
  (multislot matches)
  (multislot body))




(defrule translate-defrule:comment
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defrule 
                                 ?rule-name 
                                 ?comment
                                 $?matches
                                 =>
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defrule 
                        (rule-name ?rule-name)
                        (comment ?cvalue)
                        (parent ?parent)
                        (body ?body)
                        (matches ?matches)))

(defrule translate-defrule:no-comment
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defrule
                                 ?rule-name
                                 ?match&:(not (string-classp ?match))
                                 $?matches
                                 =>
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defrule
                        (rule-name ?rule-name)
                        (parent ?parent)
                        (matches ?match 
                                 ?matches)
                        (body ?body)))



(defrule translate-match:no-binding
         (stage (current parse))
         ?f <- (object (is-a defrule)
                       (name ?parent)
                       (matches $?before ?list $?after))
         (object (is-a list)
                 (name ?list)
                 (contents $?contents))
         =>
         (unmake-instance ?list)
         (make-instance ?list of match
                        (parent ?parent)
                        (contents ?contents)))

;TODO: handle multiline strings
(defrule translate-match:binding
         "Before we construct defrule's we have to capture bound matches to prevent a matching ambiguity in a defrule between a comment and a bound match (both of them will show up as strings)"
         (declare (salience ?*priority:three*))
         (stage (current parse))
         (declaration parsed for ?rule)
         (object (is-a defrule)
                 (name ?rule)
                 (matches $?before ?var <- ?list $?after))
         ?f2 <- (object (is-a list)
                        (name ?list)
                        (contents $?contents))
         =>
         (unmake-instance ?f2)
         (modify-instance ?rule
                          (matches $?before
                                   (make-instance ?list of match
                                                  (parent ?rule)
                                                  (binding ?var)
                                                  (contents ?contents))
                                   $?after)))

(defrule check-defrule-decls:salience
         "Check the (declare ) list at the top of the match set"
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a defrule)
                       (matches ?decl
                                $?)
                       (name ?rule))
         (object (is-a list)
                 (name ?decl)
                 (contents declare
                           $?before
                           ?salience
                           $?after))
         (object (is-a list)
                 (name ?salience)
                 (contents salience
                           ?s))
         (not (exists (declaration parsed for ?rule)))
         =>
         (assert (please retract ?decl))
         (unmake-instance ?salience)
         (modify-instance ?decl
                          (contents ?before ?after))
         (modify-instance ?f 
                          (salience ?s)))

(defrule check-defrule-decls:auto-focus
         "Check the (declare ) list at the top of the match set"
         (declare (salience 2))
         (stage (current parse))
         ?f <- (object (is-a defrule)
                       (matches ?decl
                                $?)
                       (name ?rule))
         (object (is-a list)
                 (name ?decl)
                 (contents declare
                           $?before
                           ?af
                           $?after))
         (object (is-a list)
                 (name ?af)
                 (contents auto-focus 
                           ?s))
         (not (exists (declaration parsed for ?rule)))
         =>
         (assert (please retract ?decl))
         (unmake-instance ?af)
         (modify-instance ?decl
                          (contents ?before ?after))
         (modify-instance ?f 
                          (auto-focus ?s)))

(defrule check-defrule-decls:illegal-value
         (declare (salience 2))
         (stage (current parse))
         ?f <- (object (is-a defrule)
                       (matches ?decl
                                $?)
                       (rule-name ?rn)
                       (name ?ne))
         (object (is-a list)
                 (name ?decl)
                 (contents declare
                           $?before
                           ?target
                           $?after))
         (object (is-a list)
                 (name ?target)
                 (contents ?title&~salience&~auto-focus
                           $?))
         =>
         (printout werror 
                   "ERROR: found an illegal field " ?title 
                   " in the declare statement of rule " ?rn "( " ?ne " )"  crlf)
         (halt))



(defrule check-defrule-decls:retract-declare-statement
         "When done pulling the contents of the declaration out, retract it"
         (declare (salience 1))
         ?f <- (please retract ?decl)
         (object (is-a list)
                 (name ?decl)
                 (contents))
         (object (is-a defrule)
                 (matches ?decl 
                          $?)
                 (name ?rule))
         =>
         (retract ?f)
         (unmake-instance ?decl)
         (assert (declaration parsed for ?rule))
         (slot-delete$ ?rule
                       matches
                       1 1))


(defclass message-handler-documentation
  (is-a node)
  (slot handler-name
        (type SYMBOL)
        (default ?NONE))
  (slot handler-type
        (type SYMBOL)
        (allowed-symbols primary 
                         around 
                         before 
                         after)))
(defclass defclass
  (is-a node
        has-comment)
  (slot class-name
        (type SYMBOL)
        (default ?NONE))
  (slot role
        (type SYMBOL)
        (allowed-symbols concrete 
                         abstract))
  (slot pattern-match
        (type SYMBOL)
        (allowed-symbols reactive 
                         non-reactive))
  (multislot inherits-from)
  (multislot contents))

(defrule translate-defclass:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defclass 
                                 ?class-name
                                 ?comment
                                 ?is-a
                                 $?rest)
                       (parent ?parent)
                       (name ?name))
         ?f3 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         ?f2 <- (object (is-a list)
                        (name ?is-a)
                        (contents is-a $?ia))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (make-instance ?name of defclass
                        (class-name ?class-name)
                        (parent ?parent)
                        (comment ?cvalue)
                        (inherits-from ?ia)
                        (contents ?rest)))


(defrule translate-defclass:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defclass 
                                 ?class-name
                                 ?is-a
                                 $?rest)
                       (parent ?parent)
                       (name ?name))
         ?f2 <- (object (is-a list)
                        (name ?is-a)
                        (contents is-a $?ia))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defclass
                        (class-name ?class-name)
                        (parent ?parent)
                        (inherits-from ?ia)
                        (contents ?rest)))



(defrule translate-defclass:populate-role
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents ?first
                                 $?rest))
         ?q <- (object (is-a list)
                       (name ?first)
                       (contents role ?role))
         =>
         (unmake-instance ?q)
         (modify-instance ?f 
                          (role ?role)
                          (contents ?rest)))
(defrule translate-defclass:populate-pattern-match
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents ?first 
                                 $?rest))
         ?q <- (object (is-a list)
                       (name ?first)
                       (contents pattern-match ?pattern-match))
         =>
         (unmake-instance ?q)
         (modify-instance ?f 
                          (pattern-match ?pattern-match)
                          (contents ?rest)))

(defrule translate-defclass:convert-message-handler-documentation:no-type
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents $?before ?curr $?after)
                       (name ?parent))
         ?q <- (object (is-a list)
                       (name ?curr)
                       (contents message-handler ?name))
         =>
         (unmake-instance ?q)
         (make-instance ?curr of message-handler-documentation
                        (parent ?parent)
                        (handler-name ?name)))

(defrule translate-defclass:convert-message-handler-documentation:type
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents $?before ?curr $?after)
                       (name ?parent))
         ?q <- (object (is-a list)
                       (name ?curr)
                       (contents message-handler ?name ?type))
         =>
         (unmake-instance ?q)
         (make-instance ?curr of message-handler-documentation
                        (parent ?parent)
                        (handler-name ?name)
                        (handler-type ?type)))



(defclass deftemplate
  (is-a node
        has-comment)
  (slot template-name
        (type SYMBOL)
        (default ?NONE))
  (multislot slots))

(defrule translate-deftemplate:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deftemplate
                                 ?template-name 
                                 ?comment&:(string-classp ?comment)
                                 $?slots)
                       (parent ?parent)
                       (name ?name))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of deftemplate
                        (template-name ?template-name)
                        (comment ?cvalue)
                        (parent ?parent)
                        (slots $?slots)))

(defrule translate-deftemplate:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deftemplate
                                 ?template-name 
                                 ?slot&:(not (string-classp ?slot))
                                 $?slots)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of deftemplate
                        (template-name ?template-name)
                        (parent ?parent)
                        (slots ?slot 
                               $?slots)))

(defclass default-facet
  (is-a node)
  (role abstract)
  (pattern-match non-reactive)
  (multislot expressions))

(defclass default
  (is-a default-facet)
  (role concrete)
  (pattern-match reactive)
  ; define the ?DERIVE, ?NONE thingy
  (slot variable
        (type LEXEME)
        (allowed-symbols undefined
                         nil)
        (allowed-strings "?DERIVE"
                         "?NONE")
        (default-dynamic "?DERIVE")))

(defclass default-dynamic
  (is-a default-facet)
  (role concrete)
  (pattern-match reactive))
(defclass basic-slot
  (is-a node)
  (role abstract)
  (pattern-match non-reactive)
  (slot slot-name
        (type SYMBOL)
        (default ?NONE))
  (slot default-value
        (type SYMBOL INSTANCE)
        (allowed-symbols nil)
        (allowed-classes default
                         default-dynamic)
        (default-dynamic nil))
  (multislot type
             (type LEXEME)
             (cardinality 1 
                          ?VARIABLE)
             (allowed-symbols SYMBOL
                              STRING
                              LEXEME
                              FLOAT
                              NUMBER
                              INSTANCE-NAME
                              INSTANCE-ADDRESS
                              INSTANCE
                              EXTERNAL-ADDRESS
                              FACT-ADDRESS)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-symbols
             (type LEXEME)
             (cardinality 1
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-strings
             (type STRING)
             (cardinality 1
                          ?VARIABLE)
             (default-dynamic "?VARIABLE"))
  (multislot allowed-lexemes
             (type LEXEME)
             (cardinality 1 
                          ?VARIABLE)
             (default-dynamic "?VARIABLE"))
  (multislot allowed-integers
             (type INTEGER 
                   STRING)
             (cardinality 1 
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-float
             (type FLOAT 
                   STRING)
             (cardinality 1 
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-numbers
             (type NUMBER 
                   STRING)
             (cardinality 1 
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-instance-names
             (type INSTANCE-NAME 
                   STRING)
             (cardinality 1
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-classes
             (type LEXEME)
             (cardinality 1
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-values
             (type LEXEME 
                   NUMBER
                   INSTANCE-NAME)
             (cardinality 1
                          ?VARIABLE)
             (default-dynamic "?VARIABLE"))
  (multislot range
             (type NUMBER
                   STRING)
             (cardinality 2 
                          2)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"
                              "?VARIABLE"))
  (multislot cardinality
             (type INTEGER
                   STRING)
             (cardinality 2 
                          2)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"
                              "?VARIABLE"))

  (multislot facets))


(defclass defclass-slot
  (is-a basic-slot)
  (role abstract)
  (pattern-match non-reactive)
  (slot storage
        (type SYMBOL)
        (allowed-symbols local
                         shared))
  (slot access
        (type SYMBOL)
        (allowed-symbols read-write
                         read-only
                         initialize-only))
  (slot propagation 
        (type SYMBOL)
        (allowed-symbols inherit
                         no-inherit))
  (slot source
        (type SYMBOL)
        (allowed-symbols exclusive
                         composite))
  (slot pattern-match
        (type SYMBOL)
        (allowed-symbols reactive
                         non-reactive))
  (slot visibility
        (type SYMBOL)
        (allowed-symbols private
                         public))
  (slot create-accessor
        (type LEXEME)
        (allowed-strings "?NONE")
        (allowed-symbols read
                         write
                         read-write)
        (default-dynamic read-write))
  (slot override-message
        (type LEXEME)
        (allowed-strings "?DEFAULT")
        (default-dynamic "?DEFAULT")))

(defclass defclass-single-slot
  (is-a defclass-slot)
  (role concrete)
  (pattern-match reactive))

(defclass defclass-multislot
  (is-a defclass-slot)
  (role concrete)
  (pattern-match reactive))

(defclass deftemplate-slot
  (is-a basic-slot)
  (role abstract)
  (pattern-match non-reactive))

(defclass deftemplate-single-slot 
  (is-a deftemplate-slot)
  (role concrete)
  (pattern-match reactive))

(defclass deftemplate-multislot
  (is-a deftemplate-slot)
  (role concrete)
  (pattern-match reactive))

(defrule translate-defclass:slot
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents $?before 
                                 ?curr 
                                 $?after)
                       (name ?parent))
         ?q <- (object (is-a list)
                       (name ?curr)
                       (contents slot|single-slot 
                                 ?name 
                                 $?rest))
         =>
         (unmake-instance ?q)
         (make-instance ?curr of defclass-single-slot
                        (slot-name ?name)
                        (parent ?parent)
                        (facets ?rest)))

(defrule translate-defclass:multislot
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents $?before 
                                 ?curr 
                                 $?after)
                       (name ?parent))
         ?q <- (object (is-a list)
                       (name ?curr)
                       (contents multislot 
                                 ?name 
                                 $?rest))
         =>
         (unmake-instance ?q)
         (make-instance ?curr of defclass-multislot 
                        (slot-name ?name)
                        (parent ?parent)
                        (facets ?rest)))

(defrule translate-slot:type
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents type 
                                  ?first
                                  $?types))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f
                          (facets $?a $?b)
                          (type ?first
                                $?types)))

(defrule translate-slot:range
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents range 
                                  ?from 
                                  ?to))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f
                          (facets $?a $?b)
                          (range ?from
                                 ?to)))

(defrule translate-slot:cardinality
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents cardinality 
                                  ?from 
                                  ?to))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f
                          (facets $?a $?b)
                          (cardinality ?from
                                       ?to)))

(defrule translate-slot:allowed-symbols
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-symbols
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-symbols ?first 
                                           $?rest)))

(defrule translate-slot:allowed-strings
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-strings
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-strings ?first 
                                           $?rest)))

(defrule translate-slot:allowed-lexemes
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-lexemes
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-lexemes ?first 
                                           $?rest)))

(defrule translate-slot:allowed-integers
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-integers
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-integers ?first 
                                            $?rest)))

(defrule translate-slot:allowed-floats
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-floats
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-floats ?first 
                                          $?rest)))

(defrule translate-slot:allowed-numbers
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-numbers
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-numbers ?first 
                                           $?rest)))
(defrule translate-slot:allowed-instance-names
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-instance-names
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-instance-names ?first 
                                                  $?rest)))

(defrule translate-slot:allowed-classes
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-classes
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-classes ?first 
                                           $?rest)))
(defrule translate-slot:allowed-values
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-values
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-values ?first 
                                          $?rest)))

(defrule translate-slot:default:expression
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents default
                                  $?expressions))
         =>
         (unmake-instance ?f2)
         (make-instance ?curr of default
                        (parent ?parent)
                        (variable nil)
                        (expressions ?expressions))
         (modify-instance ?f
                          (facets ?a ?b)
                          (default-value ?curr)))

(defrule translate-slot:default:none-derive
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents default
                                  ?c))
         ?f3 <- (object (is-a singlefield-variable)
                        (name ?c)
                        (value ?value&"?NONE"|"?DERIVE"))
         =>
         (unmake-instance ?f2)
         (make-instance ?curr of default
                        (parent ?parent)
                        (variable ?c))
         (modify-instance ?f
                          (facets ?a ?b)
                          (default-value ?curr)))


(defrule translate-slot:default-dynamic:expression
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents default-dynamic
                                  $?expressions))
         =>
         (unmake-instance ?f2)
         (make-instance ?curr of default-dynamic
                        (parent ?parent)
                        (expressions ?expressions))
         (modify-instance ?f (facets ?a ?b)
                          (default-value ?curr)))

(defrule translate-slot:defclass-slot:storage
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents storage 
                                  ?storage))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (storage ?storage)))

(defrule translate-slot:defclass-slot:access
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents access 
                                  ?access))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (access ?access)))

(defrule translate-slot:defclass-slot:propagation
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents propagation 
                                  ?propagation))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (propagation ?propagation)))

(defrule translate-slot:defclass-slot:source
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents source 
                                  ?source))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (source ?source)))

(defrule translate-slot:defclass-slot:pattern-match
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents pattern-match 
                                  ?pattern-match))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (pattern-match ?pattern-match)))
(defrule translate-slot:defclass-slot:visibility
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents visibility 
                                  ?visibility))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (visibility ?visibility)))

(defrule translate-slot:defclass-slot:create-accessor
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents create-accessor 
                                  ?create-accessor))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (create-accessor ?create-accessor)))
(defrule translate-slot:defclass-slot:override-message
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents override-message 
                                  ?override-message))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (override-message ?override-message)))

(defrule translate-deftemplate:slot
         (stage (current parse))
         ?f <- (object (is-a deftemplate)
                       (slots $?a 
                              ?slot
                              $?b)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?slot)
                        (contents slot 
                                  ?name
                                  $?facets))
         =>
         (unmake-instance ?f2)
         (make-instance ?slot of deftemplate-single-slot
                        (parent ?parent)
                        (slot-name ?name)
                        (facets ?facets)))

(defrule translate-deftemplate:multislot
         (stage (current parse))
         ?f <- (object (is-a deftemplate)
                       (slots $?a 
                              ?slot
                              $?b)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?slot)
                        (contents multislot 
                                  ?name
                                  $?facets))
         =>
         (unmake-instance ?f2)
         (make-instance ?slot of deftemplate-multislot
                        (parent ?parent)
                        (slot-name ?name)
                        (facets ?facets)))

(defclass function
  (is-a node
        has-comment)

  (slot function-name
        (visibility public)
        (type SYMBOL)
        (default ?NONE))
  (multislot arguments
             (visibility public))
  (multislot local-binds
             (visibility public))
  (multislot body
             (visibility public)))

(defclass deffunction
  (is-a function))

(defrule translate-deffunction:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deffunction 
                                 ?func-name 
                                 ?comment
                                 ?args
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         ?k <- (object (is-a string)
                       (name ?comment)
                       (value ?cvalue))
         ?j <- (object (is-a list)
                       (name ?args)
                       (contents $?a))
         =>
         (unmake-instance ?j ?f ?k)
         (progn$ (?ag ?a) 
                 (send ?ag put-parent ?name))
         (make-instance ?name of deffunction
                        (function-name ?func-name)
                        (parent ?parent)
                        (comment ?cvalue)
                        (arguments ?a)
                        (body ?body)))
(defrule translate-deffunction:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deffunction 
                                 ?func-name 
                                 ?args
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         ?j <- (object (is-a list)
                       (name ?args)
                       (contents $?a))
         =>
         (unmake-instance ?j ?f)
         (progn$ (?ag ?a) 
                 (send ?ag put-parent ?name))
         (make-instance ?name of deffunction
                        (function-name ?func-name)
                        (parent ?parent)
                        (arguments ?a)
                        (body ?body)))


(defglobal MAIN
           ?*handler-types* = (create$ primary 
                                       around
                                       before
                                       after))
(deffunction valid-handlerp
             (?handler-type)
             (not (neq ?handler-type 
                       $?*handler-types*)))

(defclass defmessage-handler
  "A wrapper for a defmessage-handler declaration"
  (is-a function)
  (slot target-class
        (type SYMBOL
              INSTANCE-NAME)
        (visibility public)
        (default ?NONE))
  (slot handler-type
        (type SYMBOL)
        (allowed-symbols primary
                         around
                         before
                         after)))


(defrule convert-defmessage-handler:all
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmessage-handler
                                 ?class-name&:(symbolp ?class-name)
                                 ?message-name&:(symbolp ?message-name)
                                 ?handler-type&:(valid-handlerp ?handler-type)
                                 ?comment
                                 ?parameters
                                 $?actions)
                       (name ?id)
                       (parent ?parent))
         (object (is-a defclass)
                 (class-name ?class-name)
                 (name ?class-id))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         ?f3 <- (object (is-a list)
                        (name ?parameters)
                        (contents $?params))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (progn$ (?p ?params)
                 (send ?p put-parent ?id))
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (comment ?cvalue)
                        (target-class ?class-id)
                        (function-name ?message-name)
                        (handler-type ?handler-type)
                        (arguments ?params)
                        (body ?actions)))


(defrule convert-defmessage-handler:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmessage-handler
                                 ?class-name&:(symbolp ?class-name)
                                 ?message-name&:(symbolp ?message-name)
                                 ?handler-type&:(valid-handlerp ?handler-type)
                                 ?parameters
                                 $?actions)
                       (name ?id)
                       (parent ?parent))
         (object (is-a defclass)
                 (class-name ?class-name)
                 (name ?class-id))
         ?f3 <- (object (is-a list)
                        (name ?parameters)
                        (contents $?params))
         =>
         (unmake-instance ?f ?f3)
         (progn$ (?p ?params)
                 (send ?p put-parent ?id))
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (target-class ?class-id)
                        (function-name ?message-name)
                        (handler-type ?handler-type)
                        (arguments ?params)
                        (body ?actions)))

(defrule convert-defmessage-handler:no-handler
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmessage-handler
                                 ?class-name&:(symbolp ?class-name)
                                 ?message-name&:(symbolp ?message-name)
                                 ?comment
                                 ?parameters
                                 $?actions)
                       (name ?id)
                       (parent ?parent))
         (object (is-a defclass)
                 (class-name ?class-name)
                 (name ?class-id))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         ?f3 <- (object (is-a list)
                        (name ?parameters)
                        (contents $?params))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (progn$ (?p ?params)
                 (send ?p put-parent ?id))
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (comment ?cvalue)
                        (target-class ?class-id)
                        (function-name ?message-name)
                        (arguments ?params)
                        (body ?actions)))

(defrule convert-defmessage-handler:no-handler-or-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmessage-handler
                                 ?class-name&:(symbolp ?class-name)
                                 ?message-name&:(symbolp ?message-name)
                                 ?parameters
                                 $?actions)
                       (name ?id)
                       (parent ?parent))
         (object (is-a defclass)
                 (class-name ?class-name)
                 (name ?class-id))
         ?f3 <- (object (is-a list)
                        (name ?parameters)
                        (contents $?params))
         =>
         (unmake-instance ?f ?f3)
         (progn$ (?p ?params)
                 (send ?p put-parent ?id))
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (target-class ?class-id)
                        (function-name ?message-name)
                        (arguments ?params)
                        (body ?actions)))

(deffunction strip-singlefield
             (?element)
             (if (eq (class ?element)
                     singlefield-variable) then
               ?element 
               else 
               (create$)))

(deffunction wildcardp
             (?element)
             (eq (class ?element)
                 multifield-variable))

(deffunction has-wildcard-parameter
             (?list)
             (exists wildcardp
                     (expand$ ?list)))

(deffunction only-single-fields
             (?list)
             (not-exists wildcardp
                         (expand$ ?list)))


(defrule error:convert-function-arguments:multiple-wildcards
         (stage (current parse))
         ?q <- (object (is-a function)
                       (arguments $?sfs&:(not (only-single-fields ?sfs))
                                  ?wc)
                       (function-name ?function))
         (object (is-a multifield-variable)
                 (name ?wc))
         =>
         (printout werror "ERROR: extra wildcard defined in argument list of " (class ?q) " " ?function crlf)
         (halt))

(defrule error:convert-function-arguments:out-of-order-wildcard
         (stage (current parse))
         ?q <- (object (is-a function)
                       (arguments $?sf&:(has-wildcard-parameter ?sf)
                                  ?sf0)
                       (function-name ?function))
         (object (is-a singlefield-variable)
                 (name ?sf0))
         =>
         (printout werror "ERROR: wildcard parameter is not defined as the last argument in " (class ?q) " " ?function crlf)
         (halt))

(defrule reference-function-arguments
         (declare (salience ?*priority:three*)) ; has to go before the bind operations
         (stage (current associate))
         (object (is-a function)
                 (arguments $? ?arg $?)
                 (name ?function))
         (object (name ?arg)
                 (is-a singlefield-variable)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-variable)
                       (value ?cvalue)
                       (name ?targ&~?arg)
                       (parent ?parent))
         (test (send ?f 
                     parent-is 
                     ?function))
         =>
         (unmake-instance ?f)
         (make-instance ?targ of reference
                        (parent ?parent)
                        (value ?arg)))

(defrule reference-function-arguments:last:multifield:exact
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a function)
                 (arguments $? ?last)
                 (name ?function))
         (object (is-a multifield-variable)
                 (name ?last)
                 (value ?cvalue))
         ?f <- (object (is-a multifield-variable)
                       (value ?cvalue)
                       (name ?name&~?last)
                       (parent ?parent))
         (test (send ?f 
                     parent-is 
                     ?function))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (parent ?parent)
                        (value ?last)
                        (expand TRUE)))

(defrule reference-function-arguments:last:multifield:mismatch
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a function)
                 (arguments $? ?last)
                 (name ?function))
         (object (is-a multifield-variable)
                 (name ?last)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-variable)
                       (value ?qvalue&:(eq ?cvalue
                                           (str-cat "$" ?qvalue)))
                       (name ?name)
                       (parent ?parent))
         (test (send ?f 
                     parent-is 
                     ?function))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (parent ?parent)
                        (value ?last)
                        (expand TRUE)))

(defrule reference-function-local-binds:singlefield:exact
         (declare (salience ?*priority:three*)) ; has to go before the bind operations
         (stage (current associate))
         (object (is-a function)
                 (local-binds $? ?arg $?)
                 (name ?function))
         (object (is-a singlefield-variable)
                 (name ?arg)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-variable)
                       (value ?cvalue)
                       (name ?targ&~?arg)
                       (parent ?parent))
         (test (send ?f 
                     parent-is 
                     ?function))
         =>
         (unmake-instance ?f)
         (make-instance ?targ of reference
                        (parent ?parent)
                        (value ?arg)))

(defrule reference-function-local-binds:singlefield:mismatch
         (declare (salience ?*priority:three*)) ; has to go before the bind operations
         (stage (current associate))
         (object (is-a function)
                 (local-binds $? ?arg $?)
                 (name ?function))
         (object (is-a singlefield-variable)
                 (name ?arg)
                 (value ?cvalue))
         ?f <- (object (is-a multifield-variable)
                       (value ?mfvalue&:(eq ?mfvalue 
                                            (format nil "$%s" ?cvalue)))
                       (name ?targ&~?arg)
                       (parent ?parent))
         (test (send ?f 
                     parent-is 
                     ?function))
         =>
         (unmake-instance ?f)
         (make-instance ?targ of reference
                        (parent ?parent)
                        (value ?arg)))

(defrule reference-function-local-binds:last:multifield:exact
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a function)
                 (local-binds $? ?last $?)
                 (name ?function))
         (object (is-a multifield-variable)
                 (name ?last)
                 (value ?cvalue))
         ?f <- (object (is-a multifield-variable)
                       (value ?cvalue)
                       (name ?name&~?last)
                       (parent ?parent))
         (test (send ?f 
                     parent-is 
                     ?function))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (parent ?parent)
                        (value ?last)
                        (expand TRUE)))

(defrule reference-function-local-binds:multifield:mismatch
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a function)
                 (local-binds $? ?last $?)
                 (name ?function))
         (object (is-a multifield-variable)
                 (name ?last)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-variable)
                       (value ?qvalue&:(eq ?cvalue
                                           (format nil "$%s" ?qvalue)))
                       (name ?name)
                       (parent ?parent))
         (test (send ?f 
                     parent-is 
                     ?function))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (parent ?parent)
                        (value ?last)
                        (expand TRUE)))


(defclass defgeneric
  (is-a node
        has-comment)
  (slot generic-name
        (type SYMBOL)
        (default ?NONE)))

(defrule translate-defgeneric:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defgeneric
                                 ?gen-name)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defgeneric 
                        (generic-name ?gen-name)
                        (parent ?parent)))
(defrule translate-defgeneric:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defgeneric
                                 ?gen-name
                                 ?comment)
                       (parent ?parent)
                       (name ?name))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defgeneric 
                        (generic-name ?gen-name)
                        (comment ?cvalue)
                        (parent ?parent)))

(defclass defglobal
  (is-a node)
  (slot module 
        (type SYMBOL))
  (multislot assignments))
(defclass defglobal-assignment
  (is-a node)
  (slot variable 
        (type INSTANCE)
        (allowed-classes variable)
        (default ?NONE))
  (slot value
        (default ?NONE)))

(deffunction equals-sign () =)
(defrule translate-defglobal:module
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defglobal 
                                 ?module&:(symbolp ?module)
                                 $?rest)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defglobal
                        (parent ?parent)
                        (module ?module)
                        (assignments ?rest)))

(defrule translate-defglobal:no-module
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defglobal 
                                 $?rest)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defglobal
                        (parent ?parent)
                        (assignments ?rest)))

(defrule build-defglobal-assignment:value-is-list
         (stage (current parse))
         ?f <- (object (is-a defglobal)
                       (assignments $?before 
                                    ?var =(equals-sign) ?value
                                    $?rest)
                       (name ?parent))
         ?f2 <- (object (is-a global-variable)
                        (name ?var)
                        (parent ?parent))
         ?f3 <- (object (is-a node)
                        (name ?value)
                        (parent ?parent))
         =>
         (bind ?assignment 
               (instance-name (make-instance of defglobal-assignment
                                             (parent ?parent)
                                             (variable ?var)
                                             (value ?value))))
         (modify-instance ?f2 (parent ?assignment))
         (modify-instance ?f3 (parent ?assignment))
         ; TODO: expand this to support nested lists and such 
         ; (need to take over parentage since it is a new list)
         (modify-instance ?f 
                          (assignments ?before
                                       ?assignment
                                       ?rest)))

(defrule build-defglobal-assignment:value-is-scalar
         (stage (current parse))
         ?f <- (object (is-a defglobal)
                       (assignments $?before 
                                    ?var =(equals-sign) ?value&:(not (instance-namep ?value))
                                    $?rest)
                       (name ?parent))
         ?f2 <- (object (is-a global-variable)
                        (name ?var)
                        (parent ?parent))
         =>
         (bind ?assignment 
               (instance-name (make-instance of defglobal-assignment
                                             (parent ?parent)
                                             (variable ?var)
                                             (value ?value))))
         (modify-instance ?f2 (parent ?assignment))
         ; TODO: expand this to support nested lists and such 
         ; (need to take over parentage since it is a new list)
         (modify-instance ?f 
                          (assignments ?before
                                       ?assignment
                                       ?rest)))

(defrule reference-global-variables:singlefield:exact
         "Associate global variables even if they aren't of the exact same type"
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a defglobal)
                 (assignments $? ?a $?))
         (object (is-a defglobal-assignment)
                 (name ?a)
                 (variable ?var))
         (object (is-a singlefield-global-variable)
                 (name ?var)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-global-variable) ; we could be looking at an expansion version !
                       (value ?cvalue)
                       (name ?name&~?var)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (value ?var)
                        (parent ?p)))

(defrule reference-global-variables:singlefield:mismatch
         "Associate global variables even if they aren't of the exact same type"
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a defglobal)
                 (assignments $? ?a $?))
         (object (is-a defglobal-assignment)
                 (name ?a)
                 (variable ?var))
         (object (is-a multifield-global-variable)
                 (name ?var)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-global-variable) ; we could be looking at an expansion version !
                       (value ?value2&:(eq ?cvalue
                                           (format nil "$%s" ?value2)))
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (value ?var)
                        (parent ?p)))

(defrule reference-global-variables:multifield:exact-match
         "Associate multifield global variables"
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a defglobal)
                 (assignments $? ?a $?))
         (object (is-a defglobal-assignment)
                 (name ?a)
                 (variable ?var))
         (object (is-a multifield-global-variable)
                 (name ?var)
                 (value ?cvalue))
         ?f <- (object (is-a multifield-global-variable) ; we could be looking at an expansion version !
                       (value ?cvalue)
                       (name ?name&~?var)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (value ?var)
                        (parent ?p)
                        (expand TRUE)))

(defrule reference-global-variables:multifield:mismatch
         "Associate multifield global variables"
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a defglobal)
                 (assignments $? ?a $?))
         (object (is-a defglobal-assignment)
                 (name ?a)
                 (variable ?var))
         (object (is-a singlefield-global-variable)
                 (name ?var)
                 (value ?cvalue))
         ?f <- (object (is-a multifield-global-variable) ; we could be looking at an expansion version !
                       (value ?value2&:(eq ?value2 
                                           (format nil "$%s" ?cvalue)))
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (value ?var)
                        (parent ?p)
                        (expand TRUE)))

(defclass defmethod
  (is-a function)
  (slot offset 
        (type INTEGER)
        (default-dynamic -1)))

(defclass defmethod-argument
  "An argument in a defmethod"
  (is-a node)
  (slot argument-name
        (type INSTANCE-NAME)
        (default ?NONE))
  (multislot types
             (type SYMBOL))
  (slot query))
(defgeneric all-symbolsp
            "returns true if all the elements in a list are symbols")
(defmethod all-symbolsp
  ((?list MULTIFIELD))
  (not-exists symbolp
              (expand$ ?list)))
(defmethod all-symbolsp
  ($?list)
  (all-symbolsp ?list))

(defmethod multifield-variablep
  ((?var INSTANCE))
  (eq (class ?var)
      multifield-variable))

(defrule build:defmethod:index:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmethod
                                 ?name
                                 ?offset&:(integerp ?offset)
                                 ?comment
                                 ?args
                                 $?body)
                       (name ?id)
                       (parent ?parent))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         ; just match against it to make sure
         ?f3 <- (object (is-a list)
                        (name ?args)
                        (parent ?aparent)
                        (contents $?contents))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (progn$ (?a ?contents) 
                 (send ?a put-parent ?id))
         (make-instance ?id of defmethod
                        (parent ?parent)
                        (function-name ?name)
                        (offset ?offset)
                        (comment ?cvalue)
                        (arguments ?contents)
                        (body ?body)))
(defrule build:defmethod:index:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmethod
                                 ?name
                                 ?offset&:(integerp ?offset)
                                 ?args
                                 $?body)
                       (name ?id)
                       (parent ?parent))
         ; just match against it to make sure
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (parent ?aparent)
                        (contents $?contents))
         =>
         (unmake-instance ?f ?f2)
         (progn$ (?a ?contents) 
                 (send ?a put-parent ?id))
         (make-instance ?id of defmethod
                        (parent ?parent)
                        (function-name ?name)
                        (offset ?offset)
                        (arguments ?contents)
                        (body ?body)))

(defrule build:defmethod:no-index:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmethod
                                 ?name
                                 ?comment
                                 ?args
                                 $?body)
                       (name ?id)
                       (parent ?parent))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         ?f3 <- (object (is-a list)
                        (name ?args)
                        (parent ?aparent)
                        (contents $?contents))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (progn$ (?a ?contents) 
                 (send ?a put-parent ?id))
         (make-instance ?id of defmethod
                        (parent ?parent)
                        (function-name ?name)
                        (comment ?cvalue)
                        (arguments ?contents)
                        (body ?body)))

(defrule build:defmethod:no-index:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmethod
                                 ?name
                                 ?args
                                 $?body)
                       (name ?id)
                       (parent ?parent))
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (parent ?aparent)
                        (contents $?contents))
         =>
         (unmake-instance ?f 
                          ?f2)
         (progn$ (?a ?contents) 
                 (send ?a put-parent ?id))
         (make-instance ?id of defmethod
                        (parent ?parent)
                        (function-name ?name)
                        (arguments ?args)
                        (body ?body)))


; we leave the bare ? and $? options alone as it make reconstruction easier
(defrule build:defmethod-argument:wildcard-parameter:nested-list:types-and-query
         (stage (current parse))
         (object (is-a defmethod)
                 (name ?args)
                 (arguments $?before ?last))
         ?f2 <- (object (is-a list)
                        (name ?last)
                        (contents ?mname
                                  ?type&:(symbolp ?type)
                                  $?types&:(all-symbolsp ?types)
                                  ?query))
         (object (is-a multifield-variable)
                 (name ?mname))
         (object (is-a list|global-variable)
                 (name ?query))
         =>
         (unmake-instance ?f2)
         (make-instance ?last of defmethod-argument
                        (argument-name ?mname)
                        (parent ?args)
                        (types ?type ?types)
                        (query ?query)))

(defrule build:defmethod-argument:wildcard-parameter:nested-list:types-only
         (stage (current parse))
         (object (is-a defmethod)
                 (name ?args)
                 (arguments $?before ?last))
         ?f2 <- (object (is-a list)
                        (name ?last)
                        (contents ?mname
                                  ?type&:(symbolp ?type)
                                  $?types&:(all-symbolsp ?types)))
         (object (is-a multifield-variable)
                 (name ?mname))
         =>
         (unmake-instance ?f2)
         (make-instance ?last of defmethod-argument
                        (argument-name ?mname)
                        (parent ?args)
                        (types ?type 
                               ?types)))


(defrule build:defmethod-argument:wildcard-parameter:nested-list:query
         (stage (current parse))
         (object (is-a defmethod)
                 (name ?args)
                 (arguments $?before ?last))
         ?f2 <- (object (is-a list)
                        (name ?last)
                        (contents ?mname
                                  ?query))
         (object (is-a multifield-variable)
                 (name ?mname))
         (object (is-a list|global-variable)
                 (name ?query))
         =>
         (unmake-instance ?f2)
         (make-instance ?last of defmethod-argument
                        (argument-name ?mname)
                        (parent ?args)
                        (query ?query)))

; Error states
;(defrule error:defmethod-argument:wildcard-parameter:nested-list:no-types-or-query
;         (stage (current parse))
;         (object (is-a defmethod)
;                 (arguments $?before ?last)
;                 (parent ?parent))
;         (object (is-a list)
;                 (name ?last)
;                 (contents ?mname))
;         (object (is-a defmethod)
;                 (name ?parent)
;                 (function-name ?name))
;
;         =>
;         (printout werror "ERROR: no type information provided in wildcard-parameter nested-list in defmethod: " ?name crlf)
;         (halt))

;(defrule error:defmethod-argument:wildcard-parameter:not-last-argument:bare
;         (stage (current parse))
;         (object (is-a defmethod)
;                 (arguments $?before ?last ? $?)
;                 (parent ?parent))
;         (object (is-a multifield-variable)
;                 (name ?last))
;         (object (is-a defmethod)
;                 (name ?parent)
;                 (function-name ?name))
;
;         =>
;         (printout werror "ERROR: only the last argument of a defmethod list can be a wildcard-parameter!" crlf
;                   tab "Offending method is: " ?name crlf)
;         (halt))

;(defrule error:defmethod-argument:wildcard-parameter:not-last-argument:nested-list
;         (stage (current parse))
;         (object (is-a defmethod)
;                 (arguments $?before ?last ? $?)
;                 (parent ?parent))
;         (object (is-a list)
;                 (name ?last)
;                 (contents ?arg $?))
;         (object (is-a multifield-variable)
;                 (name ?arg))
;         (object (is-a defmethod)
;                 (name ?parent)
;                 (function-name ?name))
;
;         =>
;         (printout werror "ERROR: only the last argument of a defmethod list can be a wildcard-parameter!" crlf
;                   tab "Offending method is: " ?name crlf)
;         (halt))

(defrule build:defmethod-argument:singlefield-argument:all
         (stage (current parse))
         (object (is-a defmethod)
                 (arguments $? ?curr $?))
         ?f <- (object (is-a list)
                       (name ?curr)
                       (parent ?parent)
                       (contents ?mname
                                 ?type&:(symbolp ?type)
                                 $?types&:(all-symbolsp ?types)
                                 ?query))
         (object (is-a singlefield-variable)
                 (name ?mname))
         (object (is-a list|global-variable)
                 (name ?query))
         =>
         (unmake-instance ?f)
         (make-instance ?curr of defmethod-argument
                        (parent ?parent)
                        (argument-name ?mname)
                        (types ?type ?types)
                        (query ?query)))

(defrule build:defmethod-argument:singlefield-argument:types-only
         (stage (current parse))
         (object (is-a defmethod)
                 (name ?args)
                 (arguments $? ?curr $?))
         ?f <- (object (is-a list)
                       (name ?curr)
                       (contents ?mname
                                 ?type&:(symbolp ?type)
                                 $?types&:(all-symbolsp ?types)))
         (object (is-a singlefield-variable)
                 (name ?mname))
         =>
         (unmake-instance ?f)
         (make-instance ?curr of defmethod-argument
                        (parent ?args)
                        (argument-name ?mname)
                        (types ?type ?types)))

(defrule build:defmethod-argument:singlefield-argument:query-only
         (stage (current parse))
         (object (is-a defmethod)
                 (name ?args)
                 (arguments $? ?curr $?))
         ?f <- (object (is-a list)
                       (name ?curr)
                       (contents ?mname
                                 ?query))
         (object (is-a singlefield-variable)
                 (name ?mname))
         (object (is-a list|global-variable)
                 (name ?query))
         =>
         (unmake-instance ?f)
         (make-instance ?curr of defmethod-argument
                        (parent ?args)
                        (argument-name ?mname)
                        (query ?query)))

(defclass defmodule
  (is-a node
        has-comment)
  (multislot specifications))
; TODO: flesh out further
(defrule build-defmodule:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defmodules
                                 ?name
                                 ?comment
                                 $?specs))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defmodule
                        (parent  ?parent)
                        (comment ?cvalue)
                        (specifications ?specs)))

(defrule build-defmodule:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defmodules
                                 ?name
                                 $?specs&:(no-strings-in-list ?specs)))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defmodule
                        (parent  ?parent)
                        (specifications ?specs)))



(defclass port-specification
  (is-a node)
  (role abstract)
  (pattern-match non-reactive)
  (slot construct
        (type SYMBOL)
        (allowed-symbols undefined
                         nil
                         deftemplate
                         defclass
                         defglobal
                         deffunction
                         defgeneric))
  (multislot qualifiers))

(defclass export-specification
  (is-a port-specification)
  (role concrete)
  (pattern-match reactive))
(defclass import-specification
  (is-a port-specification)
  (role concrete)
  (pattern-match reactive)
  (slot module-name
        (type SYMBOL)
        (visibility public)
        (default ?NONE)))

(defrule build-export-specification
         (stage (current parse))
         ?f <- (object (is-a defmodule)
                       (name ?parent)
                       (specifications $?a 
                                       ?b 
                                       $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents export 
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (make-instance ?b of export-specification
                        (parent ?parent)
                        (items $?rest)))
(defrule expand-export-specification:all-or-none
         (stage (current parse))
         ?f <- (object (is-a export-specification)
                       (construct undefined)
                       (qualifiers "?ALL"|"?NONE"))
         =>
         (modify-instance ?f (construct nil)))
(defrule expand-export-specification:specific-construct-all-or-none
         (stage (current parse))
         ?f  <- (object (is-a export-specification)
                        (construct undefined)
                        (qualifiers ?construct&deftemplate|defclass|defglobal|deffunction|defgeneric
                                    ?qualifier))
         ?f2 <- (object (is-a singlefield-variable)
                        (name ?qualifier)
                        (value "?ALL"|"?NONE"))
         =>
         (modify-instance ?f 
                          (construct ?construct)
                          (qualifiers ?qualifier)))
(defrule expand-export-specification:specific-construct-and-qualifiers
         (stage (current parse))
         ?f <- (object (is-a export-specification)
                       (construct undefined)
                       (qualifiers ?construct&deftemplate|defclass|defglobal|deffunction|defgeneric
                                   $?qualifiers&:(and (not (empty$ ?qualifiers))
                                                      (no-primitive-strings-in-list $?qualifiers))))
         =>
         (modify-instance ?f
                          (construct ?construct)
                          (qualifiers ?qualifiers)))

(defrule expand-export-specification:error:no-qualifiers
         (stage (current parse))
         ?f <- (object (is-a export-specification)
                       (construct undefined)
                       (qualifiers ?construct&deftemplate|defclass|defglobal|deffunction|defgeneric))
         =>
         (printout werror "ERROR: no qualifiers specified for export-specification!" crlf)
         (halt))


(defrule build-import-specification
         (stage (current parse))
         ?f <- (object (is-a defmodule)
                       (name ?parent)
                       (specifications $?a 
                                       ?b 
                                       $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents import 
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (make-instance ?b of import-specification
                        (parent ?parent)
                        (qualifiers ?rest)))

(defrule expand-import-specification:all-or-none
         (stage (current parse))
         ?f <- (object (is-a import-specification)
                       (construct undefined)
                       (qualifiers ?module-name&:(symbolp ?module-name)
                                   ?qualifier))
         ?f2 <- (object (is-a singlefield-variable)
                        (name ?qualifier)
                        (value "?ALL"|"?NONE"))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (module-name ?module-name)
                          (construct nil)
                          (qualifiers ?qualifier)))
(defrule expand-import-specification:specific-construct-all-or-none
         (stage (current parse))
         ?f  <- (object (is-a import-specification)
                        (construct undefined)
                        (qualifiers ?module-name&:(symbolp ?module-name)
                                    ?construct&deftemplate|defclass|defglobal|deffunction|defgeneric
                                    ?qualifier))
         ?f2 <- (object (is-a singlefield-variable)
                        (name ?qualifier)
                        (value "?ALL"|"?NONE"))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (module-name ?module-name)
                          (construct ?construct)
                          (qualifiers ?qualifier)))

(defrule expand-import-specification:specific-construct-and-qualifiers
         (stage (current parse))
         ?f <- (object (is-a import-specification)
                       (construct undefined)
                       (qualifiers ?module-name&:(symbolp ?module-name)
                                   ?construct&deftemplate|defclass|defglobal|deffunction|defgeneric
                                   $?qualifiers&:(and (not (empty$ ?qualifiers))
                                                      (no-primitive-strings-in-list $?qualifiers))))
         =>
         (modify-instance ?f
                          (module-name ?module-name)
                          (construct ?construct)
                          (qualifiers ?qualifiers)))

; bind operations

(defclass bind
  (is-a node)
  (slot variable
        (type INSTANCE)
        (default ?NONE))
  (multislot value
             (default ?NONE)))


(defrule parse-bind-operation
         (stage (current parse))
         (object (is-a list)
                 (contents bind
                           ?var
                           $?value)
                 (parent ?parent)
                 (name ?name))
         (object (is-a variable)
                 (name ?var))
         =>
         (unmake-instance ?name)
         (make-instance ?name of bind
                        (parent ?parent)
                        (variable ?var)
                        (value ?value)))
; association rules
(defrule register-local-binds 
         "take ownership of the local bound variables found in the current function"
         (declare (salience ?*priority:two*))
         (stage (current associate))
         (object (is-a bind)
                 (variable ?var)
                 (name ?bind))
         (object (is-a local-variable)
                 (name ?var))
         (object (is-a function)
                 (name ?function&:(send ?bind parent-is ?function))
                 (local-binds $?lb&:(not (member$ ?var 
                                                  ?lb)))
                 (arguments $?args&:(not (member$ ?var 
                                                  ?args))))
         =>
         ; now we need to take ownership of the variable in the bind since we didn't find it 
         ; in the local binds nor the arguments
         (modify-instance ?var
                          (parent ?function))
         (bind ?ref-construction
               (symbol-to-instance-name (gensym*)))
         (modify-instance ?bind
                          (variable ?ref-construction))
         (assert (make-reference ?ref-construction ?bind ?var))
         (slot-insert$ ?function
                       local-binds
                       1
                       ?var))

(defrule build-reference-from-fact
         (declare (salience ?*priority:one*))
         (stage (current associate))
         ?f <- (make-reference ?id ?parent ?value)
         =>
         (retract ?f)
         (make-instance ?id of reference
                        (parent ?parent)
                        (value ?value)))





(defclass set-dynamic-constraint-checking 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-set-dynamic-constraint-checking
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents set-dynamic-constraint-checking ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of set-dynamic-constraint-checking
                        (argument ?arg)
                        (parent ?parent)))

(defclass batch* 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-batch*
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents batch* ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of batch*
                        (argument ?arg)
                        (parent ?parent)))

(defclass batch 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-batch
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents batch ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of batch
                        (argument ?arg)
                        (parent ?parent)))

(defclass load 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-load
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents load ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of load
                        (argument ?arg)
                        (parent ?parent)))

(defclass load* 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-load*
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents load* ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of load*
                        (argument ?arg)
                        (parent ?parent)))

(defclass stringp 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-stringp
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents stringp ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of stringp
                        (argument ?arg)
                        (parent ?parent)))

(defclass lexemep 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-lexemep
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents lexemep ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of lexemep
                        (argument ?arg)
                        (parent ?parent)))

(defclass symbolp 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-symbolp
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents symbolp ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of symbolp
                        (argument ?arg)
                        (parent ?parent)))

(defclass numberp 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-numberp
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents numberp ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of numberp
                        (argument ?arg)
                        (parent ?parent)))

(defclass integerp 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-integerp
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents integerp ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of integerp
                        (argument ?arg)
                        (parent ?parent)))

(defclass floatp 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-floatp
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents floatp ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of floatp
                        (argument ?arg)
                        (parent ?parent)))

(defclass instance-name 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-instance-name
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents instance-name ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of instance-name
                        (argument ?arg)
                        (parent ?parent)))

(defclass symbol-to-instance-name 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-symbol-to-instance-name
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents symbol-to-instance-name ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of symbol-to-instance-name
                        (argument ?arg)
                        (parent ?parent)))

(defclass instance-name-to-symbol 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-instance-name-to-symbol
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents instance-name-to-symbol ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of instance-name-to-symbol
                        (argument ?arg)
                        (parent ?parent)))

(defclass length$ 
  (is-a node)
  (slot argument
        (visibility public)
        (storage local)
        (default ?NONE)))

(defrule translate-list-to-length$
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents length$ ?arg)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of length$
                        (argument ?arg)
                        (parent ?parent)))

(defclass retract 
  (is-a node)
  (multislot arguments 
             (visibility public)
             (storage local)
             (default ?NONE)))

(defrule translate-list-to-retract
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents retract $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of retract
                        (arguments ?rest)
                        (parent ?parent)))

(defclass create$ 
  (is-a node)
  (multislot arguments 
             (visibility public)
             (storage local)
             (default ?NONE)))

(defrule translate-list-to-create$
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents create$ $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of create$
                        (arguments ?rest)
                        (parent ?parent)))

(defclass unmake-instance 
  (is-a node)
  (multislot arguments 
             (visibility public)
             (storage local)
             (default ?NONE)))

(defrule translate-list-to-unmake-instance
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents unmake-instance $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of unmake-instance
                        (arguments ?rest)
                        (parent ?parent)))

(defclass assert 
  (is-a node)
  (multislot arguments 
             (visibility public)
             (storage local)
             (default ?NONE)))

(defrule translate-list-to-assert
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents assert $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of assert
                        (arguments ?rest)
                        (parent ?parent)))


; iris18 addons
(defclass defn
  (is-a node)
  (slot title
        (type SYMBOL)
        (default ?NONE))
  (slot args
        (default ?NONE))
  (multislot body))
(defclass args
  (is-a node)
  (multislot contents
             (visibility public)
             (storage local)))

(defmessage-handler defn get-return-value primary
                    ()
                    ; find the last element of the body
                    (send (nth$ (length$ ?self:body) 
                                ?self:body)
                          get-return-value))

(defrule parse-defn
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defn ?name ?args $?body)
                       (name ?title)
                       (parent ?parent))
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (parent ?title)
                        (contents $?contents))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?title of defn
                        (parent ?parent)
                        (title ?name)
                        (args (make-instance ?args of args
                                             (contents ?contents)
                                             (parent ?title)))
                        (body ?body)))

(defclass chained-operation
  (is-a node)
  (message-handler get-return-value primary))
(defclass special-operation
  (is-a chained-operation)
  (slot operation
        (visibility public)
        (storage local)
        (default ?NONE))
  (multislot args
             (storage local)
             (visibility public)))

(defclass memory-operation
  (is-a special-operation)
  (slot bitmask
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass stack-operation
  (is-a memory-operation)
  (slot pointer
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot target 
        (storage local)
        (visibility public)
        (default ?NONE)))
(defmessage-handler stack-operation get-return-value primary
                    ()
                    ?self:target)
(defclass load-store-operation
  (is-a memory-operation)
  (slot offset
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot address
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot value 
        (storage local)
        (visibility public)
        (default ?NONE)))
(defmessage-handler load-store-operation get-return-value primary
                    ()
                    ?self:value)

(defclass store-operation
  (is-a load-store-operation)
  (slot operation
        (source composite)
        (storage shared)
        (default store)))

(defclass load-operation
  (is-a load-store-operation)
  (slot operation
        (source composite)
        (storage shared)
        (default load)))

(defclass push-operation
  (is-a stack-operation)
  (slot operation
        (source composite)
        (storage shared)
        (default push)))

(defclass pop-operation
  (is-a stack-operation)
  (slot operation
        (source composite)
        (storage shared)
        (default pop)))


(defrule parse-store-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents *memory
                                 store
                                 ?bitmask
                                 ?offset
                                 ?address
                                 ?value
                                 $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of store-operation 
                        (bitmask ?bitmask)
                        (parent ?parent)
                        (address ?address)
                        (value ?value)
                        (args $?rest)
                        (offset ?offset)))

(defrule parse-load-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents *memory
                                 load
                                 ?bitmask
                                 ?offset
                                 ?address
                                 ?value
                                 $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of load-operation 
                        (bitmask ?bitmask)
                        (parent ?parent)
                        (address ?address)
                        (value ?value)
                        (args $?rest)
                        (offset ?offset)))
(defrule parse-pop-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents *memory
                                 pop
                                 ?bitmask
                                 ?target
                                 ?pointer
                                 $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of pop-operation 
                        (bitmask ?bitmask)
                        (parent ?parent)
                        (pointer ?pointer)
                        (args $?rest)
                        (target ?target)))

(defrule parse-push-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents *memory
                                 push
                                 ?bitmask
                                 ?target
                                 ?pointer
                                 $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of push-operation 
                        (bitmask ?bitmask)
                        (parent ?parent)
                        (pointer ?pointer)
                        (args $?rest)
                        (target ?target)))

(defclass loop
  (is-a node)
  (slot id
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot body
             (storage local)
             (visibility public)))

(defrule parse-loop
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents loop
                                 ?id
                                 $?body)
                       (name ?name)
                       (parent ?parent))
         ?f2 <- (object (is-a list)
                        (name ?id)
                        (contents label ?id))
         =>
         (unmake-instance ?f
                          ?f2)
         (make-instance ?name of loop
                        (parent ?parent)
                        (id ?id)
                        (body ?body)))
(defclass arithmetic-operation
  (is-a special-operation)
  (slot immediate
        (type SYMBOL)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE
                         TRUE))
  (slot destination
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot source
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmessage-handler arithmetic-operation get-return-value primary
                    ()
                    ?self:destination)
(defrule parse-arithmetic-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents *arithmetic
                                 ?subop
                                 ?reg0
                                 ?reg1)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of arithmetic-operation
                        (parent ?parent)
                        (operation ?subop)
                        (destination ?reg0)
                        (source ?reg1)))
(defclass register
  (is-a USER))
(deffunction registerp
             (?reg)
             (and (instance-existp (bind ?tag 
                                         (symbol-to-instance-name ?reg)))
                  (eq (class ?tag)
                      register)))
(defrule tag-registers
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents $?a ?reg $?b))
         (test (and (symbolp ?reg)
                    (registerp ?reg)))
         =>
         (modify-instance ?f 
                          (contents $?a 
                                    (symbol-to-instance-name ?reg) 
                                    $?b)))
(definstances register-declarations
              (r0 of register)
              (r1 of register)
              (r2 of register)
              (r3 of register)
              (r4 of register)
              (r5 of register)
              (r6 of register)
              (r7 of register)
              (r8 of register)
              (r9 of register)
              (r10 of register)
              (r11 of register)
              (r12 of register)
              (r13 of register)
              (r14 of register)
              (r15 of register)
              (addr of register)
              (ip of register)
              (sp of register)
              (value of register)
              (cr of register))

(defclass fcall
  (is-a node)
  (slot operation
        (visibility public)
        (storage local)
        (default ?NONE))
  (multislot arguments
             (visibility public)
             (storage local)))


(defrule error-not-a-registered-function-call
         (stage (current associate))
         (object (is-a defn)
                 (body $? ?list $?))
         (object (is-a list)
                 (name ?list)
                 (contents ?operation
                           $?body))
         (not (object (is-a defn)
                      (title ?operation)))
         =>
         (printout werror "ERROR: The function " ?operation " is not defined before use!" crlf)
         (halt))

(defrule register-function-call
         (stage (current associate))
         (object (is-a defn)
                 (body $? ?list $?))
         ?f <- (object (is-a list)
                       (name ?list)
                       (contents ?operation 
                                 $?body)
                       (parent ?parent))

         (object (is-a defn)
                 (title ?operation)
                 (name ?defn))
         =>
         (unmake-instance ?f)
         (make-instance ?list of fcall
                        (operation ?defn)
                        (arguments $?body)
                        (parent ?parent)))



