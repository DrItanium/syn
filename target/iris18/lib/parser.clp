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



