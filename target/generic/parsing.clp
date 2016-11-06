(defmodule parsing
           (import cortex
                   ?ALL)
           (export ?ALL))
(defgeneric parsing::registerp
            "Is the given symbol a register?")
(defgeneric parsing::register-to-index
            "convert the given register to an index value")
(defgeneric parsing::index-to-register
            "convert the given index to a register symbol")
(defmethod parsing::registerp
  ((?sym SYMBOL))
  (and (has-prefix ?sym r)
       (integerp 
         (string-to-field
           (sub-string 2 (length$ ?sym)
                       ?sym)))))
(defmethod parsing::registerp
    ((?sym INTEGER))
    FALSE)

(defmethod parsing::register-to-index
  ((?sym SYMBOL
         (registerp ?sym)))
  (string-to-field 
    (sub-string 2 (length$ ?sym)
                ?sym)))

(defmethod parsing::index-to-register
  ((?index INTEGER
           (>= ?index 0)))
  (sym-cat r 
           ?index))
(defclass parsing::statement
  (is-a indexed-thing
        has-title))

(defclass parsing::register-alias
  (is-a statement)
  (slot actual-register
        (type SYMBOL
              INSTANCE)
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmethod parsing::register-to-index
  ((?register register-alias))
  (register-to-index (send ?register
                           get-actual-register)))
(defmethod parsing::registerp
  ((?register register-alias))
  (registerp (send ?register
                   get-actual-register)))


(defclass parsing::label
  (is-a statement)
  (slot address
        (type INTEGER
              SYMBOL
              INSTANCE)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default-dynamic undefined)))

(defclass parsing::data
  (is-a statement)
  (slot value
        (type INTEGER
              SYMBOL
              INSTANCE)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass parsing::scope
  (is-a statement
        indexed-thing-with-children))

(defclass parsing::section
  (is-a scope
        has-title))


(deftemplate parsing::line
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

(deftemplate parsing::file-information
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

(defrule parsing::open-file
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

(defrule parsing::readline
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
(defrule parsing::done
         (file-information (current-line EOF)
                           (router ?router&~nil))
         =>
         (close ?router))


(deffunction parsing::parse-file
             (?path)
             (assert (file-information (path ?path)))
             (focus parsing)
             (run))

(defrule parsing::identify-labels
         (line (exploded-line @label ?name)
               (parent ?parent))
         =>
         (make-instance of label
                        (parent ?parent)
                        (title ?name)))

(defrule parsing::identify-register-alias
         (line (exploded-line @alias ?name ?register)
               (parent ?parent))
         =>
         (make-instance of register-alias
                        (title ?name)
                        (actual-register ?register)
                        (parent ?parent)))
