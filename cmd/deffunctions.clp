; define encode and decode C++ functions
(defmodule MAIN
           (import cortex
                   ?ALL))
(deffunction binaryp
             (?sym)
             (eq (str-index 0b
                            ?sym) 1))
(deffunction hexp
             (?sm)
             (eq (str-index 0x
                            ?sm)
                 1))
(deftemplate field
             (slot name
                   (type SYMBOL)
                   (default ?NONE))
             (slot mask
                   (default ?NONE))
             (slot shift
                   (type INTEGER)
                   (default ?NONE))
             (slot input-type
                   (type LEXEME)
                   (default FALSE))
             (slot output-type
                   (type LEXEME)
                   (default ?NONE)))

(deffunction potential-convert
             (?value)
             (if (lexemep ?value) then
               (if (hexp ?value) then 
                 (hex->int ?value)
                 else
                 (if (binaryp ?value) then
                   (binary->int ?value)
                   else
                   ?value))
               else
               ?value))

(deffunction generate-encode-decode-ops
             (?n ?name ?value ?mask ?shift)
             (bind ?m 
                   (str-cat (potential-convert ?mask)))
             (bind ?s
                   (str-cat (potential-convert ?shift)))
             (bind ?base-name
                   (format nil
                           "%s-%%s-%s"
                           ?n
                           ?name))
             (bind ?encode-name
                   (format nil
                           ?base-name
                           encode))
             (bind ?decode-name 
                   (format nil 
                           ?base-name
                           decode))

             (format t 
                     "(defgeneric MAIN::%s)%n(defgeneric MAIN::%s)%n"
                     ?encode-name
                     ?decode-name)
             (format t 
                     "(defmethod MAIN::%s 
                        ((?value INTEGER))
                        (decode-bits ?value
                                     %s
                                     %s))%n"
                     ?decode-name
                     ?m
                     ?s)
             (format t
                     "(defmethod MAIN::%s
                        ((?value INTEGER)
                         (?field INTEGER))
                        (encode-bits ?value
                                     ?field
                                     %s
                                     %s))%n"
                     ?encode-name
                     ?m
                     ?s))




(defrule MAIN::generate-field:clips:use-input-type
         (field (name ?name)
                (mask ?mask)
                (shift ?shift)
                (input-type FALSE))
         (input-type ?value)
         (namespace ?n)
         =>
         (generate-encode-decode-ops ?n
                                     ?name
                                     ?value
                                     ?mask
                                     ?shift))

(defrule MAIN::generate-field:clips:use-embedded-type
         (field (name ?name)
                (mask ?mask)
                (shift ?shift)
                (input-type ?value&~FALSE))
         (namespace ?n)
         =>
         (generate-encode-decode-ops ?n
                                     ?name
                                     ?value
                                     ?mask
                                     ?shift))

(deftemplate enum
             (slot name
                   (type SYMBOL)
                   (default ?NONE))
             (slot max-size
                   (type INTEGER
                         LEXEME)
                   (range 0 ?VARIABLE)
                   (default ?NONE))
             (slot cast-to
                   (type LEXEME)
                   (default ?NONE))
             (multislot children
                        (default ?NONE)))


(defrule MAIN::generate-enum:clips
         (declare (salience 1))
         (enum (name ?name)
               (cast-to ?ct)
               (max-size ?size)
               (children $?children))
         (namespace ?n)
         =>
         (bind ?output 
               (create$))
         (progn$ (?c ?children)
                 (bind ?output
                       ?output
                       (sym-cat ?c)))
         (format t 
                 "(defglobal MAIN ?*%s-enum%s* = (create$ %s))%n"
                 ?n
                 ?name
                 (implode$ ?output)))
(defrule MAIN::generate-target-architecture
         (namespace ?ns)
         =>
         (format t
                "(deffunction MAIN::target-architecture () %s)%n"
                ?ns))
