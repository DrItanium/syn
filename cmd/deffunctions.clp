; define encode and decode C++ functions
(defmodule MAIN
           (import cortex
                   ?ALL))

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

(deffunction generate-encode-decode-ops
             (?n ?name ?value ?mask ?shift)
             (format t "(deffunction %s-decode-%s (?value) (decode-bits ?value %s %s))%n"
                     ?n
                     ?name
                     (str-cat ?mask)
                     (str-cat ?shift))
             (format t "(deffunction %s-encode-%s (?value ?field) (encode-bits ?value ?field %s %s))%n"
                     ?n
                     ?name
                     (str-cat ?mask)
                     (str-cat ?shift)))



(defrule MAIN::generate-field:clips:use-input-type
         (field (name ?name)
                (mask ?mask)
                (shift ?shift)
                (input-type FALSE))
         (input-type ?value)
         (namespace ?n)
         =>
         (generate-encode-decode-ops ?name
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
         (generate-encode-decode-ops ?name
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

