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
             (slot output-type
                   (type LEXEME)
                   (default ?NONE)))
(defrule MAIN::generate-ifndef-header
         (declare (salience ?*priority:first*))
         (title ?name)
         =>
         (printout t
                   "#ifndef " (upcase ?name) crlf
                   "#define " (upcase ?name) crlf))
(defrule MAIN::generate-endif-header
         (declare (salience ?*priority:dead-last*))
         (title ?name)
         =>
         (printout t
                   "#endif // end " (upcase ?name) crlf))

(defrule MAIN::generate-field:c++
         (input-type ?value)
         (field (name ?name)
                (mask ?mask)
                (shift ?shift)
                (output-type ?t))
         =>
         (format t
                 "inline constexpr %s decode%s(%s value) noexcept { return iris::decodeBits<%s, %s, %s, %s>(value); }%n"
                 ?t
                 ?name
                 ?value
                 ?value
                 ?t
                 (str-cat ?mask)
                 (str-cat ?shift))
         (format t
                 "inline constexpr %s encode%s(%s value, %s field) noexcept { return iris::encodeBits<%s, %s, %s, %s>(value, field); }%n"
                 ?value
                 ?name
                 ?value
                 ?t
                 ?value
                 ?t
                 (str-cat ?mask)
                 (str-cat ?shift)))

(deftemplate enum
             (slot name
                   (type SYMBOL)
                   (default ?NONE))
             (slot max-size
                   (type INTEGER
                         LEXEME)
                   (range 0 ?VARIABLE)
                   (default ?NONE))
             (slot mask
                   (default ?NONE))
             (slot shift
                   (type INTEGER)
                   (default ?NONE))
             (slot cast-to
                   (type LEXEME)
                   (default ?NONE))
             (slot field-name
                   (type LEXEME)
                   (default ?NONE))
             (multislot children
                        (default ?NONE)))

(defrule MAIN::make-field-from-enum
         (enum (field-name ?name)
               (mask ?mask)
               (shift ?shift)
               (cast-to ?t))
         (input-type ?input)
         =>
         (assert (field (name ?name)
                        (mask ?mask)
                        (shift ?shift)
                        (output-type ?input))))

(defrule MAIN::generate-enum:c++
         (enum (name ?name)
               (cast-to ?ct)
               (max-size ?size)
               (children $?children))
         =>
         (printout t "enum class " ?name " {" crlf)
         (progn$ (?c ?children)
                 (format t
                         "%s, // %d %n"
                         (str-cat ?c)
                         (- ?c-index 1)))

         (printout t "Count, };" crlf)
         (format t
                 "static_assert(static_cast<%s>(%s :: Count) <= static_cast<%s>(%s), \"%s\");%n"
                 ?ct
                 ?name
                 ?ct
                 (str-cat ?size)
                 (format nil
                         "Too many %s entries defined!"
                         ?ct)))
