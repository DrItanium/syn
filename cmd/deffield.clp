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
(deftemplate flag
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
                   (default FALSE)))
(defrule MAIN::replace-flags-for-fields
         (declare (salience ?*priority:first*))
         ?f <- (flag (name ?name)
                     (mask ?mask)
                     (shift ?shift)
                     (input-type ?type))
         =>
         (retract ?f)
         (assert (field (name (sym-cat ?name
                                       Flag))

                        (mask ?mask)
                        (shift ?shift)
                        (input-type ?type)
                        (output-type bool))))
(defrule MAIN::generate-ifndef-header
         (declare (salience ?*priority:first*))
         (title ?name)
         =>
         (assert (check-for-namespace))
         (printout t
                   "// NOTE: this file is auto generated, DO NOT MODIFY!" crlf
                   "#ifndef " (upcase ?name) crlf
                   "#define " (upcase ?name) crlf
                   "#include \"syn_base.h\"" crlf))
(defrule MAIN::generate-namespace-contents
         (declare (salience ?*priority:right-after-first*))
         (check-for-namespace)
         (namespace ?ns)
         =>
         (assert (close-namespace))
         (printout t
                   "namespace " ?ns " { " crlf))


(defrule MAIN::generate-closing-namespace-contents
         (declare (salience ?*priority:last*))
         (close-namespace)
         (namespace ?ns)
         =>
         (printout t
                   "} // end namespace " ?ns crlf))

(defrule MAIN::generate-endif-header
         (declare (salience ?*priority:dead-last*))
         (title ?name)
         =>
         (printout t
                   "#endif // end " (upcase ?name) crlf))

(deffunction generate-encode-decode-ops
             (?t ?name ?value ?mask ?shift)
             (format t
                     "inline constexpr %s decode%s(%s value) noexcept { return syn::decodeBits<%s, %s, %s, %s>(value); }%n"
                     ?t
                     ?name
                     ?value
                     ?value
                     ?t
                     (str-cat ?mask)
                     (str-cat ?shift))
             (format t
                     "inline constexpr %s encode%s(%s value, %s field) noexcept { return syn::encodeBits<%s, %s, %s, %s>(value, field); }%n"
                     ?value
                     ?name
                     ?value
                     ?t
                     ?value
                     ?t
                     (str-cat ?mask)
                     (str-cat ?shift)))

(defrule MAIN::generate-field:c++:use-input-type
         (field (name ?name)
                (mask ?mask)
                (shift ?shift)
                (output-type ?t)
                (input-type FALSE))
         (input-type ?value)
         =>
         (generate-encode-decode-ops ?t
                                     ?name
                                     ?value
                                     ?mask
                                     ?shift))

(defrule MAIN::generate-field:c++:use-embedded-type
         (field (name ?name)
                (mask ?mask)
                (shift ?shift)
                (output-type ?t)
                (input-type ?value&~FALSE))
         =>
         (generate-encode-decode-ops ?t
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


(defrule MAIN::generate-enum:c++
         (declare (salience 1))
         (enum (name ?name)
               (cast-to ?ct)
               (max-size ?size)
               (children $?children))
         =>
         (printout t "enum class " ?name " : " ?ct " {" crlf)
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
                         ?name)))
