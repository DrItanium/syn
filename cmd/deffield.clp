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
                   "#include \"Base.h\"" crlf))

(defrule MAIN::generate-extra-includes
         (declare (salience ?*priority:right-after-first*))
         ?f <- (include ?title)
         =>
         (retract ?f)
         (printout t
                   "#include \"" ?title "\"" crlf))
(defrule MAIN::generate-namespace-contents
         (declare (salience ?*priority:two-after-first*))
         (check-for-namespace)
         (namespace ?ns)
         =>
         (assert (close-namespace))
         (printout t
                   "namespace " ?ns " { " crlf
                   "template<typename T, T op> " crlf
                   "constexpr auto toExecutionUnitValue = syn::defaultErrorState<T>;" crlf))

(defrule MAIN::generate-using-decls
         (declare (salience ?*priority:two-after-first*))
         (close-namespace)
         ?f <- (using ?title
                      $?equals)
         =>
         (retract ?f)
         (assert (made-using ?title))
         (printout t
                   "using " ?title " = " (expand$ ?equals) ";" crlf))

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
                     "constexpr %s decode%s(%s value) noexcept { return syn::decodeBits<%s, %s, %s, %s>(value); }%n"
                     ?t
                     ?name
                     ?value
                     ?value
                     ?t
                     (str-cat ?mask)
                     (str-cat ?shift))
             (format t
                     "constexpr %s encode%s(%s value, %s field) noexcept { return syn::encodeBits<%s, %s, %s, %s>(value, field); }%n"
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
         (assert (constructed enum ?name))
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
                         ?name))
         (printout t
                   "template<" ?name " op>" crlf
                   "constexpr auto translate" ?name " = toExecutionUnitValue<decltype(op), op>;" crlf))

(defrule MAIN::generate-to-exec-unit-specialization
         ?f <- (to-execution-unit ?enum
                                  ?name
                                  ->
                                  ?other-type
                                  ?other-name)
         (made-using ?other-type)
         (constructed enum ?enum)
         =>
         (retract ?f)
         (printout t
                   "template<> constexpr auto toExecutionUnitValue<"
                   ?enum " , " ?enum " :: " ?name "> = "
                   ?other-type " :: " ?other-name ";" crlf))
