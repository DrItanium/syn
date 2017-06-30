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
             (bind ?decodeFunc
                   (format nil
                           "decode%s"
                           ?name))
             (bind ?encodeFunc
                   (format nil
                           "encode%s"
                           ?name))
             (format t
                     "constexpr %s %s(%s value) noexcept { return syn::decodeBits<%s, %s, %s, %s>(value); }%n"
                     ?t
                     ?decodeFunc
                     ?value
                     ?value
                     ?t
                     (str-cat ?mask)
                     (str-cat ?shift))
             (format t
                     "constexpr %s %s(%s value, %s field) noexcept { return syn::encodeBits<%s, %s, %s, %s>(value, field); }%n"
                     ?value
                     ?encodeFunc
                     ?value
                     ?t
                     ?value
                     ?t
                     (str-cat ?mask)
                     (str-cat ?shift))
             (assert (decoding-operation ?name
                                         ?decodeFunc
                                         ?t
                                         ?value)
                     (encoding-operation ?name
                                         ?encodeFunc
                                         ?t
                                         ?value)))

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

(deftemplate translation-function-builder
             (slot target-enum
                   (type LEXEME)
                   (default ?NONE))
             (slot output-type
                   (type LEXEME))
             (multislot entries))

(defrule MAIN::generate-enum:c++
         (declare (salience 1))
         (enum (name ?name)
               (cast-to ?ct)
               (max-size ?size)
               (children $?children))
         =>
         (assert (constructed enum ?name)
                 (translation-function-builder (target-enum ?name)))
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
         ?f2 <- (translation-function-builder (target-enum ?enum)
                                              (entries $?e))
         =>
         (retract ?f)
         (modify ?f2
                 (output-type ?other-type)
                 (entries $?e ?name))
         (printout t
                   "template<> constexpr auto toExecutionUnitValue<"
                   ?enum " , " ?enum " :: " ?name "> = "
                   ?other-type " :: " ?other-name ";" crlf))

(defrule MAIN::generate-translation-function-has-no-elements
         (declare (salience -1))
         ?f <- (translation-function-builder (entries))
         =>
         (retract ?f))

(defrule MAIN::generate-translation-function
         (declare (salience -2))
         ?f <- (translation-function-builder (target-enum ?name)
                                             (output-type ?output)
                                             (entries $?entries))
         =>
         (retract ?f)
         (printout t
                   "constexpr " ?output " translate(" ?name " op) noexcept {" crlf
                   "switch(op) {" crlf)
         (progn$ (?e $?entries)
                 (printout t "case " ?name " :: " ?e ": return translate" ?name "<"?name " :: " ?e ">;" crlf))
         (printout t
                   "default: return syn::defaultErrorState<" ?output ">;" crlf
                   "}" crlf
                   "}" crlf))


(defrule MAIN::deffield->field-template:no-input-type
         (declare (salience ?*priority:first*))
         ?f <- (deffield ?name
                         ?mask
                         ?shift
                         ?output-type)
         =>
         (retract ?f)
         (assert (field (name ?name)
                        (mask ?mask)
                        (shift ?shift)
                        (output-type ?output-type))))

(defrule MAIN::deffield->field-template:input-type
         (declare (salience ?*priority:first*))
         ?f <- (deffield ?name
                         ?mask
                         ?shift
                         ?output-type
                         input-type:
                         ?input-type)
         =>
         (retract ?f)
         (assert (field (name ?name)
                        (mask ?mask)
                        (shift ?shift)
                        (output-type ?output-type))))

(defrule MAIN::generate-execution-unit-converter
         (declare (salience ?*priority:first*))
         ?f <- (defspecial-execution-unit-converter ?name
                                                    ?from-type
                                                    ?to-type)
         =>
         (retract ?f)
         (build (format nil
                        "(defrule MAIN::perform-special-execution-unit-conversion:%s
                                  (declare (salience ?*priority:first*))
                                  ?f <- (%s ?a ?b)
                                  =>
                                  (retract ?f)
                                  (assert (to-execution-unit %s ?a -> %s ?b)))"
                        ?name
                        ?name
                        ?from-type
                        ?to-type)))
