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
(defgeneric MAIN::wrap-entries)
(defmethod MAIN::wrap-entries
  ((?prefix LEXEME)
   ?contents
   (?postfix LEXEME))
  (str-cat ?prefix
           ?contents
           ?postfix))
(defmethod MAIN::wrap-entries
  ((?prefix LEXEME)
   (?contents MULTIFIELD)
   (?postfix LEXEME))
  (str-cat ?prefix
           (expand$ ?contents)
           ?postfix))

(deffunction MAIN::comma-list
             ($?elements)
             (if (= (length$ ?elements) 0) then
               (return ""))
             (bind ?output
                   (expand$ (first$ ?elements)))
             (progn$ (?r (rest$ ?elements))
                     (bind ?output
                           (str-cat ?output
                                    ", " ?r)))
             ?output)



(deffunction MAIN::template-specialization
             ($?rest)
             (wrap-entries "<"
                           (comma-list ?rest)
                           ">"))
(deffunction MAIN::template-decl
             ($?rest)
             (str-cat "template"
                      (template-specialization ?rest)))

(deffunction MAIN::typename
             (?statement)
             (str-cat "typename "
                      ?statement))

(deffunction MAIN::variable
             (?type ?name)
             (str-cat ?type " " ?name))

(deffunction MAIN::explicit-enum
             (?type ?value)
             (str-cat ?type " :: "  ?value))
(deffunction MAIN::add-terminator
             (?str)
             (str-cat ?str ";"))
(deffunction MAIN::assign
             (?a ?b)
             (str-cat ?a " = " ?b))
(deffunction MAIN::standard-using-decl
             (?name ?value)
             (add-terminator (str-cat "using "
                                      (assign ?name
                                              ?value))))
(defgeneric MAIN::parens)
(defmethod MAIN::parens
  ((?thing LEXEME))
  (wrap-entries "("
                ?thing
                ")"))

(defmethod MAIN::parens
  ((?args MULTIFIELD))
  (wrap-entries "("
                (comma-list ?args)
                ")"))
(defmethod MAIN::parens
  ($?args)
  (parens ?args))
(defmethod MAIN::parens () "()")
(deffunction MAIN::static-cast
             (?type ?value)
             (str-cat "static_cast"
                      (template-specialization ?type)
                      (parens ?value)))

(deffunction MAIN::constexpr
             (?statement)
             (str-cat "constexpr "
                      ?statement))

(deffunction MAIN::decltype
             (?op)
             (wrap-entries "decltype("
                           ?op
                           ")"))

(deffunction MAIN::string-quote
             (?str)
             (format nil
                     "\"%s\""
                     ?str))
(deffunction MAIN::static-assert
             (?condition ?message)
             (add-terminator (str-cat static_assert
                                      (parens ?condition
                                              (string-quote ?message)))))
(deffunction MAIN::static-assert-cond
             ($?contents)
             (if (= (length$ ?contents) 0) then
               ""
               else
               (str-cat (expand$ ?contents))))

(deffunction MAIN::scope-body
             ($?body)
             (wrap-entries " { "
                           ?body
                           " } "))

(deffunction MAIN::terminated-scope-body
             ($?body)
             (add-terminator (scope-body ?body)))



(deffunction MAIN::string-if-true
             (?val ?prefix)
             (if ?val then
               (str-cat ?prefix " " ?val)
               else
               ""))

(deffunction MAIN::struct
             (?name ?name-postfix ?extends $?body)
             (str-cat "struct " ?name
                      (string-if-true ?name-postfix
                                      " ")
                      (string-if-true ?extends
                                      " : ")
                      (terminated-scope-body ?body)))
(deffunction MAIN::cond-fulfill
             (?result)
             (str-cat "syn::ConditionFulfillment< "
                      (if ?result then
                        true
                        else
                        false)
                      " >"))

(deffunction MAIN::generic-struct
             (?name ?template-parameters $?body)
             (str-cat (template-decl ?template-parameters) " "
                      (struct ?name
                              FALSE
                              (cond-fulfill FALSE)
                              ?body)))

(deffunction MAIN::specialize-struct
             (?name ?special-value $?body)
             (str-cat (template-decl "")
                      (struct ?name
                              (template-specialization ?special-value)
                              (cond-fulfill TRUE)
                              (expand$ ?body))))

(deffunction MAIN::return-statement
             (?statement)
             (add-terminator (str-cat "return "
                                      ?statement)))



(deffunction MAIN::function-call
             (?function $?args)
             (str-cat ?function " " (parens ?args)))
(deffunction MAIN::noexcept () noexcept)
(deffunction MAIN::function-signature
             (?return-type ?name ?arguments ?specifiers)
             (str-cat ?return-type " " ?name
                      (parens ?arguments)
                      " " ?specifiers))
(deffunction MAIN::case-statement
             (?value ?body)
             (str-cat "case " ?value ":" ?body))
(deffunction MAIN::default-case-statement
             (?body)
             (str-cat "default: " ?body))
(deffunction MAIN::switch-statement
             (?arg $?body)
             (str-cat switch
                      (parens ?arg)
                      (scope-body $?body)))
(deffunction MAIN::function-decl
             (?return-type ?name ?args ?specifiers $?body)
             (str-cat (function-signature ?return-type
                                          ?name
                                          ?args
                                          ?specifiers)
                      (scope-body ?body)))

(deffunction MAIN::templated-function-name
             (?op $?args)
             (str-cat ?op
                      (template-specialization (expand$ ?args))))
(deffunction MAIN::templated-function-call
             (?op ?template-args $?args)
             (function-call (templated-function-name ?op
                                                     ?template-args)
                            ?args))
(deffunction MAIN::return-templated-function-call
             (?op ?template-args $?args)
             (return-statement (templated-function-call ?op
                                                        ?template-args
                                                        ?args)))
(deffunction MAIN::static
             (?statement)
             (str-cat "static "
                      ?statement))
(deffunction MAIN::fulfills-condition
             (?type)
             (templated-function-call "syn::fulfillsCondition"
                                      ?type))
(deffunction MAIN::static-constexpr
             (?statement)
             (static (constexpr ?statement)))
(deffunction MAIN::constexpr-function-decl
             (?return-type ?name ?args $?body)
             (constexpr (function-decl ?return-type
                                       ?name
                                       ?args
                                       (noexcept)
                                       ?body)))
(deffunction MAIN::static-constexpr-function-decl
             (?return-type ?name ?args $?body)
             (static (constexpr-function-decl ?return-type
                                              ?name
                                              ?args
                                              ?body)))
(deffunction MAIN::constexpr-variable
             (?type ?name)
             (constexpr (variable ?type
                                  ?name)))
(deffunction MAIN::default-error-state
             (?type)
             (templated-function-name "syn::defaultErrorState"
                                      ?type))
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
                   (template-decl (typename T)
                                  (variable T op)) crlf
                   (add-terminator (assign (constexpr-variable auto
                                                               toExecutionUnitValue)
                                           (default-error-state T))) crlf))

(defrule MAIN::generate-using-decls
         (declare (salience ?*priority:two-after-first*))
         (close-namespace)
         ?f <- (using ?title
                      $?equals)
         =>
         (retract ?f)
         (assert (made-using ?title))
         (printout t
                   (standard-using-decl ?title
                                        (implode$ ?equals)) crlf))

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
                   (str-cat decode
                            ?name))
             (bind ?encodeFunc
                   (str-cat encode
                            ?name))
             (bind ?value-var
                   (variable ?value
                             value))
             (bind ?template-args
                   ?value
                   ?t
                   ?mask
                   ?shift)

             (printout t
                       (constexpr-function-decl ?t
                                                ?decodeFunc
                                                ?value-var
                                                (return-templated-function-call "syn::decodeBits"
                                                                                ?template-args
                                                                                value)) crlf

                       (constexpr-function-decl ?value
                                                ?encodeFunc
                                                (create$ ?value-var
                                                         (variable ?t
                                                                   field))
                                                (return-templated-function-call "syn::encodeBits"
                                                                                ?template-args
                                                                                value
                                                                                field))
                       crlf)

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
         (printout t
                   (static-assert (static-assert-cond (static-cast ?ct
                                                                   (explicit-enum ?name Count))
                                                      " <= "
                                                      (static-cast ?ct
                                                                   ?size))
                                  (format nil
                                          "Too many %s entries defined!"
                                          ?name)) crlf)
         (printout t
                   (template-decl (variable ?name op)) crlf
                   (add-terminator (assign (constexpr-variable auto
                                                               (str-cat translate
                                                                        ?name))
                                           (str-cat toExecutionUnitValue
                                                    (template-specialization (decltype op) op)))) crlf))



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
                   (add-terminator (assign (str-cat (template-decl "")
                                                    (constexpr-variable auto
                                                                        toExecutionUnitValue)
                                                    (template-specialization ?enum
                                                                             (explicit-enum ?enum ?name)))
                                           (explicit-enum ?other-type
                                                          ?other-name))) crlf))

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
         (bind ?cases
               (create$))
         (bind ?tfunc
               (str-cat translate
                        ?name))
         (progn$ (?e ?entries)
                 (bind ?enum-type
                       (explicit-enum ?name
                                      ?e))
                 (bind ?cases
                       ?cases
                       (case-statement ?enum-type
                                       (return-statement (templated-function-name ?tfunc
                                                                                  ?enum-type)))))

         (printout t
                   (constexpr-function-decl ?output
                                            translate
                                            (variable ?name
                                                      op)
                                            (switch-statement op
                                                              (expand$ ?cases)
                                                              (default-case-statement
                                                                (return-statement
                                                                  (default-error-state ?output)))))
                   crlf))


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

(defrule MAIN::defenum->enum-template
         (declare (salience ?*priority:first*))
         ?f <- (defenum ?name
                        ?max-size
                        ?type
                        entries:
                        $?children)
         =>
         (retract ?f)
         (assert (enum (name ?name)
                       (max-size ?max-size)
                       (cast-to ?type)
                       (children ?children))))


