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


; preprocessor.clp - functions to generate c/c++ preprocessor statements
(defgeneric #ifdef)
(defgeneric #ifndef)
(defgeneric #if)
(defgeneric #endif)
(defgeneric #define)
(defgeneric #include)
(defgeneric #undef)
(defgeneric defined)
(defgeneric macro-or)
(defgeneric macro-and)
(defgeneric macro-not)

(defmethod #include
  ((?path STRING))
  (format nil
          "#include \"%s\""
          ?path))
(defmethod #include
  ((?path SYMBOL))
  (format nil
          "#include %s"
          ?path))

(defmethod #ifdef
  ((?key SYMBOL))
  (str-cat "#ifdef "
           ?key))
(defmethod #ifdef
  ((?key SYMBOL)
   (?unused SYMBOL
            (eq ?current-argument
                do))
   (?body MULTIFIELD))
  (create$ (#ifdef ?key)
           ?body
           (#endif ?key)))

(defmethod #ifndef
  ((?key SYMBOL))
  (str-cat "#ifndef "
           ?key))

(defmethod #ifndef
  ((?key SYMBOL)
   (?unused SYMBOL
            (eq ?current-argument
                do))
   (?body MULTIFIELD))
  (create$ (#ifndef ?key)
           ?body
           (#endif ?key)))
(defmethod #ifndef
  ((?key SYMBOL)
   (?unused SYMBOL
            (eq ?current-argument
                do))
   $?body)
  (#ifndef ?key
   ?unused
   ?body))
(defmethod #ifndef
  ((?key SYMBOL)
   (?action SYMBOL
            (eq ?current-argument
                guard))
   (?body MULTIFIELD))
  (#ifndef ?key do
   (#define ?key)
   ?body))

(defmethod #ifndef
  ((?key SYMBOL)
   (?unused SYMBOL
            (eq ?current-argument
                guard))
   $?body)
  (#ifndef ?key
   ?unused
   ?body))

(defmethod #define
  ((?key SYMBOL)
   ?value)
  (str-cat "#define "
           ?key
           " "
           ?value))
(defmethod #define
  ((?key SYMBOL)
   (?value STRING))
  (format nil
          "#define %s \"%s\""
          ?key
          ?value))

(defmethod #define
  ((?key SYMBOL))
  (format nil
          "#define %s"
          ?key))

(defmethod #endif
  ()
  #endif)

(defmethod #endif
  ((?key SYMBOL))
  (format nil
          "#endif // end %s"
          ?key))

(defmethod macro-or
  ((?a LEXEME)
   (?b LEXEME))
  (format nil
          "%s || %s"
          ?a
          ?b))

(defmethod macro-or
  ((?a LEXEME)
   (?b LEXEME)
   (?rest MULTIFIELD))
  (bind ?base
        (macro-or ?a
                  ?b))
  (progn$ (?element ?rest)
          (bind ?base
                (macro-or ?base
                          ?element)))
  (format nil
          "(%s)"
          ?base))

(defmethod macro-or
  ((?a LEXEME)
   (?b LEXEME)
   $?rest)
  (macro-or ?a
            ?b
            ?rest))


(defmethod macro-and
  ((?a LEXEME)
   (?b LEXEME))
  (format nil
          "%s && %s"
          ?a
          ?b))

(defmethod macro-and
  ((?a LEXEME)
   (?b LEXEME)
   (?rest MULTIFIELD))
  (bind ?base
        (macro-and ?a
                   ?b))
  (progn$ (?element ?rest)
          (bind ?base
                (macro-and ?base
                           ?element)))
  (format nil
          "(%s)"
          ?base))

(defmethod macro-and
  ((?a LEXEME)
   (?b LEXEME)
   $?rest)
  (macro-and ?a
             ?b
             ?rest))

(defmethod macro-not
  ((?a LEXEME))
  (str-cat !
           ?a))
(defmethod defined
  ((?a SYMBOL))
  (str-cat "defined(" ?a ")"))

(defmethod #if
  (?a)
  (str-cat "#if "
           ?a))

(defmethod #if
  (?key (?unused SYMBOL
                 (eq ?current-argument
                     do))
        (?body MULTIFIELD))
  (create$ (#if ?key)
           ?body
           (#endif ?key)))

(defmethod #undef
  ((?key SYMBOL))
  (str-cat "#undef "
           ?key))
(defmethod #define
  ((?key SYMBOL)
   (?args MULTIFIELD)
   (?definition MULTIFIELD))
  (create$ (format nil
                   "%s(%s)"
                   (#define ?key)
                   (implode$ ?args)) \
           ?definition))
(defmethod #define
  ((?key SYMBOL)
   (?args MULTIFIELD)
   $?body)
  (#define ?key ?args ?body))
(defmethod #define
  ((?key SYMBOL)
   (?args MULTIFIELD)
   (?definition MULTIFIELD)
   (?unused SYMBOL
            (eq ?current-argument
                do))
   (?body MULTIFIELD))
  (create$ (#define ?key ?args ?definition)
           ?body
           (#undef ?key)))

(defmethod #define
  ((?key SYMBOL)
   (?args MULTIFIELD)
   (?definition MULTIFIELD)
   (?unused SYMBOL
            (eq ?current-argument
                do))
   $?body)
  (#define ?key
   ?args
   ?definition
   ?unused
   ?body))
