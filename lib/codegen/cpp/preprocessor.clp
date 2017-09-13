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
(defgeneric cpp::#ifdef)
(defgeneric cpp::#ifndef)
(defgeneric cpp::#if)
(defgeneric cpp::#endif)
(defgeneric cpp::#define)
(defgeneric cpp::#include)
(defgeneric cpp::#undef)
(defgeneric cpp::defined)
(defgeneric cpp::macro-or)
(defgeneric cpp::macro-and)
(defgeneric cpp::macro-not)
(defgeneric cpp::#warning)
(defgeneric cpp::#error)
(defgeneric cpp::#pragma)
(defgeneric cpp::concat#)
(defgeneric cpp::string#)

(defmethod cpp::#include
  ((?path STRING))
  (format nil
          "#include \"%s\""
          ?path))
(defmethod cpp::#include
  ((?path SYMBOL))
  (format nil
          "#include %s"
          ?path))

(defmethod cpp::#ifdef
  ((?key SYMBOL))
  (str-cat "#ifdef "
           ?key))
(defmethod cpp::#ifdef
  ((?key SYMBOL)
   (?unused SYMBOL
            (eq ?current-argument
                do))
   (?body MULTIFIELD))
  (create$ (#ifdef ?key)
           ?body
           (#endif ?key)))

(defmethod cpp::#ifndef
  ((?key SYMBOL))
  (str-cat "#ifndef "
           ?key))

(defmethod cpp::#ifndef
  ((?key SYMBOL)
   (?unused SYMBOL
            (eq ?current-argument
                do))
   (?body MULTIFIELD))
  (create$ (#ifndef ?key)
           ?body
           (#endif ?key)))
(defmethod cpp::#ifndef
  ((?key SYMBOL)
   (?unused SYMBOL
            (eq ?current-argument
                do))
   $?body)
  (#ifndef ?key
   ?unused
   ?body))
(defmethod cpp::#ifndef
  ((?key SYMBOL)
   (?action SYMBOL
            (eq ?current-argument
                guard))
   (?body MULTIFIELD))
  (#ifndef ?key do
   (#define ?key)
   ?body))

(defmethod cpp::#ifndef
  ((?key SYMBOL)
   (?unused SYMBOL
            (eq ?current-argument
                guard))
   $?body)
  (#ifndef ?key
   ?unused
   ?body))

(defmethod cpp::#define
  ((?key SYMBOL)
   ?value)
  (str-cat "#define "
           ?key
           " "
           ?value))
(defmethod cpp::#define
  ((?key SYMBOL)
   (?value STRING))
  (format nil
          "#define %s \"%s\""
          ?key
          ?value))

(defmethod cpp::#define
  ((?key SYMBOL))
  (format nil
          "#define %s"
          ?key))

(defmethod cpp::#endif
  ()
  #endif)

(defmethod cpp::#endif
  ((?key SYMBOL))
  (format nil
          "#endif // end %s"
          ?key))

(defmethod cpp::macro-or
  ((?a LEXEME)
   (?b LEXEME))
  (format nil
          "%s || %s"
          ?a
          ?b))

(defmethod cpp::macro-or
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

(defmethod cpp::macro-or
  ((?a LEXEME)
   (?b LEXEME)
   $?rest)
  (macro-or ?a
            ?b
            ?rest))


(defmethod cpp::macro-and
  ((?a LEXEME)
   (?b LEXEME))
  (format nil
          "%s && %s"
          ?a
          ?b))

(defmethod cpp::macro-and
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

(defmethod cpp::macro-and
  ((?a LEXEME)
   (?b LEXEME)
   $?rest)
  (macro-and ?a
             ?b
             ?rest))

(defmethod cpp::macro-not
  ((?a LEXEME))
  (str-cat !
           ?a))
(defmethod cpp::defined
  ((?a SYMBOL))
  (str-cat "defined(" ?a ")"))

(defmethod cpp::#if
  (?a)
  (str-cat "#if "
           ?a))

(defmethod cpp::#if
  (?key (?unused SYMBOL
                 (eq ?current-argument
                     do))
        (?body MULTIFIELD))
  (create$ (#if ?key)
           ?body
           (#endif ?key)))

(defmethod cpp::#undef
  ((?key SYMBOL))
  (str-cat "#undef "
           ?key))
(defmethod cpp::#define
  ((?key SYMBOL)
   (?args MULTIFIELD)
   (?definition MULTIFIELD))
  (create$ (format nil
                   "%s(%s)"
                   (#define ?key)
                   (implode$ ?args)) \
           ?definition))
(defmethod cpp::#define
  ((?key SYMBOL)
   (?args MULTIFIELD)
   $?body)
  (#define ?key ?args ?body))
(defmethod cpp::#define
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

(defmethod cpp::#define
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

(defmethod cpp::#pragma
  ((?elements MULTIFIELD))
  (format nil
          "#pragma %s"
          (implode$ ?elements)))
(defmethod cpp::#pragma
  ($?elements)
  (#pragma ?elements))

(defmethod cpp::#warning
  ((?message STRING))
  (format nil
          "#warning \"%s\""
          ?message))
(defmethod cpp::#error
  ((?message STRING))
  (format nil
          "#error \"%s\""
          ?message))

(defmethod cpp::string#
  ((?item SYMBOL))
  (str-cat # ?item))
(defmethod cpp::concat#
  ((?item1 LEXEME)
   (?item2 LEXEME))
  (str-cat ?item1 " ## " ?item2))
