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

; cpp.clp - c++ language related code generator routines

(defgeneric struct)
(defgeneric union)
(defgeneric field)
(defgeneric typedef)
(defgeneric typedef-function-pointer)
(defgeneric body)
(defgeneric function)
(defgeneric template)
(defgeneric class#)
(defgeneric semi-colon)
(defgeneric specialized-function)
(defgeneric using)
(defgeneric typename)
(defgeneric reference)
(defgeneric r-value-reference)
(defgeneric if#)
(defgeneric else-if#)
(defgeneric else#)
(defgeneric while#)
(defgeneric for#)
(defgeneric for-each#)
(defgeneric namespace)
(defgeneric enum)
(defgeneric enum-class)
(defgeneric with-body)


(defmethod while#
  ((?condition LEXEME))
  (format nil
          "while (%s)"
          ?condition))
(defmethod while#
  ((?condition LEXEME)
   (?body MULTIFIELD))
  (create$ (while# ?condition)
           ?body))
(defmethod while#
  ((?cond LEXEME)
   $?body)
  (while# ?cond
          ?body))
(defmethod for#
  ((?init LEXEME)
   (?cond LEXEME)
   (?incr LEXEME))
  (format nil
          "for(%s;%s;%s)"
          ?init
          ?cond
          ?incr))

(defmethod for#
  ((?init LEXEME)
   (?cond LEXEME)
   (?incr LEXEME)
   (?body MULTIFIELD))
  (create$ (for# ?init
                 ?cond
                 ?incr)
           ?body))
(defmethod for#
  ((?init LEXEME)
   (?cond LEXEME)
   (?incr LEXEME)
   $?body)
  (for# ?init
        ?cond
        ?incr
        ?body))


(defmethod else#
  ((?body MULTIFIELD))
  (create$ else
           ?body))
(defmethod else#
  ($?body)
  (else# ?body))
(defmethod else-if#
  ((?condition LEXEME)
   (?body MULTIFIELD))
  (create$ (format nil
                   "else if (%s)"
                   ?condition)
           ?body))
(defmethod else-if#
  ((?condition LEXEME)
   $?body)
  (else-if# ?condition
            ?body))

(defmethod if#
  ((?condition LEXEME))
  (format nil
          "if (%s)"
          ?condition))
(defmethod if#
  ((?condition LEXEME)
   (?unused0 SYMBOL
             (eq ?current-argument
                 then))
   (?body MULTIFIELD))
  (create$ (if# ?condition)
           ?body))

(defmethod using
  ((?alias LEXEME)
   (?raw LEXEME))
  (format nil
          "using %s = %s;"
          ?alias
          ?raw))

(defmethod semi-colon
  ((?str LEXEME))
  (format nil
          "%s;"
          ?str))

(defmethod semi-colon
  ((?collection MULTIFIELD))
  (create$ ?collection
           ";"))


(defmethod body
  ((?contents MULTIFIELD))
  (create$ { ?contents }))

(defmethod body
  ($?contents)
  (body ?contents))

(defmethod with-body
  ((?header LEXEME
            MULTIFIELD)
   (?body MULTIFIELD))
  (create$ ?header
           (body ?body)))

(defmethod with-body
  ((?header MULTIFIELD
            LEXEME)
   $?body)
  (with-body ?header
             ?body))

(defmethod struct
  ()
  struct)
(defmethod struct
  ((?title SYMBOL))
  (str-cat "struct "
           ?title))

(defmethod struct
  ((?title SYMBOL)
   (?body MULTIFIELD))
  (with-body (struct ?title)
             ?body))
(create$ (struct ?title)
         (body ?body)))

(defmethod struct
  ((?body MULTIFIELD))
  (with-body (struct)
             ?body))

(defmethod union
  ()
  union)

(defmethod union
  ((?title SYMBOL))
  (str-cat "union "
           ?title))

(defmethod union
  ((?title SYMBOL)
   (?body MULTIFIELD))
  (with-body (union ?title)
             ?body))

(defmethod union
  ((?body MULTIFIELD))
  (with-body (union)
             ?body))



(defmethod field
  ((?type SYMBOL)
   (?name SYMBOL))
  (format nil
          "%s %s;"
          ?type
          ?name))


(defmethod typedef
  ((?raw LEXEME)
   (?name LEXEME))
  (format nil
          "typedef %s %s;"
          ?raw
          ?name))

(defmethod typedef-function-pointer
  ((?name SYMBOL)
   (?return LEXEME)
   (?arguments MULTIFIELD))
  (format nil
          "typedef %s (*%s)(%s);"
          ?return
          ?name
          (implode$ ?arguments)))

(defmethod typedef-function-pointer
  ((?name SYMBOL)
   (?return LEXEME)
   $?arguments)
  (typedef-function-pointer ?name
                            ?return
                            ?arguments))
(defmethod function
  ((?name SYMBOL)
   (?args MULTIFIELD)
   (?return LEXEME)
   (?qualifiers MULTIFIELD))
  (format nil
          "%s %s(%s) %s"
          ?return
          ?name
          (implode$ ?args)
          (implode$ ?qualifiers)))

(defmethod function
  ((?name SYMBOL)
   (?args MULTIFIELD)
   (?return LEXEME)
   (?qualifiers MULTIFIELD)
   (?body MULTIFIELD))
  (create$ (function ?name
                     ?args
                     ?return
                     ?qualifiers)
           (body ?body)))
(defmethod function
  ((?name SYMBOL)
   (?args MULTIFIELD)
   (?return LEXEME)
   (?qualifiers MULTIFIELD)
   $?body)
  (function ?name
            ?args
            ?return
            ?qualifiers
            ?body))

(defmethod template-args
  ((?elements MULTIFIELD))
  (format nil
          "<%s>"
          (implode$ ?elements)))
(defmethod template-args
  ($?elements)
  (template-args ?elements))
(defmethod template
  ((?elements MULTIFIELD))
  (format nil
          "template%s"
          (template-args ?elements)))

(defmethod template
  ($?elements)
  (template ?elements))

(defmethod class#
  ((?name SYMBOL))
  (format nil
          "class %s"
          ?name))
(defmethod class#
  ((?name SYMBOL)
   (?parents MULTIFIELD))
  (bind ?base
        (class# ?name))
  (if (<> (length$ ?parents) 0) then
    (format nil
            "%s : %s"
            ?base
            (implode$ ?parents))
    else
    ?base))
(defmethod class#
  ((?name SYMBOL)
   (?parents MULTIFIELD)
   (?body MULTIFIELD))
  (create$ (class# ?name
                   ?parents)
           (body ?body)))
(defmethod class#
  ((?name SYMBOL)
   (?parents MULTIFIELD)
   $?body)
  (class# ?name
          ?parents
          ?body))

(defmethod specialized-function
  ((?name SYMBOL)
   (?args MULTIFIELD)
   (?return LEXEME)
   (?qualifiers MULTIFIELD)
   (?template-args MULTIFIELD)
   (?body MULTIFIELD))
  (create$ (template)
           (function (str-cat ?name
                              (template-args ?template-args))
                     ?args
                     ?return
                     ?qualifiers
                     ?body)))
(defmethod specialized-function
  ((?name SYMBOL)
   (?args MULTIFIELD)
   (?return LEXEME)
   (?qualifiers MULTIFIELD)
   (?template-args MULTIFIELD)
   $?body)
  (specialized-function ?name
                        ?args
                        ?return
                        ?qualifiers
                        ?template-args
                        ?body))

(defmethod typename
  ()
  typename)
(defmethod typename
  ((?name LEXEME))
  (format nil
          "typename %s"
          ?name))

(defmethod for-each#
  ((?variable LEXEME)
   (?container LEXEME))
  (format nil
          "for (%s : %s)"
          ?variable
          ?container))

(defmethod for-each#
  ((?variable LEXEME)
   (?container LEXEME)
   (?body MULTIFIELD))
  (create$ (for-each# ?variable
                      ?container)
           ?body))
(defmethod for-each#
  ((?variable LEXEME)
   (?container LEXEME)
   $?body)
  (for-each# ?variable
             ?container
             ?body))

(defmethod namespace
  ((?name LEXEME))
  (format nil
          "namespace %s"
          ?name))
(defmethod namespace
  ((?name LEXEME)
   (?body MULTIFIELD))
  (with-body (namespace ?name)
             ?body))
(defmethod namespace
  ((?name LEXEME)
   $?body)
  (namespace ?name
             ?body))

(defmethod enum
  ((?name LEXEME))
  (format nil
          "enum %s"
          ?name))
(defmethod enum
  ((?name LEXEME)
   (?body MULTIFIELD))
  (with-body (enum ?name)
             ?body))
(defmethod enum
  ((?name LEXEME)
   $?body)
  (enum ?name
        ?body))

(defmethod enum-class
  ((?name LEXEME))
  (enum (class ?name)))
(defmethod enum-class
  ((?name LEXEME)
   (?type LEXEME))
  (format nil
          "%s : %s"
          (enum-class ?name)
          ?type))
(defmethod enum-class
  ((?name LEXEME)
   (?body MULTIFIELD))
  (create$ (enum-class ?name)
           (body ?body)))
(defmethod enum-class
  ((?name LEXEME)
   (?type LEXEME)
   (?body MULTIFIELD))
  (with-body (enum-class ?name
                         ?type)
             ?body))
(defmethod enum-class
  ((?name LEXEME)
   $?body)
  (enum-class ?name
              ?body))
