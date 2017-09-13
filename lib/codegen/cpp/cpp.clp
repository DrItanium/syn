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
(defmodule cpp
           (export ?ALL))
(defgeneric cpp::comma-separated-list)
(defgeneric cpp::struct)
(defgeneric cpp::union)
(defgeneric cpp::field)
(defgeneric cpp::typedef)
(defgeneric cpp::typedef-function-pointer)
(defgeneric cpp::body)
(defgeneric cpp::function)
(defgeneric cpp::template)
(defgeneric cpp::class#)
(defgeneric cpp::semi-colon)
(defgeneric cpp::specialized-function)
(defgeneric cpp::using)
(defgeneric cpp::typename)
(defgeneric cpp::if#)
(defgeneric cpp::else-if#)
(defgeneric cpp::else#)
(defgeneric cpp::while#)
(defgeneric cpp::for#)
(defgeneric cpp::for-each#)
(defgeneric cpp::namespace)
(defgeneric cpp::enum)
(defgeneric cpp::enum-class)
(defgeneric cpp::with-body)
(defgeneric cpp::public:)
(defgeneric cpp::private:)
(defgeneric cpp::protected:)


(defmethod cpp::while#
  ((?condition LEXEME))
  (format nil
          "while (%s)"
          ?condition))
(defmethod cpp::while#
  ((?condition LEXEME)
   (?body MULTIFIELD))
  (create$ (while# ?condition)
           ?body))
(defmethod cpp::while#
  ((?cond LEXEME)
   $?body)
  (while# ?cond
          ?body))
(defmethod cpp::for#
  ((?init LEXEME)
   (?cond LEXEME)
   (?incr LEXEME))
  (format nil
          "for(%s;%s;%s)"
          ?init
          ?cond
          ?incr))

(defmethod cpp::for#
  ((?init LEXEME)
   (?cond LEXEME)
   (?incr LEXEME)
   (?body MULTIFIELD))
  (create$ (for# ?init
                 ?cond
                 ?incr)
           ?body))
(defmethod cpp::for#
  ((?init LEXEME)
   (?cond LEXEME)
   (?incr LEXEME)
   $?body)
  (for# ?init
        ?cond
        ?incr
        ?body))


(defmethod cpp::else#
  ((?body MULTIFIELD))
  (create$ else
           ?body))
(defmethod cpp::else#
  ($?body)
  (else# ?body))
(defmethod cpp::else-if#
  ((?condition LEXEME)
   (?body MULTIFIELD))
  (create$ (format nil
                   "else if (%s)"
                   ?condition)
           ?body))
(defmethod cpp::else-if#
  ((?condition LEXEME)
   $?body)
  (else-if# ?condition
            ?body))

(defmethod cpp::if#
  ((?condition LEXEME))
  (format nil
          "if (%s)"
          ?condition))
(defmethod cpp::if#
  ((?condition LEXEME)
   (?unused0 SYMBOL
             (eq ?current-argument
                 then))
   (?body MULTIFIELD))
  (create$ (if# ?condition)
           ?body))

(defmethod cpp::using
  ((?alias LEXEME)
   (?raw LEXEME))
  (format nil
          "using %s = %s;"
          ?alias
          ?raw))

(defmethod cpp::semi-colon
  ((?str LEXEME))
  (format nil
          "%s;"
          ?str))

(defmethod cpp::semi-colon
  ((?collection MULTIFIELD))
  (create$ ?collection
           ";"))


(defmethod cpp::body
  ((?contents MULTIFIELD))
  (create$ { ?contents }))

(defmethod cpp::body
  ($?contents)
  (body ?contents))

(defmethod cpp::with-body
  ((?header LEXEME
            MULTIFIELD)
   (?body MULTIFIELD))
  (create$ ?header
           (body ?body)))

(defmethod cpp::with-body
  ((?header MULTIFIELD
            LEXEME)
   $?body)
  (with-body ?header
             ?body))

(defmethod cpp::struct
  ()
  struct)
(defmethod cpp::struct
  ((?title SYMBOL))
  (str-cat "struct "
           ?title))

(defmethod cpp::struct
  ((?title SYMBOL)
   (?body MULTIFIELD))
  (with-body (struct ?title)
             ?body))
(create$ (struct ?title)
         (body ?body)))

(defmethod cpp::struct
  ((?body MULTIFIELD))
  (with-body (struct)
             ?body))

(defmethod cpp::union
  ()
  union)

(defmethod cpp::union
  ((?title SYMBOL))
  (str-cat "union "
           ?title))

(defmethod cpp::union
  ((?title SYMBOL)
   (?body MULTIFIELD))
  (with-body (union ?title)
             ?body))

(defmethod cpp::union
  ((?body MULTIFIELD))
  (with-body (union)
             ?body))



(defmethod cpp::field
  ((?type SYMBOL)
   (?name SYMBOL))
  (format nil
          "%s %s;"
          ?type
          ?name))


(defmethod cpp::typedef
  ((?raw LEXEME)
   (?name LEXEME))
  (format nil
          "typedef %s %s;"
          ?raw
          ?name))

(defmethod cpp::typedef-function-pointer
  ((?name SYMBOL)
   (?return LEXEME)
   (?arguments MULTIFIELD))
  (format nil
          "typedef %s (*%s)(%s);"
          ?return
          ?name
          (implode$ ?arguments)))

(defmethod cpp::typedef-function-pointer
  ((?name SYMBOL)
   (?return LEXEME)
   $?arguments)
  (typedef-function-pointer ?name
                            ?return
                            ?arguments))
(defmethod cpp::function
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

(defmethod cpp::function
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
(defmethod cpp::function
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

(defmethod cpp::template-args
  ((?elements MULTIFIELD))
  (format nil
          "<%s>"
          (implode$ ?elements)))
(defmethod cpp::template-args
  ($?elements)
  (template-args ?elements))
(defmethod cpp::template
  ((?elements MULTIFIELD))
  (format nil
          "template%s"
          (template-args ?elements)))

(defmethod cpp::template
  ($?elements)
  (template ?elements))

(defmethod cpp::class#
  ((?name SYMBOL))
  (format nil
          "class %s"
          ?name))
(defmethod cpp::class#
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
(defmethod cpp::class#
  ((?name SYMBOL)
   (?parents MULTIFIELD)
   (?body MULTIFIELD))
  (create$ (class# ?name
                   ?parents)
           (body ?body)))
(defmethod cpp::class#
  ((?name SYMBOL)
   (?parents MULTIFIELD)
   $?body)
  (class# ?name
          ?parents
          ?body))

(defmethod cpp::specialized-function
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
(defmethod cpp::specialized-function
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

(defmethod cpp::typename
  ()
  typename)
(defmethod cpp::typename
  ((?name LEXEME))
  (format nil
          "typename %s"
          ?name))

(defmethod cpp::for-each#
  ((?variable LEXEME)
   (?container LEXEME))
  (format nil
          "for (%s : %s)"
          ?variable
          ?container))

(defmethod cpp::for-each#
  ((?variable LEXEME)
   (?container LEXEME)
   (?body MULTIFIELD))
  (create$ (for-each# ?variable
                      ?container)
           ?body))
(defmethod cpp::for-each#
  ((?variable LEXEME)
   (?container LEXEME)
   $?body)
  (for-each# ?variable
             ?container
             ?body))

(defmethod cpp::namespace
  ((?name LEXEME))
  (format nil
          "namespace %s"
          ?name))
(defmethod cpp::namespace
  ((?name LEXEME)
   (?body MULTIFIELD))
  (with-body (namespace ?name)
             ?body))
(defmethod cpp::namespace
  ((?name LEXEME)
   $?body)
  (namespace ?name
             ?body))

(defmethod cpp::enum
  ((?name LEXEME))
  (format nil
          "enum %s"
          ?name))
(defmethod cpp::enum
  ((?name LEXEME)
   (?body MULTIFIELD))
  (with-body (enum ?name)
             ?body))
(defmethod cpp::enum
  ((?name LEXEME)
   $?body)
  (enum ?name
        ?body))

(defmethod cpp::enum-class
  ((?name LEXEME))
  (enum (class ?name)))
(defmethod cpp::enum-class
  ((?name LEXEME)
   (?type LEXEME))
  (format nil
          "%s : %s"
          (enum-class ?name)
          ?type))
(defmethod cpp::enum-class
  ((?name LEXEME)
   (?body MULTIFIELD))
  (create$ (enum-class ?name)
           (body ?body)))
(defmethod cpp::enum-class
  ((?name LEXEME)
   (?type LEXEME)
   (?body MULTIFIELD))
  (with-body (enum-class ?name
                         ?type)
             ?body))
(defmethod cpp::enum-class
  ((?name LEXEME)
   $?body)
  (enum-class ?name
              ?body))

(defmethod cpp::comma-separated-list
  ((?list MULTIFIELD
          (= (length$ ?current-argument)
             0)))
  (create$))
(defmethod cpp::comma-separated-list
  ((?list MULTIFIELD
          (> (length$ ?current-argument)
             0)))
  (bind ?collection
        (nth$ 1
              ?list))
  (progn$ (?item (rest$ ?list))
          (bind ?collection
                ,
                ?item))
  ?collection)

(defmethod cpp::comma-separated-list
  ($?list)
  (comma-separated-list ?list))

(defmethod cpp::public:
  ()
  public:)
(defmethod cpp::public:
  ((?body MULTIFIELD))
  (create$ (public:)
           ?body))
(defmethod cpp::public:
  ($?body)
  (public: ?body))
(defmethod cpp::private:
  ()
  private:)
(defmethod cpp::private:
  ((?body MULTIFIELD))
  (create$ (private:)
           ?body))
(defmethod cpp::private:
  ($?body)
  (private: ?body))
(defmethod cpp::protected:
  ()
  protected:)
(defmethod cpp::protected:
  ((?body MULTIFIELD))
  (create$ (protected:)
           ?body))
(defmethod cpp::protected:
  ($?body)
  (protected: ?body))

