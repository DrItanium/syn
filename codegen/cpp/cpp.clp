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
(defgeneric cpp::public:)
(defgeneric cpp::private:)
(defgeneric cpp::protected:)
(defgeneric cpp::do-while#)
(defgeneric cpp::inherits-from)
; preprocessor related operations
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

(deffunction cpp::bodyp
             "Is the given thing a direct body type"
             (?thing)
             (eq (class ?thing)
                 body))
(defmessage-handler PRIMITIVE to-multifield primary
                    ()
                    (create$ ?self))
(defmessage-handler MULTIFIELD to-multifield primary
                    ()
                    (bind ?contents
                          (create$))
                    (progn$ (?a ?self)
                            (bind ?contents
                                  ?contents
                                  (send ?a
                                        to-multifield)))
                    ?contents)
(defclass cpp::has-condition
  (is-a USER)
  (slot condition
        (type LEXEME)
        (storage local)
        (visibility public)
        (default-dynamic "")))
(defclass cpp::has-title
  (is-a USER)
  (slot title
        (type LEXEME)
        (storage local)
        (visibility public)
        (default-dynamic "")))

(defclass cpp::requires-condition
  (is-a has-condition)
  (slot condition
        (source composite)
        (default ?NONE)))
(defclass cpp::requires-title
  (is-a has-title)
  (slot title
        (source composite)
        (default ?NONE)))

(defclass cpp::body
  (is-a USER)
  (multislot contents
             (storage local)
             (visibility public)
             (default ?NONE))
  (message-handler to-multifield primary))
(defmessage-handler cpp::body to-multifield primary
                    ()
                    (create$ {
                             (send ?self:contents
                                   to-multifield)
                             }))
(defclass cpp::access-declaration
  (is-a body)
  (slot type
        (visibility public)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (type SYMBOL)
        (default unimplmented-type))
  (message-handler to-multifield primary))
(defmessage-handler cpp::access-declaration to-multifield primary
                    ()
                    (create$ (dynamic-get type)
                             (send ?self:contents
                                   to-multifield)))

(defclass cpp::public:
  (is-a access-declaration)
  (slot type
        (source composite)
        (default public:)))

(defclass cpp::private:
  (is-a access-declaration)
  (slot type
        (source composite)
        (default private:)))

(defclass cpp::protected:
  (is-a access-declaration)
  (slot type
        (source composite)
        (default protected:)))

(defclass cpp::while-loop
  (is-a body
        requires-condition)
  (slot condition
        (type LEXEME)
        (default ?NONE))
  (message-handler to-multifield primary))

(defmessage-handler cpp::while-loop to-multifield primary
                    ()
                    (create$ (format nil
                                     "while (%s)"
                                     (dynamic-get condition))
                             (call-next-handler)))

(defclass cpp::do-while-loop
  (is-a body
        requires-condition)
  (slot condition
        (type LEXEME)
        (default ?NONE))
  (message-handler to-multifield primary))

(defmessage-handler cpp::do-while-loop to-multifield primary
                    ()
                    (create$ do
                             (call-next-handler)
                             (semi-colon (format nil
                                                 "while (%s)"
                                                 (dynamic-get condition)))))

(defclass cpp::for-loop
  (is-a body
        has-condition)
  (slot initialize
        (type LEXEME)
        (default-dynamic ""))
  (slot condition
        (source composite)
        (default-dynamic ""))
  (slot advance
        (type LEXEME)
        (default-dynamic ""))
  (message-handler to-multifield primary))

(defmessage-handler cpp::for-loop to-multifield primary
                    ()
                    (create$ (format nil
                                     "for (%s;%s;%s)"
                                     ?self:initialize
                                     (dynamic-get condition)
                                     ?self:advance)
                             (call-next-handler)))

(defclass cpp::else
  (is-a body)
  (message-handler to-multifield primary))

(defmessage-handler cpp::else to-multifield primary
                    ()
                    (create$ else
                             (call-next-handler)))

(defclass cpp::else-if
  (is-a body
        requires-condition)
  (message-handler to-multifield primary))
(defmessage-handler cpp::else-if to-multifield primary
                    ()
                    (create$ (format nil
                                     "else if (%s)"
                                     (dynamic-get condition))
                             (call-next-handler)))
(defclass cpp::if
  (is-a body
        requires-condition)
  (message-handler to-multifield primary))

(defmessage-handler cpp::if to-multifield primary
                    ()
                    (create$ (format nil
                                     "if (%s)"
                                     (dynamic-get condition))
                             (call-next-handler)))
(defclass cpp::inherits-from
  (is-a requires-title)
  (slot accessibility
        (type LEXEME)
        (allowed-strings "")
        (allowed-symbols public
                         protected)
        (default-dynamic ""))
  (message-handler to-multifield primary))
(defmessage-handler cpp::inherits-from to-multifield primary
                    ()
                    (create$ (format nil
                                     "%s %s"
                                     (dynamic-get accessibility)
                                     (dynamic-get title))))

(defclass cpp::data-structure
  (is-a body
        has-title)
  (slot type
        (type LEXEME)
        (storage shared)
        (visibility public)
        (default UNIMPLEMENTED))
  (message-handler to-multifield primary)
  (message-handler construct-definition primary))

(defmessage-handler cpp::data-structure to-multifield primary
                    ()
                    (create$ (send ?self
                                   construct-definition)
                             (call-next-handler)))
(defmessage-handler cpp::data-structure construct-definition primary
                    ()
                    (format nil
                            "%s %s"
                            (dynamic-get type)
                            (dynamic-get title)))




(defclass cpp::inheritable-data-structure
  (is-a data-structure)
  (multislot inherits-from
             (type INSTANCE)
             (allowed-classes inherits-from))
  (message-handler construct-definition primary))

(defmessage-handler cpp::inheritable-data-structure construct-definition primary
                    ()
                    (bind ?base
                          (call-next-handler))
                    (if (= (length$ ?self:inherits-from)
                            0) then
                      ?base
                      else
                      (format nil
                              "%s : %s"
                              ?base
                              (comma-separated-list
                                (send ?self:inherits-from
                                      to-multifield)))))
(defclass cpp::class
  (is-a inheritable-data-structure)
  (slot type
        (source composite)
        (access read-only)
        (create-accessor read)
        (default class)))

(defclass cpp::struct
  (is-a inheritable-data-structure)
  (slot type
        (source composite)
        (access read-only)
        (create-accessor read)
        (default struct)))

(defclass cpp::union
  (is-a data-structure)
  (slot type
        (source composite)
        (access read-only)
        (create-accessor read)
        (default union)))

(defclass cpp::binary-operation
  (is-a USER)
  (slot operation
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot first-arg
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot second-arg
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler to-multifield primary))

(defmessage-handler cpp::binary-operation to-multifield primary
                    ()
                    (create$ "("
                             (send ?self:first-arg
                                   to-multifield)
                             (dynamic-get operation)
                             (send ?self:second-arg
                                   to-multifield)
                             ")"))
(defclass cpp::unary-operation
  (is-a USER)
  (slot operation
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot first-arg
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler to-multifield primary))
(defmessage-handler cpp::unary-operation to-multifield primary
                    ()
                    (create$ "("
                             (dynamic-get operation)
                             (send ?self:first-arg
                                   to-multifield)
                             ")"))

(defclass cpp::explicit-cast
  (is-a binary-operation)
  (slot first-arg
        (source composite)
        (type LEXEME))
  (slot operation
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default PLEASE_REDEFINE_CAST_TYPE))
  (message-handler to-multifield primary))
(defmessage-handler cpp::explicit-cast to-multifield primary
                    ()
                    (create$ (str-cat "("
                                      (dynamic-get operation)
                                      "<"
                                      (send ?self:first-arg
                                            to-multifield)
                                      ">")
                                      "("
                                      (send ?self:second-arg
                                            to-multifield)
                                      ")"))

(defclass cpp::static-cast
  (is-a explicit-cast)
  (slot operation
        (source composite)
        (default static_cast)))

(defclass cpp::dynamic-cast
  (is-a explicit-cast)
  (slot operation
        (source composite)
        (default dynamic_cast)))

(defclass cpp::const-cast
  (is-a explicit-cast)
  (slot operation
        (source composite)
        (default const_cast)))
(defclass cpp::reinterpret-cast
  (is-a explicit-cast)
  (slot operation
        (source composite)
        (default reinterpret_cast)))




(defgeneric cpp::unary-operation)
(defgeneric cpp::binary-operation)
(defgeneric cpp::logical-not#)
(defgeneric cpp::logical-and#)
(defgeneric cpp::logical-or#)
(defgeneric cpp::logical-xor#)
(defgeneric cpp::bitwise-not#)
(defgeneric cpp::bitwise-and#)
(defgeneric cpp::bitwise-or#)
(defgeneric cpp::bitwise-xor#)
(defgeneric cpp::not-equals#)
(defgeneric cpp::equals#)
(defgeneric cpp::+#)
(defgeneric cpp::-#)
(defgeneric cpp::*#)
(defgeneric cpp::/#)
(defgeneric cpp::%#)
(defgeneric cpp::shift-left#)
(defgeneric cpp::shift-right#)
(defgeneric cpp::static-cast)
(defgeneric cpp::dynamic-cast)
(defgeneric cpp::const-cast)
(defgeneric cpp::reinterpret-cast)

(defmethod cpp::static-cast
  ((?type LEXEME)
   ?arg)
  (make-instance of static-cast
                 (first-arg ?type)
                 (second-arg ?arg)))
(defmethod cpp::dynamic-cast
  ((?type LEXEME)
   ?arg)
  (make-instance of dynamic-cast
                 (first-arg ?type)
                 (second-arg ?arg)))
(defmethod cpp::const-cast
  ((?type LEXEME)
   ?arg)
  (make-instance of const-cast
                 (first-arg ?type)
                 (second-arg ?arg)))
(defmethod cpp::reinterpret-cast
  ((?type LEXEME)
   ?arg)
  (make-instance of reinterpret-cast
                 (first-arg ?type)
                 (second-arg ?arg)))

(defmethod cpp::shift-left#
  (?arg0 ?arg1)
  (binary-operation "<<"
                    ?arg0
                    ?arg1))
(defmethod cpp::shift-right#
  (?arg0 ?arg1)
  (binary-operation ">>"
                    ?arg0
                    ?arg1))
(defmethod cpp::+#
  (?arg0 ?arg1)
  (binary-operation +
                    ?arg0
                    ?arg1))
(defmethod cpp::-#
  (?arg0 ?arg1)
  (binary-operation -
                    ?arg0
                    ?arg1))
(defmethod cpp::*#
  (?arg0 ?arg1)
  (binary-operation *
                    ?arg0
                    ?arg1))
(defmethod cpp::/#
  (?arg0 ?arg1)
  (binary-operation /
                    ?arg0
                    ?arg1))
(defmethod cpp::%#
  (?arg0 ?arg1)
  (binary-operation %
                    ?arg0
                    ?arg1))

(defmethod cpp::equals#
  (?arg0 ?arg1)
  (binary-operation "=="
                    ?arg0
                    ?arg1))

(defmethod cpp::not-equals#
  (?arg0 ?arg1)
  (binary-operation "!="
                    ?arg0
                    ?arg1))

(defmethod cpp::unary-operation
  ((?operation LEXEME)
   ?arg0)
  (make-instance of unary-operation
                 (operation ?operation)
                 (first-arg ?arg0)))
(defmethod cpp::binary-operation
  ((?operation LEXEME)
   ?arg0
   ?arg1)
  (make-instance of binary-operation
                 (operation ?operation)
                 (first-arg ?arg0)
                 (second-arg ?arg1)))

(defmethod cpp::logical-not#
  (?arg0)
  (unary-operation !
                   ?arg0))
(defmethod cpp::bitwise-not#
  (?arg0)
  (unary-operation "~"
                   ?arg0))
(defmethod cpp::logical-and#
  (?arg0 ?arg1)
  (binary-operation "&&"
                    ?arg0
                    ?arg1))
(defmethod cpp::bitwise-and#
  (?arg0 ?arg1)
  (binary-operation "&"
                    ?arg0
                    ?arg1))

(defmethod cpp::logical-or#
  (?arg0 ?arg1)
  (binary-operation "||"
                    ?arg0
                    ?arg1))
(defmethod cpp::bitwise-or#
  (?arg0 ?arg1)
  (binary-operation "|"
                    ?arg0
                    ?arg1))

(defmethod cpp::logical-xor#
  (?arg0 ?arg1)
  (not-equals# ?arg0
               ?arg1))

(defmethod cpp::bitwise-xor#
  (?arg0 ?arg1)
  (binary-operation "^"
                    ?arg0
                    ?arg1))

(defmethod cpp::do-while#
  ((?condition LEXEME)
   (?body MULTIFIELD))
  (make-instance of do-while-loop
                 (condition ?condition)
                 (contents ?body)))
(defmethod cpp::do-while#
  ((?condition LEXEME)
   $?body)
  (do-while# ?condition
             ?body))
(defmethod cpp::while#
  ((?condition LEXEME)
   (?body MULTIFIELD))
  (make-instance of while-loop
                 (condition ?condition)
                 (contents ?body)))
(defmethod cpp::while#
  ((?cond LEXEME)
   $?body)
  (while# ?cond
          ?body))

(defmethod cpp::for#
  ((?init LEXEME)
   (?cond LEXEME)
   (?incr LEXEME)
   (?body MULTIFIELD))
  (make-instance of for-loop
                 (initialize ?init)
                 (condition ?cond)
                 (advance ?incr)
                 (contents ?body)))
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
  (make-instance of else
                 (contents ?body)))
(defmethod cpp::else#
  ($?body)
  (else# ?body))

(defmethod cpp::else-if#
  ((?condition LEXEME)
   (?body MULTIFIELD))
  (make-instance of else-if
                 (condition ?condition)
                 (contents ?body)))

(defmethod cpp::else-if#
  ((?condition LEXEME)
   $?body)
  (else-if# ?condition
            ?body))

(defmethod cpp::if#
  ((?condition LEXEME)
   (?unused0 SYMBOL
             (eq ?current-argument
                 then))
   (?body MULTIFIELD))
  (make-instance of if
                 (condition ?condition)
                 (contents ?body)))

(defmethod cpp::if#
  ((?condition LEXEME)
   (?unused SYMBOL
            (eq ?current-argument
                then))
   $?body)
  (if# ?condition
       ?unused
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
  (make-instance of body
                 (contents ?contents)))

(defmethod cpp::body
  ($?contents)
  (body ?contents))

(defmethod cpp::union
  ((?title SYMBOL)
   (?body MULTIFIELD))
  (make-instance of union
                 (title ?title)
                 (contents ?body)))

(defmethod cpp::union
  ((?body MULTIFIELD))
  (make-instance of union
                 (title "")
                 (contents ?body)))



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

(defclass cpp::namespace
  (is-a body
        has-title)
  (message-handler to-multifield primary))
(defmessage-handler cpp::namespace to-multifield primary
                    ()
                    (create$ (format nil
                                     "namespace %s"
                                     (dynamic-get title))
                             (call-next-handler)))
(defmethod cpp::namespace
  ((?name LEXEME)
   (?body MULTIFIELD))
  (make-instance of namespace
                 (name ?name)
                 (contents ?body)))

(defmethod cpp::namespace
  ((?name LEXEME)
   $?body)
  (namespace ?name
             ?body))
(defclass cpp::enum
  (is-a body
        has-title)
  (slot signifier
        (type LEXEME)
        (storage shared)
        (visibility public)
        (default enum))
  (slot numeric-type
        (type LEXEME)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (message-handler to-multifield primary))
(defmessage-handler cpp::enum to-multifield primary
                    ()
                    (create$ (format nil
                                     (expand$ (if ?self:numeric-type then
                                                (create$ "%s %s : %s"
                                                         (dynamic-get signifier)
                                                         (dynamic-get title)
                                                         ?self:numeric-type)
                                                else
                                                (create$ "%s %s"
                                                         (dynamic-get signifier)
                                                         (dynamic-get title)))))
                             {
                             (comma-separated-list (send (dynamic-get contents)
                                                         to-multifield))
                             "};"))

(defclass cpp::enum-class
  (is-a enum)
  (slot signifier
        (source composite)
        (default "enum class")))




(defmethod cpp::enum
  ((?name LEXEME)
   (?body MULTIFIELD))
  (make-instance of enum
                 (title ?name)
                 (contents ?body)))
(defmethod cpp::enum
  ((?name LEXEME)
   (?unused0 SYMBOL
             (eq ?current-argument
                 is))
   (?type LEXEME)
   (?body MULTIFIELD))
  (make-instance of enum
                 (title ?name)
                 (numeric-type ?type)
                 (contents ?body)))
(defmethod cpp::enum
  ((?name LEXEME)
   $?body)
  (enum ?name
        ?body))

(defmethod cpp::enum
  ((?name LEXEME)
   (?unused0 SYMBOL
             (eq ?current-argument
                 is))
   (?type LEXEME)
   $?body)
  (enum ?name
        ?unused0
        ?type
        ?body))

(defmethod cpp::enum-class
  ((?name LEXEME)
   (?body MULTIFIELD))
  (make-instance of enum-class
                 (title ?name)
                 (contents ?body)))
(defmethod cpp::enum-class
  ((?name LEXEME)
   (?unused0 SYMBOL
             (eq ?current-argument
                 is))
   (?type LEXEME)
   (?body MULTIFIELD))
  (make-instance of enum-class
                 (title ?name)
                 (numeric-type ?type)
                 (contents ?body)))
(defmethod cpp::enum-class
  ((?name LEXEME)
   $?body)
  (enum-class ?name
        ?body))

(defmethod cpp::enum-class
  ((?name LEXEME)
   (?unused0 SYMBOL
             (eq ?current-argument
                 is))
   (?type LEXEME)
   $?body)
  (enum-class ?name
        ?unused0
        ?type
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
                ?collection
                ,
                ?item))
  ?collection)

(defmethod cpp::comma-separated-list
  ($?list)
  (comma-separated-list ?list))

(defmethod cpp::public:
  ()
  (make-instance of public:
                 (contents)))
(defmethod cpp::public:
  ((?body MULTIFIELD))
  (make-instance of public:
                 (contents ?body)))
(defmethod cpp::public:
  ($?body)
  (public: ?body))

(defmethod cpp::private:
  ()
  (make-instance of private:
                 (contents)))
(defmethod cpp::private:
  ((?body MULTIFIELD))
  (make-instance of private:
                 (contents ?body)))
(defmethod cpp::private:
  ($?body)
  (private: ?body))

(defmethod cpp::protected:
  ()
  (make-instance of protected:
                 (contents)))
(defmethod cpp::protected:
  ((?body MULTIFIELD))
  (make-instance of protected:
                 (contents ?body)))
(defmethod cpp::protected:
  ($?body)
  (protected: ?body))
(defmethod cpp::inherits-from
  ((?name LEXEME)
   (?access LEXEME))
  (make-instance of inherits-from
                 (title ?name)
                 (accessibility ?access)))
(defmethod cpp::inherits-from
  ((?name LEXEME))
  (make-instance of inherits-from
                 (title ?name)))



(defmethod cpp::class#
  ((?name SYMBOL)
   (?parents MULTIFIELD)
   (?body MULTIFIELD))
  (make-instance of class
                 (title ?name)
                 (inherits-from ?parents)
                 (contents ?body)))
(defmethod cpp::class#
  ((?name SYMBOL)
   (?parents MULTIFIELD)
   $?body)
  (class# ?name
          ?parents
          ?body))


(defmethod cpp::struct
  ((?name SYMBOL)
   (?parents MULTIFIELD)
   (?body MULTIFIELD))
  (make-instance of class
                 (title ?name)
                 (inherits-from ?parents)
                 (contents ?body)))
(defmethod cpp::struct
  ((?name SYMBOL)
   (?parents MULTIFIELD)
   $?body)
  (struct ?name
          ?parents
          ?body))


; preprocessor related operations

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

