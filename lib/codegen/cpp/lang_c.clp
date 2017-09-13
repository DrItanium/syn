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

; lang_c.clp - c language related code generator routines

(defgeneric struct)
(defgeneric union)
(defgeneric field)
(defgeneric typedef)
(defgeneric typedef-function-pointer)
(defgeneric body)
(defgeneric function)


(defmethod body
  ((?contents MULTIFIELD))
  (create$ { ?contents }))
(defmethod body
  ($?contents)
  (body ?contents))


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
  (create$ (struct ?title)
           (body ?body)))

(defmethod struct
  ((?body MULTIFIELD))
  (create$ (struct)
           (body ?body)))

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
  (create$ (union ?title)
           (body ?body)))

(defmethod union
  ((?body MULTIFIELD))
  (create$ (union)
           (body ?body)))



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
   (?return LEXEME))
  (format nil
          "%s %s(%s)"
          ?return
          ?name
          (implode$ ?args)))
(defmethod function
  ((?name SYMBOL)
   (?args MULTIFIELD)
   (?return LEXEME)
   (?body MULTIFIELD))
  (create$ (function ?name
                     ?args
                     ?return)
           (body ?body)))
(defmethod function
  ((?name SYMBOL)
   (?args MULTIFIELD)
   (?return LEXEME)
   $?body)
  (function ?name
            ?args
            ?return
            ?body))
