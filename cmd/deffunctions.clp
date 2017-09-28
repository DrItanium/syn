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
(deffunction binaryp
             (?sym)
             (eq (str-index 0b
                            ?sym) 1))
(deffunction hexp
             (?sm)
             (eq (str-index 0x
                            ?sm)
                 1))
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

(deffunction potential-convert
             (?value)
             (if (lexemep ?value) then
               (if (hexp ?value) then
                 (hex->int ?value)
                 else
                 (if (binaryp ?value) then
                   (binary->int ?value)
                   else
                   ?value))
               else
               ?value))

(deffunction generate-encode-decode-ops
             (?n ?name ?value ?mask ?shift)
             (bind ?m
                   (str-cat (potential-convert ?mask)))
             (bind ?s
                   (str-cat (potential-convert ?shift)))
             (bind ?base-name
                   (format nil
                           "%s-%%s-%s"
                           ?n
                           ?name))
             (bind ?encode-name
                   (format nil
                           ?base-name
                           encode))
             (bind ?decode-name
                   (format nil
                           ?base-name
                           decode))

             (format t
                     "(defgeneric MAIN::%s)%n(defgeneric MAIN::%s)%n"
                     ?encode-name
                     ?decode-name)
             (format t
                     "(defmethod MAIN::%s
                        ((?value INTEGER))
                        (decode-bits ?value
                                     %s
                                     %s))%n"
                     ?decode-name
                     ?m
                     ?s)
             (format t
                     "(defmethod MAIN::%s
                        ((?value INTEGER)
                         (?field INTEGER))
                        (encode-bits ?value
                                     ?field
                                     %s
                                     %s))%n"
                     ?encode-name
                     ?m
                     ?s))


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


(defrule MAIN::generate-field:clips:use-input-type
         (field (name ?name)
                (mask ?mask)
                (shift ?shift)
                (input-type FALSE))
         (input-type ?value)
         (namespace ?n)
         =>
         (generate-encode-decode-ops ?n
                                     ?name
                                     ?value
                                     ?mask
                                     ?shift))

(defrule MAIN::generate-field:clips:use-embedded-type
         (field (name ?name)
                (mask ?mask)
                (shift ?shift)
                (input-type ?value&~FALSE))
         (namespace ?n)
         =>
         (generate-encode-decode-ops ?n
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


(defrule MAIN::generate-enum:clips
         (declare (salience 1))
         (enum (name ?name)
               (cast-to ?ct)
               (max-size ?size)
               (children $?children))
         (namespace ?n)
         =>
         (bind ?output
               (create$))
         (progn$ (?c ?children)
                 (bind ?output
                       ?output
                       (sym-cat ?c)))
         (format t
                 "(defglobal MAIN ?*%s-enum%s* = (create$ %s))%n"
                 ?n
                 ?name
                 (implode$ ?output)))
(defrule MAIN::generate-target-architecture
         (namespace ?ns)
         =>
         (format t
                "(deffunction MAIN::target-architecture () %s)%n"
                ?ns))
