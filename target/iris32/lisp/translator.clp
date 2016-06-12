; iris
; Copyright (c) 2013-2015, Joshua Scoggins and Contributors
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

(defmodule iris32
           (export ?ALL))

(deftemplate iris32::opcode-decl
             (slot op)
             (slot title)
             (multislot arguments))
(deffacts iris32::opcodes-defs
          (opcode-decl (op +)
                       (title add)
                       (arguments register register register))
          (opcode-decl (op +)
                       (title addi)
                       (arguments register register immediate))
          (opcode-decl (op -)
                       (title sub)
                       (arguments register register register))
          (opcode-decl (op -)
                       (title subi)
                       (arguments register register immediate))
          )

(deffunction iris32::registerp
             (?input)
             (or (and (instancep ?input)
                      (eq (class ?input)
                          register))
                 (and (lexemep ?input)
                      (has-prefix ?input r)
                      (numberp (bind ?val 
                                     (string-to-field (sub-string 2 
                                                                  (length$ ?input)
                                                                  ?input))))
                      (<= 0 ?val 255))))

(deffunction iris32::immediatep
             (?input)
             (or (numberp ?input)
                 (and (lexemep ?input)
                      (not (registerp ?input)))))

(defclass iris32::register
  (is-a USER))
(defclass iris32::immediate
  (is-a USER))
(defclass iris32::opcode
  (is-a USER)
  (slot op
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot children))

(defrule iris32::build-opcode:three-register
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register register register))
         =>
         (assert (make defgeneric ?op))
         (retract ?f)
         (format t "(defmethod iris32::op%s
                      ((?dest (registerp ?current-argument))
                       (?source0 (registerp ?current-argument))
                       (?source1 (registerp ?current-argument)))
                      (format nil 
                              \"%s %%s %%s %%s\" 
                              ?dest
                              ?source0
                              ?source1))%n"
         ?op
         ?title))

(defrule iris32::build-opcode:immediate-form
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register register immediate))
         =>
         (assert (make defgeneric ?op))
         (retract ?f)
         (format t "(defmethod iris32::op%s
                      ((?dest (registerp ?current-argument))
                       (?source0 (registerp ?current-argument))
                       (?source1 (immediatep ?current-argument)))
                      (format nil 
                              \"%s %%s %%s %%d\" 
                              ?dest
                              ?source0
                              ?source1))%n"
         ?op
         ?title))
(defrule iris32::build-opcode:two-register
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register register))
         =>
         (assert (make defgeneric ?op))
         (retract ?f)
         (format t "(defmethod iris32::op%s
                      ((?dest (registerp ?current-argument))
                       (?source (registerp ?current-argument)))
                      (format nil 
                              \"%s %%s %%s\"
                              ?dest
                              ?source))%n"
         ?op
         ?title))
(defrule iris32::build-opcode:set-form
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register immediate))
         =>
         (retract ?f)
         (assert (make defgeneric ?op))
         (format t "(defmethod iris32::op%s
                      ((?dest (registerp ?current-argument))
                       (?source (immediatep ?current-argument)))
                      (format nil
                              \"%s %%s %%s\"
                              ?dest
                              (str-cat ?source)))%n"
         ?op
         ?title))

(defrule iris32::build-defgeneric
         (declare (salience -1))
         ?f <- (make defgeneric ?op)
         =>
         (retract ?f)
         (format t "(defgeneric iris32::op%s)%n" ?op))
