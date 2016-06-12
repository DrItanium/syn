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
(defrule MAIN::init
         =>
         (focus iris32))
(defmodule iris32
           (export ?ALL))

(deftemplate iris32::opcode-decl
             (slot op)
             (slot title)
             (multislot arguments))
(defrule iris32::build-opcode-three-reg
         ?f <- (opcode:three-reg ?title)
         =>
         (retract ?f)
         (assert (opcode ?title register register register)))
(defrule iris32::build-opcode-three-reg-imm
         ?f <- (opcode:three-reg-imm ?title)
         =>
         (retract ?f)
         (assert (opcode ?title register register immediate)))

(defrule iris32::build-opcode-two-reg
         ?f <- (opcode:two-reg ?title)
         =>
         (retract ?f)
         (assert (opcode ?title register register)))

(deffacts iris32::opcodes-defs
          (opcode:three-reg add)
          (opcode:three-reg-imm addi)
          (opcode:three-reg sub)
          (opcode:three-reg-imm subi)
          (opcode:three-reg mul)
          (opcode:three-reg-imm muli)
          (opcode:three-reg div)
          (opcode:three-reg-imm divi)
          (opcode:three-reg rem)
          (opcode:three-reg-imm remi)
          (opcode @org immediate)
          (opcode @label immediate)
          (opcode @data)
          (opcode @code)
          (opcode @org immediate)
          (opcode @word immediate)
          (opcode j register)
          (opcode:two-reg jl)
          (opcode:two-reg jt)
          (opcode:two-reg jf)
          (opcode:three-reg jtl)
          (opcode:three-reg jfl)
          (opcode:three-reg ift)
          (opcode:three-reg iff)
          (opcode:three-reg iftl)
          (opcode:three-reg iffl)
          )

(defrule iris32::build-generic-op-code
         ?f <- (opcode ?title $?args)
         =>
         (retract ?f)
         (assert (opcode-decl (op ?title)
                              (title ?title)
                              (arguments $?args))))
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


(defrule iris32::build-opcode:three-register
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register register register))
         =>
         (assert (make defgeneric ?op))
         (retract ?f)
         (format t "(defmethod iris32::%s
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
         (format t "(defmethod iris32::%s
                      ((?dest (registerp ?current-argument))
                       (?source0 (registerp ?current-argument))
                       (?source1 (immediatep ?current-argument)))
                      (format nil 
                              \"%s %%s %%s %%s\" 
                              ?dest
                              ?source0
                              (str-cat ?source1)))%n"
         ?op
         ?title))
(defrule iris32::build-opcode:two-register
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register register))
         =>
         (assert (make defgeneric ?op))
         (retract ?f)
         (format t "(defmethod iris32::%s
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
         (format t "(defmethod iris32::%s
                      ((?dest (registerp ?current-argument))
                       (?source (immediatep ?current-argument)))
                      (format nil
                              \"%s %%s %%s\"
                              ?dest
                              (str-cat ?source)))%n"
         ?op
         ?title))
(defrule iris32::build-opcode:single-reg-form
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register))
         =>
         (retract ?f)
         (assert (make defgeneric ?op))
         (format t "(defmethod iris32::%s
                      ((?dest (registerp ?current-argument)))
                      (format nil 
                              \"%s %%s\"
                              ?dest))%n"
         ?op
         ?title))

(defrule iris32::build-opcode:single-immediate-form
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments immediate))
         =>
         (retract ?f)
         (assert (make defgeneric ?op))
         (format t "(defmethod iris32::%s
                      ((?dest (immediatep ?current-argument)))
                      (format nil 
                              \"%s %%s\"
                              (str-cat ?dest)))%n"
         ?op
         ?title))
(defrule iris32::build-opcode:no-args
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments))
         =>
         (retract ?f)
         (assert (make defgeneric ?op))
         (format t "(defmethod iris32::%s
                      ((?dest (immediatep ?current-argument)))
                      %s)%n"
         ?op
         ?title))
(defrule iris32::build-defgeneric
         (declare (salience -1))
         ?f <- (make defgeneric ?op)
         =>
         (retract ?f)
         (format t "(defgeneric iris32::%s)%n" ?op))
