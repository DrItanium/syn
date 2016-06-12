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
(defglobal iris32
           ?*registers* = (create$ r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10
                                   r11 r12 r13 r14 r15 r16 r17 r18 r19 r20
                                   r21 r22 r23 r24 r25 r26 r27 r28 r29 r30
                                   r31 r32 r33 r34 r35 r36 r37 r38 r39 r40
                                   r41 r42 r43 r44 r45 r46 r47 r48 r49 r50
                                   r51 r52 r53 r54 r55 r56 r57 r58 r59 r60
                                   r61 r62 r63 r64 r65 r66 r67 r68 r69 r70
                                   r71 r72 r73 r74 r75 r76 r77 r78 r79 r80
                                   r81 r82 r83 r84 r85 r86 r87 r88 r89 r90
                                   r91 r92 r93 r94 r95 r96 r97 r98 r99 r100
                                   r101 r102 r103 r104 r105 r106 r107 r108 r109 r110
                                   r111 r112 r113 r114 r115 r116 r117 r118 r119 r120
                                   r121 r122 r123 r124 r125 r126 r127 r128 r129 r130
                                   r131 r132 r133 r134 r135 r136 r137 r138 r139 r140
                                   r141 r142 r143 r144 r145 r146 r147 r148 r149 r150
                                   r151 r152 r153 r154 r155 r156 r157 r158 r159 r160
                                   r161 r162 r163 r164 r165 r166 r167 r168 r169 r170
                                   r171 r172 r173 r174 r175 r176 r177 r178 r179 r180
                                   r181 r182 r183 r184 r185 r186 r187 r188 r189 r190
                                   r191 r192 r193 r194 r195 r196 r197 r198 r199 r200
                                   r201 r202 r203 r204 r205 r206 r207 r208 r209 r210
                                   r211 r212 r213 r214 r215 r216 r217 r218 r219 r220
                                   r221 r222 r223 r224 r225 r226 r227 r228 r229 r230
                                   r231 r232 r233 r234 r235 r236 r237 r238 r239 r240
                                   r241 r242 r243 r244 r245 r246 r247 r248 r249 r250
                                   r251 r252 r253 r254 r255 
                                   ; aliases that the assembler understands
                                   cp sp lr ip))

(defrule iris32::build-opcode-single-immediate
         ?f <- (opcode: one-imm ?title)
         =>
         (retract ?f)
         (assert (opcode ?title immediate)))
(defrule iris32::build-opcode-none
         ?f <- (opcode: none ?title)
         =>
         (retract ?f)
         (assert (opcode ?title)))
(defrule iris32::build-opcode-three-reg
         ?f <- (opcode: three-reg ?title)
         =>
         (retract ?f)
         (assert (opcode ?title register register register)))
(defrule iris32::build-opcode-three-reg-imm
         ?f <- (opcode: three-reg-imm ?title)
         =>
         (retract ?f)
         (assert (opcode ?title register register immediate)))

(defrule iris32::build-opcode-two-reg
         ?f <- (opcode: two-reg ?title)
         =>
         (retract ?f)
         (assert (opcode ?title register register)))

(defrule iris32::retract-opcode-list
         ?f <- (opcode-list ?type)
         =>
         (retract ?f))

(defrule iris32::build-opcodes-from-list
         ?f <- (opcode-list ?type ?first $?rest)
         =>
         (retract ?f)
         (assert (opcode-list ?type $?rest)
                 (opcode: ?type ?first)))

(deffacts iris32::opcodes-defs
          (opcode-list three-reg 
                       add 
                       sub 
                       mul 
                       div 
                       rem 
                       shl 
                       shr
                       jtl
                       jfl
                       ift
                       iff
                       iftl
                       iffl
                       ; compare operations
                       ne
                       lt
                       gt
                       le
                       ge)

          (opcode-list three-reg-imm 
                       addi 
                       subi 
                       muli 
                       divi 
                       remi 
                       shli 
                       shri
                       ;compare
                       eqi
                       nei
                       lti
                       lei
                       gei)

          (opcode-list two-reg 
                       move
                       swap
                       ld
                       st
                       jl
                       jt
                       jf)
          (opcode-decl (op lnot)
                       (title not)
                       (arguments register register))
          (opcode-decl (op cmp_eq)
                       (title eq)
                       (arguments register register register))
          (opcode-decl (op land)
                       (title and)
                       (arguments register register register))
          (opcode-decl (op lor)
                       (title or)
                       (arguments register register register))
          (opcode setl register immediate)
          (opcode setu register immediate)
          ; jump instructions
          (opcode j register)
          (opcode push register)
          (opcode pop register)
          (opcode halve register)
          (opcode double register)
          (opcode incr register)
          (opcode decr register)
          ; assembler directives
          (opcode-list one-imm
                       @org
                       @label
                       @word)
          (opcode-list none
                       @data
                       @code))

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
