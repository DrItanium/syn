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
           ?*current-router* = t)
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
                       @code)

          )

(deffunction iris32::registerp
             (?input)
             (member$ ?input 
                      ?*registers*))

(deffunction iris32::immediatep
             (?input)
             (or (numberp ?input)
                 (and (lexemep ?input)
                      (not (registerp ?input)))))


(defrule iris32::build-generic-op-code
         ?f <- (opcode ?title $?args)
         =>
         (retract ?f)
         (assert (opcode-decl (op ?title)
                              (title ?title)
                              (arguments $?args))))
(defmethod iris32::generate-argument-code
  ((?title LEXEME)
   (?type SYMBOL
          (eq ?current-argument
              REGISTER)))
  (format nil 
          "(%s SYMBOL
               (registerp ?current-argument))%n"
          ?title))

(defmethod iris32::generate-argument-code
  ((?title LEXEME)
   (?type SYMBOL
          (eq ?current-argument
              ALIAS)))
  (format nil 
          "(%s SYMBOL
               (and (instance-existp (symbol-to-instance-name ?current-argument))
               (eq register
                   (class (symbol-to-instance-name ?current-argument)))))%n"
          ?title))


(defmethod iris32::generate-argument-code
  ((?title LEXEME)
   (?type SYMBOL
          (eq ?current-argument
              IMMEDIATE)))
  (format nil 
          "(%s (immediatep ?current-argument))%n"
          ?title))

(defmethod iris32::generate-argument-body
  ((?title LEXEME)
   (?type SYMBOL
          (eq ?current-argument
              IMMEDIATE)))
  (format nil 
          "(str-cat %s)%n" 
          ?title))
(defmethod iris32::generate-argument-body
  ((?title LEXEME)
   (?type SYMBOL
          (eq ?current-argument
              REGISTER)))
  (format nil 
          "%s%n" 
          ?title))
(defmethod iris32::generate-argument-body
  ((?title LEXEME)
   (?type SYMBOL
          (eq ?current-argument
              ALIAS)))
  (format nil 
          "(send %s get-refers-to)%n" 
          ?title))

(deffunction iris32::generate-names
             (?count)
             (bind ?output
                   (create$))
             (loop-for-count ?count do
                             (bind ?output
                                   ?output
                                   (str-cat "?" 
                                            (gensym*))))
             ?output)
(deffunction iris32::generate-string-replacements
             (?count)
             (bind ?output 
                   (create$))
             (loop-for-count ?count do
                             (bind ?output
                                   ?output
                                   %s))
             ?output)


(deffunction iris32::fuse-multifield
             (?list)
             (bind ?str
                   (create$))
             (progn$ (?a ?list)
                     (bind ?str
                           ?str
                           ?a
                           " "))
             (str-cat (expand$ ?str)))
(defmethod iris32::generate-function
  ((?op SYMBOL)
   (?title SYMBOL)
   $?args)
  ; construct the set of inputs
  (bind ?names
        (generate-names (length$ ?args)))
  (bind ?inputs
        (create$))
  (bind ?bodies
        (create$))
  (progn$ (?a ?args)
          (bind ?name 
                (nth$ ?a-index
                      ?names))
          (bind ?bodies
                ?bodies
                (generate-argument-body ?name
                                        ?a))
          (bind ?inputs
                ?inputs
                (generate-argument-code ?name
                                        ?a)))

  (format ?*current-router*
          "(defmethod iris32::%s
             (%s)
             (format nil \"%s %s\" %s))%n"
          ?op
          (fuse-multifield ?inputs)
          ?title
          (fuse-multifield (generate-string-replacements (length$ ?args)))
          (fuse-multifield ?bodies)))






(defrule iris32::build-opcode:three-register
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register register register))
         =>
         (assert (make defgeneric ?op))
         (retract ?f)
         (generate-function ?op 
                            ?title
                            REGISTER
                            REGISTER
                            REGISTER)
         (generate-function ?op
                            ?title
                            REGISTER
                            REGISTER
                            ALIAS)
         (generate-function ?op
                            ?title
                            REGISTER
                            ALIAS
                            REGISTER)
         (generate-function ?op
                            ?title
                            REGISTER
                            ALIAS
                            ALIAS)
         (generate-function ?op
                            ?title
                            ALIAS
                            REGISTER
                            REGISTER)
         (generate-function ?op
                            ?title
                            ALIAS
                            REGISTER
                            ALIAS)
         (generate-function ?op
                            ?title
                            ALIAS
                            ALIAS
                            REGISTER)
         (generate-function ?op
                            ?title
                            ALIAS
                            ALIAS
                            ALIAS))


(defrule iris32::build-opcode:immediate-form
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register register immediate))
         =>
         (assert (make defgeneric ?op))
         (retract ?f)
         (generate-function ?op
                            ?title
                            REGISTER
                            REGISTER
                            IMMEDIATE)
         (generate-function ?op
                            ?title
                            REGISTER
                            ALIAS
                            IMMEDIATE)
         (generate-function ?op
                            ?title
                            ALIAS
                            REGISTER
                            IMMEDIATE)
         (generate-function ?op
                            ?title
                            ALIAS
                            ALIAS
                            IMMEDIATE))
(defrule iris32::build-opcode:two-register
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register register))
         =>
         (assert (make defgeneric ?op))
         (retract ?f)
         (generate-function ?op
                            ?title
                            REGISTER
                            REGISTER)
         (generate-function ?op
                            ?title
                            REGISTER
                            ALIAS)
         (generate-function ?op
                            ?title
                            ALIAS
                            REGISTER)
         (generate-function ?op
                            ?title
                            ALIAS
                            ALIAS))
(defrule iris32::build-opcode:set-form
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register immediate))
         =>
         (retract ?f)
         (assert (make defgeneric ?op))
         (generate-function ?op
                            ?title
                            ALIAS
                            IMMEDIATE)
         (generate-function ?op 
                            ?title 
                            REGISTER 
                            IMMEDIATE))
(defrule iris32::build-opcode:single-reg-form
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments register))
         =>
         (retract ?f)
         (assert (make defgeneric ?op))
         (generate-function ?op
                            ?title
                            REGISTER)
         (generate-function ?op
                            ?title
                            ALIAS))

(defrule iris32::build-opcode:single-immediate-form
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments immediate))
         =>
         (retract ?f)
         (assert (make defgeneric ?op))
         (generate-function ?op
                            ?title
                            IMMEDIATE))

(defrule iris32::build-opcode:no-args
         ?f <- (opcode-decl (op ?op)
                            (title ?title)
                            (arguments))
         =>
         (retract ?f)
         (assert (make defgeneric ?op))
         (format ?*current-router* "(defmethod iris32::%s
                                      ((?dest (immediatep ?current-argument)))
                                      %s)%n"
         ?op
         ?title))
(defrule iris32::build-defgeneric
         (declare (salience -1))
         ?f <- (make defgeneric ?op)
         =>
         (retract ?f)
         (format ?*current-router* "(defgeneric iris32::%s)%n" ?op))
(reset)
(run)
(exit)
