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
(defclass iris32::register
  (is-a USER)
  (slot refers-to
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE)))

(definstances iris32::register-aliases
              (global0 of register (refers-to r0))
              (global1 of register (refers-to r1))
              (global2 of register (refers-to r2))
              (global3 of register (refers-to r3))
              (global4 of register (refers-to r4))
              (global5 of register (refers-to r5))
              (global6 of register (refers-to r6))
              (global7 of register (refers-to r7))
              (global8 of register (refers-to r8))
              (global9 of register (refers-to r9))
              (global10 of register (refers-to r10))
              (global11 of register (refers-to r11))
              (global12 of register (refers-to r12))
              (global13 of register (refers-to r13))
              (global14 of register (refers-to r14))
              (global15 of register (refers-to r15))
              (global16 of register (refers-to r16))
              (global17 of register (refers-to r17))
              (global18 of register (refers-to r18))
              (global19 of register (refers-to r19))
              (global20 of register (refers-to r20))
              (global21 of register (refers-to r21))
              (global22 of register (refers-to r22))
              (global23 of register (refers-to r23))
              (global24 of register (refers-to r24))
              (global25 of register (refers-to r25))
              (global26 of register (refers-to r26))
              (global27 of register (refers-to r27))
              (global28 of register (refers-to r28))
              (global29 of register (refers-to r29))
              (global30 of register (refers-to r30))
              (global31 of register (refers-to r31))
              (in0 of register (refers-to r32))
              (in1 of register (refers-to r33))
              (in2 of register (refers-to r34))
              (in3 of register (refers-to r35))
              (in4 of register (refers-to r36))
              (in5 of register (refers-to r37))
              (in6 of register (refers-to r38))
              (in7 of register (refers-to r39))
              (in8 of register (refers-to r40))
              (in9 of register (refers-to r41))
              (in10 of register (refers-to r42))
              (in11 of register (refers-to r43))
              (in12 of register (refers-to r44))
              (in13 of register (refers-to r45))
              (in14 of register (refers-to r46))
              (in15 of register (refers-to r47))
              (out0 of register (refers-to r48))
              (out1 of register (refers-to r49))
              (out2 of register (refers-to r50))
              (out3 of register (refers-to r51))
              (out4 of register (refers-to r52))
              (out5 of register (refers-to r53))
              (out6 of register (refers-to r54))
              (out7 of register (refers-to r55))
              (out8 of register (refers-to r56))
              (out9 of register (refers-to r57))
              (out10 of register (refers-to r58))
              (out11 of register (refers-to r59))
              (out12 of register (refers-to r60))
              (out13 of register (refers-to r61))
              (out14 of register (refers-to r62))
              (out15 of register (refers-to r63))
              (temp0 of register (refers-to r64))
              (temp1 of register (refers-to r65))
              (temp2 of register (refers-to r66))
              (temp3 of register (refers-to r67))
              (temp4 of register (refers-to r68))
              (temp5 of register (refers-to r69))
              (temp6 of register (refers-to r70))
              (temp7 of register (refers-to r71))
              (temp8 of register (refers-to r72))
              (temp9 of register (refers-to r73))
              (temp10 of register (refers-to r74))
              (temp11 of register (refers-to r75))
              (temp12 of register (refers-to r76))
              (temp13 of register (refers-to r77))
              (temp14 of register (refers-to r78))
              (temp15 of register (refers-to r79))
              (temp16 of register (refers-to r80))
              (temp17 of register (refers-to r81))
              (temp18 of register (refers-to r82))
              (temp19 of register (refers-to r83))
              (temp20 of register (refers-to r84))
              (temp21 of register (refers-to r85))
              (temp22 of register (refers-to r86))
              (temp23 of register (refers-to r87))
              (temp24 of register (refers-to r88))
              (temp25 of register (refers-to r89))
              (temp26 of register (refers-to r90))
              (temp27 of register (refers-to r91))
              (temp28 of register (refers-to r92))
              (temp29 of register (refers-to r93))
              (temp30 of register (refers-to r94))
              (temp31 of register (refers-to r95))
              )
(defglobal iris32
           ?*max-input-count* = 16
           ?*max-return-count* = 16
           ?*registers* = (create$ r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10
                                   r11 r12 r13 r14 r15 r16 r17 r18 r19 r20
                                   r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31 
                                   r32 r33 r34 r35 r36 r37 r38 r39 r40
                                   r41 r42 r43 r44 r45 r46 r47
                                   r48 r49 r50 r51 r52 r53 r54 r55 r56 r57 r58 r59 r60
                                   r61 r62 r63
                                   r64 r65 r66 r67 r68 r69 r70 r71 r72 r73 
                                   r74 r75 r76 r77 r78 r79 r80 r81 r82 r83 r84
                                   r85 r86 r87 r88 r89 r90 r91 r92 r93 r94 r95
                                   r96 r97 r98 r99 r100
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
                                   cr sp lr ip)
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
(deffunction iris32::register-aliasp
             (?input)
             (and (not (registerp ?input))
                  (instance-existp (bind ?tmp 
                                         (symbol-to-instance-name ?input)))
                  (eq register
                      (class ?tmp))))

(defgeneric iris32::system-op)
(defmethod iris32::system-op
  ((?code INTEGER
          (<= 0 ?current-argument 255))
   (?arg0 SYMBOL 
          (registerp ?current-argument))
   (?arg1 SYMBOL 
          (registerp ?current-argument)))
  (format nil "system %d %s %s" ?code ?arg0 ?arg1))

(defmethod iris32::system-op
  ((?code INTEGER
          (<= 0 ?current-argument 255))
   (?arg0 (register-aliasp ?current-argument))
   (?arg1 SYMBOL 
          (registerp ?current-argument)))
  (format nil 
          "system %d %s %s" 
          ?code 
          (send (symbol-to-instance-name ?arg0) get-refers-to)
          ?arg1))

(defmethod iris32::system-op
  ((?code INTEGER
          (<= 0 ?current-argument 255))
   (?arg0 SYMBOL 
          (registerp ?current-argument))
   (?arg1 (register-aliasp ?current-argument)))
  (format nil 
          "system %d %s %s" 
          ?code 
          ?arg0
          (send (symbol-to-instance-name ?arg1) get-refers-to)))

(defmethod iris32::system-op
  ((?code INTEGER
          (<= 0 ?current-argument 255))
   (?arg0 (register-aliasp ?current-argument))
   (?arg1 (register-aliasp ?current-argument)))
  (format nil 
          "system %d %s %s" 
          ?code 
          (send (symbol-to-instance-name ?arg0) get-refers-to)
          (send (symbol-to-instance-name ?arg1) get-refers-to)))


(defgeneric iris32::ge)
(defgeneric iris32::le)
(defgeneric iris32::gt)
(defgeneric iris32::lt)
(defgeneric iris32::ne)
(defgeneric iris32::iffl)
(defgeneric iris32::iftl)
(defgeneric iris32::iff)
(defgeneric iris32::ift)
(defgeneric iris32::jfl)
(defgeneric iris32::jtl)
(defgeneric iris32::shr)
(defgeneric iris32::shl)
(defgeneric iris32::rem)
(defgeneric iris32::div)
(defgeneric iris32::mul)
(defgeneric iris32::sub)
(defgeneric iris32::add)
(defgeneric iris32::gei)
(defgeneric iris32::lei)
(defgeneric iris32::lti)
(defgeneric iris32::nei)
(defgeneric iris32::eqi)
(defgeneric iris32::shri)
(defgeneric iris32::shli)
(defgeneric iris32::remi)
(defgeneric iris32::divi)
(defgeneric iris32::muli)
(defgeneric iris32::subi)
(defgeneric iris32::addi)
(defgeneric iris32::jf)
(defgeneric iris32::jt)
(defgeneric iris32::jl)
(defgeneric iris32::st)
(defgeneric iris32::ld)
(defgeneric iris32::swap)
(defgeneric iris32::move)
(defgeneric iris32::lnot)
(defgeneric iris32::cmp_eq)
(defgeneric iris32::land)
(defgeneric iris32::lor)
(defgeneric iris32::setl)
(defgeneric iris32::setu)
(defgeneric iris32::j)
(defgeneric iris32::push)
(defgeneric iris32::pop)
(defgeneric iris32::halve)
(defgeneric iris32::double)
(defgeneric iris32::incr)
(defgeneric iris32::decr)
(defgeneric iris32::@word)
(defgeneric iris32::@label)
(defgeneric iris32::@org)
(defgeneric iris32::@code)
(defgeneric iris32::@data)




(defmethod iris32::@data
                                      ((?dest (immediatep ?current-argument)))
                                      @data)
(defmethod iris32::@code
                                      ((?dest (immediatep ?current-argument)))
                                      @code)
(defmethod iris32::@org
             ((?gen1 (immediatep ?current-argument))
 )
             (format nil "@org %s " (str-cat ?gen1)
 ))
(defmethod iris32::@label
             ((?gen2 (immediatep ?current-argument))
 )
             (format nil "@label %s " (str-cat ?gen2)
 ))
(defmethod iris32::@word
             ((?gen3 (immediatep ?current-argument))
 )
             (format nil "@word %s " (str-cat ?gen3)
 ))
(defmethod iris32::decr
             ((?gen4 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "decr %s " ?gen4
 ))
(defmethod iris32::decr
             ((?gen5 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "decr %s " (send (symbol-to-instance-name ?gen5) get-refers-to)
 ))
(defmethod iris32::incr
             ((?gen6 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "incr %s " ?gen6
 ))
(defmethod iris32::incr
             ((?gen7 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "incr %s " (send (symbol-to-instance-name ?gen7) get-refers-to)
 ))
(defmethod iris32::double
             ((?gen8 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "double %s " ?gen8
 ))
(defmethod iris32::double
             ((?gen9 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "double %s " (send (symbol-to-instance-name ?gen9) get-refers-to)
 ))
(defmethod iris32::halve
             ((?gen10 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "halve %s " ?gen10
 ))
(defmethod iris32::halve
             ((?gen11 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "halve %s " (send (symbol-to-instance-name ?gen11) get-refers-to)
 ))
(defmethod iris32::pop
             ((?gen12 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "pop %s " ?gen12
 ))
(defmethod iris32::pop
             ((?gen13 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "pop %s " (send (symbol-to-instance-name ?gen13) get-refers-to)
 ))
(defmethod iris32::push
             ((?gen14 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "push %s " ?gen14
 ))
(defmethod iris32::push
             ((?gen15 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "push %s " (send (symbol-to-instance-name ?gen15) get-refers-to)
 ))
(defmethod iris32::jl
             ((?gen16 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jl %s " ?gen16
 ))
(defmethod iris32::jl
             ((?gen17 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jl %s " (send (symbol-to-instance-name ?gen17) get-refers-to)
 ))
(defmethod iris32::j
             ((?gen18 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "j %s " ?gen18
 ))
(defmethod iris32::j
             ((?gen19 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "j %s " (send (symbol-to-instance-name ?gen19) get-refers-to)
 ))
(defmethod iris32::setu
             ((?gen20 SYMBOL
               (register-aliasp ?current-argument))
 (?gen21 (immediatep ?current-argument))
 )
             (format nil "setu %s %s " (send (symbol-to-instance-name ?gen20) get-refers-to)
 (str-cat ?gen21)
 ))
(defmethod iris32::setu
             ((?gen22 SYMBOL
               (registerp ?current-argument))
 (?gen23 (immediatep ?current-argument))
 )
             (format nil "setu %s %s " ?gen22
 (str-cat ?gen23)
 ))
(defmethod iris32::setl
             ((?gen24 SYMBOL
               (register-aliasp ?current-argument))
 (?gen25 (immediatep ?current-argument))
 )
             (format nil "setl %s %s " (send (symbol-to-instance-name ?gen24) get-refers-to)
 (str-cat ?gen25)
 ))
(defmethod iris32::setl
             ((?gen26 SYMBOL
               (registerp ?current-argument))
 (?gen27 (immediatep ?current-argument))
 )
             (format nil "setl %s %s " ?gen26
 (str-cat ?gen27)
 ))
(defmethod iris32::lor
             ((?gen28 SYMBOL
               (registerp ?current-argument))
 (?gen29 SYMBOL
               (registerp ?current-argument))
 (?gen30 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "or %s %s %s " ?gen28
 ?gen29
 ?gen30
 ))
(defmethod iris32::lor
             ((?gen31 SYMBOL
               (registerp ?current-argument))
 (?gen32 SYMBOL
               (registerp ?current-argument))
 (?gen33 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "or %s %s %s " ?gen31
 ?gen32
 (send (symbol-to-instance-name ?gen33) get-refers-to)
 ))
(defmethod iris32::lor
             ((?gen34 SYMBOL
               (registerp ?current-argument))
 (?gen35 SYMBOL
               (register-aliasp ?current-argument))
 (?gen36 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "or %s %s %s " ?gen34
 (send (symbol-to-instance-name ?gen35) get-refers-to)
 ?gen36
 ))
(defmethod iris32::lor
             ((?gen37 SYMBOL
               (registerp ?current-argument))
 (?gen38 SYMBOL
               (register-aliasp ?current-argument))
 (?gen39 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "or %s %s %s " ?gen37
 (send (symbol-to-instance-name ?gen38) get-refers-to)
 (send (symbol-to-instance-name ?gen39) get-refers-to)
 ))
(defmethod iris32::lor
             ((?gen40 SYMBOL
               (register-aliasp ?current-argument))
 (?gen41 SYMBOL
               (registerp ?current-argument))
 (?gen42 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "or %s %s %s " (send (symbol-to-instance-name ?gen40) get-refers-to)
 ?gen41
 ?gen42
 ))
(defmethod iris32::lor
             ((?gen43 SYMBOL
               (register-aliasp ?current-argument))
 (?gen44 SYMBOL
               (registerp ?current-argument))
 (?gen45 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "or %s %s %s " (send (symbol-to-instance-name ?gen43) get-refers-to)
 ?gen44
 (send (symbol-to-instance-name ?gen45) get-refers-to)
 ))
(defmethod iris32::lor
             ((?gen46 SYMBOL
               (register-aliasp ?current-argument))
 (?gen47 SYMBOL
               (register-aliasp ?current-argument))
 (?gen48 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "or %s %s %s " (send (symbol-to-instance-name ?gen46) get-refers-to)
 (send (symbol-to-instance-name ?gen47) get-refers-to)
 ?gen48
 ))
(defmethod iris32::lor
             ((?gen49 SYMBOL
               (register-aliasp ?current-argument))
 (?gen50 SYMBOL
               (register-aliasp ?current-argument))
 (?gen51 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "or %s %s %s " (send (symbol-to-instance-name ?gen49) get-refers-to)
 (send (symbol-to-instance-name ?gen50) get-refers-to)
 (send (symbol-to-instance-name ?gen51) get-refers-to)
 ))
(defmethod iris32::land
             ((?gen52 SYMBOL
               (registerp ?current-argument))
 (?gen53 SYMBOL
               (registerp ?current-argument))
 (?gen54 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "and %s %s %s " ?gen52
 ?gen53
 ?gen54
 ))
(defmethod iris32::land
             ((?gen55 SYMBOL
               (registerp ?current-argument))
 (?gen56 SYMBOL
               (registerp ?current-argument))
 (?gen57 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "and %s %s %s " ?gen55
 ?gen56
 (send (symbol-to-instance-name ?gen57) get-refers-to)
 ))
(defmethod iris32::land
             ((?gen58 SYMBOL
               (registerp ?current-argument))
 (?gen59 SYMBOL
               (register-aliasp ?current-argument))
 (?gen60 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "and %s %s %s " ?gen58
 (send (symbol-to-instance-name ?gen59) get-refers-to)
 ?gen60
 ))
(defmethod iris32::land
             ((?gen61 SYMBOL
               (registerp ?current-argument))
 (?gen62 SYMBOL
               (register-aliasp ?current-argument))
 (?gen63 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "and %s %s %s " ?gen61
 (send (symbol-to-instance-name ?gen62) get-refers-to)
 (send (symbol-to-instance-name ?gen63) get-refers-to)
 ))
(defmethod iris32::land
             ((?gen64 SYMBOL
               (register-aliasp ?current-argument))
 (?gen65 SYMBOL
               (registerp ?current-argument))
 (?gen66 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "and %s %s %s " (send (symbol-to-instance-name ?gen64) get-refers-to)
 ?gen65
 ?gen66
 ))
(defmethod iris32::land
             ((?gen67 SYMBOL
               (register-aliasp ?current-argument))
 (?gen68 SYMBOL
               (registerp ?current-argument))
 (?gen69 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "and %s %s %s " (send (symbol-to-instance-name ?gen67) get-refers-to)
 ?gen68
 (send (symbol-to-instance-name ?gen69) get-refers-to)
 ))
(defmethod iris32::land
             ((?gen70 SYMBOL
               (register-aliasp ?current-argument))
 (?gen71 SYMBOL
               (register-aliasp ?current-argument))
 (?gen72 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "and %s %s %s " (send (symbol-to-instance-name ?gen70) get-refers-to)
 (send (symbol-to-instance-name ?gen71) get-refers-to)
 ?gen72
 ))
(defmethod iris32::land
             ((?gen73 SYMBOL
               (register-aliasp ?current-argument))
 (?gen74 SYMBOL
               (register-aliasp ?current-argument))
 (?gen75 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "and %s %s %s " (send (symbol-to-instance-name ?gen73) get-refers-to)
 (send (symbol-to-instance-name ?gen74) get-refers-to)
 (send (symbol-to-instance-name ?gen75) get-refers-to)
 ))
(defmethod iris32::cmp_eq
             ((?gen76 SYMBOL
               (registerp ?current-argument))
 (?gen77 SYMBOL
               (registerp ?current-argument))
 (?gen78 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "eq %s %s %s " ?gen76
 ?gen77
 ?gen78
 ))
(defmethod iris32::cmp_eq
             ((?gen79 SYMBOL
               (registerp ?current-argument))
 (?gen80 SYMBOL
               (registerp ?current-argument))
 (?gen81 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "eq %s %s %s " ?gen79
 ?gen80
 (send (symbol-to-instance-name ?gen81) get-refers-to)
 ))
(defmethod iris32::cmp_eq
             ((?gen82 SYMBOL
               (registerp ?current-argument))
 (?gen83 SYMBOL
               (register-aliasp ?current-argument))
 (?gen84 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "eq %s %s %s " ?gen82
 (send (symbol-to-instance-name ?gen83) get-refers-to)
 ?gen84
 ))
(defmethod iris32::cmp_eq
             ((?gen85 SYMBOL
               (registerp ?current-argument))
 (?gen86 SYMBOL
               (register-aliasp ?current-argument))
 (?gen87 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "eq %s %s %s " ?gen85
 (send (symbol-to-instance-name ?gen86) get-refers-to)
 (send (symbol-to-instance-name ?gen87) get-refers-to)
 ))
(defmethod iris32::cmp_eq
             ((?gen88 SYMBOL
               (register-aliasp ?current-argument))
 (?gen89 SYMBOL
               (registerp ?current-argument))
 (?gen90 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "eq %s %s %s " (send (symbol-to-instance-name ?gen88) get-refers-to)
 ?gen89
 ?gen90
 ))
(defmethod iris32::cmp_eq
             ((?gen91 SYMBOL
               (register-aliasp ?current-argument))
 (?gen92 SYMBOL
               (registerp ?current-argument))
 (?gen93 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "eq %s %s %s " (send (symbol-to-instance-name ?gen91) get-refers-to)
 ?gen92
 (send (symbol-to-instance-name ?gen93) get-refers-to)
 ))
(defmethod iris32::cmp_eq
             ((?gen94 SYMBOL
               (register-aliasp ?current-argument))
 (?gen95 SYMBOL
               (register-aliasp ?current-argument))
 (?gen96 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "eq %s %s %s " (send (symbol-to-instance-name ?gen94) get-refers-to)
 (send (symbol-to-instance-name ?gen95) get-refers-to)
 ?gen96
 ))
(defmethod iris32::cmp_eq
             ((?gen97 SYMBOL
               (register-aliasp ?current-argument))
 (?gen98 SYMBOL
               (register-aliasp ?current-argument))
 (?gen99 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "eq %s %s %s " (send (symbol-to-instance-name ?gen97) get-refers-to)
 (send (symbol-to-instance-name ?gen98) get-refers-to)
 (send (symbol-to-instance-name ?gen99) get-refers-to)
 ))
(defmethod iris32::lnot
             ((?gen100 SYMBOL
               (registerp ?current-argument))
 (?gen101 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "not %s %s " ?gen100
 ?gen101
 ))
(defmethod iris32::lnot
             ((?gen102 SYMBOL
               (registerp ?current-argument))
 (?gen103 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "not %s %s " ?gen102
 (send (symbol-to-instance-name ?gen103) get-refers-to)
 ))
(defmethod iris32::lnot
             ((?gen104 SYMBOL
               (register-aliasp ?current-argument))
 (?gen105 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "not %s %s " (send (symbol-to-instance-name ?gen104) get-refers-to)
 ?gen105
 ))
(defmethod iris32::lnot
             ((?gen106 SYMBOL
               (register-aliasp ?current-argument))
 (?gen107 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "not %s %s " (send (symbol-to-instance-name ?gen106) get-refers-to)
 (send (symbol-to-instance-name ?gen107) get-refers-to)
 ))
(defmethod iris32::move
             ((?gen108 SYMBOL
               (registerp ?current-argument))
 (?gen109 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "move %s %s " ?gen108
 ?gen109
 ))
(defmethod iris32::move
             ((?gen110 SYMBOL
               (registerp ?current-argument))
 (?gen111 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "move %s %s " ?gen110
 (send (symbol-to-instance-name ?gen111) get-refers-to)
 ))
(defmethod iris32::move
             ((?gen112 SYMBOL
               (register-aliasp ?current-argument))
 (?gen113 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "move %s %s " (send (symbol-to-instance-name ?gen112) get-refers-to)
 ?gen113
 ))
(defmethod iris32::move
             ((?gen114 SYMBOL
               (register-aliasp ?current-argument))
 (?gen115 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "move %s %s " (send (symbol-to-instance-name ?gen114) get-refers-to)
 (send (symbol-to-instance-name ?gen115) get-refers-to)
 ))
(defmethod iris32::swap
             ((?gen116 SYMBOL
               (registerp ?current-argument))
 (?gen117 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "swap %s %s " ?gen116
 ?gen117
 ))
(defmethod iris32::swap
             ((?gen118 SYMBOL
               (registerp ?current-argument))
 (?gen119 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "swap %s %s " ?gen118
 (send (symbol-to-instance-name ?gen119) get-refers-to)
 ))
(defmethod iris32::swap
             ((?gen120 SYMBOL
               (register-aliasp ?current-argument))
 (?gen121 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "swap %s %s " (send (symbol-to-instance-name ?gen120) get-refers-to)
 ?gen121
 ))
(defmethod iris32::swap
             ((?gen122 SYMBOL
               (register-aliasp ?current-argument))
 (?gen123 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "swap %s %s " (send (symbol-to-instance-name ?gen122) get-refers-to)
 (send (symbol-to-instance-name ?gen123) get-refers-to)
 ))
(defmethod iris32::ld
             ((?gen124 SYMBOL
               (registerp ?current-argument))
 (?gen125 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ld %s %s " ?gen124
 ?gen125
 ))
(defmethod iris32::ld
             ((?gen126 SYMBOL
               (registerp ?current-argument))
 (?gen127 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ld %s %s " ?gen126
 (send (symbol-to-instance-name ?gen127) get-refers-to)
 ))
(defmethod iris32::ld
             ((?gen128 SYMBOL
               (register-aliasp ?current-argument))
 (?gen129 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ld %s %s " (send (symbol-to-instance-name ?gen128) get-refers-to)
 ?gen129
 ))
(defmethod iris32::ld
             ((?gen130 SYMBOL
               (register-aliasp ?current-argument))
 (?gen131 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ld %s %s " (send (symbol-to-instance-name ?gen130) get-refers-to)
 (send (symbol-to-instance-name ?gen131) get-refers-to)
 ))
(defmethod iris32::st
             ((?gen132 SYMBOL
               (registerp ?current-argument))
 (?gen133 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "st %s %s " ?gen132
 ?gen133
 ))
(defmethod iris32::st
             ((?gen134 SYMBOL
               (registerp ?current-argument))
 (?gen135 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "st %s %s " ?gen134
 (send (symbol-to-instance-name ?gen135) get-refers-to)
 ))
(defmethod iris32::st
             ((?gen136 SYMBOL
               (register-aliasp ?current-argument))
 (?gen137 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "st %s %s " (send (symbol-to-instance-name ?gen136) get-refers-to)
 ?gen137
 ))
(defmethod iris32::st
             ((?gen138 SYMBOL
               (register-aliasp ?current-argument))
 (?gen139 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "st %s %s " (send (symbol-to-instance-name ?gen138) get-refers-to)
 (send (symbol-to-instance-name ?gen139) get-refers-to)
 ))
(defmethod iris32::jt
             ((?gen140 SYMBOL
               (registerp ?current-argument))
 (?gen141 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jt %s %s " ?gen140
 ?gen141
 ))
(defmethod iris32::jt
             ((?gen142 SYMBOL
               (registerp ?current-argument))
 (?gen143 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jt %s %s " ?gen142
 (send (symbol-to-instance-name ?gen143) get-refers-to)
 ))
(defmethod iris32::jt
             ((?gen144 SYMBOL
               (register-aliasp ?current-argument))
 (?gen145 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jt %s %s " (send (symbol-to-instance-name ?gen144) get-refers-to)
 ?gen145
 ))
(defmethod iris32::jt
             ((?gen146 SYMBOL
               (register-aliasp ?current-argument))
 (?gen147 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jt %s %s " (send (symbol-to-instance-name ?gen146) get-refers-to)
 (send (symbol-to-instance-name ?gen147) get-refers-to)
 ))
(defmethod iris32::jf
             ((?gen148 SYMBOL
               (registerp ?current-argument))
 (?gen149 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jf %s %s " ?gen148
 ?gen149
 ))
(defmethod iris32::jf
             ((?gen150 SYMBOL
               (registerp ?current-argument))
 (?gen151 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jf %s %s " ?gen150
 (send (symbol-to-instance-name ?gen151) get-refers-to)
 ))
(defmethod iris32::jf
             ((?gen152 SYMBOL
               (register-aliasp ?current-argument))
 (?gen153 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jf %s %s " (send (symbol-to-instance-name ?gen152) get-refers-to)
 ?gen153
 ))
(defmethod iris32::jf
             ((?gen154 SYMBOL
               (register-aliasp ?current-argument))
 (?gen155 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jf %s %s " (send (symbol-to-instance-name ?gen154) get-refers-to)
 (send (symbol-to-instance-name ?gen155) get-refers-to)
 ))
(defmethod iris32::addi
             ((?gen156 SYMBOL
               (registerp ?current-argument))
 (?gen157 SYMBOL
               (registerp ?current-argument))
 (?gen158 (immediatep ?current-argument))
 )
             (format nil "addi %s %s %s " ?gen156
 ?gen157
 (str-cat ?gen158)
 ))
(defmethod iris32::addi
             ((?gen159 SYMBOL
               (registerp ?current-argument))
 (?gen160 SYMBOL
               (register-aliasp ?current-argument))
 (?gen161 (immediatep ?current-argument))
 )
             (format nil "addi %s %s %s " ?gen159
 (send (symbol-to-instance-name ?gen160) get-refers-to)
 (str-cat ?gen161)
 ))
(defmethod iris32::addi
             ((?gen162 SYMBOL
               (register-aliasp ?current-argument))
 (?gen163 SYMBOL
               (registerp ?current-argument))
 (?gen164 (immediatep ?current-argument))
 )
             (format nil "addi %s %s %s " (send (symbol-to-instance-name ?gen162) get-refers-to)
 ?gen163
 (str-cat ?gen164)
 ))
(defmethod iris32::addi
             ((?gen165 SYMBOL
               (register-aliasp ?current-argument))
 (?gen166 SYMBOL
               (register-aliasp ?current-argument))
 (?gen167 (immediatep ?current-argument))
 )
             (format nil "addi %s %s %s " (send (symbol-to-instance-name ?gen165) get-refers-to)
 (send (symbol-to-instance-name ?gen166) get-refers-to)
 (str-cat ?gen167)
 ))
(defmethod iris32::subi
             ((?gen168 SYMBOL
               (registerp ?current-argument))
 (?gen169 SYMBOL
               (registerp ?current-argument))
 (?gen170 (immediatep ?current-argument))
 )
             (format nil "subi %s %s %s " ?gen168
 ?gen169
 (str-cat ?gen170)
 ))
(defmethod iris32::subi
             ((?gen171 SYMBOL
               (registerp ?current-argument))
 (?gen172 SYMBOL
               (register-aliasp ?current-argument))
 (?gen173 (immediatep ?current-argument))
 )
             (format nil "subi %s %s %s " ?gen171
 (send (symbol-to-instance-name ?gen172) get-refers-to)
 (str-cat ?gen173)
 ))
(defmethod iris32::subi
             ((?gen174 SYMBOL
               (register-aliasp ?current-argument))
 (?gen175 SYMBOL
               (registerp ?current-argument))
 (?gen176 (immediatep ?current-argument))
 )
             (format nil "subi %s %s %s " (send (symbol-to-instance-name ?gen174) get-refers-to)
 ?gen175
 (str-cat ?gen176)
 ))
(defmethod iris32::subi
             ((?gen177 SYMBOL
               (register-aliasp ?current-argument))
 (?gen178 SYMBOL
               (register-aliasp ?current-argument))
 (?gen179 (immediatep ?current-argument))
 )
             (format nil "subi %s %s %s " (send (symbol-to-instance-name ?gen177) get-refers-to)
 (send (symbol-to-instance-name ?gen178) get-refers-to)
 (str-cat ?gen179)
 ))
(defmethod iris32::muli
             ((?gen180 SYMBOL
               (registerp ?current-argument))
 (?gen181 SYMBOL
               (registerp ?current-argument))
 (?gen182 (immediatep ?current-argument))
 )
             (format nil "muli %s %s %s " ?gen180
 ?gen181
 (str-cat ?gen182)
 ))
(defmethod iris32::muli
             ((?gen183 SYMBOL
               (registerp ?current-argument))
 (?gen184 SYMBOL
               (register-aliasp ?current-argument))
 (?gen185 (immediatep ?current-argument))
 )
             (format nil "muli %s %s %s " ?gen183
 (send (symbol-to-instance-name ?gen184) get-refers-to)
 (str-cat ?gen185)
 ))
(defmethod iris32::muli
             ((?gen186 SYMBOL
               (register-aliasp ?current-argument))
 (?gen187 SYMBOL
               (registerp ?current-argument))
 (?gen188 (immediatep ?current-argument))
 )
             (format nil "muli %s %s %s " (send (symbol-to-instance-name ?gen186) get-refers-to)
 ?gen187
 (str-cat ?gen188)
 ))
(defmethod iris32::muli
             ((?gen189 SYMBOL
               (register-aliasp ?current-argument))
 (?gen190 SYMBOL
               (register-aliasp ?current-argument))
 (?gen191 (immediatep ?current-argument))
 )
             (format nil "muli %s %s %s " (send (symbol-to-instance-name ?gen189) get-refers-to)
 (send (symbol-to-instance-name ?gen190) get-refers-to)
 (str-cat ?gen191)
 ))
(defmethod iris32::divi
             ((?gen192 SYMBOL
               (registerp ?current-argument))
 (?gen193 SYMBOL
               (registerp ?current-argument))
 (?gen194 (immediatep ?current-argument))
 )
             (format nil "divi %s %s %s " ?gen192
 ?gen193
 (str-cat ?gen194)
 ))
(defmethod iris32::divi
             ((?gen195 SYMBOL
               (registerp ?current-argument))
 (?gen196 SYMBOL
               (register-aliasp ?current-argument))
 (?gen197 (immediatep ?current-argument))
 )
             (format nil "divi %s %s %s " ?gen195
 (send (symbol-to-instance-name ?gen196) get-refers-to)
 (str-cat ?gen197)
 ))
(defmethod iris32::divi
             ((?gen198 SYMBOL
               (register-aliasp ?current-argument))
 (?gen199 SYMBOL
               (registerp ?current-argument))
 (?gen200 (immediatep ?current-argument))
 )
             (format nil "divi %s %s %s " (send (symbol-to-instance-name ?gen198) get-refers-to)
 ?gen199
 (str-cat ?gen200)
 ))
(defmethod iris32::divi
             ((?gen201 SYMBOL
               (register-aliasp ?current-argument))
 (?gen202 SYMBOL
               (register-aliasp ?current-argument))
 (?gen203 (immediatep ?current-argument))
 )
             (format nil "divi %s %s %s " (send (symbol-to-instance-name ?gen201) get-refers-to)
 (send (symbol-to-instance-name ?gen202) get-refers-to)
 (str-cat ?gen203)
 ))
(defmethod iris32::remi
             ((?gen204 SYMBOL
               (registerp ?current-argument))
 (?gen205 SYMBOL
               (registerp ?current-argument))
 (?gen206 (immediatep ?current-argument))
 )
             (format nil "remi %s %s %s " ?gen204
 ?gen205
 (str-cat ?gen206)
 ))
(defmethod iris32::remi
             ((?gen207 SYMBOL
               (registerp ?current-argument))
 (?gen208 SYMBOL
               (register-aliasp ?current-argument))
 (?gen209 (immediatep ?current-argument))
 )
             (format nil "remi %s %s %s " ?gen207
 (send (symbol-to-instance-name ?gen208) get-refers-to)
 (str-cat ?gen209)
 ))
(defmethod iris32::remi
             ((?gen210 SYMBOL
               (register-aliasp ?current-argument))
 (?gen211 SYMBOL
               (registerp ?current-argument))
 (?gen212 (immediatep ?current-argument))
 )
             (format nil "remi %s %s %s " (send (symbol-to-instance-name ?gen210) get-refers-to)
 ?gen211
 (str-cat ?gen212)
 ))
(defmethod iris32::remi
             ((?gen213 SYMBOL
               (register-aliasp ?current-argument))
 (?gen214 SYMBOL
               (register-aliasp ?current-argument))
 (?gen215 (immediatep ?current-argument))
 )
             (format nil "remi %s %s %s " (send (symbol-to-instance-name ?gen213) get-refers-to)
 (send (symbol-to-instance-name ?gen214) get-refers-to)
 (str-cat ?gen215)
 ))
(defmethod iris32::shli
             ((?gen216 SYMBOL
               (registerp ?current-argument))
 (?gen217 SYMBOL
               (registerp ?current-argument))
 (?gen218 (immediatep ?current-argument))
 )
             (format nil "shli %s %s %s " ?gen216
 ?gen217
 (str-cat ?gen218)
 ))
(defmethod iris32::shli
             ((?gen219 SYMBOL
               (registerp ?current-argument))
 (?gen220 SYMBOL
               (register-aliasp ?current-argument))
 (?gen221 (immediatep ?current-argument))
 )
             (format nil "shli %s %s %s " ?gen219
 (send (symbol-to-instance-name ?gen220) get-refers-to)
 (str-cat ?gen221)
 ))
(defmethod iris32::shli
             ((?gen222 SYMBOL
               (register-aliasp ?current-argument))
 (?gen223 SYMBOL
               (registerp ?current-argument))
 (?gen224 (immediatep ?current-argument))
 )
             (format nil "shli %s %s %s " (send (symbol-to-instance-name ?gen222) get-refers-to)
 ?gen223
 (str-cat ?gen224)
 ))
(defmethod iris32::shli
             ((?gen225 SYMBOL
               (register-aliasp ?current-argument))
 (?gen226 SYMBOL
               (register-aliasp ?current-argument))
 (?gen227 (immediatep ?current-argument))
 )
             (format nil "shli %s %s %s " (send (symbol-to-instance-name ?gen225) get-refers-to)
 (send (symbol-to-instance-name ?gen226) get-refers-to)
 (str-cat ?gen227)
 ))
(defmethod iris32::shri
             ((?gen228 SYMBOL
               (registerp ?current-argument))
 (?gen229 SYMBOL
               (registerp ?current-argument))
 (?gen230 (immediatep ?current-argument))
 )
             (format nil "shri %s %s %s " ?gen228
 ?gen229
 (str-cat ?gen230)
 ))
(defmethod iris32::shri
             ((?gen231 SYMBOL
               (registerp ?current-argument))
 (?gen232 SYMBOL
               (register-aliasp ?current-argument))
 (?gen233 (immediatep ?current-argument))
 )
             (format nil "shri %s %s %s " ?gen231
 (send (symbol-to-instance-name ?gen232) get-refers-to)
 (str-cat ?gen233)
 ))
(defmethod iris32::shri
             ((?gen234 SYMBOL
               (register-aliasp ?current-argument))
 (?gen235 SYMBOL
               (registerp ?current-argument))
 (?gen236 (immediatep ?current-argument))
 )
             (format nil "shri %s %s %s " (send (symbol-to-instance-name ?gen234) get-refers-to)
 ?gen235
 (str-cat ?gen236)
 ))
(defmethod iris32::shri
             ((?gen237 SYMBOL
               (register-aliasp ?current-argument))
 (?gen238 SYMBOL
               (register-aliasp ?current-argument))
 (?gen239 (immediatep ?current-argument))
 )
             (format nil "shri %s %s %s " (send (symbol-to-instance-name ?gen237) get-refers-to)
 (send (symbol-to-instance-name ?gen238) get-refers-to)
 (str-cat ?gen239)
 ))
(defmethod iris32::eqi
             ((?gen240 SYMBOL
               (registerp ?current-argument))
 (?gen241 SYMBOL
               (registerp ?current-argument))
 (?gen242 (immediatep ?current-argument))
 )
             (format nil "eqi %s %s %s " ?gen240
 ?gen241
 (str-cat ?gen242)
 ))
(defmethod iris32::eqi
             ((?gen243 SYMBOL
               (registerp ?current-argument))
 (?gen244 SYMBOL
               (register-aliasp ?current-argument))
 (?gen245 (immediatep ?current-argument))
 )
             (format nil "eqi %s %s %s " ?gen243
 (send (symbol-to-instance-name ?gen244) get-refers-to)
 (str-cat ?gen245)
 ))
(defmethod iris32::eqi
             ((?gen246 SYMBOL
               (register-aliasp ?current-argument))
 (?gen247 SYMBOL
               (registerp ?current-argument))
 (?gen248 (immediatep ?current-argument))
 )
             (format nil "eqi %s %s %s " (send (symbol-to-instance-name ?gen246) get-refers-to)
 ?gen247
 (str-cat ?gen248)
 ))
(defmethod iris32::eqi
             ((?gen249 SYMBOL
               (register-aliasp ?current-argument))
 (?gen250 SYMBOL
               (register-aliasp ?current-argument))
 (?gen251 (immediatep ?current-argument))
 )
             (format nil "eqi %s %s %s " (send (symbol-to-instance-name ?gen249) get-refers-to)
 (send (symbol-to-instance-name ?gen250) get-refers-to)
 (str-cat ?gen251)
 ))
(defmethod iris32::nei
             ((?gen252 SYMBOL
               (registerp ?current-argument))
 (?gen253 SYMBOL
               (registerp ?current-argument))
 (?gen254 (immediatep ?current-argument))
 )
             (format nil "nei %s %s %s " ?gen252
 ?gen253
 (str-cat ?gen254)
 ))
(defmethod iris32::nei
             ((?gen255 SYMBOL
               (registerp ?current-argument))
 (?gen256 SYMBOL
               (register-aliasp ?current-argument))
 (?gen257 (immediatep ?current-argument))
 )
             (format nil "nei %s %s %s " ?gen255
 (send (symbol-to-instance-name ?gen256) get-refers-to)
 (str-cat ?gen257)
 ))
(defmethod iris32::nei
             ((?gen258 SYMBOL
               (register-aliasp ?current-argument))
 (?gen259 SYMBOL
               (registerp ?current-argument))
 (?gen260 (immediatep ?current-argument))
 )
             (format nil "nei %s %s %s " (send (symbol-to-instance-name ?gen258) get-refers-to)
 ?gen259
 (str-cat ?gen260)
 ))
(defmethod iris32::nei
             ((?gen261 SYMBOL
               (register-aliasp ?current-argument))
 (?gen262 SYMBOL
               (register-aliasp ?current-argument))
 (?gen263 (immediatep ?current-argument))
 )
             (format nil "nei %s %s %s " (send (symbol-to-instance-name ?gen261) get-refers-to)
 (send (symbol-to-instance-name ?gen262) get-refers-to)
 (str-cat ?gen263)
 ))
(defmethod iris32::lti
             ((?gen264 SYMBOL
               (registerp ?current-argument))
 (?gen265 SYMBOL
               (registerp ?current-argument))
 (?gen266 (immediatep ?current-argument))
 )
             (format nil "lti %s %s %s " ?gen264
 ?gen265
 (str-cat ?gen266)
 ))
(defmethod iris32::lti
             ((?gen267 SYMBOL
               (registerp ?current-argument))
 (?gen268 SYMBOL
               (register-aliasp ?current-argument))
 (?gen269 (immediatep ?current-argument))
 )
             (format nil "lti %s %s %s " ?gen267
 (send (symbol-to-instance-name ?gen268) get-refers-to)
 (str-cat ?gen269)
 ))
(defmethod iris32::lti
             ((?gen270 SYMBOL
               (register-aliasp ?current-argument))
 (?gen271 SYMBOL
               (registerp ?current-argument))
 (?gen272 (immediatep ?current-argument))
 )
             (format nil "lti %s %s %s " (send (symbol-to-instance-name ?gen270) get-refers-to)
 ?gen271
 (str-cat ?gen272)
 ))
(defmethod iris32::lti
             ((?gen273 SYMBOL
               (register-aliasp ?current-argument))
 (?gen274 SYMBOL
               (register-aliasp ?current-argument))
 (?gen275 (immediatep ?current-argument))
 )
             (format nil "lti %s %s %s " (send (symbol-to-instance-name ?gen273) get-refers-to)
 (send (symbol-to-instance-name ?gen274) get-refers-to)
 (str-cat ?gen275)
 ))
(defmethod iris32::lei
             ((?gen276 SYMBOL
               (registerp ?current-argument))
 (?gen277 SYMBOL
               (registerp ?current-argument))
 (?gen278 (immediatep ?current-argument))
 )
             (format nil "lei %s %s %s " ?gen276
 ?gen277
 (str-cat ?gen278)
 ))
(defmethod iris32::lei
             ((?gen279 SYMBOL
               (registerp ?current-argument))
 (?gen280 SYMBOL
               (register-aliasp ?current-argument))
 (?gen281 (immediatep ?current-argument))
 )
             (format nil "lei %s %s %s " ?gen279
 (send (symbol-to-instance-name ?gen280) get-refers-to)
 (str-cat ?gen281)
 ))
(defmethod iris32::lei
             ((?gen282 SYMBOL
               (register-aliasp ?current-argument))
 (?gen283 SYMBOL
               (registerp ?current-argument))
 (?gen284 (immediatep ?current-argument))
 )
             (format nil "lei %s %s %s " (send (symbol-to-instance-name ?gen282) get-refers-to)
 ?gen283
 (str-cat ?gen284)
 ))
(defmethod iris32::lei
             ((?gen285 SYMBOL
               (register-aliasp ?current-argument))
 (?gen286 SYMBOL
               (register-aliasp ?current-argument))
 (?gen287 (immediatep ?current-argument))
 )
             (format nil "lei %s %s %s " (send (symbol-to-instance-name ?gen285) get-refers-to)
 (send (symbol-to-instance-name ?gen286) get-refers-to)
 (str-cat ?gen287)
 ))
(defmethod iris32::gei
             ((?gen288 SYMBOL
               (registerp ?current-argument))
 (?gen289 SYMBOL
               (registerp ?current-argument))
 (?gen290 (immediatep ?current-argument))
 )
             (format nil "gei %s %s %s " ?gen288
 ?gen289
 (str-cat ?gen290)
 ))
(defmethod iris32::gei
             ((?gen291 SYMBOL
               (registerp ?current-argument))
 (?gen292 SYMBOL
               (register-aliasp ?current-argument))
 (?gen293 (immediatep ?current-argument))
 )
             (format nil "gei %s %s %s " ?gen291
 (send (symbol-to-instance-name ?gen292) get-refers-to)
 (str-cat ?gen293)
 ))
(defmethod iris32::gei
             ((?gen294 SYMBOL
               (register-aliasp ?current-argument))
 (?gen295 SYMBOL
               (registerp ?current-argument))
 (?gen296 (immediatep ?current-argument))
 )
             (format nil "gei %s %s %s " (send (symbol-to-instance-name ?gen294) get-refers-to)
 ?gen295
 (str-cat ?gen296)
 ))
(defmethod iris32::gei
             ((?gen297 SYMBOL
               (register-aliasp ?current-argument))
 (?gen298 SYMBOL
               (register-aliasp ?current-argument))
 (?gen299 (immediatep ?current-argument))
 )
             (format nil "gei %s %s %s " (send (symbol-to-instance-name ?gen297) get-refers-to)
 (send (symbol-to-instance-name ?gen298) get-refers-to)
 (str-cat ?gen299)
 ))
(defmethod iris32::add
             ((?gen300 SYMBOL
               (registerp ?current-argument))
 (?gen301 SYMBOL
               (registerp ?current-argument))
 (?gen302 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "add %s %s %s " ?gen300
 ?gen301
 ?gen302
 ))
(defmethod iris32::add
             ((?gen303 SYMBOL
               (registerp ?current-argument))
 (?gen304 SYMBOL
               (registerp ?current-argument))
 (?gen305 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "add %s %s %s " ?gen303
 ?gen304
 (send (symbol-to-instance-name ?gen305) get-refers-to)
 ))
(defmethod iris32::add
             ((?gen306 SYMBOL
               (registerp ?current-argument))
 (?gen307 SYMBOL
               (register-aliasp ?current-argument))
 (?gen308 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "add %s %s %s " ?gen306
 (send (symbol-to-instance-name ?gen307) get-refers-to)
 ?gen308
 ))
(defmethod iris32::add
             ((?gen309 SYMBOL
               (registerp ?current-argument))
 (?gen310 SYMBOL
               (register-aliasp ?current-argument))
 (?gen311 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "add %s %s %s " ?gen309
 (send (symbol-to-instance-name ?gen310) get-refers-to)
 (send (symbol-to-instance-name ?gen311) get-refers-to)
 ))
(defmethod iris32::add
             ((?gen312 SYMBOL
               (register-aliasp ?current-argument))
 (?gen313 SYMBOL
               (registerp ?current-argument))
 (?gen314 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "add %s %s %s " (send (symbol-to-instance-name ?gen312) get-refers-to)
 ?gen313
 ?gen314
 ))
(defmethod iris32::add
             ((?gen315 SYMBOL
               (register-aliasp ?current-argument))
 (?gen316 SYMBOL
               (registerp ?current-argument))
 (?gen317 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "add %s %s %s " (send (symbol-to-instance-name ?gen315) get-refers-to)
 ?gen316
 (send (symbol-to-instance-name ?gen317) get-refers-to)
 ))
(defmethod iris32::add
             ((?gen318 SYMBOL
               (register-aliasp ?current-argument))
 (?gen319 SYMBOL
               (register-aliasp ?current-argument))
 (?gen320 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "add %s %s %s " (send (symbol-to-instance-name ?gen318) get-refers-to)
 (send (symbol-to-instance-name ?gen319) get-refers-to)
 ?gen320
 ))
(defmethod iris32::add
             ((?gen321 SYMBOL
               (register-aliasp ?current-argument))
 (?gen322 SYMBOL
               (register-aliasp ?current-argument))
 (?gen323 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "add %s %s %s " (send (symbol-to-instance-name ?gen321) get-refers-to)
 (send (symbol-to-instance-name ?gen322) get-refers-to)
 (send (symbol-to-instance-name ?gen323) get-refers-to)
 ))
(defmethod iris32::sub
             ((?gen324 SYMBOL
               (registerp ?current-argument))
 (?gen325 SYMBOL
               (registerp ?current-argument))
 (?gen326 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "sub %s %s %s " ?gen324
 ?gen325
 ?gen326
 ))
(defmethod iris32::sub
             ((?gen327 SYMBOL
               (registerp ?current-argument))
 (?gen328 SYMBOL
               (registerp ?current-argument))
 (?gen329 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "sub %s %s %s " ?gen327
 ?gen328
 (send (symbol-to-instance-name ?gen329) get-refers-to)
 ))
(defmethod iris32::sub
             ((?gen330 SYMBOL
               (registerp ?current-argument))
 (?gen331 SYMBOL
               (register-aliasp ?current-argument))
 (?gen332 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "sub %s %s %s " ?gen330
 (send (symbol-to-instance-name ?gen331) get-refers-to)
 ?gen332
 ))
(defmethod iris32::sub
             ((?gen333 SYMBOL
               (registerp ?current-argument))
 (?gen334 SYMBOL
               (register-aliasp ?current-argument))
 (?gen335 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "sub %s %s %s " ?gen333
 (send (symbol-to-instance-name ?gen334) get-refers-to)
 (send (symbol-to-instance-name ?gen335) get-refers-to)
 ))
(defmethod iris32::sub
             ((?gen336 SYMBOL
               (register-aliasp ?current-argument))
 (?gen337 SYMBOL
               (registerp ?current-argument))
 (?gen338 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "sub %s %s %s " (send (symbol-to-instance-name ?gen336) get-refers-to)
 ?gen337
 ?gen338
 ))
(defmethod iris32::sub
             ((?gen339 SYMBOL
               (register-aliasp ?current-argument))
 (?gen340 SYMBOL
               (registerp ?current-argument))
 (?gen341 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "sub %s %s %s " (send (symbol-to-instance-name ?gen339) get-refers-to)
 ?gen340
 (send (symbol-to-instance-name ?gen341) get-refers-to)
 ))
(defmethod iris32::sub
             ((?gen342 SYMBOL
               (register-aliasp ?current-argument))
 (?gen343 SYMBOL
               (register-aliasp ?current-argument))
 (?gen344 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "sub %s %s %s " (send (symbol-to-instance-name ?gen342) get-refers-to)
 (send (symbol-to-instance-name ?gen343) get-refers-to)
 ?gen344
 ))
(defmethod iris32::sub
             ((?gen345 SYMBOL
               (register-aliasp ?current-argument))
 (?gen346 SYMBOL
               (register-aliasp ?current-argument))
 (?gen347 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "sub %s %s %s " (send (symbol-to-instance-name ?gen345) get-refers-to)
 (send (symbol-to-instance-name ?gen346) get-refers-to)
 (send (symbol-to-instance-name ?gen347) get-refers-to)
 ))
(defmethod iris32::mul
             ((?gen348 SYMBOL
               (registerp ?current-argument))
 (?gen349 SYMBOL
               (registerp ?current-argument))
 (?gen350 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "mul %s %s %s " ?gen348
 ?gen349
 ?gen350
 ))
(defmethod iris32::mul
             ((?gen351 SYMBOL
               (registerp ?current-argument))
 (?gen352 SYMBOL
               (registerp ?current-argument))
 (?gen353 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "mul %s %s %s " ?gen351
 ?gen352
 (send (symbol-to-instance-name ?gen353) get-refers-to)
 ))
(defmethod iris32::mul
             ((?gen354 SYMBOL
               (registerp ?current-argument))
 (?gen355 SYMBOL
               (register-aliasp ?current-argument))
 (?gen356 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "mul %s %s %s " ?gen354
 (send (symbol-to-instance-name ?gen355) get-refers-to)
 ?gen356
 ))
(defmethod iris32::mul
             ((?gen357 SYMBOL
               (registerp ?current-argument))
 (?gen358 SYMBOL
               (register-aliasp ?current-argument))
 (?gen359 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "mul %s %s %s " ?gen357
 (send (symbol-to-instance-name ?gen358) get-refers-to)
 (send (symbol-to-instance-name ?gen359) get-refers-to)
 ))
(defmethod iris32::mul
             ((?gen360 SYMBOL
               (register-aliasp ?current-argument))
 (?gen361 SYMBOL
               (registerp ?current-argument))
 (?gen362 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "mul %s %s %s " (send (symbol-to-instance-name ?gen360) get-refers-to)
 ?gen361
 ?gen362
 ))
(defmethod iris32::mul
             ((?gen363 SYMBOL
               (register-aliasp ?current-argument))
 (?gen364 SYMBOL
               (registerp ?current-argument))
 (?gen365 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "mul %s %s %s " (send (symbol-to-instance-name ?gen363) get-refers-to)
 ?gen364
 (send (symbol-to-instance-name ?gen365) get-refers-to)
 ))
(defmethod iris32::mul
             ((?gen366 SYMBOL
               (register-aliasp ?current-argument))
 (?gen367 SYMBOL
               (register-aliasp ?current-argument))
 (?gen368 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "mul %s %s %s " (send (symbol-to-instance-name ?gen366) get-refers-to)
 (send (symbol-to-instance-name ?gen367) get-refers-to)
 ?gen368
 ))
(defmethod iris32::mul
             ((?gen369 SYMBOL
               (register-aliasp ?current-argument))
 (?gen370 SYMBOL
               (register-aliasp ?current-argument))
 (?gen371 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "mul %s %s %s " (send (symbol-to-instance-name ?gen369) get-refers-to)
 (send (symbol-to-instance-name ?gen370) get-refers-to)
 (send (symbol-to-instance-name ?gen371) get-refers-to)
 ))
(defmethod iris32::div
             ((?gen372 SYMBOL
               (registerp ?current-argument))
 (?gen373 SYMBOL
               (registerp ?current-argument))
 (?gen374 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "div %s %s %s " ?gen372
 ?gen373
 ?gen374
 ))
(defmethod iris32::div
             ((?gen375 SYMBOL
               (registerp ?current-argument))
 (?gen376 SYMBOL
               (registerp ?current-argument))
 (?gen377 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "div %s %s %s " ?gen375
 ?gen376
 (send (symbol-to-instance-name ?gen377) get-refers-to)
 ))
(defmethod iris32::div
             ((?gen378 SYMBOL
               (registerp ?current-argument))
 (?gen379 SYMBOL
               (register-aliasp ?current-argument))
 (?gen380 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "div %s %s %s " ?gen378
 (send (symbol-to-instance-name ?gen379) get-refers-to)
 ?gen380
 ))
(defmethod iris32::div
             ((?gen381 SYMBOL
               (registerp ?current-argument))
 (?gen382 SYMBOL
               (register-aliasp ?current-argument))
 (?gen383 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "div %s %s %s " ?gen381
 (send (symbol-to-instance-name ?gen382) get-refers-to)
 (send (symbol-to-instance-name ?gen383) get-refers-to)
 ))
(defmethod iris32::div
             ((?gen384 SYMBOL
               (register-aliasp ?current-argument))
 (?gen385 SYMBOL
               (registerp ?current-argument))
 (?gen386 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "div %s %s %s " (send (symbol-to-instance-name ?gen384) get-refers-to)
 ?gen385
 ?gen386
 ))
(defmethod iris32::div
             ((?gen387 SYMBOL
               (register-aliasp ?current-argument))
 (?gen388 SYMBOL
               (registerp ?current-argument))
 (?gen389 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "div %s %s %s " (send (symbol-to-instance-name ?gen387) get-refers-to)
 ?gen388
 (send (symbol-to-instance-name ?gen389) get-refers-to)
 ))
(defmethod iris32::div
             ((?gen390 SYMBOL
               (register-aliasp ?current-argument))
 (?gen391 SYMBOL
               (register-aliasp ?current-argument))
 (?gen392 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "div %s %s %s " (send (symbol-to-instance-name ?gen390) get-refers-to)
 (send (symbol-to-instance-name ?gen391) get-refers-to)
 ?gen392
 ))
(defmethod iris32::div
             ((?gen393 SYMBOL
               (register-aliasp ?current-argument))
 (?gen394 SYMBOL
               (register-aliasp ?current-argument))
 (?gen395 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "div %s %s %s " (send (symbol-to-instance-name ?gen393) get-refers-to)
 (send (symbol-to-instance-name ?gen394) get-refers-to)
 (send (symbol-to-instance-name ?gen395) get-refers-to)
 ))
(defmethod iris32::rem
             ((?gen396 SYMBOL
               (registerp ?current-argument))
 (?gen397 SYMBOL
               (registerp ?current-argument))
 (?gen398 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "rem %s %s %s " ?gen396
 ?gen397
 ?gen398
 ))
(defmethod iris32::rem
             ((?gen399 SYMBOL
               (registerp ?current-argument))
 (?gen400 SYMBOL
               (registerp ?current-argument))
 (?gen401 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "rem %s %s %s " ?gen399
 ?gen400
 (send (symbol-to-instance-name ?gen401) get-refers-to)
 ))
(defmethod iris32::rem
             ((?gen402 SYMBOL
               (registerp ?current-argument))
 (?gen403 SYMBOL
               (register-aliasp ?current-argument))
 (?gen404 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "rem %s %s %s " ?gen402
 (send (symbol-to-instance-name ?gen403) get-refers-to)
 ?gen404
 ))
(defmethod iris32::rem
             ((?gen405 SYMBOL
               (registerp ?current-argument))
 (?gen406 SYMBOL
               (register-aliasp ?current-argument))
 (?gen407 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "rem %s %s %s " ?gen405
 (send (symbol-to-instance-name ?gen406) get-refers-to)
 (send (symbol-to-instance-name ?gen407) get-refers-to)
 ))
(defmethod iris32::rem
             ((?gen408 SYMBOL
               (register-aliasp ?current-argument))
 (?gen409 SYMBOL
               (registerp ?current-argument))
 (?gen410 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "rem %s %s %s " (send (symbol-to-instance-name ?gen408) get-refers-to)
 ?gen409
 ?gen410
 ))
(defmethod iris32::rem
             ((?gen411 SYMBOL
               (register-aliasp ?current-argument))
 (?gen412 SYMBOL
               (registerp ?current-argument))
 (?gen413 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "rem %s %s %s " (send (symbol-to-instance-name ?gen411) get-refers-to)
 ?gen412
 (send (symbol-to-instance-name ?gen413) get-refers-to)
 ))
(defmethod iris32::rem
             ((?gen414 SYMBOL
               (register-aliasp ?current-argument))
 (?gen415 SYMBOL
               (register-aliasp ?current-argument))
 (?gen416 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "rem %s %s %s " (send (symbol-to-instance-name ?gen414) get-refers-to)
 (send (symbol-to-instance-name ?gen415) get-refers-to)
 ?gen416
 ))
(defmethod iris32::rem
             ((?gen417 SYMBOL
               (register-aliasp ?current-argument))
 (?gen418 SYMBOL
               (register-aliasp ?current-argument))
 (?gen419 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "rem %s %s %s " (send (symbol-to-instance-name ?gen417) get-refers-to)
 (send (symbol-to-instance-name ?gen418) get-refers-to)
 (send (symbol-to-instance-name ?gen419) get-refers-to)
 ))
(defmethod iris32::shl
             ((?gen420 SYMBOL
               (registerp ?current-argument))
 (?gen421 SYMBOL
               (registerp ?current-argument))
 (?gen422 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "shl %s %s %s " ?gen420
 ?gen421
 ?gen422
 ))
(defmethod iris32::shl
             ((?gen423 SYMBOL
               (registerp ?current-argument))
 (?gen424 SYMBOL
               (registerp ?current-argument))
 (?gen425 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "shl %s %s %s " ?gen423
 ?gen424
 (send (symbol-to-instance-name ?gen425) get-refers-to)
 ))
(defmethod iris32::shl
             ((?gen426 SYMBOL
               (registerp ?current-argument))
 (?gen427 SYMBOL
               (register-aliasp ?current-argument))
 (?gen428 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "shl %s %s %s " ?gen426
 (send (symbol-to-instance-name ?gen427) get-refers-to)
 ?gen428
 ))
(defmethod iris32::shl
             ((?gen429 SYMBOL
               (registerp ?current-argument))
 (?gen430 SYMBOL
               (register-aliasp ?current-argument))
 (?gen431 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "shl %s %s %s " ?gen429
 (send (symbol-to-instance-name ?gen430) get-refers-to)
 (send (symbol-to-instance-name ?gen431) get-refers-to)
 ))
(defmethod iris32::shl
             ((?gen432 SYMBOL
               (register-aliasp ?current-argument))
 (?gen433 SYMBOL
               (registerp ?current-argument))
 (?gen434 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "shl %s %s %s " (send (symbol-to-instance-name ?gen432) get-refers-to)
 ?gen433
 ?gen434
 ))
(defmethod iris32::shl
             ((?gen435 SYMBOL
               (register-aliasp ?current-argument))
 (?gen436 SYMBOL
               (registerp ?current-argument))
 (?gen437 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "shl %s %s %s " (send (symbol-to-instance-name ?gen435) get-refers-to)
 ?gen436
 (send (symbol-to-instance-name ?gen437) get-refers-to)
 ))
(defmethod iris32::shl
             ((?gen438 SYMBOL
               (register-aliasp ?current-argument))
 (?gen439 SYMBOL
               (register-aliasp ?current-argument))
 (?gen440 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "shl %s %s %s " (send (symbol-to-instance-name ?gen438) get-refers-to)
 (send (symbol-to-instance-name ?gen439) get-refers-to)
 ?gen440
 ))
(defmethod iris32::shl
             ((?gen441 SYMBOL
               (register-aliasp ?current-argument))
 (?gen442 SYMBOL
               (register-aliasp ?current-argument))
 (?gen443 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "shl %s %s %s " (send (symbol-to-instance-name ?gen441) get-refers-to)
 (send (symbol-to-instance-name ?gen442) get-refers-to)
 (send (symbol-to-instance-name ?gen443) get-refers-to)
 ))
(defmethod iris32::shr
             ((?gen444 SYMBOL
               (registerp ?current-argument))
 (?gen445 SYMBOL
               (registerp ?current-argument))
 (?gen446 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "shr %s %s %s " ?gen444
 ?gen445
 ?gen446
 ))
(defmethod iris32::shr
             ((?gen447 SYMBOL
               (registerp ?current-argument))
 (?gen448 SYMBOL
               (registerp ?current-argument))
 (?gen449 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "shr %s %s %s " ?gen447
 ?gen448
 (send (symbol-to-instance-name ?gen449) get-refers-to)
 ))
(defmethod iris32::shr
             ((?gen450 SYMBOL
               (registerp ?current-argument))
 (?gen451 SYMBOL
               (register-aliasp ?current-argument))
 (?gen452 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "shr %s %s %s " ?gen450
 (send (symbol-to-instance-name ?gen451) get-refers-to)
 ?gen452
 ))
(defmethod iris32::shr
             ((?gen453 SYMBOL
               (registerp ?current-argument))
 (?gen454 SYMBOL
               (register-aliasp ?current-argument))
 (?gen455 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "shr %s %s %s " ?gen453
 (send (symbol-to-instance-name ?gen454) get-refers-to)
 (send (symbol-to-instance-name ?gen455) get-refers-to)
 ))
(defmethod iris32::shr
             ((?gen456 SYMBOL
               (register-aliasp ?current-argument))
 (?gen457 SYMBOL
               (registerp ?current-argument))
 (?gen458 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "shr %s %s %s " (send (symbol-to-instance-name ?gen456) get-refers-to)
 ?gen457
 ?gen458
 ))
(defmethod iris32::shr
             ((?gen459 SYMBOL
               (register-aliasp ?current-argument))
 (?gen460 SYMBOL
               (registerp ?current-argument))
 (?gen461 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "shr %s %s %s " (send (symbol-to-instance-name ?gen459) get-refers-to)
 ?gen460
 (send (symbol-to-instance-name ?gen461) get-refers-to)
 ))
(defmethod iris32::shr
             ((?gen462 SYMBOL
               (register-aliasp ?current-argument))
 (?gen463 SYMBOL
               (register-aliasp ?current-argument))
 (?gen464 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "shr %s %s %s " (send (symbol-to-instance-name ?gen462) get-refers-to)
 (send (symbol-to-instance-name ?gen463) get-refers-to)
 ?gen464
 ))
(defmethod iris32::shr
             ((?gen465 SYMBOL
               (register-aliasp ?current-argument))
 (?gen466 SYMBOL
               (register-aliasp ?current-argument))
 (?gen467 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "shr %s %s %s " (send (symbol-to-instance-name ?gen465) get-refers-to)
 (send (symbol-to-instance-name ?gen466) get-refers-to)
 (send (symbol-to-instance-name ?gen467) get-refers-to)
 ))
(defmethod iris32::jtl
             ((?gen468 SYMBOL
               (registerp ?current-argument))
 (?gen469 SYMBOL
               (registerp ?current-argument))
 (?gen470 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jtl %s %s %s " ?gen468
 ?gen469
 ?gen470
 ))
(defmethod iris32::jtl
             ((?gen471 SYMBOL
               (registerp ?current-argument))
 (?gen472 SYMBOL
               (registerp ?current-argument))
 (?gen473 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jtl %s %s %s " ?gen471
 ?gen472
 (send (symbol-to-instance-name ?gen473) get-refers-to)
 ))
(defmethod iris32::jtl
             ((?gen474 SYMBOL
               (registerp ?current-argument))
 (?gen475 SYMBOL
               (register-aliasp ?current-argument))
 (?gen476 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jtl %s %s %s " ?gen474
 (send (symbol-to-instance-name ?gen475) get-refers-to)
 ?gen476
 ))
(defmethod iris32::jtl
             ((?gen477 SYMBOL
               (registerp ?current-argument))
 (?gen478 SYMBOL
               (register-aliasp ?current-argument))
 (?gen479 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jtl %s %s %s " ?gen477
 (send (symbol-to-instance-name ?gen478) get-refers-to)
 (send (symbol-to-instance-name ?gen479) get-refers-to)
 ))
(defmethod iris32::jtl
             ((?gen480 SYMBOL
               (register-aliasp ?current-argument))
 (?gen481 SYMBOL
               (registerp ?current-argument))
 (?gen482 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jtl %s %s %s " (send (symbol-to-instance-name ?gen480) get-refers-to)
 ?gen481
 ?gen482
 ))
(defmethod iris32::jtl
             ((?gen483 SYMBOL
               (register-aliasp ?current-argument))
 (?gen484 SYMBOL
               (registerp ?current-argument))
 (?gen485 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jtl %s %s %s " (send (symbol-to-instance-name ?gen483) get-refers-to)
 ?gen484
 (send (symbol-to-instance-name ?gen485) get-refers-to)
 ))
(defmethod iris32::jtl
             ((?gen486 SYMBOL
               (register-aliasp ?current-argument))
 (?gen487 SYMBOL
               (register-aliasp ?current-argument))
 (?gen488 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jtl %s %s %s " (send (symbol-to-instance-name ?gen486) get-refers-to)
 (send (symbol-to-instance-name ?gen487) get-refers-to)
 ?gen488
 ))
(defmethod iris32::jtl
             ((?gen489 SYMBOL
               (register-aliasp ?current-argument))
 (?gen490 SYMBOL
               (register-aliasp ?current-argument))
 (?gen491 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jtl %s %s %s " (send (symbol-to-instance-name ?gen489) get-refers-to)
 (send (symbol-to-instance-name ?gen490) get-refers-to)
 (send (symbol-to-instance-name ?gen491) get-refers-to)
 ))
(defmethod iris32::jfl
             ((?gen492 SYMBOL
               (registerp ?current-argument))
 (?gen493 SYMBOL
               (registerp ?current-argument))
 (?gen494 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jfl %s %s %s " ?gen492
 ?gen493
 ?gen494
 ))
(defmethod iris32::jfl
             ((?gen495 SYMBOL
               (registerp ?current-argument))
 (?gen496 SYMBOL
               (registerp ?current-argument))
 (?gen497 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jfl %s %s %s " ?gen495
 ?gen496
 (send (symbol-to-instance-name ?gen497) get-refers-to)
 ))
(defmethod iris32::jfl
             ((?gen498 SYMBOL
               (registerp ?current-argument))
 (?gen499 SYMBOL
               (register-aliasp ?current-argument))
 (?gen500 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jfl %s %s %s " ?gen498
 (send (symbol-to-instance-name ?gen499) get-refers-to)
 ?gen500
 ))
(defmethod iris32::jfl
             ((?gen501 SYMBOL
               (registerp ?current-argument))
 (?gen502 SYMBOL
               (register-aliasp ?current-argument))
 (?gen503 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jfl %s %s %s " ?gen501
 (send (symbol-to-instance-name ?gen502) get-refers-to)
 (send (symbol-to-instance-name ?gen503) get-refers-to)
 ))
(defmethod iris32::jfl
             ((?gen504 SYMBOL
               (register-aliasp ?current-argument))
 (?gen505 SYMBOL
               (registerp ?current-argument))
 (?gen506 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jfl %s %s %s " (send (symbol-to-instance-name ?gen504) get-refers-to)
 ?gen505
 ?gen506
 ))
(defmethod iris32::jfl
             ((?gen507 SYMBOL
               (register-aliasp ?current-argument))
 (?gen508 SYMBOL
               (registerp ?current-argument))
 (?gen509 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jfl %s %s %s " (send (symbol-to-instance-name ?gen507) get-refers-to)
 ?gen508
 (send (symbol-to-instance-name ?gen509) get-refers-to)
 ))
(defmethod iris32::jfl
             ((?gen510 SYMBOL
               (register-aliasp ?current-argument))
 (?gen511 SYMBOL
               (register-aliasp ?current-argument))
 (?gen512 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "jfl %s %s %s " (send (symbol-to-instance-name ?gen510) get-refers-to)
 (send (symbol-to-instance-name ?gen511) get-refers-to)
 ?gen512
 ))
(defmethod iris32::jfl
             ((?gen513 SYMBOL
               (register-aliasp ?current-argument))
 (?gen514 SYMBOL
               (register-aliasp ?current-argument))
 (?gen515 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "jfl %s %s %s " (send (symbol-to-instance-name ?gen513) get-refers-to)
 (send (symbol-to-instance-name ?gen514) get-refers-to)
 (send (symbol-to-instance-name ?gen515) get-refers-to)
 ))
(defmethod iris32::ift
             ((?gen516 SYMBOL
               (registerp ?current-argument))
 (?gen517 SYMBOL
               (registerp ?current-argument))
 (?gen518 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ift %s %s %s " ?gen516
 ?gen517
 ?gen518
 ))
(defmethod iris32::ift
             ((?gen519 SYMBOL
               (registerp ?current-argument))
 (?gen520 SYMBOL
               (registerp ?current-argument))
 (?gen521 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ift %s %s %s " ?gen519
 ?gen520
 (send (symbol-to-instance-name ?gen521) get-refers-to)
 ))
(defmethod iris32::ift
             ((?gen522 SYMBOL
               (registerp ?current-argument))
 (?gen523 SYMBOL
               (register-aliasp ?current-argument))
 (?gen524 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ift %s %s %s " ?gen522
 (send (symbol-to-instance-name ?gen523) get-refers-to)
 ?gen524
 ))
(defmethod iris32::ift
             ((?gen525 SYMBOL
               (registerp ?current-argument))
 (?gen526 SYMBOL
               (register-aliasp ?current-argument))
 (?gen527 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ift %s %s %s " ?gen525
 (send (symbol-to-instance-name ?gen526) get-refers-to)
 (send (symbol-to-instance-name ?gen527) get-refers-to)
 ))
(defmethod iris32::ift
             ((?gen528 SYMBOL
               (register-aliasp ?current-argument))
 (?gen529 SYMBOL
               (registerp ?current-argument))
 (?gen530 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ift %s %s %s " (send (symbol-to-instance-name ?gen528) get-refers-to)
 ?gen529
 ?gen530
 ))
(defmethod iris32::ift
             ((?gen531 SYMBOL
               (register-aliasp ?current-argument))
 (?gen532 SYMBOL
               (registerp ?current-argument))
 (?gen533 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ift %s %s %s " (send (symbol-to-instance-name ?gen531) get-refers-to)
 ?gen532
 (send (symbol-to-instance-name ?gen533) get-refers-to)
 ))
(defmethod iris32::ift
             ((?gen534 SYMBOL
               (register-aliasp ?current-argument))
 (?gen535 SYMBOL
               (register-aliasp ?current-argument))
 (?gen536 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ift %s %s %s " (send (symbol-to-instance-name ?gen534) get-refers-to)
 (send (symbol-to-instance-name ?gen535) get-refers-to)
 ?gen536
 ))
(defmethod iris32::ift
             ((?gen537 SYMBOL
               (register-aliasp ?current-argument))
 (?gen538 SYMBOL
               (register-aliasp ?current-argument))
 (?gen539 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ift %s %s %s " (send (symbol-to-instance-name ?gen537) get-refers-to)
 (send (symbol-to-instance-name ?gen538) get-refers-to)
 (send (symbol-to-instance-name ?gen539) get-refers-to)
 ))
(defmethod iris32::iff
             ((?gen540 SYMBOL
               (registerp ?current-argument))
 (?gen541 SYMBOL
               (registerp ?current-argument))
 (?gen542 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iff %s %s %s " ?gen540
 ?gen541
 ?gen542
 ))
(defmethod iris32::iff
             ((?gen543 SYMBOL
               (registerp ?current-argument))
 (?gen544 SYMBOL
               (registerp ?current-argument))
 (?gen545 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iff %s %s %s " ?gen543
 ?gen544
 (send (symbol-to-instance-name ?gen545) get-refers-to)
 ))
(defmethod iris32::iff
             ((?gen546 SYMBOL
               (registerp ?current-argument))
 (?gen547 SYMBOL
               (register-aliasp ?current-argument))
 (?gen548 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iff %s %s %s " ?gen546
 (send (symbol-to-instance-name ?gen547) get-refers-to)
 ?gen548
 ))
(defmethod iris32::iff
             ((?gen549 SYMBOL
               (registerp ?current-argument))
 (?gen550 SYMBOL
               (register-aliasp ?current-argument))
 (?gen551 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iff %s %s %s " ?gen549
 (send (symbol-to-instance-name ?gen550) get-refers-to)
 (send (symbol-to-instance-name ?gen551) get-refers-to)
 ))
(defmethod iris32::iff
             ((?gen552 SYMBOL
               (register-aliasp ?current-argument))
 (?gen553 SYMBOL
               (registerp ?current-argument))
 (?gen554 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iff %s %s %s " (send (symbol-to-instance-name ?gen552) get-refers-to)
 ?gen553
 ?gen554
 ))
(defmethod iris32::iff
             ((?gen555 SYMBOL
               (register-aliasp ?current-argument))
 (?gen556 SYMBOL
               (registerp ?current-argument))
 (?gen557 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iff %s %s %s " (send (symbol-to-instance-name ?gen555) get-refers-to)
 ?gen556
 (send (symbol-to-instance-name ?gen557) get-refers-to)
 ))
(defmethod iris32::iff
             ((?gen558 SYMBOL
               (register-aliasp ?current-argument))
 (?gen559 SYMBOL
               (register-aliasp ?current-argument))
 (?gen560 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iff %s %s %s " (send (symbol-to-instance-name ?gen558) get-refers-to)
 (send (symbol-to-instance-name ?gen559) get-refers-to)
 ?gen560
 ))
(defmethod iris32::iff
             ((?gen561 SYMBOL
               (register-aliasp ?current-argument))
 (?gen562 SYMBOL
               (register-aliasp ?current-argument))
 (?gen563 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iff %s %s %s " (send (symbol-to-instance-name ?gen561) get-refers-to)
 (send (symbol-to-instance-name ?gen562) get-refers-to)
 (send (symbol-to-instance-name ?gen563) get-refers-to)
 ))
(defmethod iris32::iftl
             ((?gen564 SYMBOL
               (registerp ?current-argument))
 (?gen565 SYMBOL
               (registerp ?current-argument))
 (?gen566 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iftl %s %s %s " ?gen564
 ?gen565
 ?gen566
 ))
(defmethod iris32::iftl
             ((?gen567 SYMBOL
               (registerp ?current-argument))
 (?gen568 SYMBOL
               (registerp ?current-argument))
 (?gen569 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iftl %s %s %s " ?gen567
 ?gen568
 (send (symbol-to-instance-name ?gen569) get-refers-to)
 ))
(defmethod iris32::iftl
             ((?gen570 SYMBOL
               (registerp ?current-argument))
 (?gen571 SYMBOL
               (register-aliasp ?current-argument))
 (?gen572 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iftl %s %s %s " ?gen570
 (send (symbol-to-instance-name ?gen571) get-refers-to)
 ?gen572
 ))
(defmethod iris32::iftl
             ((?gen573 SYMBOL
               (registerp ?current-argument))
 (?gen574 SYMBOL
               (register-aliasp ?current-argument))
 (?gen575 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iftl %s %s %s " ?gen573
 (send (symbol-to-instance-name ?gen574) get-refers-to)
 (send (symbol-to-instance-name ?gen575) get-refers-to)
 ))
(defmethod iris32::iftl
             ((?gen576 SYMBOL
               (register-aliasp ?current-argument))
 (?gen577 SYMBOL
               (registerp ?current-argument))
 (?gen578 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iftl %s %s %s " (send (symbol-to-instance-name ?gen576) get-refers-to)
 ?gen577
 ?gen578
 ))
(defmethod iris32::iftl
             ((?gen579 SYMBOL
               (register-aliasp ?current-argument))
 (?gen580 SYMBOL
               (registerp ?current-argument))
 (?gen581 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iftl %s %s %s " (send (symbol-to-instance-name ?gen579) get-refers-to)
 ?gen580
 (send (symbol-to-instance-name ?gen581) get-refers-to)
 ))
(defmethod iris32::iftl
             ((?gen582 SYMBOL
               (register-aliasp ?current-argument))
 (?gen583 SYMBOL
               (register-aliasp ?current-argument))
 (?gen584 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iftl %s %s %s " (send (symbol-to-instance-name ?gen582) get-refers-to)
 (send (symbol-to-instance-name ?gen583) get-refers-to)
 ?gen584
 ))
(defmethod iris32::iftl
             ((?gen585 SYMBOL
               (register-aliasp ?current-argument))
 (?gen586 SYMBOL
               (register-aliasp ?current-argument))
 (?gen587 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iftl %s %s %s " (send (symbol-to-instance-name ?gen585) get-refers-to)
 (send (symbol-to-instance-name ?gen586) get-refers-to)
 (send (symbol-to-instance-name ?gen587) get-refers-to)
 ))
(defmethod iris32::iffl
             ((?gen588 SYMBOL
               (registerp ?current-argument))
 (?gen589 SYMBOL
               (registerp ?current-argument))
 (?gen590 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iffl %s %s %s " ?gen588
 ?gen589
 ?gen590
 ))
(defmethod iris32::iffl
             ((?gen591 SYMBOL
               (registerp ?current-argument))
 (?gen592 SYMBOL
               (registerp ?current-argument))
 (?gen593 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iffl %s %s %s " ?gen591
 ?gen592
 (send (symbol-to-instance-name ?gen593) get-refers-to)
 ))
(defmethod iris32::iffl
             ((?gen594 SYMBOL
               (registerp ?current-argument))
 (?gen595 SYMBOL
               (register-aliasp ?current-argument))
 (?gen596 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iffl %s %s %s " ?gen594
 (send (symbol-to-instance-name ?gen595) get-refers-to)
 ?gen596
 ))
(defmethod iris32::iffl
             ((?gen597 SYMBOL
               (registerp ?current-argument))
 (?gen598 SYMBOL
               (register-aliasp ?current-argument))
 (?gen599 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iffl %s %s %s " ?gen597
 (send (symbol-to-instance-name ?gen598) get-refers-to)
 (send (symbol-to-instance-name ?gen599) get-refers-to)
 ))
(defmethod iris32::iffl
             ((?gen600 SYMBOL
               (register-aliasp ?current-argument))
 (?gen601 SYMBOL
               (registerp ?current-argument))
 (?gen602 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iffl %s %s %s " (send (symbol-to-instance-name ?gen600) get-refers-to)
 ?gen601
 ?gen602
 ))
(defmethod iris32::iffl
             ((?gen603 SYMBOL
               (register-aliasp ?current-argument))
 (?gen604 SYMBOL
               (registerp ?current-argument))
 (?gen605 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iffl %s %s %s " (send (symbol-to-instance-name ?gen603) get-refers-to)
 ?gen604
 (send (symbol-to-instance-name ?gen605) get-refers-to)
 ))
(defmethod iris32::iffl
             ((?gen606 SYMBOL
               (register-aliasp ?current-argument))
 (?gen607 SYMBOL
               (register-aliasp ?current-argument))
 (?gen608 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "iffl %s %s %s " (send (symbol-to-instance-name ?gen606) get-refers-to)
 (send (symbol-to-instance-name ?gen607) get-refers-to)
 ?gen608
 ))
(defmethod iris32::iffl
             ((?gen609 SYMBOL
               (register-aliasp ?current-argument))
 (?gen610 SYMBOL
               (register-aliasp ?current-argument))
 (?gen611 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "iffl %s %s %s " (send (symbol-to-instance-name ?gen609) get-refers-to)
 (send (symbol-to-instance-name ?gen610) get-refers-to)
 (send (symbol-to-instance-name ?gen611) get-refers-to)
 ))
(defmethod iris32::ne
             ((?gen612 SYMBOL
               (registerp ?current-argument))
 (?gen613 SYMBOL
               (registerp ?current-argument))
 (?gen614 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ne %s %s %s " ?gen612
 ?gen613
 ?gen614
 ))
(defmethod iris32::ne
             ((?gen615 SYMBOL
               (registerp ?current-argument))
 (?gen616 SYMBOL
               (registerp ?current-argument))
 (?gen617 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ne %s %s %s " ?gen615
 ?gen616
 (send (symbol-to-instance-name ?gen617) get-refers-to)
 ))
(defmethod iris32::ne
             ((?gen618 SYMBOL
               (registerp ?current-argument))
 (?gen619 SYMBOL
               (register-aliasp ?current-argument))
 (?gen620 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ne %s %s %s " ?gen618
 (send (symbol-to-instance-name ?gen619) get-refers-to)
 ?gen620
 ))
(defmethod iris32::ne
             ((?gen621 SYMBOL
               (registerp ?current-argument))
 (?gen622 SYMBOL
               (register-aliasp ?current-argument))
 (?gen623 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ne %s %s %s " ?gen621
 (send (symbol-to-instance-name ?gen622) get-refers-to)
 (send (symbol-to-instance-name ?gen623) get-refers-to)
 ))
(defmethod iris32::ne
             ((?gen624 SYMBOL
               (register-aliasp ?current-argument))
 (?gen625 SYMBOL
               (registerp ?current-argument))
 (?gen626 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ne %s %s %s " (send (symbol-to-instance-name ?gen624) get-refers-to)
 ?gen625
 ?gen626
 ))
(defmethod iris32::ne
             ((?gen627 SYMBOL
               (register-aliasp ?current-argument))
 (?gen628 SYMBOL
               (registerp ?current-argument))
 (?gen629 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ne %s %s %s " (send (symbol-to-instance-name ?gen627) get-refers-to)
 ?gen628
 (send (symbol-to-instance-name ?gen629) get-refers-to)
 ))
(defmethod iris32::ne
             ((?gen630 SYMBOL
               (register-aliasp ?current-argument))
 (?gen631 SYMBOL
               (register-aliasp ?current-argument))
 (?gen632 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ne %s %s %s " (send (symbol-to-instance-name ?gen630) get-refers-to)
 (send (symbol-to-instance-name ?gen631) get-refers-to)
 ?gen632
 ))
(defmethod iris32::ne
             ((?gen633 SYMBOL
               (register-aliasp ?current-argument))
 (?gen634 SYMBOL
               (register-aliasp ?current-argument))
 (?gen635 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ne %s %s %s " (send (symbol-to-instance-name ?gen633) get-refers-to)
 (send (symbol-to-instance-name ?gen634) get-refers-to)
 (send (symbol-to-instance-name ?gen635) get-refers-to)
 ))
(defmethod iris32::lt
             ((?gen636 SYMBOL
               (registerp ?current-argument))
 (?gen637 SYMBOL
               (registerp ?current-argument))
 (?gen638 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "lt %s %s %s " ?gen636
 ?gen637
 ?gen638
 ))
(defmethod iris32::lt
             ((?gen639 SYMBOL
               (registerp ?current-argument))
 (?gen640 SYMBOL
               (registerp ?current-argument))
 (?gen641 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "lt %s %s %s " ?gen639
 ?gen640
 (send (symbol-to-instance-name ?gen641) get-refers-to)
 ))
(defmethod iris32::lt
             ((?gen642 SYMBOL
               (registerp ?current-argument))
 (?gen643 SYMBOL
               (register-aliasp ?current-argument))
 (?gen644 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "lt %s %s %s " ?gen642
 (send (symbol-to-instance-name ?gen643) get-refers-to)
 ?gen644
 ))
(defmethod iris32::lt
             ((?gen645 SYMBOL
               (registerp ?current-argument))
 (?gen646 SYMBOL
               (register-aliasp ?current-argument))
 (?gen647 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "lt %s %s %s " ?gen645
 (send (symbol-to-instance-name ?gen646) get-refers-to)
 (send (symbol-to-instance-name ?gen647) get-refers-to)
 ))
(defmethod iris32::lt
             ((?gen648 SYMBOL
               (register-aliasp ?current-argument))
 (?gen649 SYMBOL
               (registerp ?current-argument))
 (?gen650 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "lt %s %s %s " (send (symbol-to-instance-name ?gen648) get-refers-to)
 ?gen649
 ?gen650
 ))
(defmethod iris32::lt
             ((?gen651 SYMBOL
               (register-aliasp ?current-argument))
 (?gen652 SYMBOL
               (registerp ?current-argument))
 (?gen653 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "lt %s %s %s " (send (symbol-to-instance-name ?gen651) get-refers-to)
 ?gen652
 (send (symbol-to-instance-name ?gen653) get-refers-to)
 ))
(defmethod iris32::lt
             ((?gen654 SYMBOL
               (register-aliasp ?current-argument))
 (?gen655 SYMBOL
               (register-aliasp ?current-argument))
 (?gen656 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "lt %s %s %s " (send (symbol-to-instance-name ?gen654) get-refers-to)
 (send (symbol-to-instance-name ?gen655) get-refers-to)
 ?gen656
 ))
(defmethod iris32::lt
             ((?gen657 SYMBOL
               (register-aliasp ?current-argument))
 (?gen658 SYMBOL
               (register-aliasp ?current-argument))
 (?gen659 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "lt %s %s %s " (send (symbol-to-instance-name ?gen657) get-refers-to)
 (send (symbol-to-instance-name ?gen658) get-refers-to)
 (send (symbol-to-instance-name ?gen659) get-refers-to)
 ))
(defmethod iris32::gt
             ((?gen660 SYMBOL
               (registerp ?current-argument))
 (?gen661 SYMBOL
               (registerp ?current-argument))
 (?gen662 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "gt %s %s %s " ?gen660
 ?gen661
 ?gen662
 ))
(defmethod iris32::gt
             ((?gen663 SYMBOL
               (registerp ?current-argument))
 (?gen664 SYMBOL
               (registerp ?current-argument))
 (?gen665 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "gt %s %s %s " ?gen663
 ?gen664
 (send (symbol-to-instance-name ?gen665) get-refers-to)
 ))
(defmethod iris32::gt
             ((?gen666 SYMBOL
               (registerp ?current-argument))
 (?gen667 SYMBOL
               (register-aliasp ?current-argument))
 (?gen668 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "gt %s %s %s " ?gen666
 (send (symbol-to-instance-name ?gen667) get-refers-to)
 ?gen668
 ))
(defmethod iris32::gt
             ((?gen669 SYMBOL
               (registerp ?current-argument))
 (?gen670 SYMBOL
               (register-aliasp ?current-argument))
 (?gen671 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "gt %s %s %s " ?gen669
 (send (symbol-to-instance-name ?gen670) get-refers-to)
 (send (symbol-to-instance-name ?gen671) get-refers-to)
 ))
(defmethod iris32::gt
             ((?gen672 SYMBOL
               (register-aliasp ?current-argument))
 (?gen673 SYMBOL
               (registerp ?current-argument))
 (?gen674 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "gt %s %s %s " (send (symbol-to-instance-name ?gen672) get-refers-to)
 ?gen673
 ?gen674
 ))
(defmethod iris32::gt
             ((?gen675 SYMBOL
               (register-aliasp ?current-argument))
 (?gen676 SYMBOL
               (registerp ?current-argument))
 (?gen677 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "gt %s %s %s " (send (symbol-to-instance-name ?gen675) get-refers-to)
 ?gen676
 (send (symbol-to-instance-name ?gen677) get-refers-to)
 ))
(defmethod iris32::gt
             ((?gen678 SYMBOL
               (register-aliasp ?current-argument))
 (?gen679 SYMBOL
               (register-aliasp ?current-argument))
 (?gen680 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "gt %s %s %s " (send (symbol-to-instance-name ?gen678) get-refers-to)
 (send (symbol-to-instance-name ?gen679) get-refers-to)
 ?gen680
 ))
(defmethod iris32::gt
             ((?gen681 SYMBOL
               (register-aliasp ?current-argument))
 (?gen682 SYMBOL
               (register-aliasp ?current-argument))
 (?gen683 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "gt %s %s %s " (send (symbol-to-instance-name ?gen681) get-refers-to)
 (send (symbol-to-instance-name ?gen682) get-refers-to)
 (send (symbol-to-instance-name ?gen683) get-refers-to)
 ))
(defmethod iris32::le
             ((?gen684 SYMBOL
               (registerp ?current-argument))
 (?gen685 SYMBOL
               (registerp ?current-argument))
 (?gen686 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "le %s %s %s " ?gen684
 ?gen685
 ?gen686
 ))
(defmethod iris32::le
             ((?gen687 SYMBOL
               (registerp ?current-argument))
 (?gen688 SYMBOL
               (registerp ?current-argument))
 (?gen689 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "le %s %s %s " ?gen687
 ?gen688
 (send (symbol-to-instance-name ?gen689) get-refers-to)
 ))
(defmethod iris32::le
             ((?gen690 SYMBOL
               (registerp ?current-argument))
 (?gen691 SYMBOL
               (register-aliasp ?current-argument))
 (?gen692 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "le %s %s %s " ?gen690
 (send (symbol-to-instance-name ?gen691) get-refers-to)
 ?gen692
 ))
(defmethod iris32::le
             ((?gen693 SYMBOL
               (registerp ?current-argument))
 (?gen694 SYMBOL
               (register-aliasp ?current-argument))
 (?gen695 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "le %s %s %s " ?gen693
 (send (symbol-to-instance-name ?gen694) get-refers-to)
 (send (symbol-to-instance-name ?gen695) get-refers-to)
 ))
(defmethod iris32::le
             ((?gen696 SYMBOL
               (register-aliasp ?current-argument))
 (?gen697 SYMBOL
               (registerp ?current-argument))
 (?gen698 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "le %s %s %s " (send (symbol-to-instance-name ?gen696) get-refers-to)
 ?gen697
 ?gen698
 ))
(defmethod iris32::le
             ((?gen699 SYMBOL
               (register-aliasp ?current-argument))
 (?gen700 SYMBOL
               (registerp ?current-argument))
 (?gen701 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "le %s %s %s " (send (symbol-to-instance-name ?gen699) get-refers-to)
 ?gen700
 (send (symbol-to-instance-name ?gen701) get-refers-to)
 ))
(defmethod iris32::le
             ((?gen702 SYMBOL
               (register-aliasp ?current-argument))
 (?gen703 SYMBOL
               (register-aliasp ?current-argument))
 (?gen704 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "le %s %s %s " (send (symbol-to-instance-name ?gen702) get-refers-to)
 (send (symbol-to-instance-name ?gen703) get-refers-to)
 ?gen704
 ))
(defmethod iris32::le
             ((?gen705 SYMBOL
               (register-aliasp ?current-argument))
 (?gen706 SYMBOL
               (register-aliasp ?current-argument))
 (?gen707 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "le %s %s %s " (send (symbol-to-instance-name ?gen705) get-refers-to)
 (send (symbol-to-instance-name ?gen706) get-refers-to)
 (send (symbol-to-instance-name ?gen707) get-refers-to)
 ))
(defmethod iris32::ge
             ((?gen708 SYMBOL
               (registerp ?current-argument))
 (?gen709 SYMBOL
               (registerp ?current-argument))
 (?gen710 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ge %s %s %s " ?gen708
 ?gen709
 ?gen710
 ))
(defmethod iris32::ge
             ((?gen711 SYMBOL
               (registerp ?current-argument))
 (?gen712 SYMBOL
               (registerp ?current-argument))
 (?gen713 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ge %s %s %s " ?gen711
 ?gen712
 (send (symbol-to-instance-name ?gen713) get-refers-to)
 ))
(defmethod iris32::ge
             ((?gen714 SYMBOL
               (registerp ?current-argument))
 (?gen715 SYMBOL
               (register-aliasp ?current-argument))
 (?gen716 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ge %s %s %s " ?gen714
 (send (symbol-to-instance-name ?gen715) get-refers-to)
 ?gen716
 ))
(defmethod iris32::ge
             ((?gen717 SYMBOL
               (registerp ?current-argument))
 (?gen718 SYMBOL
               (register-aliasp ?current-argument))
 (?gen719 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ge %s %s %s " ?gen717
 (send (symbol-to-instance-name ?gen718) get-refers-to)
 (send (symbol-to-instance-name ?gen719) get-refers-to)
 ))
(defmethod iris32::ge
             ((?gen720 SYMBOL
               (register-aliasp ?current-argument))
 (?gen721 SYMBOL
               (registerp ?current-argument))
 (?gen722 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ge %s %s %s " (send (symbol-to-instance-name ?gen720) get-refers-to)
 ?gen721
 ?gen722
 ))
(defmethod iris32::ge
             ((?gen723 SYMBOL
               (register-aliasp ?current-argument))
 (?gen724 SYMBOL
               (registerp ?current-argument))
 (?gen725 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ge %s %s %s " (send (symbol-to-instance-name ?gen723) get-refers-to)
 ?gen724
 (send (symbol-to-instance-name ?gen725) get-refers-to)
 ))
(defmethod iris32::ge
             ((?gen726 SYMBOL
               (register-aliasp ?current-argument))
 (?gen727 SYMBOL
               (register-aliasp ?current-argument))
 (?gen728 SYMBOL
               (registerp ?current-argument))
 )
             (format nil "ge %s %s %s " (send (symbol-to-instance-name ?gen726) get-refers-to)
 (send (symbol-to-instance-name ?gen727) get-refers-to)
 ?gen728
 ))
(defmethod iris32::ge
             ((?gen729 SYMBOL
               (register-aliasp ?current-argument))
 (?gen730 SYMBOL
               (register-aliasp ?current-argument))
 (?gen731 SYMBOL
               (register-aliasp ?current-argument))
 )
             (format nil "ge %s %s %s " (send (symbol-to-instance-name ?gen729) get-refers-to)
 (send (symbol-to-instance-name ?gen730) get-refers-to)
 (send (symbol-to-instance-name ?gen731) get-refers-to)
 ))
