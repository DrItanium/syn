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
                                   cp sp lr ip)
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

(defgeneric iris32::system-op)
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
  ((?gen5 INSTANCE
          register)
   )
  (format nil "decr %s " (send ?gen5 get-refers-to)
          ))
(defmethod iris32::incr
  ((?gen6 SYMBOL
          (registerp ?current-argument))
   )
  (format nil "incr %s " ?gen6
          ))
(defmethod iris32::incr
  ((?gen7 INSTANCE
          register)
   )
  (format nil "incr %s " (send ?gen7 get-refers-to)
          ))
(defmethod iris32::double
  ((?gen8 SYMBOL
          (registerp ?current-argument))
   )
  (format nil "double %s " ?gen8
          ))
(defmethod iris32::double
  ((?gen9 INSTANCE
          register)
   )
  (format nil "double %s " (send ?gen9 get-refers-to)
          ))
(defmethod iris32::halve
  ((?gen10 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "halve %s " ?gen10
          ))
(defmethod iris32::halve
  ((?gen11 INSTANCE
           register)
   )
  (format nil "halve %s " (send ?gen11 get-refers-to)
          ))
(defmethod iris32::pop
  ((?gen12 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "pop %s " ?gen12
          ))
(defmethod iris32::pop
  ((?gen13 INSTANCE
           register)
   )
  (format nil "pop %s " (send ?gen13 get-refers-to)
          ))
(defmethod iris32::push
  ((?gen14 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "push %s " ?gen14
          ))
(defmethod iris32::push
  ((?gen15 INSTANCE
           register)
   )
  (format nil "push %s " (send ?gen15 get-refers-to)
          ))
(defmethod iris32::j
  ((?gen16 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "j %s " ?gen16
          ))
(defmethod iris32::j
  ((?gen17 INSTANCE
           register)
   )
  (format nil "j %s " (send ?gen17 get-refers-to)
          ))
(defmethod iris32::setu
  ((?gen18 INSTANCE
           register)
   (?gen19 (immediatep ?current-argument))
   )
  (format nil "setu %s %s " (send ?gen18 get-refers-to)
          (str-cat ?gen19)
          ))
(defmethod iris32::setu
  ((?gen20 SYMBOL
           (registerp ?current-argument))
   (?gen21 (immediatep ?current-argument))
   )
  (format nil "setu %s %s " ?gen20
          (str-cat ?gen21)
          ))
(defmethod iris32::setl
  ((?gen22 INSTANCE
           register)
   (?gen23 (immediatep ?current-argument))
   )
  (format nil "setl %s %s " (send ?gen22 get-refers-to)
          (str-cat ?gen23)
          ))
(defmethod iris32::setl
  ((?gen24 SYMBOL
           (registerp ?current-argument))
   (?gen25 (immediatep ?current-argument))
   )
  (format nil "setl %s %s " ?gen24
          (str-cat ?gen25)
          ))
(defmethod iris32::lor
  ((?gen26 SYMBOL
           (registerp ?current-argument))
   (?gen27 SYMBOL
           (registerp ?current-argument))
   (?gen28 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "or %s %s %s " ?gen26
          ?gen27
          ?gen28
          ))
(defmethod iris32::lor
  ((?gen29 SYMBOL
           (registerp ?current-argument))
   (?gen30 SYMBOL
           (registerp ?current-argument))
   (?gen31 INSTANCE
           register)
   )
  (format nil "or %s %s %s " ?gen29
          ?gen30
          (send ?gen31 get-refers-to)
          ))
(defmethod iris32::lor
  ((?gen32 SYMBOL
           (registerp ?current-argument))
   (?gen33 INSTANCE
           register)
   (?gen34 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "or %s %s %s " ?gen32
          (send ?gen33 get-refers-to)
          ?gen34
          ))
(defmethod iris32::lor
  ((?gen35 SYMBOL
           (registerp ?current-argument))
   (?gen36 INSTANCE
           register)
   (?gen37 INSTANCE
           register)
   )
  (format nil "or %s %s %s " ?gen35
          (send ?gen36 get-refers-to)
          (send ?gen37 get-refers-to)
          ))
(defmethod iris32::lor
  ((?gen38 INSTANCE
           register)
   (?gen39 SYMBOL
           (registerp ?current-argument))
   (?gen40 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "or %s %s %s " (send ?gen38 get-refers-to)
          ?gen39
          ?gen40
          ))
(defmethod iris32::lor
  ((?gen41 INSTANCE
           register)
   (?gen42 SYMBOL
           (registerp ?current-argument))
   (?gen43 INSTANCE
           register)
   )
  (format nil "or %s %s %s " (send ?gen41 get-refers-to)
          ?gen42
          (send ?gen43 get-refers-to)
          ))
(defmethod iris32::lor
  ((?gen44 INSTANCE
           register)
   (?gen45 INSTANCE
           register)
   (?gen46 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "or %s %s %s " (send ?gen44 get-refers-to)
          (send ?gen45 get-refers-to)
          ?gen46
          ))
(defmethod iris32::lor
  ((?gen47 INSTANCE
           register)
   (?gen48 INSTANCE
           register)
   (?gen49 INSTANCE
           register)
   )
  (format nil "or %s %s %s " (send ?gen47 get-refers-to)
          (send ?gen48 get-refers-to)
          (send ?gen49 get-refers-to)
          ))
(defmethod iris32::land
  ((?gen50 SYMBOL
           (registerp ?current-argument))
   (?gen51 SYMBOL
           (registerp ?current-argument))
   (?gen52 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "and %s %s %s " ?gen50
          ?gen51
          ?gen52
          ))
(defmethod iris32::land
  ((?gen53 SYMBOL
           (registerp ?current-argument))
   (?gen54 SYMBOL
           (registerp ?current-argument))
   (?gen55 INSTANCE
           register)
   )
  (format nil "and %s %s %s " ?gen53
          ?gen54
          (send ?gen55 get-refers-to)
          ))
(defmethod iris32::land
  ((?gen56 SYMBOL
           (registerp ?current-argument))
   (?gen57 INSTANCE
           register)
   (?gen58 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "and %s %s %s " ?gen56
          (send ?gen57 get-refers-to)
          ?gen58
          ))
(defmethod iris32::land
  ((?gen59 SYMBOL
           (registerp ?current-argument))
   (?gen60 INSTANCE
           register)
   (?gen61 INSTANCE
           register)
   )
  (format nil "and %s %s %s " ?gen59
          (send ?gen60 get-refers-to)
          (send ?gen61 get-refers-to)
          ))
(defmethod iris32::land
  ((?gen62 INSTANCE
           register)
   (?gen63 SYMBOL
           (registerp ?current-argument))
   (?gen64 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "and %s %s %s " (send ?gen62 get-refers-to)
          ?gen63
          ?gen64
          ))
(defmethod iris32::land
  ((?gen65 INSTANCE
           register)
   (?gen66 SYMBOL
           (registerp ?current-argument))
   (?gen67 INSTANCE
           register)
   )
  (format nil "and %s %s %s " (send ?gen65 get-refers-to)
          ?gen66
          (send ?gen67 get-refers-to)
          ))
(defmethod iris32::land
  ((?gen68 INSTANCE
           register)
   (?gen69 INSTANCE
           register)
   (?gen70 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "and %s %s %s " (send ?gen68 get-refers-to)
          (send ?gen69 get-refers-to)
          ?gen70
          ))
(defmethod iris32::land
  ((?gen71 INSTANCE
           register)
   (?gen72 INSTANCE
           register)
   (?gen73 INSTANCE
           register)
   )
  (format nil "and %s %s %s " (send ?gen71 get-refers-to)
          (send ?gen72 get-refers-to)
          (send ?gen73 get-refers-to)
          ))
(defmethod iris32::cmp_eq
  ((?gen74 SYMBOL
           (registerp ?current-argument))
   (?gen75 SYMBOL
           (registerp ?current-argument))
   (?gen76 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "eq %s %s %s " ?gen74
          ?gen75
          ?gen76
          ))
(defmethod iris32::cmp_eq
  ((?gen77 SYMBOL
           (registerp ?current-argument))
   (?gen78 SYMBOL
           (registerp ?current-argument))
   (?gen79 INSTANCE
           register)
   )
  (format nil "eq %s %s %s " ?gen77
          ?gen78
          (send ?gen79 get-refers-to)
          ))
(defmethod iris32::cmp_eq
  ((?gen80 SYMBOL
           (registerp ?current-argument))
   (?gen81 INSTANCE
           register)
   (?gen82 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "eq %s %s %s " ?gen80
          (send ?gen81 get-refers-to)
          ?gen82
          ))
(defmethod iris32::cmp_eq
  ((?gen83 SYMBOL
           (registerp ?current-argument))
   (?gen84 INSTANCE
           register)
   (?gen85 INSTANCE
           register)
   )
  (format nil "eq %s %s %s " ?gen83
          (send ?gen84 get-refers-to)
          (send ?gen85 get-refers-to)
          ))
(defmethod iris32::cmp_eq
  ((?gen86 INSTANCE
           register)
   (?gen87 SYMBOL
           (registerp ?current-argument))
   (?gen88 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "eq %s %s %s " (send ?gen86 get-refers-to)
          ?gen87
          ?gen88
          ))
(defmethod iris32::cmp_eq
  ((?gen89 INSTANCE
           register)
   (?gen90 SYMBOL
           (registerp ?current-argument))
   (?gen91 INSTANCE
           register)
   )
  (format nil "eq %s %s %s " (send ?gen89 get-refers-to)
          ?gen90
          (send ?gen91 get-refers-to)
          ))
(defmethod iris32::cmp_eq
  ((?gen92 INSTANCE
           register)
   (?gen93 INSTANCE
           register)
   (?gen94 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "eq %s %s %s " (send ?gen92 get-refers-to)
          (send ?gen93 get-refers-to)
          ?gen94
          ))
(defmethod iris32::cmp_eq
  ((?gen95 INSTANCE
           register)
   (?gen96 INSTANCE
           register)
   (?gen97 INSTANCE
           register)
   )
  (format nil "eq %s %s %s " (send ?gen95 get-refers-to)
          (send ?gen96 get-refers-to)
          (send ?gen97 get-refers-to)
          ))
(defmethod iris32::lnot
  ((?gen98 SYMBOL
           (registerp ?current-argument))
   (?gen99 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "not %s %s " ?gen98
          ?gen99
          ))
(defmethod iris32::lnot
  ((?gen100 SYMBOL
            (registerp ?current-argument))
   (?gen101 INSTANCE
            register)
   )
  (format nil "not %s %s " ?gen100
          (send ?gen101 get-refers-to)
          ))
(defmethod iris32::lnot
  ((?gen102 INSTANCE
            register)
   (?gen103 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "not %s %s " (send ?gen102 get-refers-to)
          ?gen103
          ))
(defmethod iris32::lnot
  ((?gen104 INSTANCE
            register)
   (?gen105 INSTANCE
            register)
   )
  (format nil "not %s %s " (send ?gen104 get-refers-to)
          (send ?gen105 get-refers-to)
          ))
(defmethod iris32::move
  ((?gen106 SYMBOL
            (registerp ?current-argument))
   (?gen107 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "move %s %s " ?gen106
          ?gen107
          ))
(defmethod iris32::move
  ((?gen108 SYMBOL
            (registerp ?current-argument))
   (?gen109 INSTANCE
            register)
   )
  (format nil "move %s %s " ?gen108
          (send ?gen109 get-refers-to)
          ))
(defmethod iris32::move
  ((?gen110 INSTANCE
            register)
   (?gen111 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "move %s %s " (send ?gen110 get-refers-to)
          ?gen111
          ))
(defmethod iris32::move
  ((?gen112 INSTANCE
            register)
   (?gen113 INSTANCE
            register)
   )
  (format nil "move %s %s " (send ?gen112 get-refers-to)
          (send ?gen113 get-refers-to)
          ))
(defmethod iris32::swap
  ((?gen114 SYMBOL
            (registerp ?current-argument))
   (?gen115 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "swap %s %s " ?gen114
          ?gen115
          ))
(defmethod iris32::swap
  ((?gen116 SYMBOL
            (registerp ?current-argument))
   (?gen117 INSTANCE
            register)
   )
  (format nil "swap %s %s " ?gen116
          (send ?gen117 get-refers-to)
          ))
(defmethod iris32::swap
  ((?gen118 INSTANCE
            register)
   (?gen119 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "swap %s %s " (send ?gen118 get-refers-to)
          ?gen119
          ))
(defmethod iris32::swap
  ((?gen120 INSTANCE
            register)
   (?gen121 INSTANCE
            register)
   )
  (format nil "swap %s %s " (send ?gen120 get-refers-to)
          (send ?gen121 get-refers-to)
          ))
(defmethod iris32::ld
  ((?gen122 SYMBOL
            (registerp ?current-argument))
   (?gen123 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ld %s %s " ?gen122
          ?gen123
          ))
(defmethod iris32::ld
  ((?gen124 SYMBOL
            (registerp ?current-argument))
   (?gen125 INSTANCE
            register)
   )
  (format nil "ld %s %s " ?gen124
          (send ?gen125 get-refers-to)
          ))
(defmethod iris32::ld
  ((?gen126 INSTANCE
            register)
   (?gen127 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ld %s %s " (send ?gen126 get-refers-to)
          ?gen127
          ))
(defmethod iris32::ld
  ((?gen128 INSTANCE
            register)
   (?gen129 INSTANCE
            register)
   )
  (format nil "ld %s %s " (send ?gen128 get-refers-to)
          (send ?gen129 get-refers-to)
          ))
(defmethod iris32::st
  ((?gen130 SYMBOL
            (registerp ?current-argument))
   (?gen131 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "st %s %s " ?gen130
          ?gen131
          ))
(defmethod iris32::st
  ((?gen132 SYMBOL
            (registerp ?current-argument))
   (?gen133 INSTANCE
            register)
   )
  (format nil "st %s %s " ?gen132
          (send ?gen133 get-refers-to)
          ))
(defmethod iris32::st
  ((?gen134 INSTANCE
            register)
   (?gen135 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "st %s %s " (send ?gen134 get-refers-to)
          ?gen135
          ))
(defmethod iris32::st
  ((?gen136 INSTANCE
            register)
   (?gen137 INSTANCE
            register)
   )
  (format nil "st %s %s " (send ?gen136 get-refers-to)
          (send ?gen137 get-refers-to)
          ))
(defmethod iris32::jl
  ((?gen138 SYMBOL
            (registerp ?current-argument))
   (?gen139 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jl %s %s " ?gen138
          ?gen139
          ))
(defmethod iris32::jl
  ((?gen140 SYMBOL
            (registerp ?current-argument))
   (?gen141 INSTANCE
            register)
   )
  (format nil "jl %s %s " ?gen140
          (send ?gen141 get-refers-to)
          ))
(defmethod iris32::jl
  ((?gen142 INSTANCE
            register)
   (?gen143 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jl %s %s " (send ?gen142 get-refers-to)
          ?gen143
          ))
(defmethod iris32::jl
  ((?gen144 INSTANCE
            register)
   (?gen145 INSTANCE
            register)
   )
  (format nil "jl %s %s " (send ?gen144 get-refers-to)
          (send ?gen145 get-refers-to)
          ))
(defmethod iris32::jt
  ((?gen146 SYMBOL
            (registerp ?current-argument))
   (?gen147 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jt %s %s " ?gen146
          ?gen147
          ))
(defmethod iris32::jt
  ((?gen148 SYMBOL
            (registerp ?current-argument))
   (?gen149 INSTANCE
            register)
   )
  (format nil "jt %s %s " ?gen148
          (send ?gen149 get-refers-to)
          ))
(defmethod iris32::jt
  ((?gen150 INSTANCE
            register)
   (?gen151 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jt %s %s " (send ?gen150 get-refers-to)
          ?gen151
          ))
(defmethod iris32::jt
  ((?gen152 INSTANCE
            register)
   (?gen153 INSTANCE
            register)
   )
  (format nil "jt %s %s " (send ?gen152 get-refers-to)
          (send ?gen153 get-refers-to)
          ))
(defmethod iris32::jf
  ((?gen154 SYMBOL
            (registerp ?current-argument))
   (?gen155 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jf %s %s " ?gen154
          ?gen155
          ))
(defmethod iris32::jf
  ((?gen156 SYMBOL
            (registerp ?current-argument))
   (?gen157 INSTANCE
            register)
   )
  (format nil "jf %s %s " ?gen156
          (send ?gen157 get-refers-to)
          ))
(defmethod iris32::jf
  ((?gen158 INSTANCE
            register)
   (?gen159 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jf %s %s " (send ?gen158 get-refers-to)
          ?gen159
          ))
(defmethod iris32::jf
  ((?gen160 INSTANCE
            register)
   (?gen161 INSTANCE
            register)
   )
  (format nil "jf %s %s " (send ?gen160 get-refers-to)
          (send ?gen161 get-refers-to)
          ))
(defmethod iris32::addi
  ((?gen162 SYMBOL
            (registerp ?current-argument))
   (?gen163 SYMBOL
            (registerp ?current-argument))
   (?gen164 (immediatep ?current-argument))
   )
  (format nil "addi %s %s %s " ?gen162
          ?gen163
          (str-cat ?gen164)
          ))
(defmethod iris32::addi
  ((?gen165 SYMBOL
            (registerp ?current-argument))
   (?gen166 INSTANCE
            register)
   (?gen167 (immediatep ?current-argument))
   )
  (format nil "addi %s %s %s " ?gen165
          (send ?gen166 get-refers-to)
          (str-cat ?gen167)
          ))
(defmethod iris32::addi
  ((?gen168 INSTANCE
            register)
   (?gen169 SYMBOL
            (registerp ?current-argument))
   (?gen170 (immediatep ?current-argument))
   )
  (format nil "addi %s %s %s " (send ?gen168 get-refers-to)
          ?gen169
          (str-cat ?gen170)
          ))
(defmethod iris32::addi
  ((?gen171 INSTANCE
            register)
   (?gen172 INSTANCE
            register)
   (?gen173 (immediatep ?current-argument))
   )
  (format nil "addi %s %s %s " (send ?gen171 get-refers-to)
          (send ?gen172 get-refers-to)
          (str-cat ?gen173)
          ))
(defmethod iris32::subi
  ((?gen174 SYMBOL
            (registerp ?current-argument))
   (?gen175 SYMBOL
            (registerp ?current-argument))
   (?gen176 (immediatep ?current-argument))
   )
  (format nil "subi %s %s %s " ?gen174
          ?gen175
          (str-cat ?gen176)
          ))
(defmethod iris32::subi
  ((?gen177 SYMBOL
            (registerp ?current-argument))
   (?gen178 INSTANCE
            register)
   (?gen179 (immediatep ?current-argument))
   )
  (format nil "subi %s %s %s " ?gen177
          (send ?gen178 get-refers-to)
          (str-cat ?gen179)
          ))
(defmethod iris32::subi
  ((?gen180 INSTANCE
            register)
   (?gen181 SYMBOL
            (registerp ?current-argument))
   (?gen182 (immediatep ?current-argument))
   )
  (format nil "subi %s %s %s " (send ?gen180 get-refers-to)
          ?gen181
          (str-cat ?gen182)
          ))
(defmethod iris32::subi
  ((?gen183 INSTANCE
            register)
   (?gen184 INSTANCE
            register)
   (?gen185 (immediatep ?current-argument))
   )
  (format nil "subi %s %s %s " (send ?gen183 get-refers-to)
          (send ?gen184 get-refers-to)
          (str-cat ?gen185)
          ))
(defmethod iris32::muli
  ((?gen186 SYMBOL
            (registerp ?current-argument))
   (?gen187 SYMBOL
            (registerp ?current-argument))
   (?gen188 (immediatep ?current-argument))
   )
  (format nil "muli %s %s %s " ?gen186
          ?gen187
          (str-cat ?gen188)
          ))
(defmethod iris32::muli
  ((?gen189 SYMBOL
            (registerp ?current-argument))
   (?gen190 INSTANCE
            register)
   (?gen191 (immediatep ?current-argument))
   )
  (format nil "muli %s %s %s " ?gen189
          (send ?gen190 get-refers-to)
          (str-cat ?gen191)
          ))
(defmethod iris32::muli
  ((?gen192 INSTANCE
            register)
   (?gen193 SYMBOL
            (registerp ?current-argument))
   (?gen194 (immediatep ?current-argument))
   )
  (format nil "muli %s %s %s " (send ?gen192 get-refers-to)
          ?gen193
          (str-cat ?gen194)
          ))
(defmethod iris32::muli
  ((?gen195 INSTANCE
            register)
   (?gen196 INSTANCE
            register)
   (?gen197 (immediatep ?current-argument))
   )
  (format nil "muli %s %s %s " (send ?gen195 get-refers-to)
          (send ?gen196 get-refers-to)
          (str-cat ?gen197)
          ))
(defmethod iris32::divi
  ((?gen198 SYMBOL
            (registerp ?current-argument))
   (?gen199 SYMBOL
            (registerp ?current-argument))
   (?gen200 (immediatep ?current-argument))
   )
  (format nil "divi %s %s %s " ?gen198
          ?gen199
          (str-cat ?gen200)
          ))
(defmethod iris32::divi
  ((?gen201 SYMBOL
            (registerp ?current-argument))
   (?gen202 INSTANCE
            register)
   (?gen203 (immediatep ?current-argument))
   )
  (format nil "divi %s %s %s " ?gen201
          (send ?gen202 get-refers-to)
          (str-cat ?gen203)
          ))
(defmethod iris32::divi
  ((?gen204 INSTANCE
            register)
   (?gen205 SYMBOL
            (registerp ?current-argument))
   (?gen206 (immediatep ?current-argument))
   )
  (format nil "divi %s %s %s " (send ?gen204 get-refers-to)
          ?gen205
          (str-cat ?gen206)
          ))
(defmethod iris32::divi
  ((?gen207 INSTANCE
            register)
   (?gen208 INSTANCE
            register)
   (?gen209 (immediatep ?current-argument))
   )
  (format nil "divi %s %s %s " (send ?gen207 get-refers-to)
          (send ?gen208 get-refers-to)
          (str-cat ?gen209)
          ))
(defmethod iris32::remi
  ((?gen210 SYMBOL
            (registerp ?current-argument))
   (?gen211 SYMBOL
            (registerp ?current-argument))
   (?gen212 (immediatep ?current-argument))
   )
  (format nil "remi %s %s %s " ?gen210
          ?gen211
          (str-cat ?gen212)
          ))
(defmethod iris32::remi
  ((?gen213 SYMBOL
            (registerp ?current-argument))
   (?gen214 INSTANCE
            register)
   (?gen215 (immediatep ?current-argument))
   )
  (format nil "remi %s %s %s " ?gen213
          (send ?gen214 get-refers-to)
          (str-cat ?gen215)
          ))
(defmethod iris32::remi
  ((?gen216 INSTANCE
            register)
   (?gen217 SYMBOL
            (registerp ?current-argument))
   (?gen218 (immediatep ?current-argument))
   )
  (format nil "remi %s %s %s " (send ?gen216 get-refers-to)
          ?gen217
          (str-cat ?gen218)
          ))
(defmethod iris32::remi
  ((?gen219 INSTANCE
            register)
   (?gen220 INSTANCE
            register)
   (?gen221 (immediatep ?current-argument))
   )
  (format nil "remi %s %s %s " (send ?gen219 get-refers-to)
          (send ?gen220 get-refers-to)
          (str-cat ?gen221)
          ))
(defmethod iris32::shli
  ((?gen222 SYMBOL
            (registerp ?current-argument))
   (?gen223 SYMBOL
            (registerp ?current-argument))
   (?gen224 (immediatep ?current-argument))
   )
  (format nil "shli %s %s %s " ?gen222
          ?gen223
          (str-cat ?gen224)
          ))
(defmethod iris32::shli
  ((?gen225 SYMBOL
            (registerp ?current-argument))
   (?gen226 INSTANCE
            register)
   (?gen227 (immediatep ?current-argument))
   )
  (format nil "shli %s %s %s " ?gen225
          (send ?gen226 get-refers-to)
          (str-cat ?gen227)
          ))
(defmethod iris32::shli
  ((?gen228 INSTANCE
            register)
   (?gen229 SYMBOL
            (registerp ?current-argument))
   (?gen230 (immediatep ?current-argument))
   )
  (format nil "shli %s %s %s " (send ?gen228 get-refers-to)
          ?gen229
          (str-cat ?gen230)
          ))
(defmethod iris32::shli
  ((?gen231 INSTANCE
            register)
   (?gen232 INSTANCE
            register)
   (?gen233 (immediatep ?current-argument))
   )
  (format nil "shli %s %s %s " (send ?gen231 get-refers-to)
          (send ?gen232 get-refers-to)
          (str-cat ?gen233)
          ))
(defmethod iris32::shri
  ((?gen234 SYMBOL
            (registerp ?current-argument))
   (?gen235 SYMBOL
            (registerp ?current-argument))
   (?gen236 (immediatep ?current-argument))
   )
  (format nil "shri %s %s %s " ?gen234
          ?gen235
          (str-cat ?gen236)
          ))
(defmethod iris32::shri
  ((?gen237 SYMBOL
            (registerp ?current-argument))
   (?gen238 INSTANCE
            register)
   (?gen239 (immediatep ?current-argument))
   )
  (format nil "shri %s %s %s " ?gen237
          (send ?gen238 get-refers-to)
          (str-cat ?gen239)
          ))
(defmethod iris32::shri
  ((?gen240 INSTANCE
            register)
   (?gen241 SYMBOL
            (registerp ?current-argument))
   (?gen242 (immediatep ?current-argument))
   )
  (format nil "shri %s %s %s " (send ?gen240 get-refers-to)
          ?gen241
          (str-cat ?gen242)
          ))
(defmethod iris32::shri
  ((?gen243 INSTANCE
            register)
   (?gen244 INSTANCE
            register)
   (?gen245 (immediatep ?current-argument))
   )
  (format nil "shri %s %s %s " (send ?gen243 get-refers-to)
          (send ?gen244 get-refers-to)
          (str-cat ?gen245)
          ))
(defmethod iris32::eqi
  ((?gen246 SYMBOL
            (registerp ?current-argument))
   (?gen247 SYMBOL
            (registerp ?current-argument))
   (?gen248 (immediatep ?current-argument))
   )
  (format nil "eqi %s %s %s " ?gen246
          ?gen247
          (str-cat ?gen248)
          ))
(defmethod iris32::eqi
  ((?gen249 SYMBOL
            (registerp ?current-argument))
   (?gen250 INSTANCE
            register)
   (?gen251 (immediatep ?current-argument))
   )
  (format nil "eqi %s %s %s " ?gen249
          (send ?gen250 get-refers-to)
          (str-cat ?gen251)
          ))
(defmethod iris32::eqi
  ((?gen252 INSTANCE
            register)
   (?gen253 SYMBOL
            (registerp ?current-argument))
   (?gen254 (immediatep ?current-argument))
   )
  (format nil "eqi %s %s %s " (send ?gen252 get-refers-to)
          ?gen253
          (str-cat ?gen254)
          ))
(defmethod iris32::eqi
  ((?gen255 INSTANCE
            register)
   (?gen256 INSTANCE
            register)
   (?gen257 (immediatep ?current-argument))
   )
  (format nil "eqi %s %s %s " (send ?gen255 get-refers-to)
          (send ?gen256 get-refers-to)
          (str-cat ?gen257)
          ))
(defmethod iris32::nei
  ((?gen258 SYMBOL
            (registerp ?current-argument))
   (?gen259 SYMBOL
            (registerp ?current-argument))
   (?gen260 (immediatep ?current-argument))
   )
  (format nil "nei %s %s %s " ?gen258
          ?gen259
          (str-cat ?gen260)
          ))
(defmethod iris32::nei
  ((?gen261 SYMBOL
            (registerp ?current-argument))
   (?gen262 INSTANCE
            register)
   (?gen263 (immediatep ?current-argument))
   )
  (format nil "nei %s %s %s " ?gen261
          (send ?gen262 get-refers-to)
          (str-cat ?gen263)
          ))
(defmethod iris32::nei
  ((?gen264 INSTANCE
            register)
   (?gen265 SYMBOL
            (registerp ?current-argument))
   (?gen266 (immediatep ?current-argument))
   )
  (format nil "nei %s %s %s " (send ?gen264 get-refers-to)
          ?gen265
          (str-cat ?gen266)
          ))
(defmethod iris32::nei
  ((?gen267 INSTANCE
            register)
   (?gen268 INSTANCE
            register)
   (?gen269 (immediatep ?current-argument))
   )
  (format nil "nei %s %s %s " (send ?gen267 get-refers-to)
          (send ?gen268 get-refers-to)
          (str-cat ?gen269)
          ))
(defmethod iris32::lti
  ((?gen270 SYMBOL
            (registerp ?current-argument))
   (?gen271 SYMBOL
            (registerp ?current-argument))
   (?gen272 (immediatep ?current-argument))
   )
  (format nil "lti %s %s %s " ?gen270
          ?gen271
          (str-cat ?gen272)
          ))
(defmethod iris32::lti
  ((?gen273 SYMBOL
            (registerp ?current-argument))
   (?gen274 INSTANCE
            register)
   (?gen275 (immediatep ?current-argument))
   )
  (format nil "lti %s %s %s " ?gen273
          (send ?gen274 get-refers-to)
          (str-cat ?gen275)
          ))
(defmethod iris32::lti
  ((?gen276 INSTANCE
            register)
   (?gen277 SYMBOL
            (registerp ?current-argument))
   (?gen278 (immediatep ?current-argument))
   )
  (format nil "lti %s %s %s " (send ?gen276 get-refers-to)
          ?gen277
          (str-cat ?gen278)
          ))
(defmethod iris32::lti
  ((?gen279 INSTANCE
            register)
   (?gen280 INSTANCE
            register)
   (?gen281 (immediatep ?current-argument))
   )
  (format nil "lti %s %s %s " (send ?gen279 get-refers-to)
          (send ?gen280 get-refers-to)
          (str-cat ?gen281)
          ))
(defmethod iris32::lei
  ((?gen282 SYMBOL
            (registerp ?current-argument))
   (?gen283 SYMBOL
            (registerp ?current-argument))
   (?gen284 (immediatep ?current-argument))
   )
  (format nil "lei %s %s %s " ?gen282
          ?gen283
          (str-cat ?gen284)
          ))
(defmethod iris32::lei
  ((?gen285 SYMBOL
            (registerp ?current-argument))
   (?gen286 INSTANCE
            register)
   (?gen287 (immediatep ?current-argument))
   )
  (format nil "lei %s %s %s " ?gen285
          (send ?gen286 get-refers-to)
          (str-cat ?gen287)
          ))
(defmethod iris32::lei
  ((?gen288 INSTANCE
            register)
   (?gen289 SYMBOL
            (registerp ?current-argument))
   (?gen290 (immediatep ?current-argument))
   )
  (format nil "lei %s %s %s " (send ?gen288 get-refers-to)
          ?gen289
          (str-cat ?gen290)
          ))
(defmethod iris32::lei
  ((?gen291 INSTANCE
            register)
   (?gen292 INSTANCE
            register)
   (?gen293 (immediatep ?current-argument))
   )
  (format nil "lei %s %s %s " (send ?gen291 get-refers-to)
          (send ?gen292 get-refers-to)
          (str-cat ?gen293)
          ))
(defmethod iris32::gei
  ((?gen294 SYMBOL
            (registerp ?current-argument))
   (?gen295 SYMBOL
            (registerp ?current-argument))
   (?gen296 (immediatep ?current-argument))
   )
  (format nil "gei %s %s %s " ?gen294
          ?gen295
          (str-cat ?gen296)
          ))
(defmethod iris32::gei
  ((?gen297 SYMBOL
            (registerp ?current-argument))
   (?gen298 INSTANCE
            register)
   (?gen299 (immediatep ?current-argument))
   )
  (format nil "gei %s %s %s " ?gen297
          (send ?gen298 get-refers-to)
          (str-cat ?gen299)
          ))
(defmethod iris32::gei
  ((?gen300 INSTANCE
            register)
   (?gen301 SYMBOL
            (registerp ?current-argument))
   (?gen302 (immediatep ?current-argument))
   )
  (format nil "gei %s %s %s " (send ?gen300 get-refers-to)
          ?gen301
          (str-cat ?gen302)
          ))
(defmethod iris32::gei
  ((?gen303 INSTANCE
            register)
   (?gen304 INSTANCE
            register)
   (?gen305 (immediatep ?current-argument))
   )
  (format nil "gei %s %s %s " (send ?gen303 get-refers-to)
          (send ?gen304 get-refers-to)
          (str-cat ?gen305)
          ))
(defmethod iris32::add
  ((?gen306 SYMBOL
            (registerp ?current-argument))
   (?gen307 SYMBOL
            (registerp ?current-argument))
   (?gen308 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "add %s %s %s " ?gen306
          ?gen307
          ?gen308
          ))
(defmethod iris32::add
  ((?gen309 SYMBOL
            (registerp ?current-argument))
   (?gen310 SYMBOL
            (registerp ?current-argument))
   (?gen311 INSTANCE
            register)
   )
  (format nil "add %s %s %s " ?gen309
          ?gen310
          (send ?gen311 get-refers-to)
          ))
(defmethod iris32::add
  ((?gen312 SYMBOL
            (registerp ?current-argument))
   (?gen313 INSTANCE
            register)
   (?gen314 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "add %s %s %s " ?gen312
          (send ?gen313 get-refers-to)
          ?gen314
          ))
(defmethod iris32::add
  ((?gen315 SYMBOL
            (registerp ?current-argument))
   (?gen316 INSTANCE
            register)
   (?gen317 INSTANCE
            register)
   )
  (format nil "add %s %s %s " ?gen315
          (send ?gen316 get-refers-to)
          (send ?gen317 get-refers-to)
          ))
(defmethod iris32::add
  ((?gen318 INSTANCE
            register)
   (?gen319 SYMBOL
            (registerp ?current-argument))
   (?gen320 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "add %s %s %s " (send ?gen318 get-refers-to)
          ?gen319
          ?gen320
          ))
(defmethod iris32::add
  ((?gen321 INSTANCE
            register)
   (?gen322 SYMBOL
            (registerp ?current-argument))
   (?gen323 INSTANCE
            register)
   )
  (format nil "add %s %s %s " (send ?gen321 get-refers-to)
          ?gen322
          (send ?gen323 get-refers-to)
          ))
(defmethod iris32::add
  ((?gen324 INSTANCE
            register)
   (?gen325 INSTANCE
            register)
   (?gen326 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "add %s %s %s " (send ?gen324 get-refers-to)
          (send ?gen325 get-refers-to)
          ?gen326
          ))
(defmethod iris32::add
  ((?gen327 INSTANCE
            register)
   (?gen328 INSTANCE
            register)
   (?gen329 INSTANCE
            register)
   )
  (format nil "add %s %s %s " (send ?gen327 get-refers-to)
          (send ?gen328 get-refers-to)
          (send ?gen329 get-refers-to)
          ))
(defmethod iris32::sub
  ((?gen330 SYMBOL
            (registerp ?current-argument))
   (?gen331 SYMBOL
            (registerp ?current-argument))
   (?gen332 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "sub %s %s %s " ?gen330
          ?gen331
          ?gen332
          ))
(defmethod iris32::sub
  ((?gen333 SYMBOL
            (registerp ?current-argument))
   (?gen334 SYMBOL
            (registerp ?current-argument))
   (?gen335 INSTANCE
            register)
   )
  (format nil "sub %s %s %s " ?gen333
          ?gen334
          (send ?gen335 get-refers-to)
          ))
(defmethod iris32::sub
  ((?gen336 SYMBOL
            (registerp ?current-argument))
   (?gen337 INSTANCE
            register)
   (?gen338 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "sub %s %s %s " ?gen336
          (send ?gen337 get-refers-to)
          ?gen338
          ))
(defmethod iris32::sub
  ((?gen339 SYMBOL
            (registerp ?current-argument))
   (?gen340 INSTANCE
            register)
   (?gen341 INSTANCE
            register)
   )
  (format nil "sub %s %s %s " ?gen339
          (send ?gen340 get-refers-to)
          (send ?gen341 get-refers-to)
          ))
(defmethod iris32::sub
  ((?gen342 INSTANCE
            register)
   (?gen343 SYMBOL
            (registerp ?current-argument))
   (?gen344 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "sub %s %s %s " (send ?gen342 get-refers-to)
          ?gen343
          ?gen344
          ))
(defmethod iris32::sub
  ((?gen345 INSTANCE
            register)
   (?gen346 SYMBOL
            (registerp ?current-argument))
   (?gen347 INSTANCE
            register)
   )
  (format nil "sub %s %s %s " (send ?gen345 get-refers-to)
          ?gen346
          (send ?gen347 get-refers-to)
          ))
(defmethod iris32::sub
  ((?gen348 INSTANCE
            register)
   (?gen349 INSTANCE
            register)
   (?gen350 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "sub %s %s %s " (send ?gen348 get-refers-to)
          (send ?gen349 get-refers-to)
          ?gen350
          ))
(defmethod iris32::sub
  ((?gen351 INSTANCE
            register)
   (?gen352 INSTANCE
            register)
   (?gen353 INSTANCE
            register)
   )
  (format nil "sub %s %s %s " (send ?gen351 get-refers-to)
          (send ?gen352 get-refers-to)
          (send ?gen353 get-refers-to)
          ))
(defmethod iris32::mul
  ((?gen354 SYMBOL
            (registerp ?current-argument))
   (?gen355 SYMBOL
            (registerp ?current-argument))
   (?gen356 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "mul %s %s %s " ?gen354
          ?gen355
          ?gen356
          ))
(defmethod iris32::mul
  ((?gen357 SYMBOL
            (registerp ?current-argument))
   (?gen358 SYMBOL
            (registerp ?current-argument))
   (?gen359 INSTANCE
            register)
   )
  (format nil "mul %s %s %s " ?gen357
          ?gen358
          (send ?gen359 get-refers-to)
          ))
(defmethod iris32::mul
  ((?gen360 SYMBOL
            (registerp ?current-argument))
   (?gen361 INSTANCE
            register)
   (?gen362 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "mul %s %s %s " ?gen360
          (send ?gen361 get-refers-to)
          ?gen362
          ))
(defmethod iris32::mul
  ((?gen363 SYMBOL
            (registerp ?current-argument))
   (?gen364 INSTANCE
            register)
   (?gen365 INSTANCE
            register)
   )
  (format nil "mul %s %s %s " ?gen363
          (send ?gen364 get-refers-to)
          (send ?gen365 get-refers-to)
          ))
(defmethod iris32::mul
  ((?gen366 INSTANCE
            register)
   (?gen367 SYMBOL
            (registerp ?current-argument))
   (?gen368 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "mul %s %s %s " (send ?gen366 get-refers-to)
          ?gen367
          ?gen368
          ))
(defmethod iris32::mul
  ((?gen369 INSTANCE
            register)
   (?gen370 SYMBOL
            (registerp ?current-argument))
   (?gen371 INSTANCE
            register)
   )
  (format nil "mul %s %s %s " (send ?gen369 get-refers-to)
          ?gen370
          (send ?gen371 get-refers-to)
          ))
(defmethod iris32::mul
  ((?gen372 INSTANCE
            register)
   (?gen373 INSTANCE
            register)
   (?gen374 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "mul %s %s %s " (send ?gen372 get-refers-to)
          (send ?gen373 get-refers-to)
          ?gen374
          ))
(defmethod iris32::mul
  ((?gen375 INSTANCE
            register)
   (?gen376 INSTANCE
            register)
   (?gen377 INSTANCE
            register)
   )
  (format nil "mul %s %s %s " (send ?gen375 get-refers-to)
          (send ?gen376 get-refers-to)
          (send ?gen377 get-refers-to)
          ))
(defmethod iris32::div
  ((?gen378 SYMBOL
            (registerp ?current-argument))
   (?gen379 SYMBOL
            (registerp ?current-argument))
   (?gen380 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "div %s %s %s " ?gen378
          ?gen379
          ?gen380
          ))
(defmethod iris32::div
  ((?gen381 SYMBOL
            (registerp ?current-argument))
   (?gen382 SYMBOL
            (registerp ?current-argument))
   (?gen383 INSTANCE
            register)
   )
  (format nil "div %s %s %s " ?gen381
          ?gen382
          (send ?gen383 get-refers-to)
          ))
(defmethod iris32::div
  ((?gen384 SYMBOL
            (registerp ?current-argument))
   (?gen385 INSTANCE
            register)
   (?gen386 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "div %s %s %s " ?gen384
          (send ?gen385 get-refers-to)
          ?gen386
          ))
(defmethod iris32::div
  ((?gen387 SYMBOL
            (registerp ?current-argument))
   (?gen388 INSTANCE
            register)
   (?gen389 INSTANCE
            register)
   )
  (format nil "div %s %s %s " ?gen387
          (send ?gen388 get-refers-to)
          (send ?gen389 get-refers-to)
          ))
(defmethod iris32::div
  ((?gen390 INSTANCE
            register)
   (?gen391 SYMBOL
            (registerp ?current-argument))
   (?gen392 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "div %s %s %s " (send ?gen390 get-refers-to)
          ?gen391
          ?gen392
          ))
(defmethod iris32::div
  ((?gen393 INSTANCE
            register)
   (?gen394 SYMBOL
            (registerp ?current-argument))
   (?gen395 INSTANCE
            register)
   )
  (format nil "div %s %s %s " (send ?gen393 get-refers-to)
          ?gen394
          (send ?gen395 get-refers-to)
          ))
(defmethod iris32::div
  ((?gen396 INSTANCE
            register)
   (?gen397 INSTANCE
            register)
   (?gen398 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "div %s %s %s " (send ?gen396 get-refers-to)
          (send ?gen397 get-refers-to)
          ?gen398
          ))
(defmethod iris32::div
  ((?gen399 INSTANCE
            register)
   (?gen400 INSTANCE
            register)
   (?gen401 INSTANCE
            register)
   )
  (format nil "div %s %s %s " (send ?gen399 get-refers-to)
          (send ?gen400 get-refers-to)
          (send ?gen401 get-refers-to)
          ))
(defmethod iris32::rem
  ((?gen402 SYMBOL
            (registerp ?current-argument))
   (?gen403 SYMBOL
            (registerp ?current-argument))
   (?gen404 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "rem %s %s %s " ?gen402
          ?gen403
          ?gen404
          ))
(defmethod iris32::rem
  ((?gen405 SYMBOL
            (registerp ?current-argument))
   (?gen406 SYMBOL
            (registerp ?current-argument))
   (?gen407 INSTANCE
            register)
   )
  (format nil "rem %s %s %s " ?gen405
          ?gen406
          (send ?gen407 get-refers-to)
          ))
(defmethod iris32::rem
  ((?gen408 SYMBOL
            (registerp ?current-argument))
   (?gen409 INSTANCE
            register)
   (?gen410 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "rem %s %s %s " ?gen408
          (send ?gen409 get-refers-to)
          ?gen410
          ))
(defmethod iris32::rem
  ((?gen411 SYMBOL
            (registerp ?current-argument))
   (?gen412 INSTANCE
            register)
   (?gen413 INSTANCE
            register)
   )
  (format nil "rem %s %s %s " ?gen411
          (send ?gen412 get-refers-to)
          (send ?gen413 get-refers-to)
          ))
(defmethod iris32::rem
  ((?gen414 INSTANCE
            register)
   (?gen415 SYMBOL
            (registerp ?current-argument))
   (?gen416 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "rem %s %s %s " (send ?gen414 get-refers-to)
          ?gen415
          ?gen416
          ))
(defmethod iris32::rem
  ((?gen417 INSTANCE
            register)
   (?gen418 SYMBOL
            (registerp ?current-argument))
   (?gen419 INSTANCE
            register)
   )
  (format nil "rem %s %s %s " (send ?gen417 get-refers-to)
          ?gen418
          (send ?gen419 get-refers-to)
          ))
(defmethod iris32::rem
  ((?gen420 INSTANCE
            register)
   (?gen421 INSTANCE
            register)
   (?gen422 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "rem %s %s %s " (send ?gen420 get-refers-to)
          (send ?gen421 get-refers-to)
          ?gen422
          ))
(defmethod iris32::rem
  ((?gen423 INSTANCE
            register)
   (?gen424 INSTANCE
            register)
   (?gen425 INSTANCE
            register)
   )
  (format nil "rem %s %s %s " (send ?gen423 get-refers-to)
          (send ?gen424 get-refers-to)
          (send ?gen425 get-refers-to)
          ))
(defmethod iris32::shl
  ((?gen426 SYMBOL
            (registerp ?current-argument))
   (?gen427 SYMBOL
            (registerp ?current-argument))
   (?gen428 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shl %s %s %s " ?gen426
          ?gen427
          ?gen428
          ))
(defmethod iris32::shl
  ((?gen429 SYMBOL
            (registerp ?current-argument))
   (?gen430 SYMBOL
            (registerp ?current-argument))
   (?gen431 INSTANCE
            register)
   )
  (format nil "shl %s %s %s " ?gen429
          ?gen430
          (send ?gen431 get-refers-to)
          ))
(defmethod iris32::shl
  ((?gen432 SYMBOL
            (registerp ?current-argument))
   (?gen433 INSTANCE
            register)
   (?gen434 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shl %s %s %s " ?gen432
          (send ?gen433 get-refers-to)
          ?gen434
          ))
(defmethod iris32::shl
  ((?gen435 SYMBOL
            (registerp ?current-argument))
   (?gen436 INSTANCE
            register)
   (?gen437 INSTANCE
            register)
   )
  (format nil "shl %s %s %s " ?gen435
          (send ?gen436 get-refers-to)
          (send ?gen437 get-refers-to)
          ))
(defmethod iris32::shl
  ((?gen438 INSTANCE
            register)
   (?gen439 SYMBOL
            (registerp ?current-argument))
   (?gen440 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shl %s %s %s " (send ?gen438 get-refers-to)
          ?gen439
          ?gen440
          ))
(defmethod iris32::shl
  ((?gen441 INSTANCE
            register)
   (?gen442 SYMBOL
            (registerp ?current-argument))
   (?gen443 INSTANCE
            register)
   )
  (format nil "shl %s %s %s " (send ?gen441 get-refers-to)
          ?gen442
          (send ?gen443 get-refers-to)
          ))
(defmethod iris32::shl
  ((?gen444 INSTANCE
            register)
   (?gen445 INSTANCE
            register)
   (?gen446 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shl %s %s %s " (send ?gen444 get-refers-to)
          (send ?gen445 get-refers-to)
          ?gen446
          ))
(defmethod iris32::shl
  ((?gen447 INSTANCE
            register)
   (?gen448 INSTANCE
            register)
   (?gen449 INSTANCE
            register)
   )
  (format nil "shl %s %s %s " (send ?gen447 get-refers-to)
          (send ?gen448 get-refers-to)
          (send ?gen449 get-refers-to)
          ))
(defmethod iris32::shr
  ((?gen450 SYMBOL
            (registerp ?current-argument))
   (?gen451 SYMBOL
            (registerp ?current-argument))
   (?gen452 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shr %s %s %s " ?gen450
          ?gen451
          ?gen452
          ))
(defmethod iris32::shr
  ((?gen453 SYMBOL
            (registerp ?current-argument))
   (?gen454 SYMBOL
            (registerp ?current-argument))
   (?gen455 INSTANCE
            register)
   )
  (format nil "shr %s %s %s " ?gen453
          ?gen454
          (send ?gen455 get-refers-to)
          ))
(defmethod iris32::shr
  ((?gen456 SYMBOL
            (registerp ?current-argument))
   (?gen457 INSTANCE
            register)
   (?gen458 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shr %s %s %s " ?gen456
          (send ?gen457 get-refers-to)
          ?gen458
          ))
(defmethod iris32::shr
  ((?gen459 SYMBOL
            (registerp ?current-argument))
   (?gen460 INSTANCE
            register)
   (?gen461 INSTANCE
            register)
   )
  (format nil "shr %s %s %s " ?gen459
          (send ?gen460 get-refers-to)
          (send ?gen461 get-refers-to)
          ))
(defmethod iris32::shr
  ((?gen462 INSTANCE
            register)
   (?gen463 SYMBOL
            (registerp ?current-argument))
   (?gen464 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shr %s %s %s " (send ?gen462 get-refers-to)
          ?gen463
          ?gen464
          ))
(defmethod iris32::shr
  ((?gen465 INSTANCE
            register)
   (?gen466 SYMBOL
            (registerp ?current-argument))
   (?gen467 INSTANCE
            register)
   )
  (format nil "shr %s %s %s " (send ?gen465 get-refers-to)
          ?gen466
          (send ?gen467 get-refers-to)
          ))
(defmethod iris32::shr
  ((?gen468 INSTANCE
            register)
   (?gen469 INSTANCE
            register)
   (?gen470 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shr %s %s %s " (send ?gen468 get-refers-to)
          (send ?gen469 get-refers-to)
          ?gen470
          ))
(defmethod iris32::shr
  ((?gen471 INSTANCE
            register)
   (?gen472 INSTANCE
            register)
   (?gen473 INSTANCE
            register)
   )
  (format nil "shr %s %s %s " (send ?gen471 get-refers-to)
          (send ?gen472 get-refers-to)
          (send ?gen473 get-refers-to)
          ))
(defmethod iris32::jtl
  ((?gen474 SYMBOL
            (registerp ?current-argument))
   (?gen475 SYMBOL
            (registerp ?current-argument))
   (?gen476 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jtl %s %s %s " ?gen474
          ?gen475
          ?gen476
          ))
(defmethod iris32::jtl
  ((?gen477 SYMBOL
            (registerp ?current-argument))
   (?gen478 SYMBOL
            (registerp ?current-argument))
   (?gen479 INSTANCE
            register)
   )
  (format nil "jtl %s %s %s " ?gen477
          ?gen478
          (send ?gen479 get-refers-to)
          ))
(defmethod iris32::jtl
  ((?gen480 SYMBOL
            (registerp ?current-argument))
   (?gen481 INSTANCE
            register)
   (?gen482 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jtl %s %s %s " ?gen480
          (send ?gen481 get-refers-to)
          ?gen482
          ))
(defmethod iris32::jtl
  ((?gen483 SYMBOL
            (registerp ?current-argument))
   (?gen484 INSTANCE
            register)
   (?gen485 INSTANCE
            register)
   )
  (format nil "jtl %s %s %s " ?gen483
          (send ?gen484 get-refers-to)
          (send ?gen485 get-refers-to)
          ))
(defmethod iris32::jtl
  ((?gen486 INSTANCE
            register)
   (?gen487 SYMBOL
            (registerp ?current-argument))
   (?gen488 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jtl %s %s %s " (send ?gen486 get-refers-to)
          ?gen487
          ?gen488
          ))
(defmethod iris32::jtl
  ((?gen489 INSTANCE
            register)
   (?gen490 SYMBOL
            (registerp ?current-argument))
   (?gen491 INSTANCE
            register)
   )
  (format nil "jtl %s %s %s " (send ?gen489 get-refers-to)
          ?gen490
          (send ?gen491 get-refers-to)
          ))
(defmethod iris32::jtl
  ((?gen492 INSTANCE
            register)
   (?gen493 INSTANCE
            register)
   (?gen494 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jtl %s %s %s " (send ?gen492 get-refers-to)
          (send ?gen493 get-refers-to)
          ?gen494
          ))
(defmethod iris32::jtl
  ((?gen495 INSTANCE
            register)
   (?gen496 INSTANCE
            register)
   (?gen497 INSTANCE
            register)
   )
  (format nil "jtl %s %s %s " (send ?gen495 get-refers-to)
          (send ?gen496 get-refers-to)
          (send ?gen497 get-refers-to)
          ))
(defmethod iris32::jfl
  ((?gen498 SYMBOL
            (registerp ?current-argument))
   (?gen499 SYMBOL
            (registerp ?current-argument))
   (?gen500 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jfl %s %s %s " ?gen498
          ?gen499
          ?gen500
          ))
(defmethod iris32::jfl
  ((?gen501 SYMBOL
            (registerp ?current-argument))
   (?gen502 SYMBOL
            (registerp ?current-argument))
   (?gen503 INSTANCE
            register)
   )
  (format nil "jfl %s %s %s " ?gen501
          ?gen502
          (send ?gen503 get-refers-to)
          ))
(defmethod iris32::jfl
  ((?gen504 SYMBOL
            (registerp ?current-argument))
   (?gen505 INSTANCE
            register)
   (?gen506 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jfl %s %s %s " ?gen504
          (send ?gen505 get-refers-to)
          ?gen506
          ))
(defmethod iris32::jfl
  ((?gen507 SYMBOL
            (registerp ?current-argument))
   (?gen508 INSTANCE
            register)
   (?gen509 INSTANCE
            register)
   )
  (format nil "jfl %s %s %s " ?gen507
          (send ?gen508 get-refers-to)
          (send ?gen509 get-refers-to)
          ))
(defmethod iris32::jfl
  ((?gen510 INSTANCE
            register)
   (?gen511 SYMBOL
            (registerp ?current-argument))
   (?gen512 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jfl %s %s %s " (send ?gen510 get-refers-to)
          ?gen511
          ?gen512
          ))
(defmethod iris32::jfl
  ((?gen513 INSTANCE
            register)
   (?gen514 SYMBOL
            (registerp ?current-argument))
   (?gen515 INSTANCE
            register)
   )
  (format nil "jfl %s %s %s " (send ?gen513 get-refers-to)
          ?gen514
          (send ?gen515 get-refers-to)
          ))
(defmethod iris32::jfl
  ((?gen516 INSTANCE
            register)
   (?gen517 INSTANCE
            register)
   (?gen518 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jfl %s %s %s " (send ?gen516 get-refers-to)
          (send ?gen517 get-refers-to)
          ?gen518
          ))
(defmethod iris32::jfl
  ((?gen519 INSTANCE
            register)
   (?gen520 INSTANCE
            register)
   (?gen521 INSTANCE
            register)
   )
  (format nil "jfl %s %s %s " (send ?gen519 get-refers-to)
          (send ?gen520 get-refers-to)
          (send ?gen521 get-refers-to)
          ))
(defmethod iris32::ift
  ((?gen522 SYMBOL
            (registerp ?current-argument))
   (?gen523 SYMBOL
            (registerp ?current-argument))
   (?gen524 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ift %s %s %s " ?gen522
          ?gen523
          ?gen524
          ))
(defmethod iris32::ift
  ((?gen525 SYMBOL
            (registerp ?current-argument))
   (?gen526 SYMBOL
            (registerp ?current-argument))
   (?gen527 INSTANCE
            register)
   )
  (format nil "ift %s %s %s " ?gen525
          ?gen526
          (send ?gen527 get-refers-to)
          ))
(defmethod iris32::ift
  ((?gen528 SYMBOL
            (registerp ?current-argument))
   (?gen529 INSTANCE
            register)
   (?gen530 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ift %s %s %s " ?gen528
          (send ?gen529 get-refers-to)
          ?gen530
          ))
(defmethod iris32::ift
  ((?gen531 SYMBOL
            (registerp ?current-argument))
   (?gen532 INSTANCE
            register)
   (?gen533 INSTANCE
            register)
   )
  (format nil "ift %s %s %s " ?gen531
          (send ?gen532 get-refers-to)
          (send ?gen533 get-refers-to)
          ))
(defmethod iris32::ift
  ((?gen534 INSTANCE
            register)
   (?gen535 SYMBOL
            (registerp ?current-argument))
   (?gen536 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ift %s %s %s " (send ?gen534 get-refers-to)
          ?gen535
          ?gen536
          ))
(defmethod iris32::ift
  ((?gen537 INSTANCE
            register)
   (?gen538 SYMBOL
            (registerp ?current-argument))
   (?gen539 INSTANCE
            register)
   )
  (format nil "ift %s %s %s " (send ?gen537 get-refers-to)
          ?gen538
          (send ?gen539 get-refers-to)
          ))
(defmethod iris32::ift
  ((?gen540 INSTANCE
            register)
   (?gen541 INSTANCE
            register)
   (?gen542 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ift %s %s %s " (send ?gen540 get-refers-to)
          (send ?gen541 get-refers-to)
          ?gen542
          ))
(defmethod iris32::ift
  ((?gen543 INSTANCE
            register)
   (?gen544 INSTANCE
            register)
   (?gen545 INSTANCE
            register)
   )
  (format nil "ift %s %s %s " (send ?gen543 get-refers-to)
          (send ?gen544 get-refers-to)
          (send ?gen545 get-refers-to)
          ))
(defmethod iris32::iff
  ((?gen546 SYMBOL
            (registerp ?current-argument))
   (?gen547 SYMBOL
            (registerp ?current-argument))
   (?gen548 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iff %s %s %s " ?gen546
          ?gen547
          ?gen548
          ))
(defmethod iris32::iff
  ((?gen549 SYMBOL
            (registerp ?current-argument))
   (?gen550 SYMBOL
            (registerp ?current-argument))
   (?gen551 INSTANCE
            register)
   )
  (format nil "iff %s %s %s " ?gen549
          ?gen550
          (send ?gen551 get-refers-to)
          ))
(defmethod iris32::iff
  ((?gen552 SYMBOL
            (registerp ?current-argument))
   (?gen553 INSTANCE
            register)
   (?gen554 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iff %s %s %s " ?gen552
          (send ?gen553 get-refers-to)
          ?gen554
          ))
(defmethod iris32::iff
  ((?gen555 SYMBOL
            (registerp ?current-argument))
   (?gen556 INSTANCE
            register)
   (?gen557 INSTANCE
            register)
   )
  (format nil "iff %s %s %s " ?gen555
          (send ?gen556 get-refers-to)
          (send ?gen557 get-refers-to)
          ))
(defmethod iris32::iff
  ((?gen558 INSTANCE
            register)
   (?gen559 SYMBOL
            (registerp ?current-argument))
   (?gen560 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iff %s %s %s " (send ?gen558 get-refers-to)
          ?gen559
          ?gen560
          ))
(defmethod iris32::iff
  ((?gen561 INSTANCE
            register)
   (?gen562 SYMBOL
            (registerp ?current-argument))
   (?gen563 INSTANCE
            register)
   )
  (format nil "iff %s %s %s " (send ?gen561 get-refers-to)
          ?gen562
          (send ?gen563 get-refers-to)
          ))
(defmethod iris32::iff
  ((?gen564 INSTANCE
            register)
   (?gen565 INSTANCE
            register)
   (?gen566 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iff %s %s %s " (send ?gen564 get-refers-to)
          (send ?gen565 get-refers-to)
          ?gen566
          ))
(defmethod iris32::iff
  ((?gen567 INSTANCE
            register)
   (?gen568 INSTANCE
            register)
   (?gen569 INSTANCE
            register)
   )
  (format nil "iff %s %s %s " (send ?gen567 get-refers-to)
          (send ?gen568 get-refers-to)
          (send ?gen569 get-refers-to)
          ))
(defmethod iris32::iftl
  ((?gen570 SYMBOL
            (registerp ?current-argument))
   (?gen571 SYMBOL
            (registerp ?current-argument))
   (?gen572 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iftl %s %s %s " ?gen570
          ?gen571
          ?gen572
          ))
(defmethod iris32::iftl
  ((?gen573 SYMBOL
            (registerp ?current-argument))
   (?gen574 SYMBOL
            (registerp ?current-argument))
   (?gen575 INSTANCE
            register)
   )
  (format nil "iftl %s %s %s " ?gen573
          ?gen574
          (send ?gen575 get-refers-to)
          ))
(defmethod iris32::iftl
  ((?gen576 SYMBOL
            (registerp ?current-argument))
   (?gen577 INSTANCE
            register)
   (?gen578 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iftl %s %s %s " ?gen576
          (send ?gen577 get-refers-to)
          ?gen578
          ))
(defmethod iris32::iftl
  ((?gen579 SYMBOL
            (registerp ?current-argument))
   (?gen580 INSTANCE
            register)
   (?gen581 INSTANCE
            register)
   )
  (format nil "iftl %s %s %s " ?gen579
          (send ?gen580 get-refers-to)
          (send ?gen581 get-refers-to)
          ))
(defmethod iris32::iftl
  ((?gen582 INSTANCE
            register)
   (?gen583 SYMBOL
            (registerp ?current-argument))
   (?gen584 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iftl %s %s %s " (send ?gen582 get-refers-to)
          ?gen583
          ?gen584
          ))
(defmethod iris32::iftl
  ((?gen585 INSTANCE
            register)
   (?gen586 SYMBOL
            (registerp ?current-argument))
   (?gen587 INSTANCE
            register)
   )
  (format nil "iftl %s %s %s " (send ?gen585 get-refers-to)
          ?gen586
          (send ?gen587 get-refers-to)
          ))
(defmethod iris32::iftl
  ((?gen588 INSTANCE
            register)
   (?gen589 INSTANCE
            register)
   (?gen590 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iftl %s %s %s " (send ?gen588 get-refers-to)
          (send ?gen589 get-refers-to)
          ?gen590
          ))
(defmethod iris32::iftl
  ((?gen591 INSTANCE
            register)
   (?gen592 INSTANCE
            register)
   (?gen593 INSTANCE
            register)
   )
  (format nil "iftl %s %s %s " (send ?gen591 get-refers-to)
          (send ?gen592 get-refers-to)
          (send ?gen593 get-refers-to)
          ))
(defmethod iris32::iffl
  ((?gen594 SYMBOL
            (registerp ?current-argument))
   (?gen595 SYMBOL
            (registerp ?current-argument))
   (?gen596 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iffl %s %s %s " ?gen594
          ?gen595
          ?gen596
          ))
(defmethod iris32::iffl
  ((?gen597 SYMBOL
            (registerp ?current-argument))
   (?gen598 SYMBOL
            (registerp ?current-argument))
   (?gen599 INSTANCE
            register)
   )
  (format nil "iffl %s %s %s " ?gen597
          ?gen598
          (send ?gen599 get-refers-to)
          ))
(defmethod iris32::iffl
  ((?gen600 SYMBOL
            (registerp ?current-argument))
   (?gen601 INSTANCE
            register)
   (?gen602 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iffl %s %s %s " ?gen600
          (send ?gen601 get-refers-to)
          ?gen602
          ))
(defmethod iris32::iffl
  ((?gen603 SYMBOL
            (registerp ?current-argument))
   (?gen604 INSTANCE
            register)
   (?gen605 INSTANCE
            register)
   )
  (format nil "iffl %s %s %s " ?gen603
          (send ?gen604 get-refers-to)
          (send ?gen605 get-refers-to)
          ))
(defmethod iris32::iffl
  ((?gen606 INSTANCE
            register)
   (?gen607 SYMBOL
            (registerp ?current-argument))
   (?gen608 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iffl %s %s %s " (send ?gen606 get-refers-to)
          ?gen607
          ?gen608
          ))
(defmethod iris32::iffl
  ((?gen609 INSTANCE
            register)
   (?gen610 SYMBOL
            (registerp ?current-argument))
   (?gen611 INSTANCE
            register)
   )
  (format nil "iffl %s %s %s " (send ?gen609 get-refers-to)
          ?gen610
          (send ?gen611 get-refers-to)
          ))
(defmethod iris32::iffl
  ((?gen612 INSTANCE
            register)
   (?gen613 INSTANCE
            register)
   (?gen614 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iffl %s %s %s " (send ?gen612 get-refers-to)
          (send ?gen613 get-refers-to)
          ?gen614
          ))
(defmethod iris32::iffl
  ((?gen615 INSTANCE
            register)
   (?gen616 INSTANCE
            register)
   (?gen617 INSTANCE
            register)
   )
  (format nil "iffl %s %s %s " (send ?gen615 get-refers-to)
          (send ?gen616 get-refers-to)
          (send ?gen617 get-refers-to)
          ))
(defmethod iris32::ne
  ((?gen618 SYMBOL
            (registerp ?current-argument))
   (?gen619 SYMBOL
            (registerp ?current-argument))
   (?gen620 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ne %s %s %s " ?gen618
          ?gen619
          ?gen620
          ))
(defmethod iris32::ne
  ((?gen621 SYMBOL
            (registerp ?current-argument))
   (?gen622 SYMBOL
            (registerp ?current-argument))
   (?gen623 INSTANCE
            register)
   )
  (format nil "ne %s %s %s " ?gen621
          ?gen622
          (send ?gen623 get-refers-to)
          ))
(defmethod iris32::ne
  ((?gen624 SYMBOL
            (registerp ?current-argument))
   (?gen625 INSTANCE
            register)
   (?gen626 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ne %s %s %s " ?gen624
          (send ?gen625 get-refers-to)
          ?gen626
          ))
(defmethod iris32::ne
  ((?gen627 SYMBOL
            (registerp ?current-argument))
   (?gen628 INSTANCE
            register)
   (?gen629 INSTANCE
            register)
   )
  (format nil "ne %s %s %s " ?gen627
          (send ?gen628 get-refers-to)
          (send ?gen629 get-refers-to)
          ))
(defmethod iris32::ne
  ((?gen630 INSTANCE
            register)
   (?gen631 SYMBOL
            (registerp ?current-argument))
   (?gen632 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ne %s %s %s " (send ?gen630 get-refers-to)
          ?gen631
          ?gen632
          ))
(defmethod iris32::ne
  ((?gen633 INSTANCE
            register)
   (?gen634 SYMBOL
            (registerp ?current-argument))
   (?gen635 INSTANCE
            register)
   )
  (format nil "ne %s %s %s " (send ?gen633 get-refers-to)
          ?gen634
          (send ?gen635 get-refers-to)
          ))
(defmethod iris32::ne
  ((?gen636 INSTANCE
            register)
   (?gen637 INSTANCE
            register)
   (?gen638 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ne %s %s %s " (send ?gen636 get-refers-to)
          (send ?gen637 get-refers-to)
          ?gen638
          ))
(defmethod iris32::ne
  ((?gen639 INSTANCE
            register)
   (?gen640 INSTANCE
            register)
   (?gen641 INSTANCE
            register)
   )
  (format nil "ne %s %s %s " (send ?gen639 get-refers-to)
          (send ?gen640 get-refers-to)
          (send ?gen641 get-refers-to)
          ))
(defmethod iris32::lt
  ((?gen642 SYMBOL
            (registerp ?current-argument))
   (?gen643 SYMBOL
            (registerp ?current-argument))
   (?gen644 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "lt %s %s %s " ?gen642
          ?gen643
          ?gen644
          ))
(defmethod iris32::lt
  ((?gen645 SYMBOL
            (registerp ?current-argument))
   (?gen646 SYMBOL
            (registerp ?current-argument))
   (?gen647 INSTANCE
            register)
   )
  (format nil "lt %s %s %s " ?gen645
          ?gen646
          (send ?gen647 get-refers-to)
          ))
(defmethod iris32::lt
  ((?gen648 SYMBOL
            (registerp ?current-argument))
   (?gen649 INSTANCE
            register)
   (?gen650 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "lt %s %s %s " ?gen648
          (send ?gen649 get-refers-to)
          ?gen650
          ))
(defmethod iris32::lt
  ((?gen651 SYMBOL
            (registerp ?current-argument))
   (?gen652 INSTANCE
            register)
   (?gen653 INSTANCE
            register)
   )
  (format nil "lt %s %s %s " ?gen651
          (send ?gen652 get-refers-to)
          (send ?gen653 get-refers-to)
          ))
(defmethod iris32::lt
  ((?gen654 INSTANCE
            register)
   (?gen655 SYMBOL
            (registerp ?current-argument))
   (?gen656 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "lt %s %s %s " (send ?gen654 get-refers-to)
          ?gen655
          ?gen656
          ))
(defmethod iris32::lt
  ((?gen657 INSTANCE
            register)
   (?gen658 SYMBOL
            (registerp ?current-argument))
   (?gen659 INSTANCE
            register)
   )
  (format nil "lt %s %s %s " (send ?gen657 get-refers-to)
          ?gen658
          (send ?gen659 get-refers-to)
          ))
(defmethod iris32::lt
  ((?gen660 INSTANCE
            register)
   (?gen661 INSTANCE
            register)
   (?gen662 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "lt %s %s %s " (send ?gen660 get-refers-to)
          (send ?gen661 get-refers-to)
          ?gen662
          ))
(defmethod iris32::lt
  ((?gen663 INSTANCE
            register)
   (?gen664 INSTANCE
            register)
   (?gen665 INSTANCE
            register)
   )
  (format nil "lt %s %s %s " (send ?gen663 get-refers-to)
          (send ?gen664 get-refers-to)
          (send ?gen665 get-refers-to)
          ))
(defmethod iris32::gt
  ((?gen666 SYMBOL
            (registerp ?current-argument))
   (?gen667 SYMBOL
            (registerp ?current-argument))
   (?gen668 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "gt %s %s %s " ?gen666
          ?gen667
          ?gen668
          ))
(defmethod iris32::gt
  ((?gen669 SYMBOL
            (registerp ?current-argument))
   (?gen670 SYMBOL
            (registerp ?current-argument))
   (?gen671 INSTANCE
            register)
   )
  (format nil "gt %s %s %s " ?gen669
          ?gen670
          (send ?gen671 get-refers-to)
          ))
(defmethod iris32::gt
  ((?gen672 SYMBOL
            (registerp ?current-argument))
   (?gen673 INSTANCE
            register)
   (?gen674 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "gt %s %s %s " ?gen672
          (send ?gen673 get-refers-to)
          ?gen674
          ))
(defmethod iris32::gt
  ((?gen675 SYMBOL
            (registerp ?current-argument))
   (?gen676 INSTANCE
            register)
   (?gen677 INSTANCE
            register)
   )
  (format nil "gt %s %s %s " ?gen675
          (send ?gen676 get-refers-to)
          (send ?gen677 get-refers-to)
          ))
(defmethod iris32::gt
  ((?gen678 INSTANCE
            register)
   (?gen679 SYMBOL
            (registerp ?current-argument))
   (?gen680 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "gt %s %s %s " (send ?gen678 get-refers-to)
          ?gen679
          ?gen680
          ))
(defmethod iris32::gt
  ((?gen681 INSTANCE
            register)
   (?gen682 SYMBOL
            (registerp ?current-argument))
   (?gen683 INSTANCE
            register)
   )
  (format nil "gt %s %s %s " (send ?gen681 get-refers-to)
          ?gen682
          (send ?gen683 get-refers-to)
          ))
(defmethod iris32::gt
  ((?gen684 INSTANCE
            register)
   (?gen685 INSTANCE
            register)
   (?gen686 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "gt %s %s %s " (send ?gen684 get-refers-to)
          (send ?gen685 get-refers-to)
          ?gen686
          ))
(defmethod iris32::gt
  ((?gen687 INSTANCE
            register)
   (?gen688 INSTANCE
            register)
   (?gen689 INSTANCE
            register)
   )
  (format nil "gt %s %s %s " (send ?gen687 get-refers-to)
          (send ?gen688 get-refers-to)
          (send ?gen689 get-refers-to)
          ))
(defmethod iris32::le
  ((?gen690 SYMBOL
            (registerp ?current-argument))
   (?gen691 SYMBOL
            (registerp ?current-argument))
   (?gen692 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "le %s %s %s " ?gen690
          ?gen691
          ?gen692
          ))
(defmethod iris32::le
  ((?gen693 SYMBOL
            (registerp ?current-argument))
   (?gen694 SYMBOL
            (registerp ?current-argument))
   (?gen695 INSTANCE
            register)
   )
  (format nil "le %s %s %s " ?gen693
          ?gen694
          (send ?gen695 get-refers-to)
          ))
(defmethod iris32::le
  ((?gen696 SYMBOL
            (registerp ?current-argument))
   (?gen697 INSTANCE
            register)
   (?gen698 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "le %s %s %s " ?gen696
          (send ?gen697 get-refers-to)
          ?gen698
          ))
(defmethod iris32::le
  ((?gen699 SYMBOL
            (registerp ?current-argument))
   (?gen700 INSTANCE
            register)
   (?gen701 INSTANCE
            register)
   )
  (format nil "le %s %s %s " ?gen699
          (send ?gen700 get-refers-to)
          (send ?gen701 get-refers-to)
          ))
(defmethod iris32::le
  ((?gen702 INSTANCE
            register)
   (?gen703 SYMBOL
            (registerp ?current-argument))
   (?gen704 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "le %s %s %s " (send ?gen702 get-refers-to)
          ?gen703
          ?gen704
          ))
(defmethod iris32::le
  ((?gen705 INSTANCE
            register)
   (?gen706 SYMBOL
            (registerp ?current-argument))
   (?gen707 INSTANCE
            register)
   )
  (format nil "le %s %s %s " (send ?gen705 get-refers-to)
          ?gen706
          (send ?gen707 get-refers-to)
          ))
(defmethod iris32::le
  ((?gen708 INSTANCE
            register)
   (?gen709 INSTANCE
            register)
   (?gen710 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "le %s %s %s " (send ?gen708 get-refers-to)
          (send ?gen709 get-refers-to)
          ?gen710
          ))
(defmethod iris32::le
  ((?gen711 INSTANCE
            register)
   (?gen712 INSTANCE
            register)
   (?gen713 INSTANCE
            register)
   )
  (format nil "le %s %s %s " (send ?gen711 get-refers-to)
          (send ?gen712 get-refers-to)
          (send ?gen713 get-refers-to)
          ))
(defmethod iris32::ge
  ((?gen714 SYMBOL
            (registerp ?current-argument))
   (?gen715 SYMBOL
            (registerp ?current-argument))
   (?gen716 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ge %s %s %s " ?gen714
          ?gen715
          ?gen716
          ))
(defmethod iris32::ge
  ((?gen717 SYMBOL
            (registerp ?current-argument))
   (?gen718 SYMBOL
            (registerp ?current-argument))
   (?gen719 INSTANCE
            register)
   )
  (format nil "ge %s %s %s " ?gen717
          ?gen718
          (send ?gen719 get-refers-to)
          ))
(defmethod iris32::ge
  ((?gen720 SYMBOL
            (registerp ?current-argument))
   (?gen721 INSTANCE
            register)
   (?gen722 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ge %s %s %s " ?gen720
          (send ?gen721 get-refers-to)
          ?gen722
          ))
(defmethod iris32::ge
  ((?gen723 SYMBOL
            (registerp ?current-argument))
   (?gen724 INSTANCE
            register)
   (?gen725 INSTANCE
            register)
   )
  (format nil "ge %s %s %s " ?gen723
          (send ?gen724 get-refers-to)
          (send ?gen725 get-refers-to)
          ))
(defmethod iris32::ge
  ((?gen726 INSTANCE
            register)
   (?gen727 SYMBOL
            (registerp ?current-argument))
   (?gen728 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ge %s %s %s " (send ?gen726 get-refers-to)
          ?gen727
          ?gen728
          ))
(defmethod iris32::ge
  ((?gen729 INSTANCE
            register)
   (?gen730 SYMBOL
            (registerp ?current-argument))
   (?gen731 INSTANCE
            register)
   )
  (format nil "ge %s %s %s " (send ?gen729 get-refers-to)
          ?gen730
          (send ?gen731 get-refers-to)
          ))
(defmethod iris32::ge
  ((?gen732 INSTANCE
            register)
   (?gen733 INSTANCE
            register)
   (?gen734 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ge %s %s %s " (send ?gen732 get-refers-to)
          (send ?gen733 get-refers-to)
          ?gen734
          ))
(defmethod iris32::ge
  ((?gen735 INSTANCE
            register)
   (?gen736 INSTANCE
            register)
   (?gen737 INSTANCE
            register)
   )
  (format nil "ge %s %s %s " (send ?gen735 get-refers-to)
          (send ?gen736 get-refers-to)
          (send ?gen737 get-refers-to)
          ))
(defmethod iris32::system-op
  ((?code INTEGER
          (<= 0 ?current-argument 255))
   (?arg0 SYMBOL 
          (registerp ?current-argument))
   (?arg1 SYMBOL 
          (registerp ?current-argument)))
  (format nil "system %d %s %s" ?code ?arg0 ?arg1))

