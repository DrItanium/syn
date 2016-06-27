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
(defmethod iris32::system-op
  ((?code INTEGER
          (<= 0 ?current-argument 255))
   (?arg0 SYMBOL 
          (registerp ?current-argument))
   (?arg1 SYMBOL 
          (registerp ?current-argument)))
  (format nil "system %d %s %s" ?code ?arg0 ?arg1))


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
          (and (instance-existp (symbol-to-instance-name ?current-argument))
               (eq register
                   (class (symbol-to-instance-name ?current-argument)))))
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
          (and (instance-existp (symbol-to-instance-name ?current-argument))
               (eq register
                   (class (symbol-to-instance-name ?current-argument)))))
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
          (and (instance-existp (symbol-to-instance-name ?current-argument))
               (eq register
                   (class (symbol-to-instance-name ?current-argument)))))
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
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
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
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
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
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "push %s " (send (symbol-to-instance-name ?gen15) get-refers-to)
          ))
(defmethod iris32::j
  ((?gen16 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "j %s " ?gen16
          ))
(defmethod iris32::j
  ((?gen17 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "j %s " (send (symbol-to-instance-name ?gen17) get-refers-to)
          ))
(defmethod iris32::setu
  ((?gen18 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen19 (immediatep ?current-argument))
   )
  (format nil "setu %s %s " (send (symbol-to-instance-name ?gen18) get-refers-to)
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
  ((?gen22 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen23 (immediatep ?current-argument))
   )
  (format nil "setl %s %s " (send (symbol-to-instance-name ?gen22) get-refers-to)
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
   (?gen31 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "or %s %s %s " ?gen29
          ?gen30
          (send (symbol-to-instance-name ?gen31) get-refers-to)
          ))
(defmethod iris32::lor
  ((?gen32 SYMBOL
           (registerp ?current-argument))
   (?gen33 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen34 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "or %s %s %s " ?gen32
          (send (symbol-to-instance-name ?gen33) get-refers-to)
          ?gen34
          ))
(defmethod iris32::lor
  ((?gen35 SYMBOL
           (registerp ?current-argument))
   (?gen36 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen37 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "or %s %s %s " ?gen35
          (send (symbol-to-instance-name ?gen36) get-refers-to)
          (send (symbol-to-instance-name ?gen37) get-refers-to)
          ))
(defmethod iris32::lor
  ((?gen38 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen39 SYMBOL
           (registerp ?current-argument))
   (?gen40 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "or %s %s %s " (send (symbol-to-instance-name ?gen38) get-refers-to)
          ?gen39
          ?gen40
          ))
(defmethod iris32::lor
  ((?gen41 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen42 SYMBOL
           (registerp ?current-argument))
   (?gen43 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "or %s %s %s " (send (symbol-to-instance-name ?gen41) get-refers-to)
          ?gen42
          (send (symbol-to-instance-name ?gen43) get-refers-to)
          ))
(defmethod iris32::lor
  ((?gen44 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen45 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen46 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "or %s %s %s " (send (symbol-to-instance-name ?gen44) get-refers-to)
          (send (symbol-to-instance-name ?gen45) get-refers-to)
          ?gen46
          ))
(defmethod iris32::lor
  ((?gen47 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen48 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen49 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "or %s %s %s " (send (symbol-to-instance-name ?gen47) get-refers-to)
          (send (symbol-to-instance-name ?gen48) get-refers-to)
          (send (symbol-to-instance-name ?gen49) get-refers-to)
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
   (?gen55 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "and %s %s %s " ?gen53
          ?gen54
          (send (symbol-to-instance-name ?gen55) get-refers-to)
          ))
(defmethod iris32::land
  ((?gen56 SYMBOL
           (registerp ?current-argument))
   (?gen57 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen58 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "and %s %s %s " ?gen56
          (send (symbol-to-instance-name ?gen57) get-refers-to)
          ?gen58
          ))
(defmethod iris32::land
  ((?gen59 SYMBOL
           (registerp ?current-argument))
   (?gen60 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen61 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "and %s %s %s " ?gen59
          (send (symbol-to-instance-name ?gen60) get-refers-to)
          (send (symbol-to-instance-name ?gen61) get-refers-to)
          ))
(defmethod iris32::land
  ((?gen62 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen63 SYMBOL
           (registerp ?current-argument))
   (?gen64 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "and %s %s %s " (send (symbol-to-instance-name ?gen62) get-refers-to)
          ?gen63
          ?gen64
          ))
(defmethod iris32::land
  ((?gen65 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen66 SYMBOL
           (registerp ?current-argument))
   (?gen67 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "and %s %s %s " (send (symbol-to-instance-name ?gen65) get-refers-to)
          ?gen66
          (send (symbol-to-instance-name ?gen67) get-refers-to)
          ))
(defmethod iris32::land
  ((?gen68 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen69 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen70 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "and %s %s %s " (send (symbol-to-instance-name ?gen68) get-refers-to)
          (send (symbol-to-instance-name ?gen69) get-refers-to)
          ?gen70
          ))
(defmethod iris32::land
  ((?gen71 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen72 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen73 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "and %s %s %s " (send (symbol-to-instance-name ?gen71) get-refers-to)
          (send (symbol-to-instance-name ?gen72) get-refers-to)
          (send (symbol-to-instance-name ?gen73) get-refers-to)
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
   (?gen79 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "eq %s %s %s " ?gen77
          ?gen78
          (send (symbol-to-instance-name ?gen79) get-refers-to)
          ))
(defmethod iris32::cmp_eq
  ((?gen80 SYMBOL
           (registerp ?current-argument))
   (?gen81 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen82 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "eq %s %s %s " ?gen80
          (send (symbol-to-instance-name ?gen81) get-refers-to)
          ?gen82
          ))
(defmethod iris32::cmp_eq
  ((?gen83 SYMBOL
           (registerp ?current-argument))
   (?gen84 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen85 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "eq %s %s %s " ?gen83
          (send (symbol-to-instance-name ?gen84) get-refers-to)
          (send (symbol-to-instance-name ?gen85) get-refers-to)
          ))
(defmethod iris32::cmp_eq
  ((?gen86 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen87 SYMBOL
           (registerp ?current-argument))
   (?gen88 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "eq %s %s %s " (send (symbol-to-instance-name ?gen86) get-refers-to)
          ?gen87
          ?gen88
          ))
(defmethod iris32::cmp_eq
  ((?gen89 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen90 SYMBOL
           (registerp ?current-argument))
   (?gen91 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "eq %s %s %s " (send (symbol-to-instance-name ?gen89) get-refers-to)
          ?gen90
          (send (symbol-to-instance-name ?gen91) get-refers-to)
          ))
(defmethod iris32::cmp_eq
  ((?gen92 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen93 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen94 SYMBOL
           (registerp ?current-argument))
   )
  (format nil "eq %s %s %s " (send (symbol-to-instance-name ?gen92) get-refers-to)
          (send (symbol-to-instance-name ?gen93) get-refers-to)
          ?gen94
          ))
(defmethod iris32::cmp_eq
  ((?gen95 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen96 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   (?gen97 SYMBOL
           (and (instance-existp (symbol-to-instance-name ?current-argument))
                (eq register
                    (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "eq %s %s %s " (send (symbol-to-instance-name ?gen95) get-refers-to)
          (send (symbol-to-instance-name ?gen96) get-refers-to)
          (send (symbol-to-instance-name ?gen97) get-refers-to)
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
   (?gen101 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "not %s %s " ?gen100
          (send (symbol-to-instance-name ?gen101) get-refers-to)
          ))
(defmethod iris32::lnot
  ((?gen102 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen103 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "not %s %s " (send (symbol-to-instance-name ?gen102) get-refers-to)
          ?gen103
          ))
(defmethod iris32::lnot
  ((?gen104 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen105 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "not %s %s " (send (symbol-to-instance-name ?gen104) get-refers-to)
          (send (symbol-to-instance-name ?gen105) get-refers-to)
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
   (?gen109 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "move %s %s " ?gen108
          (send (symbol-to-instance-name ?gen109) get-refers-to)
          ))
(defmethod iris32::move
  ((?gen110 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen111 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "move %s %s " (send (symbol-to-instance-name ?gen110) get-refers-to)
          ?gen111
          ))
(defmethod iris32::move
  ((?gen112 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen113 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "move %s %s " (send (symbol-to-instance-name ?gen112) get-refers-to)
          (send (symbol-to-instance-name ?gen113) get-refers-to)
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
   (?gen117 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "swap %s %s " ?gen116
          (send (symbol-to-instance-name ?gen117) get-refers-to)
          ))
(defmethod iris32::swap
  ((?gen118 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen119 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "swap %s %s " (send (symbol-to-instance-name ?gen118) get-refers-to)
          ?gen119
          ))
(defmethod iris32::swap
  ((?gen120 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen121 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "swap %s %s " (send (symbol-to-instance-name ?gen120) get-refers-to)
          (send (symbol-to-instance-name ?gen121) get-refers-to)
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
   (?gen125 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ld %s %s " ?gen124
          (send (symbol-to-instance-name ?gen125) get-refers-to)
          ))
(defmethod iris32::ld
  ((?gen126 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen127 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ld %s %s " (send (symbol-to-instance-name ?gen126) get-refers-to)
          ?gen127
          ))
(defmethod iris32::ld
  ((?gen128 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen129 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ld %s %s " (send (symbol-to-instance-name ?gen128) get-refers-to)
          (send (symbol-to-instance-name ?gen129) get-refers-to)
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
   (?gen133 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "st %s %s " ?gen132
          (send (symbol-to-instance-name ?gen133) get-refers-to)
          ))
(defmethod iris32::st
  ((?gen134 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen135 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "st %s %s " (send (symbol-to-instance-name ?gen134) get-refers-to)
          ?gen135
          ))
(defmethod iris32::st
  ((?gen136 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen137 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "st %s %s " (send (symbol-to-instance-name ?gen136) get-refers-to)
          (send (symbol-to-instance-name ?gen137) get-refers-to)
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
   (?gen141 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jl %s %s " ?gen140
          (send (symbol-to-instance-name ?gen141) get-refers-to)
          ))
(defmethod iris32::jl
  ((?gen142 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen143 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jl %s %s " (send (symbol-to-instance-name ?gen142) get-refers-to)
          ?gen143
          ))
(defmethod iris32::jl
  ((?gen144 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen145 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jl %s %s " (send (symbol-to-instance-name ?gen144) get-refers-to)
          (send (symbol-to-instance-name ?gen145) get-refers-to)
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
   (?gen149 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jt %s %s " ?gen148
          (send (symbol-to-instance-name ?gen149) get-refers-to)
          ))
(defmethod iris32::jt
  ((?gen150 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen151 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jt %s %s " (send (symbol-to-instance-name ?gen150) get-refers-to)
          ?gen151
          ))
(defmethod iris32::jt
  ((?gen152 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen153 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jt %s %s " (send (symbol-to-instance-name ?gen152) get-refers-to)
          (send (symbol-to-instance-name ?gen153) get-refers-to)
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
   (?gen157 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jf %s %s " ?gen156
          (send (symbol-to-instance-name ?gen157) get-refers-to)
          ))
(defmethod iris32::jf
  ((?gen158 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen159 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jf %s %s " (send (symbol-to-instance-name ?gen158) get-refers-to)
          ?gen159
          ))
(defmethod iris32::jf
  ((?gen160 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen161 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jf %s %s " (send (symbol-to-instance-name ?gen160) get-refers-to)
          (send (symbol-to-instance-name ?gen161) get-refers-to)
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
   (?gen166 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen167 (immediatep ?current-argument))
   )
  (format nil "addi %s %s %s " ?gen165
          (send (symbol-to-instance-name ?gen166) get-refers-to)
          (str-cat ?gen167)
          ))
(defmethod iris32::addi
  ((?gen168 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen169 SYMBOL
            (registerp ?current-argument))
   (?gen170 (immediatep ?current-argument))
   )
  (format nil "addi %s %s %s " (send (symbol-to-instance-name ?gen168) get-refers-to)
          ?gen169
          (str-cat ?gen170)
          ))
(defmethod iris32::addi
  ((?gen171 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen172 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen173 (immediatep ?current-argument))
   )
  (format nil "addi %s %s %s " (send (symbol-to-instance-name ?gen171) get-refers-to)
          (send (symbol-to-instance-name ?gen172) get-refers-to)
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
   (?gen178 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen179 (immediatep ?current-argument))
   )
  (format nil "subi %s %s %s " ?gen177
          (send (symbol-to-instance-name ?gen178) get-refers-to)
          (str-cat ?gen179)
          ))
(defmethod iris32::subi
  ((?gen180 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen181 SYMBOL
            (registerp ?current-argument))
   (?gen182 (immediatep ?current-argument))
   )
  (format nil "subi %s %s %s " (send (symbol-to-instance-name ?gen180) get-refers-to)
          ?gen181
          (str-cat ?gen182)
          ))
(defmethod iris32::subi
  ((?gen183 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen184 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen185 (immediatep ?current-argument))
   )
  (format nil "subi %s %s %s " (send (symbol-to-instance-name ?gen183) get-refers-to)
          (send (symbol-to-instance-name ?gen184) get-refers-to)
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
   (?gen190 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen191 (immediatep ?current-argument))
   )
  (format nil "muli %s %s %s " ?gen189
          (send (symbol-to-instance-name ?gen190) get-refers-to)
          (str-cat ?gen191)
          ))
(defmethod iris32::muli
  ((?gen192 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen193 SYMBOL
            (registerp ?current-argument))
   (?gen194 (immediatep ?current-argument))
   )
  (format nil "muli %s %s %s " (send (symbol-to-instance-name ?gen192) get-refers-to)
          ?gen193
          (str-cat ?gen194)
          ))
(defmethod iris32::muli
  ((?gen195 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen196 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen197 (immediatep ?current-argument))
   )
  (format nil "muli %s %s %s " (send (symbol-to-instance-name ?gen195) get-refers-to)
          (send (symbol-to-instance-name ?gen196) get-refers-to)
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
   (?gen202 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen203 (immediatep ?current-argument))
   )
  (format nil "divi %s %s %s " ?gen201
          (send (symbol-to-instance-name ?gen202) get-refers-to)
          (str-cat ?gen203)
          ))
(defmethod iris32::divi
  ((?gen204 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen205 SYMBOL
            (registerp ?current-argument))
   (?gen206 (immediatep ?current-argument))
   )
  (format nil "divi %s %s %s " (send (symbol-to-instance-name ?gen204) get-refers-to)
          ?gen205
          (str-cat ?gen206)
          ))
(defmethod iris32::divi
  ((?gen207 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen208 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen209 (immediatep ?current-argument))
   )
  (format nil "divi %s %s %s " (send (symbol-to-instance-name ?gen207) get-refers-to)
          (send (symbol-to-instance-name ?gen208) get-refers-to)
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
   (?gen214 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen215 (immediatep ?current-argument))
   )
  (format nil "remi %s %s %s " ?gen213
          (send (symbol-to-instance-name ?gen214) get-refers-to)
          (str-cat ?gen215)
          ))
(defmethod iris32::remi
  ((?gen216 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen217 SYMBOL
            (registerp ?current-argument))
   (?gen218 (immediatep ?current-argument))
   )
  (format nil "remi %s %s %s " (send (symbol-to-instance-name ?gen216) get-refers-to)
          ?gen217
          (str-cat ?gen218)
          ))
(defmethod iris32::remi
  ((?gen219 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen220 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen221 (immediatep ?current-argument))
   )
  (format nil "remi %s %s %s " (send (symbol-to-instance-name ?gen219) get-refers-to)
          (send (symbol-to-instance-name ?gen220) get-refers-to)
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
   (?gen226 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen227 (immediatep ?current-argument))
   )
  (format nil "shli %s %s %s " ?gen225
          (send (symbol-to-instance-name ?gen226) get-refers-to)
          (str-cat ?gen227)
          ))
(defmethod iris32::shli
  ((?gen228 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen229 SYMBOL
            (registerp ?current-argument))
   (?gen230 (immediatep ?current-argument))
   )
  (format nil "shli %s %s %s " (send (symbol-to-instance-name ?gen228) get-refers-to)
          ?gen229
          (str-cat ?gen230)
          ))
(defmethod iris32::shli
  ((?gen231 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen232 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen233 (immediatep ?current-argument))
   )
  (format nil "shli %s %s %s " (send (symbol-to-instance-name ?gen231) get-refers-to)
          (send (symbol-to-instance-name ?gen232) get-refers-to)
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
   (?gen238 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen239 (immediatep ?current-argument))
   )
  (format nil "shri %s %s %s " ?gen237
          (send (symbol-to-instance-name ?gen238) get-refers-to)
          (str-cat ?gen239)
          ))
(defmethod iris32::shri
  ((?gen240 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen241 SYMBOL
            (registerp ?current-argument))
   (?gen242 (immediatep ?current-argument))
   )
  (format nil "shri %s %s %s " (send (symbol-to-instance-name ?gen240) get-refers-to)
          ?gen241
          (str-cat ?gen242)
          ))
(defmethod iris32::shri
  ((?gen243 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen244 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen245 (immediatep ?current-argument))
   )
  (format nil "shri %s %s %s " (send (symbol-to-instance-name ?gen243) get-refers-to)
          (send (symbol-to-instance-name ?gen244) get-refers-to)
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
   (?gen250 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen251 (immediatep ?current-argument))
   )
  (format nil "eqi %s %s %s " ?gen249
          (send (symbol-to-instance-name ?gen250) get-refers-to)
          (str-cat ?gen251)
          ))
(defmethod iris32::eqi
  ((?gen252 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen253 SYMBOL
            (registerp ?current-argument))
   (?gen254 (immediatep ?current-argument))
   )
  (format nil "eqi %s %s %s " (send (symbol-to-instance-name ?gen252) get-refers-to)
          ?gen253
          (str-cat ?gen254)
          ))
(defmethod iris32::eqi
  ((?gen255 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen256 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen257 (immediatep ?current-argument))
   )
  (format nil "eqi %s %s %s " (send (symbol-to-instance-name ?gen255) get-refers-to)
          (send (symbol-to-instance-name ?gen256) get-refers-to)
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
   (?gen262 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen263 (immediatep ?current-argument))
   )
  (format nil "nei %s %s %s " ?gen261
          (send (symbol-to-instance-name ?gen262) get-refers-to)
          (str-cat ?gen263)
          ))
(defmethod iris32::nei
  ((?gen264 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen265 SYMBOL
            (registerp ?current-argument))
   (?gen266 (immediatep ?current-argument))
   )
  (format nil "nei %s %s %s " (send (symbol-to-instance-name ?gen264) get-refers-to)
          ?gen265
          (str-cat ?gen266)
          ))
(defmethod iris32::nei
  ((?gen267 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen268 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen269 (immediatep ?current-argument))
   )
  (format nil "nei %s %s %s " (send (symbol-to-instance-name ?gen267) get-refers-to)
          (send (symbol-to-instance-name ?gen268) get-refers-to)
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
   (?gen274 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen275 (immediatep ?current-argument))
   )
  (format nil "lti %s %s %s " ?gen273
          (send (symbol-to-instance-name ?gen274) get-refers-to)
          (str-cat ?gen275)
          ))
(defmethod iris32::lti
  ((?gen276 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen277 SYMBOL
            (registerp ?current-argument))
   (?gen278 (immediatep ?current-argument))
   )
  (format nil "lti %s %s %s " (send (symbol-to-instance-name ?gen276) get-refers-to)
          ?gen277
          (str-cat ?gen278)
          ))
(defmethod iris32::lti
  ((?gen279 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen280 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen281 (immediatep ?current-argument))
   )
  (format nil "lti %s %s %s " (send (symbol-to-instance-name ?gen279) get-refers-to)
          (send (symbol-to-instance-name ?gen280) get-refers-to)
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
   (?gen286 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen287 (immediatep ?current-argument))
   )
  (format nil "lei %s %s %s " ?gen285
          (send (symbol-to-instance-name ?gen286) get-refers-to)
          (str-cat ?gen287)
          ))
(defmethod iris32::lei
  ((?gen288 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen289 SYMBOL
            (registerp ?current-argument))
   (?gen290 (immediatep ?current-argument))
   )
  (format nil "lei %s %s %s " (send (symbol-to-instance-name ?gen288) get-refers-to)
          ?gen289
          (str-cat ?gen290)
          ))
(defmethod iris32::lei
  ((?gen291 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen292 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen293 (immediatep ?current-argument))
   )
  (format nil "lei %s %s %s " (send (symbol-to-instance-name ?gen291) get-refers-to)
          (send (symbol-to-instance-name ?gen292) get-refers-to)
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
   (?gen298 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen299 (immediatep ?current-argument))
   )
  (format nil "gei %s %s %s " ?gen297
          (send (symbol-to-instance-name ?gen298) get-refers-to)
          (str-cat ?gen299)
          ))
(defmethod iris32::gei
  ((?gen300 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen301 SYMBOL
            (registerp ?current-argument))
   (?gen302 (immediatep ?current-argument))
   )
  (format nil "gei %s %s %s " (send (symbol-to-instance-name ?gen300) get-refers-to)
          ?gen301
          (str-cat ?gen302)
          ))
(defmethod iris32::gei
  ((?gen303 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen304 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen305 (immediatep ?current-argument))
   )
  (format nil "gei %s %s %s " (send (symbol-to-instance-name ?gen303) get-refers-to)
          (send (symbol-to-instance-name ?gen304) get-refers-to)
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
   (?gen311 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "add %s %s %s " ?gen309
          ?gen310
          (send (symbol-to-instance-name ?gen311) get-refers-to)
          ))
(defmethod iris32::add
  ((?gen312 SYMBOL
            (registerp ?current-argument))
   (?gen313 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen314 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "add %s %s %s " ?gen312
          (send (symbol-to-instance-name ?gen313) get-refers-to)
          ?gen314
          ))
(defmethod iris32::add
  ((?gen315 SYMBOL
            (registerp ?current-argument))
   (?gen316 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen317 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "add %s %s %s " ?gen315
          (send (symbol-to-instance-name ?gen316) get-refers-to)
          (send (symbol-to-instance-name ?gen317) get-refers-to)
          ))
(defmethod iris32::add
  ((?gen318 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen319 SYMBOL
            (registerp ?current-argument))
   (?gen320 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "add %s %s %s " (send (symbol-to-instance-name ?gen318) get-refers-to)
          ?gen319
          ?gen320
          ))
(defmethod iris32::add
  ((?gen321 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen322 SYMBOL
            (registerp ?current-argument))
   (?gen323 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "add %s %s %s " (send (symbol-to-instance-name ?gen321) get-refers-to)
          ?gen322
          (send (symbol-to-instance-name ?gen323) get-refers-to)
          ))
(defmethod iris32::add
  ((?gen324 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen325 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen326 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "add %s %s %s " (send (symbol-to-instance-name ?gen324) get-refers-to)
          (send (symbol-to-instance-name ?gen325) get-refers-to)
          ?gen326
          ))
(defmethod iris32::add
  ((?gen327 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen328 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen329 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "add %s %s %s " (send (symbol-to-instance-name ?gen327) get-refers-to)
          (send (symbol-to-instance-name ?gen328) get-refers-to)
          (send (symbol-to-instance-name ?gen329) get-refers-to)
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
   (?gen335 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "sub %s %s %s " ?gen333
          ?gen334
          (send (symbol-to-instance-name ?gen335) get-refers-to)
          ))
(defmethod iris32::sub
  ((?gen336 SYMBOL
            (registerp ?current-argument))
   (?gen337 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen338 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "sub %s %s %s " ?gen336
          (send (symbol-to-instance-name ?gen337) get-refers-to)
          ?gen338
          ))
(defmethod iris32::sub
  ((?gen339 SYMBOL
            (registerp ?current-argument))
   (?gen340 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen341 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "sub %s %s %s " ?gen339
          (send (symbol-to-instance-name ?gen340) get-refers-to)
          (send (symbol-to-instance-name ?gen341) get-refers-to)
          ))
(defmethod iris32::sub
  ((?gen342 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen343 SYMBOL
            (registerp ?current-argument))
   (?gen344 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "sub %s %s %s " (send (symbol-to-instance-name ?gen342) get-refers-to)
          ?gen343
          ?gen344
          ))
(defmethod iris32::sub
  ((?gen345 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen346 SYMBOL
            (registerp ?current-argument))
   (?gen347 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "sub %s %s %s " (send (symbol-to-instance-name ?gen345) get-refers-to)
          ?gen346
          (send (symbol-to-instance-name ?gen347) get-refers-to)
          ))
(defmethod iris32::sub
  ((?gen348 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen349 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen350 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "sub %s %s %s " (send (symbol-to-instance-name ?gen348) get-refers-to)
          (send (symbol-to-instance-name ?gen349) get-refers-to)
          ?gen350
          ))
(defmethod iris32::sub
  ((?gen351 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen352 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen353 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "sub %s %s %s " (send (symbol-to-instance-name ?gen351) get-refers-to)
          (send (symbol-to-instance-name ?gen352) get-refers-to)
          (send (symbol-to-instance-name ?gen353) get-refers-to)
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
   (?gen359 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "mul %s %s %s " ?gen357
          ?gen358
          (send (symbol-to-instance-name ?gen359) get-refers-to)
          ))
(defmethod iris32::mul
  ((?gen360 SYMBOL
            (registerp ?current-argument))
   (?gen361 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen362 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "mul %s %s %s " ?gen360
          (send (symbol-to-instance-name ?gen361) get-refers-to)
          ?gen362
          ))
(defmethod iris32::mul
  ((?gen363 SYMBOL
            (registerp ?current-argument))
   (?gen364 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen365 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "mul %s %s %s " ?gen363
          (send (symbol-to-instance-name ?gen364) get-refers-to)
          (send (symbol-to-instance-name ?gen365) get-refers-to)
          ))
(defmethod iris32::mul
  ((?gen366 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen367 SYMBOL
            (registerp ?current-argument))
   (?gen368 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "mul %s %s %s " (send (symbol-to-instance-name ?gen366) get-refers-to)
          ?gen367
          ?gen368
          ))
(defmethod iris32::mul
  ((?gen369 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen370 SYMBOL
            (registerp ?current-argument))
   (?gen371 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "mul %s %s %s " (send (symbol-to-instance-name ?gen369) get-refers-to)
          ?gen370
          (send (symbol-to-instance-name ?gen371) get-refers-to)
          ))
(defmethod iris32::mul
  ((?gen372 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen373 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen374 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "mul %s %s %s " (send (symbol-to-instance-name ?gen372) get-refers-to)
          (send (symbol-to-instance-name ?gen373) get-refers-to)
          ?gen374
          ))
(defmethod iris32::mul
  ((?gen375 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen376 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen377 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "mul %s %s %s " (send (symbol-to-instance-name ?gen375) get-refers-to)
          (send (symbol-to-instance-name ?gen376) get-refers-to)
          (send (symbol-to-instance-name ?gen377) get-refers-to)
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
   (?gen383 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "div %s %s %s " ?gen381
          ?gen382
          (send (symbol-to-instance-name ?gen383) get-refers-to)
          ))
(defmethod iris32::div
  ((?gen384 SYMBOL
            (registerp ?current-argument))
   (?gen385 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen386 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "div %s %s %s " ?gen384
          (send (symbol-to-instance-name ?gen385) get-refers-to)
          ?gen386
          ))
(defmethod iris32::div
  ((?gen387 SYMBOL
            (registerp ?current-argument))
   (?gen388 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen389 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "div %s %s %s " ?gen387
          (send (symbol-to-instance-name ?gen388) get-refers-to)
          (send (symbol-to-instance-name ?gen389) get-refers-to)
          ))
(defmethod iris32::div
  ((?gen390 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen391 SYMBOL
            (registerp ?current-argument))
   (?gen392 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "div %s %s %s " (send (symbol-to-instance-name ?gen390) get-refers-to)
          ?gen391
          ?gen392
          ))
(defmethod iris32::div
  ((?gen393 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen394 SYMBOL
            (registerp ?current-argument))
   (?gen395 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "div %s %s %s " (send (symbol-to-instance-name ?gen393) get-refers-to)
          ?gen394
          (send (symbol-to-instance-name ?gen395) get-refers-to)
          ))
(defmethod iris32::div
  ((?gen396 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen397 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen398 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "div %s %s %s " (send (symbol-to-instance-name ?gen396) get-refers-to)
          (send (symbol-to-instance-name ?gen397) get-refers-to)
          ?gen398
          ))
(defmethod iris32::div
  ((?gen399 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen400 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen401 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "div %s %s %s " (send (symbol-to-instance-name ?gen399) get-refers-to)
          (send (symbol-to-instance-name ?gen400) get-refers-to)
          (send (symbol-to-instance-name ?gen401) get-refers-to)
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
   (?gen407 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "rem %s %s %s " ?gen405
          ?gen406
          (send (symbol-to-instance-name ?gen407) get-refers-to)
          ))
(defmethod iris32::rem
  ((?gen408 SYMBOL
            (registerp ?current-argument))
   (?gen409 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen410 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "rem %s %s %s " ?gen408
          (send (symbol-to-instance-name ?gen409) get-refers-to)
          ?gen410
          ))
(defmethod iris32::rem
  ((?gen411 SYMBOL
            (registerp ?current-argument))
   (?gen412 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen413 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "rem %s %s %s " ?gen411
          (send (symbol-to-instance-name ?gen412) get-refers-to)
          (send (symbol-to-instance-name ?gen413) get-refers-to)
          ))
(defmethod iris32::rem
  ((?gen414 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen415 SYMBOL
            (registerp ?current-argument))
   (?gen416 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "rem %s %s %s " (send (symbol-to-instance-name ?gen414) get-refers-to)
          ?gen415
          ?gen416
          ))
(defmethod iris32::rem
  ((?gen417 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen418 SYMBOL
            (registerp ?current-argument))
   (?gen419 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "rem %s %s %s " (send (symbol-to-instance-name ?gen417) get-refers-to)
          ?gen418
          (send (symbol-to-instance-name ?gen419) get-refers-to)
          ))
(defmethod iris32::rem
  ((?gen420 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen421 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen422 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "rem %s %s %s " (send (symbol-to-instance-name ?gen420) get-refers-to)
          (send (symbol-to-instance-name ?gen421) get-refers-to)
          ?gen422
          ))
(defmethod iris32::rem
  ((?gen423 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen424 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen425 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "rem %s %s %s " (send (symbol-to-instance-name ?gen423) get-refers-to)
          (send (symbol-to-instance-name ?gen424) get-refers-to)
          (send (symbol-to-instance-name ?gen425) get-refers-to)
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
   (?gen431 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "shl %s %s %s " ?gen429
          ?gen430
          (send (symbol-to-instance-name ?gen431) get-refers-to)
          ))
(defmethod iris32::shl
  ((?gen432 SYMBOL
            (registerp ?current-argument))
   (?gen433 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen434 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shl %s %s %s " ?gen432
          (send (symbol-to-instance-name ?gen433) get-refers-to)
          ?gen434
          ))
(defmethod iris32::shl
  ((?gen435 SYMBOL
            (registerp ?current-argument))
   (?gen436 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen437 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "shl %s %s %s " ?gen435
          (send (symbol-to-instance-name ?gen436) get-refers-to)
          (send (symbol-to-instance-name ?gen437) get-refers-to)
          ))
(defmethod iris32::shl
  ((?gen438 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen439 SYMBOL
            (registerp ?current-argument))
   (?gen440 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shl %s %s %s " (send (symbol-to-instance-name ?gen438) get-refers-to)
          ?gen439
          ?gen440
          ))
(defmethod iris32::shl
  ((?gen441 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen442 SYMBOL
            (registerp ?current-argument))
   (?gen443 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "shl %s %s %s " (send (symbol-to-instance-name ?gen441) get-refers-to)
          ?gen442
          (send (symbol-to-instance-name ?gen443) get-refers-to)
          ))
(defmethod iris32::shl
  ((?gen444 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen445 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen446 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shl %s %s %s " (send (symbol-to-instance-name ?gen444) get-refers-to)
          (send (symbol-to-instance-name ?gen445) get-refers-to)
          ?gen446
          ))
(defmethod iris32::shl
  ((?gen447 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen448 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen449 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "shl %s %s %s " (send (symbol-to-instance-name ?gen447) get-refers-to)
          (send (symbol-to-instance-name ?gen448) get-refers-to)
          (send (symbol-to-instance-name ?gen449) get-refers-to)
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
   (?gen455 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "shr %s %s %s " ?gen453
          ?gen454
          (send (symbol-to-instance-name ?gen455) get-refers-to)
          ))
(defmethod iris32::shr
  ((?gen456 SYMBOL
            (registerp ?current-argument))
   (?gen457 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen458 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shr %s %s %s " ?gen456
          (send (symbol-to-instance-name ?gen457) get-refers-to)
          ?gen458
          ))
(defmethod iris32::shr
  ((?gen459 SYMBOL
            (registerp ?current-argument))
   (?gen460 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen461 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "shr %s %s %s " ?gen459
          (send (symbol-to-instance-name ?gen460) get-refers-to)
          (send (symbol-to-instance-name ?gen461) get-refers-to)
          ))
(defmethod iris32::shr
  ((?gen462 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen463 SYMBOL
            (registerp ?current-argument))
   (?gen464 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shr %s %s %s " (send (symbol-to-instance-name ?gen462) get-refers-to)
          ?gen463
          ?gen464
          ))
(defmethod iris32::shr
  ((?gen465 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen466 SYMBOL
            (registerp ?current-argument))
   (?gen467 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "shr %s %s %s " (send (symbol-to-instance-name ?gen465) get-refers-to)
          ?gen466
          (send (symbol-to-instance-name ?gen467) get-refers-to)
          ))
(defmethod iris32::shr
  ((?gen468 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen469 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen470 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "shr %s %s %s " (send (symbol-to-instance-name ?gen468) get-refers-to)
          (send (symbol-to-instance-name ?gen469) get-refers-to)
          ?gen470
          ))
(defmethod iris32::shr
  ((?gen471 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen472 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen473 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "shr %s %s %s " (send (symbol-to-instance-name ?gen471) get-refers-to)
          (send (symbol-to-instance-name ?gen472) get-refers-to)
          (send (symbol-to-instance-name ?gen473) get-refers-to)
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
   (?gen479 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jtl %s %s %s " ?gen477
          ?gen478
          (send (symbol-to-instance-name ?gen479) get-refers-to)
          ))
(defmethod iris32::jtl
  ((?gen480 SYMBOL
            (registerp ?current-argument))
   (?gen481 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen482 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jtl %s %s %s " ?gen480
          (send (symbol-to-instance-name ?gen481) get-refers-to)
          ?gen482
          ))
(defmethod iris32::jtl
  ((?gen483 SYMBOL
            (registerp ?current-argument))
   (?gen484 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen485 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jtl %s %s %s " ?gen483
          (send (symbol-to-instance-name ?gen484) get-refers-to)
          (send (symbol-to-instance-name ?gen485) get-refers-to)
          ))
(defmethod iris32::jtl
  ((?gen486 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen487 SYMBOL
            (registerp ?current-argument))
   (?gen488 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jtl %s %s %s " (send (symbol-to-instance-name ?gen486) get-refers-to)
          ?gen487
          ?gen488
          ))
(defmethod iris32::jtl
  ((?gen489 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen490 SYMBOL
            (registerp ?current-argument))
   (?gen491 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jtl %s %s %s " (send (symbol-to-instance-name ?gen489) get-refers-to)
          ?gen490
          (send (symbol-to-instance-name ?gen491) get-refers-to)
          ))
(defmethod iris32::jtl
  ((?gen492 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen493 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen494 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jtl %s %s %s " (send (symbol-to-instance-name ?gen492) get-refers-to)
          (send (symbol-to-instance-name ?gen493) get-refers-to)
          ?gen494
          ))
(defmethod iris32::jtl
  ((?gen495 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen496 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen497 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jtl %s %s %s " (send (symbol-to-instance-name ?gen495) get-refers-to)
          (send (symbol-to-instance-name ?gen496) get-refers-to)
          (send (symbol-to-instance-name ?gen497) get-refers-to)
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
   (?gen503 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jfl %s %s %s " ?gen501
          ?gen502
          (send (symbol-to-instance-name ?gen503) get-refers-to)
          ))
(defmethod iris32::jfl
  ((?gen504 SYMBOL
            (registerp ?current-argument))
   (?gen505 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen506 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jfl %s %s %s " ?gen504
          (send (symbol-to-instance-name ?gen505) get-refers-to)
          ?gen506
          ))
(defmethod iris32::jfl
  ((?gen507 SYMBOL
            (registerp ?current-argument))
   (?gen508 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen509 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jfl %s %s %s " ?gen507
          (send (symbol-to-instance-name ?gen508) get-refers-to)
          (send (symbol-to-instance-name ?gen509) get-refers-to)
          ))
(defmethod iris32::jfl
  ((?gen510 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen511 SYMBOL
            (registerp ?current-argument))
   (?gen512 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jfl %s %s %s " (send (symbol-to-instance-name ?gen510) get-refers-to)
          ?gen511
          ?gen512
          ))
(defmethod iris32::jfl
  ((?gen513 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen514 SYMBOL
            (registerp ?current-argument))
   (?gen515 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jfl %s %s %s " (send (symbol-to-instance-name ?gen513) get-refers-to)
          ?gen514
          (send (symbol-to-instance-name ?gen515) get-refers-to)
          ))
(defmethod iris32::jfl
  ((?gen516 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen517 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen518 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "jfl %s %s %s " (send (symbol-to-instance-name ?gen516) get-refers-to)
          (send (symbol-to-instance-name ?gen517) get-refers-to)
          ?gen518
          ))
(defmethod iris32::jfl
  ((?gen519 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen520 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen521 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "jfl %s %s %s " (send (symbol-to-instance-name ?gen519) get-refers-to)
          (send (symbol-to-instance-name ?gen520) get-refers-to)
          (send (symbol-to-instance-name ?gen521) get-refers-to)
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
   (?gen527 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ift %s %s %s " ?gen525
          ?gen526
          (send (symbol-to-instance-name ?gen527) get-refers-to)
          ))
(defmethod iris32::ift
  ((?gen528 SYMBOL
            (registerp ?current-argument))
   (?gen529 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen530 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ift %s %s %s " ?gen528
          (send (symbol-to-instance-name ?gen529) get-refers-to)
          ?gen530
          ))
(defmethod iris32::ift
  ((?gen531 SYMBOL
            (registerp ?current-argument))
   (?gen532 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen533 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ift %s %s %s " ?gen531
          (send (symbol-to-instance-name ?gen532) get-refers-to)
          (send (symbol-to-instance-name ?gen533) get-refers-to)
          ))
(defmethod iris32::ift
  ((?gen534 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen535 SYMBOL
            (registerp ?current-argument))
   (?gen536 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ift %s %s %s " (send (symbol-to-instance-name ?gen534) get-refers-to)
          ?gen535
          ?gen536
          ))
(defmethod iris32::ift
  ((?gen537 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen538 SYMBOL
            (registerp ?current-argument))
   (?gen539 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ift %s %s %s " (send (symbol-to-instance-name ?gen537) get-refers-to)
          ?gen538
          (send (symbol-to-instance-name ?gen539) get-refers-to)
          ))
(defmethod iris32::ift
  ((?gen540 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen541 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen542 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ift %s %s %s " (send (symbol-to-instance-name ?gen540) get-refers-to)
          (send (symbol-to-instance-name ?gen541) get-refers-to)
          ?gen542
          ))
(defmethod iris32::ift
  ((?gen543 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen544 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen545 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ift %s %s %s " (send (symbol-to-instance-name ?gen543) get-refers-to)
          (send (symbol-to-instance-name ?gen544) get-refers-to)
          (send (symbol-to-instance-name ?gen545) get-refers-to)
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
   (?gen551 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iff %s %s %s " ?gen549
          ?gen550
          (send (symbol-to-instance-name ?gen551) get-refers-to)
          ))
(defmethod iris32::iff
  ((?gen552 SYMBOL
            (registerp ?current-argument))
   (?gen553 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen554 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iff %s %s %s " ?gen552
          (send (symbol-to-instance-name ?gen553) get-refers-to)
          ?gen554
          ))
(defmethod iris32::iff
  ((?gen555 SYMBOL
            (registerp ?current-argument))
   (?gen556 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen557 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iff %s %s %s " ?gen555
          (send (symbol-to-instance-name ?gen556) get-refers-to)
          (send (symbol-to-instance-name ?gen557) get-refers-to)
          ))
(defmethod iris32::iff
  ((?gen558 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen559 SYMBOL
            (registerp ?current-argument))
   (?gen560 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iff %s %s %s " (send (symbol-to-instance-name ?gen558) get-refers-to)
          ?gen559
          ?gen560
          ))
(defmethod iris32::iff
  ((?gen561 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen562 SYMBOL
            (registerp ?current-argument))
   (?gen563 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iff %s %s %s " (send (symbol-to-instance-name ?gen561) get-refers-to)
          ?gen562
          (send (symbol-to-instance-name ?gen563) get-refers-to)
          ))
(defmethod iris32::iff
  ((?gen564 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen565 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen566 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iff %s %s %s " (send (symbol-to-instance-name ?gen564) get-refers-to)
          (send (symbol-to-instance-name ?gen565) get-refers-to)
          ?gen566
          ))
(defmethod iris32::iff
  ((?gen567 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen568 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen569 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iff %s %s %s " (send (symbol-to-instance-name ?gen567) get-refers-to)
          (send (symbol-to-instance-name ?gen568) get-refers-to)
          (send (symbol-to-instance-name ?gen569) get-refers-to)
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
   (?gen575 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iftl %s %s %s " ?gen573
          ?gen574
          (send (symbol-to-instance-name ?gen575) get-refers-to)
          ))
(defmethod iris32::iftl
  ((?gen576 SYMBOL
            (registerp ?current-argument))
   (?gen577 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen578 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iftl %s %s %s " ?gen576
          (send (symbol-to-instance-name ?gen577) get-refers-to)
          ?gen578
          ))
(defmethod iris32::iftl
  ((?gen579 SYMBOL
            (registerp ?current-argument))
   (?gen580 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen581 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iftl %s %s %s " ?gen579
          (send (symbol-to-instance-name ?gen580) get-refers-to)
          (send (symbol-to-instance-name ?gen581) get-refers-to)
          ))
(defmethod iris32::iftl
  ((?gen582 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen583 SYMBOL
            (registerp ?current-argument))
   (?gen584 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iftl %s %s %s " (send (symbol-to-instance-name ?gen582) get-refers-to)
          ?gen583
          ?gen584
          ))
(defmethod iris32::iftl
  ((?gen585 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen586 SYMBOL
            (registerp ?current-argument))
   (?gen587 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iftl %s %s %s " (send (symbol-to-instance-name ?gen585) get-refers-to)
          ?gen586
          (send (symbol-to-instance-name ?gen587) get-refers-to)
          ))
(defmethod iris32::iftl
  ((?gen588 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen589 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen590 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iftl %s %s %s " (send (symbol-to-instance-name ?gen588) get-refers-to)
          (send (symbol-to-instance-name ?gen589) get-refers-to)
          ?gen590
          ))
(defmethod iris32::iftl
  ((?gen591 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen592 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen593 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iftl %s %s %s " (send (symbol-to-instance-name ?gen591) get-refers-to)
          (send (symbol-to-instance-name ?gen592) get-refers-to)
          (send (symbol-to-instance-name ?gen593) get-refers-to)
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
   (?gen599 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iffl %s %s %s " ?gen597
          ?gen598
          (send (symbol-to-instance-name ?gen599) get-refers-to)
          ))
(defmethod iris32::iffl
  ((?gen600 SYMBOL
            (registerp ?current-argument))
   (?gen601 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen602 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iffl %s %s %s " ?gen600
          (send (symbol-to-instance-name ?gen601) get-refers-to)
          ?gen602
          ))
(defmethod iris32::iffl
  ((?gen603 SYMBOL
            (registerp ?current-argument))
   (?gen604 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen605 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iffl %s %s %s " ?gen603
          (send (symbol-to-instance-name ?gen604) get-refers-to)
          (send (symbol-to-instance-name ?gen605) get-refers-to)
          ))
(defmethod iris32::iffl
  ((?gen606 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen607 SYMBOL
            (registerp ?current-argument))
   (?gen608 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iffl %s %s %s " (send (symbol-to-instance-name ?gen606) get-refers-to)
          ?gen607
          ?gen608
          ))
(defmethod iris32::iffl
  ((?gen609 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen610 SYMBOL
            (registerp ?current-argument))
   (?gen611 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iffl %s %s %s " (send (symbol-to-instance-name ?gen609) get-refers-to)
          ?gen610
          (send (symbol-to-instance-name ?gen611) get-refers-to)
          ))
(defmethod iris32::iffl
  ((?gen612 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen613 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen614 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "iffl %s %s %s " (send (symbol-to-instance-name ?gen612) get-refers-to)
          (send (symbol-to-instance-name ?gen613) get-refers-to)
          ?gen614
          ))
(defmethod iris32::iffl
  ((?gen615 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen616 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen617 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "iffl %s %s %s " (send (symbol-to-instance-name ?gen615) get-refers-to)
          (send (symbol-to-instance-name ?gen616) get-refers-to)
          (send (symbol-to-instance-name ?gen617) get-refers-to)
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
   (?gen623 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ne %s %s %s " ?gen621
          ?gen622
          (send (symbol-to-instance-name ?gen623) get-refers-to)
          ))
(defmethod iris32::ne
  ((?gen624 SYMBOL
            (registerp ?current-argument))
   (?gen625 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen626 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ne %s %s %s " ?gen624
          (send (symbol-to-instance-name ?gen625) get-refers-to)
          ?gen626
          ))
(defmethod iris32::ne
  ((?gen627 SYMBOL
            (registerp ?current-argument))
   (?gen628 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen629 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ne %s %s %s " ?gen627
          (send (symbol-to-instance-name ?gen628) get-refers-to)
          (send (symbol-to-instance-name ?gen629) get-refers-to)
          ))
(defmethod iris32::ne
  ((?gen630 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen631 SYMBOL
            (registerp ?current-argument))
   (?gen632 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ne %s %s %s " (send (symbol-to-instance-name ?gen630) get-refers-to)
          ?gen631
          ?gen632
          ))
(defmethod iris32::ne
  ((?gen633 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen634 SYMBOL
            (registerp ?current-argument))
   (?gen635 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ne %s %s %s " (send (symbol-to-instance-name ?gen633) get-refers-to)
          ?gen634
          (send (symbol-to-instance-name ?gen635) get-refers-to)
          ))
(defmethod iris32::ne
  ((?gen636 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen637 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen638 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ne %s %s %s " (send (symbol-to-instance-name ?gen636) get-refers-to)
          (send (symbol-to-instance-name ?gen637) get-refers-to)
          ?gen638
          ))
(defmethod iris32::ne
  ((?gen639 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen640 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen641 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ne %s %s %s " (send (symbol-to-instance-name ?gen639) get-refers-to)
          (send (symbol-to-instance-name ?gen640) get-refers-to)
          (send (symbol-to-instance-name ?gen641) get-refers-to)
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
   (?gen647 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "lt %s %s %s " ?gen645
          ?gen646
          (send (symbol-to-instance-name ?gen647) get-refers-to)
          ))
(defmethod iris32::lt
  ((?gen648 SYMBOL
            (registerp ?current-argument))
   (?gen649 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen650 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "lt %s %s %s " ?gen648
          (send (symbol-to-instance-name ?gen649) get-refers-to)
          ?gen650
          ))
(defmethod iris32::lt
  ((?gen651 SYMBOL
            (registerp ?current-argument))
   (?gen652 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen653 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "lt %s %s %s " ?gen651
          (send (symbol-to-instance-name ?gen652) get-refers-to)
          (send (symbol-to-instance-name ?gen653) get-refers-to)
          ))
(defmethod iris32::lt
  ((?gen654 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen655 SYMBOL
            (registerp ?current-argument))
   (?gen656 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "lt %s %s %s " (send (symbol-to-instance-name ?gen654) get-refers-to)
          ?gen655
          ?gen656
          ))
(defmethod iris32::lt
  ((?gen657 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen658 SYMBOL
            (registerp ?current-argument))
   (?gen659 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "lt %s %s %s " (send (symbol-to-instance-name ?gen657) get-refers-to)
          ?gen658
          (send (symbol-to-instance-name ?gen659) get-refers-to)
          ))
(defmethod iris32::lt
  ((?gen660 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen661 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen662 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "lt %s %s %s " (send (symbol-to-instance-name ?gen660) get-refers-to)
          (send (symbol-to-instance-name ?gen661) get-refers-to)
          ?gen662
          ))
(defmethod iris32::lt
  ((?gen663 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen664 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen665 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "lt %s %s %s " (send (symbol-to-instance-name ?gen663) get-refers-to)
          (send (symbol-to-instance-name ?gen664) get-refers-to)
          (send (symbol-to-instance-name ?gen665) get-refers-to)
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
   (?gen671 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "gt %s %s %s " ?gen669
          ?gen670
          (send (symbol-to-instance-name ?gen671) get-refers-to)
          ))
(defmethod iris32::gt
  ((?gen672 SYMBOL
            (registerp ?current-argument))
   (?gen673 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen674 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "gt %s %s %s " ?gen672
          (send (symbol-to-instance-name ?gen673) get-refers-to)
          ?gen674
          ))
(defmethod iris32::gt
  ((?gen675 SYMBOL
            (registerp ?current-argument))
   (?gen676 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen677 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "gt %s %s %s " ?gen675
          (send (symbol-to-instance-name ?gen676) get-refers-to)
          (send (symbol-to-instance-name ?gen677) get-refers-to)
          ))
(defmethod iris32::gt
  ((?gen678 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen679 SYMBOL
            (registerp ?current-argument))
   (?gen680 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "gt %s %s %s " (send (symbol-to-instance-name ?gen678) get-refers-to)
          ?gen679
          ?gen680
          ))
(defmethod iris32::gt
  ((?gen681 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen682 SYMBOL
            (registerp ?current-argument))
   (?gen683 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "gt %s %s %s " (send (symbol-to-instance-name ?gen681) get-refers-to)
          ?gen682
          (send (symbol-to-instance-name ?gen683) get-refers-to)
          ))
(defmethod iris32::gt
  ((?gen684 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen685 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen686 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "gt %s %s %s " (send (symbol-to-instance-name ?gen684) get-refers-to)
          (send (symbol-to-instance-name ?gen685) get-refers-to)
          ?gen686
          ))
(defmethod iris32::gt
  ((?gen687 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen688 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen689 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "gt %s %s %s " (send (symbol-to-instance-name ?gen687) get-refers-to)
          (send (symbol-to-instance-name ?gen688) get-refers-to)
          (send (symbol-to-instance-name ?gen689) get-refers-to)
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
   (?gen695 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "le %s %s %s " ?gen693
          ?gen694
          (send (symbol-to-instance-name ?gen695) get-refers-to)
          ))
(defmethod iris32::le
  ((?gen696 SYMBOL
            (registerp ?current-argument))
   (?gen697 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen698 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "le %s %s %s " ?gen696
          (send (symbol-to-instance-name ?gen697) get-refers-to)
          ?gen698
          ))
(defmethod iris32::le
  ((?gen699 SYMBOL
            (registerp ?current-argument))
   (?gen700 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen701 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "le %s %s %s " ?gen699
          (send (symbol-to-instance-name ?gen700) get-refers-to)
          (send (symbol-to-instance-name ?gen701) get-refers-to)
          ))
(defmethod iris32::le
  ((?gen702 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen703 SYMBOL
            (registerp ?current-argument))
   (?gen704 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "le %s %s %s " (send (symbol-to-instance-name ?gen702) get-refers-to)
          ?gen703
          ?gen704
          ))
(defmethod iris32::le
  ((?gen705 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen706 SYMBOL
            (registerp ?current-argument))
   (?gen707 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "le %s %s %s " (send (symbol-to-instance-name ?gen705) get-refers-to)
          ?gen706
          (send (symbol-to-instance-name ?gen707) get-refers-to)
          ))
(defmethod iris32::le
  ((?gen708 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen709 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen710 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "le %s %s %s " (send (symbol-to-instance-name ?gen708) get-refers-to)
          (send (symbol-to-instance-name ?gen709) get-refers-to)
          ?gen710
          ))
(defmethod iris32::le
  ((?gen711 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen712 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen713 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "le %s %s %s " (send (symbol-to-instance-name ?gen711) get-refers-to)
          (send (symbol-to-instance-name ?gen712) get-refers-to)
          (send (symbol-to-instance-name ?gen713) get-refers-to)
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
   (?gen719 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ge %s %s %s " ?gen717
          ?gen718
          (send (symbol-to-instance-name ?gen719) get-refers-to)
          ))
(defmethod iris32::ge
  ((?gen720 SYMBOL
            (registerp ?current-argument))
   (?gen721 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen722 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ge %s %s %s " ?gen720
          (send (symbol-to-instance-name ?gen721) get-refers-to)
          ?gen722
          ))
(defmethod iris32::ge
  ((?gen723 SYMBOL
            (registerp ?current-argument))
   (?gen724 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen725 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ge %s %s %s " ?gen723
          (send (symbol-to-instance-name ?gen724) get-refers-to)
          (send (symbol-to-instance-name ?gen725) get-refers-to)
          ))
(defmethod iris32::ge
  ((?gen726 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen727 SYMBOL
            (registerp ?current-argument))
   (?gen728 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ge %s %s %s " (send (symbol-to-instance-name ?gen726) get-refers-to)
          ?gen727
          ?gen728
          ))
(defmethod iris32::ge
  ((?gen729 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen730 SYMBOL
            (registerp ?current-argument))
   (?gen731 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ge %s %s %s " (send (symbol-to-instance-name ?gen729) get-refers-to)
          ?gen730
          (send (symbol-to-instance-name ?gen731) get-refers-to)
          ))
(defmethod iris32::ge
  ((?gen732 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen733 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen734 SYMBOL
            (registerp ?current-argument))
   )
  (format nil "ge %s %s %s " (send (symbol-to-instance-name ?gen732) get-refers-to)
          (send (symbol-to-instance-name ?gen733) get-refers-to)
          ?gen734
          ))
(defmethod iris32::ge
  ((?gen735 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen736 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   (?gen737 SYMBOL
            (and (instance-existp (symbol-to-instance-name ?current-argument))
                 (eq register
                     (class (symbol-to-instance-name ?current-argument)))))
   )
  (format nil "ge %s %s %s " (send (symbol-to-instance-name ?gen735) get-refers-to)
          (send (symbol-to-instance-name ?gen736) get-refers-to)
          (send (symbol-to-instance-name ?gen737) get-refers-to)
          ))
