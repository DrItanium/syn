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
                                   cp sp lr ip
                                   tmp))
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
  ((?dest (immediatep ?current-argument)))
  (format nil 
          "@org %s"
          (str-cat ?dest)))
(defmethod iris32::@label
  ((?dest (immediatep ?current-argument)))
  (format nil 
          "@label %s"
          (str-cat ?dest)))
(defmethod iris32::@word
  ((?dest (immediatep ?current-argument)))
  (format nil 
          "@word %s"
          (str-cat ?dest)))
(defmethod iris32::decr
  ((?dest SYMBOL
          (registerp ?current-argument)))
  (format nil 
          "decr %s"
          ?dest))
(defmethod iris32::incr
  ((?dest SYMBOL
          (registerp ?current-argument)))
  (format nil 
          "incr %s"
          ?dest))
(defmethod iris32::double
  ((?dest SYMBOL
          (registerp ?current-argument)))
  (format nil 
          "double %s"
          ?dest))
(defmethod iris32::halve
  ((?dest SYMBOL
          (registerp ?current-argument)))
  (format nil 
          "halve %s"
          ?dest))
(defmethod iris32::pop
  ((?dest SYMBOL
          (registerp ?current-argument)))
  (format nil 
          "pop %s"
          ?dest))
(defmethod iris32::push
  ((?dest SYMBOL
          (registerp ?current-argument)))
  (format nil 
          "push %s"
          ?dest))
(defmethod iris32::j
  ((?dest SYMBOL
          (registerp ?current-argument)))
  (format nil 
          "j %s"
          ?dest))
(defmethod iris32::setu
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source (immediatep ?current-argument)))
  (format nil
          "setu %s %s"
          ?dest
          (str-cat ?source)))
(defmethod iris32::setl
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source (immediatep ?current-argument)))
  (format nil
          "setl %s %s"
          ?dest
          (str-cat ?source)))
(defmethod iris32::lor
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "or %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::land
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "and %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::cmp_eq
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "eq %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::lnot
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source SYMBOL
            (registerp ?current-argument)))
  (format nil 
          "not %s %s"
          ?dest
          ?source))
(defmethod iris32::move
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source SYMBOL
            (registerp ?current-argument)))
  (format nil 
          "move %s %s"
          ?dest
          ?source))
(defmethod iris32::swap
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source SYMBOL
            (registerp ?current-argument)))
  (format nil 
          "swap %s %s"
          ?dest
          ?source))
(defmethod iris32::ld
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source SYMBOL
            (registerp ?current-argument)))
  (format nil 
          "ld %s %s"
          ?dest
          ?source))
(defmethod iris32::st
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source SYMBOL
            (registerp ?current-argument)))
  (format nil 
          "st %s %s"
          ?dest
          ?source))
(defmethod iris32::jl
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source SYMBOL
            (registerp ?current-argument)))
  (format nil 
          "jl %s %s"
          ?dest
          ?source))
(defmethod iris32::jt
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source SYMBOL
            (registerp ?current-argument)))
  (format nil 
          "jt %s %s"
          ?dest
          ?source))
(defmethod iris32::jf
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source SYMBOL
            (registerp ?current-argument)))
  (format nil 
          "jf %s %s"
          ?dest
          ?source))
(defmethod iris32::addi
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "addi %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::subi
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "subi %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::muli
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "muli %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::divi
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "divi %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::remi
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "remi %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::shli
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "shli %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::shri
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "shri %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::eqi
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "eqi %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::nei
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "nei %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::lti
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "lti %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::lei
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "lei %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::gei
  ((?dest SYMBOL
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 (immediatep ?current-argument)))
  (format nil 
          "gei %s %s %s" 
          ?dest
          ?source0
          (str-cat ?source1)))
(defmethod iris32::add
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "add %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::sub
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "sub %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::mul
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "mul %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::div
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "div %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::rem
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "rem %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::shl
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "shl %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::shr
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "shr %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::jtl
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "jtl %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::jfl
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "jfl %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::ift
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "ift %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::iff
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "iff %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::iftl
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "iftl %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::iffl
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "iffl %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::ne
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "ne %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::lt
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "lt %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::gt
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "gt %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::le
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "le %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::ge
  ((?dest SYMBOL 
          (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument)))
  (format nil 
          "ge %s %s %s" 
          ?dest
          ?source0
          ?source1))
(defmethod iris32::system-op
  ((?code INTEGER
          (<= 0 ?current-argument 255))
   (?arg0 SYMBOL 
          (registerp ?current-argument))
   (?arg1 SYMBOL 
          (registerp ?current-argument)))
  (format nil "system %d %s %s" ?code ?arg0 ?arg1))
