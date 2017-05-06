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

; extended rules for forth specific behavior and wrapper functions!

(deffacts lower::register-conventions
          (alias sp <- call-stack <- cs)
          (alias r8 <- data-stack <- ds)
          (alias r7 <- current-address <- ca)
          (alias r6 <- temp0 <- t0)
          (alias r5 <- temp1 <- t1)
          (alias r4 <- temp2 <- t2)
          (alias r3 <- temp3 <- t3))
(defrule lower::do-swap-stack-pointer
         ?f <- (object (is-a list)
                       (contents swap-stack-pointer
                                 ?with
                                 $?body)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                swap
                                sp
                                ?with)
                       $?body
                       (mk-list ?n
                                swap
                                sp
                                ?with)))

(defrule lower::do-pops
         "according to 430eFORTH, this is the base pop instruction"
         ?f <- (object (is-a list)
                       (contents pops
                                 ?target)
                       (name ?n)
                       (parent ?p))
         =>
         (modify-instance ?f
                          (contents swap-stack-pointer
                                    ds
                                    (mk-list ?n
                                             pop32
                                             ?target))))

(defrule lower::do-pushs
         ?f <- (object (is-a list)
                       (contents pushs
                                 ?target)
                       (name ?n)
                       (parent ?p))
         =>
         (modify-instance ?f
                          (contents swap-stack-pointer
                                    ds
                                    (mk-list ?n
                                             push32
                                             ?target))))

;(defrule lower::build-forth-record
;         "In a forth dictionary entry we have several fields that we have to
;         fill in:
;         1) the address of the next data structure (two words)
;         2) The control block of the system (two words)
;            2a) Lowest 8 bits contains the string length
;            2b) Next 8 bits contains the base control bits
;            2c) Upper 16-bits are zero!
;         3) The string itself (maximum of 255 characters)
;         4) Zero word
;         6)
