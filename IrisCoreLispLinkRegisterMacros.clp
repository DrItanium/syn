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


;------------------------------------------------------------------------------
; Link Register macro operations 
;------------------------------------------------------------------------------
(defrule lower::pop-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents pop
                                 lr
                                 ?stack)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                pop
                                iv0
                                ?stack)
                       (mk-move-op ?n
                                   lr
                                   iv0)))

(defrule lower::push-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents push
                                 ?stack
                                 lr)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-move-op ?n
                                   iv0
                                   lr)
                       (mk-list ?n
                                push 
                                ?stack 
                                iv0)))

(defrule lower::store-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents st
                                 ?address
                                 lr)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-move-op ?n
                                   iv0
                                   lr)
                       (mk-list ?n
                                st
                                ?address
                                iv0)))

(defrule lower::load-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents ld
                                 lr
                                 ?address)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                ld
                                iv0
                                ?address)
                       (mk-move-op ?n
                                   lr
                                   iv0)))

(defrule lower::make-swap-lr-register-lr-first
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents swap
                                 lr
                                 ?register)
                       (name ?name)
                       (parent ?p))
         (object (is-a register)
                 (name ?register))
         =>
         (unmake-instance ?f)
         (mk-container ?name
                       ?p
                       (mk-move-op ?name
                                   iv0
                                   lr)
                       (mk-list ?name
                                swap
                                iv0
                                ?register)
                       (mk-move-op ?name
                                   lr
                                   iv0)))

(defrule lower::make-swap-lr-register-lr-second
         (declare (salience 2))
         ?f <- (object (is-a list)
                       (contents swap
                                 ?register
                                 lr)
                       (name ?name)
                       (parent ?p))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f 
                          (contents swap
                                    lr
                                    ?register)))

