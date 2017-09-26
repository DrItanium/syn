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

; extended rules for standard C/RISC calling behavior

; Pass arguments through registers! Four registers for arguments with three
; temporaries.

(deffacts lower::load-argument-macros
          (alias temp0 <- argument-register0 <- arg0)
          (alias temp1 <- argument-register1 <- arg1)
          (alias temp2 <- argument-register2 <- arg2)
          (alias temp3 <- argument-register3 <- arg3 <- rest-param)
          (alias temp4 <- local0)
          (alias temp5 <- local1)
          (alias temp6 <- local2))

(deffacts lower::lower-eight-registers:parameter-passing-conventions
          (alias r7 <- result-register <- result)
          (alias r6 <- temporary-register0 <- temporary0 <- temp0)
          (alias r5 <- temporary-register1 <- temporary1 <- temp1)
          (alias r4 <- temporary-register2 <- temporary2 <- temp2)
          (alias r3 <- temporary-register3 <- temporary3 <- temp3)
          (alias r2 <- temporary-register4 <- temporary4 <- temp4)
          (alias r1 <- temporary-register5 <- temporary5 <- temp5)
          (alias r0 <- temporary-register6 <- temporary6 <- temp6))

(defrule lower::defunc-basic
         ?f <- (object (is-a list)
                       (contents defunc-basic
                                 ?title
                                 $?body)
                       (name ?name))
         =>
         (modify-instance ?f
                          (contents label
                                    ?title
                                    $?body
                                    (mk-list ?name
                                             return))))
(defrule lower::defunc-full
         "Automatically save the addr and value registers as they will most likely be used!"
         ?f <- (object (is-a list)
                       (contents defunc
                                 ?title
                                 $?body)
                       (name ?name))
         =>
         (modify-instance ?f
                          (contents defunc-basic
                                    ?title
                                    (mk-use-registers-block (gensym*)
                                                            ?name
                                                            (create$ addr
                                                                     value)
                                                            ?body))))
