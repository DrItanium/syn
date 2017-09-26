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


; Register descriptions:
; r0 - r31 - scratch / unused registers
; r
; setup the system as needed
; TODO: insert code for the error handler setup

(let ErrorDispatchVector be 0x00FF)
(let ZeroAddress be 0x0000)
(let MessageBufferStart be 0x0100)
(let GetCAddress be 0x0001)
(let PutCAddress be 0x0002)
(let TerminateAddress be 0x0000)
(let ROMRoutines be 0xFE00)
(let ErrorHandler be 0xFF00)
(alias r0 as scratch0)
(alias r1 as scratch1)
(alias r2 as scratch2)
(alias r3 as scratch3)
(alias r4 as scratch4)
(alias r5 as scratch5)
(alias r6 as scratch6)
(alias r7 as scratch7)
(alias r8 as internal-stack)
(alias r9 as count0)
(alias r10 as count1)
(alias r11 as retval0)
(alias r12 as retval1)
(alias r13 as retval2)
(alias r14 as retval3)

; in case our internal routines require return values, keep it off of the
; visible stack

(alias r128 as romscratch0)
(alias r129 as romscratch1)
(alias r130 as romscratch2)
(alias r131 as romscratch3)
(alias r132 as romarg0)
(alias r133 as romarg1)
(alias r134 as romarg2)
(alias r135 as romarg3)
(alias r135 as romret0)
(alias r136 as romret1)
(alias r137 as romret2)
(alias r138 as romret3)

(alias p14 as rompredicate-true)
(alias p15 as rompredicate-false)

(section code
         (org ROMRoutines
              (label ROMTerminateExecution
                     ; if we are at this point then we can easily just use
                     ; whatever register we want!
                     (xor romscratch0
                          romscratch0
                          romscratch0)
                     ; don't waste the space for the return
                     (stio romscratch0
                           romscratch0))
              (label ROMReadCharacter
                     (push internal-stack
                           romscratch0)
                     (set romscratch0
                          GetCAddress)
                     (ldio romret0
                           romscratch0)
                     (pop romscratch0
                          internal-stack)
                     (blr))
              (label ROMWriteCharacter
                     (push internal-stack
                           romscratch0)
                     (set romscratch0
                          PutCAddress)
                     (stio romscratch0
                           romarg0)
                     (pop romscratch0
                          internal-stack)
                     (blr))
              (label ROMForceDivideByZero
                     (divi romscratch0
                           romscratch0
                           0x0)
                     (blr))
              (label ROMIsSpace
                     (eqi rompredicate-true
                          rompredicate-false
                          romarg0
                          0x20)
                     (blr)))
         (org ErrorHandler
              (divi scratch0
                    scratch0
                    0x0)))
(section data
         (org ErrorDispatchVector
              (word ErrorHandler))
         (org MessageBufferStart
              (label MessageIOStorage
                     (label MessageIOLength
                            (word 0x0000))
                     (label MessageIOStorage
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000))
                     )
              )
         )
