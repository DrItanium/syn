;------------------------------------------------------------------------------
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
; RegisterFile.clp - A simple memory block with 256 entries
;------------------------------------------------------------------------------
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* ExternalAddressWrapper.clp)
(batch* Device.clp)
(batch* MemoryBlock.clp)
(batch* order.clp)
(batch* SimpleServer.clp)
(defglobal MAIN
           ?*register-file-capacity* = 256)
(definstances MAIN::register-file
              (gpr of memory-block 
                   (capacity ?*register-file-capacity*)))
;----------------------------------------------------------------
; commands are: add, sub, mul, div, rem, shift-left/left-shift, shift-right/right-shift, shutdown, list-commands
;----------------------------------------------------------------
(deffunction MAIN::op-load
             (?address)
             (send [gpr]
                   read
                   ?address))
(deffunction MAIN::op-store
             (?address ?value)
             (send [gpr]
                   write 
                   ?address
                   ?value))

(deffunction MAIN::op-swap
             (?a ?b)
             (send [gpr]
                   swap
                   ?a
                   ?b))
(deffunction MAIN::op-move
             (?from ?to)
             (send [gpr]
                   move
                   ?from
                   ?to))

(deffunction MAIN::op-length
             ()
             ?*register-file-capacity*)
(deffunction MAIN::op-increment
             (?addr)
             (send [gpr]
                   increment
                   ?addr))

(deffunction MAIN::op-decrement
             (?addr)
             (send [gpr]
                   decrement
                   ?addr))

(deffacts MAIN::gpr-commands
          (make legal-commands length size -> op-length)
          (make legal-commands move -> op-move)
          (make legal-commands swap -> op-swap)
          (make legal-commands load ld -> op-load)
          (make legal-commands store st -> op-store)
          (make legal-commands increment incr ++ -> op-increment)
          (make legal-commands decrement decr -- -> op-decrement))
