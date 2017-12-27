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
; SimpleFrontend.clp - A program to control various other devices
;------------------------------------------------------------------------------
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* ExternalAddressWrapper.clp)
(batch* Device.clp)
(batch* MemoryBlock.clp)
(batch* Paragraph.clp)
(batch* order.clp)
(batch* SimpleServer.clp)


(defrule MAIN::add-memory-location-to-command
         (stage (current read))
         ?f <- (inspect action)
         ?k <- (action $?body)
         (object (is-a iris64-encyclopedia)
                 (name ?target))
         =>
         (retract ?f ?k)
         (assert (action ?body from ?target)))

; TODO: add support for restarting execution
;----------------------------------------------------------------
; Commands are - read, write, shutdown

(defrule MAIN::read-memory
         (stage (current dispatch))
         ?k <- (action read ?address callback ?callback from ?target)
         (object (is-a iris64-encyclopedia)
                 (name ?target))
         =>
         (retract ?k)
         (assert (write callback ?callback 
                        command: (send ?target
                                       read
                                       ?address))))

(defrule MAIN::write-memory
         (stage (current dispatch))
         ?k <- (action write ?address ?value callback ?callback from ?target)
         (object (is-a iris64-encyclopedia)
                 (name ?target))
         =>
         (retract ?k)
         (assert (write callback ?callback 
                        command: (send ?target
                                       write
                                       ?address
                                       ?value))))


(deffacts MAIN::connection-info
          (setup connection /tmp/syn/memory))
