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

; Register.clp - wrapper class for the register concept

(defclass MAIN::register
  (is-a external-address-wrapper)
  (slot backing-type
        (storage shared)
        (access read-only)
        (create-accessor read)
        (source composite)
        (default register))
  (message-handler set-mask primary)
  (message-handler get-mask primary)
  (message-handler write primary)
  (message-handler read primary)
  (message-handler increment primary)
  (message-handler decrement primary)
  (message-handler decode primary)
  (message-handler encode primary))
(defmessage-handler MAIN::register set-mask primary
                    (?mask)
                    (send ?self
                          call
                          set-mask
                          ?mask))
(defmessage-handler MAIN::register get-mask primary
                    ()
                    (send ?self
                          call
                          get-mask))

(defmessage-handler MAIN::register write primary
                    (?value)
                    (send ?self
                          call
                          set
                          ?value))
(defmessage-handler MAIN::register read primary
                    ()
                    (send ?self
                          call
                          get))
(defmessage-handler MAIN::register increment primary
                    ($?count)
                    (send ?self
                          call
                          increment
                          ?count))
(defmessage-handler MAIN::register decrement primary
                    ($?count)
                    (send ?self
                          call
                          decrement
                          ?count))

(defmessage-handler MAIN::register decode primary
                    (?mask ?shift)
                    (send ?self
                          call
                          decode
                          ?mask
                          ?shift))

(defmessage-handler MAIN::register encode primary
                    (?value ?mask ?shift)
                    (send ?self
                          call
                          encode
                          ?value
                          ?mask
                          ?shift))
