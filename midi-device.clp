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
;
; midi-device.clp - class for interfacing with the midi external-device
; depends on midi.clp as well
(defclass MAIN::midi-device
  (is-a external-device)
  (slot backing-type
        (source composite)
        (access read-only)
        (create-accessor read)
        (default midi-connection))
  (slot hardware-id
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler init around)
  (message-handler init after)
  (message-handler delete before)
  (message-handler read primary)
  (message-handler write primary))

(defmessage-handler MAIN::midi-device init around
                    ()
                    (call-next-handler)
                    ; at this point we can call open!
                    (if (not (call (dynamic-get backing-store)
                                   openp)) then
                      (call (dynamic-get backing-store)
                            open)))
(defmessage-handler MAIN::midi-device init after
                    ()
                    (bind ?self:constructor-args
                          ?self:hardware-id))
(defmessage-handler MAIN::midi-device delete before
                    ()
                    (send ?self
                          call
                          close))

(defmessage-handler MAIN::midi-device read primary
                    (?capacity)
                    (send ?self
                          call
                          read
                          ?capacity))
(defmessage-handler MAIN::midi-device write primary
                    ($?elements)
                    (send ?self
                          call
                          write
                          ?elements))
