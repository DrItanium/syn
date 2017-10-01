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
; Device.clp - Common class concept for external address wrappers
;------------------------------------------------------------------------------
(defgeneric MAIN::device-initialize)
(defgeneric MAIN::device-shutdown)

(defmethod MAIN::device-initialize
  ((?x EXTERNAL-ADDRESS))
  (call ?x
        initialize))

(defmethod MAIN::device-shutdown
  ((?x EXTERNAL-ADDRESS))
  (call ?x
        shutdown))

(defclass MAIN::external-device
  (is-a external-address-wrapper)
  (role abstract)
  (pattern-match non-reactive)
  (message-handler init after)
  (message-handler delete before))

(defmessage-handler MAIN::external-device delete before
                    ()
                    (device-shutdown (dynamic-get backing-store)))

(defmessage-handler MAIN::external-device init around 
                    ()
                    (call-next-handler)
                    (device-initialize (dynamic-get backing-store)))
