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

; Define the base wrapper classes for the different cores

(defclass MAIN::core
  (is-a USER)
  (slot reference
        (type EXTERNAL-ADDRESS)
        (visibility public)
        (storage local))
  (slot type
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (default PLEASE-OVERRIDE-IN-SUBTYPES))
  (multislot init-arguments
             (visibility public)
             (storage local)
             (default ?NONE))
  (message-handler init after)
  (message-handler delete before)
  (message-handler call primary))

(defmessage-handler MAIN::core init after
                    ()
                    (call (bind ?self:reference
                                (new (dynamic-get type)
                                     (expand$ (dynamic-get init-arguments))))
                          initialize))
(defmessage-handler MAIN::core delete before
                    ()
                    (call ?self:reference
                          shutdown))

(defmessage-handler MAIN::core call primary
                    (?operation $?args)
                    (call ?self:reference
                          ?operation
                          (expand$ ?args)))

(defclass MAIN::cisc0-core
  (is-a core)
  (slot type
        (source composite)
        (default cisc0-core)))

(defclass MAIN::iris-core
  (is-a core)
  (slot type
        (source composite)
        (default iris-core)))
