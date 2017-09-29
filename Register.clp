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
(defmethod decode-bits
  ((?value INTEGER)
   (?mask INTEGER))
  (decode-bits ?value
               ?mask
               0))

(defmethod encode-bits
  ((?value INTEGER)
   (?insert INTEGER)
   (?mask INTEGER))
  (encode-bits ?value
               ?insert
               ?mask
               0))

(defclass MAIN::register
  (is-a USER)
  (slot mask
        (type INTEGER)
        (storage local)
        (visibility public)
        (default-dynamic -1))
  (slot value
        (type INTEGER)
        (storage local)
        (visibility public)
        (default-dynamic 0))
  (message-handler put-mask after)
  (message-handler put-value around)
  (message-handler decode primary)
  (message-handler encode primary))

(defmessage-handler MAIN::register put-value around
                    "Mask the value before storing it!"
                    (?value)
                    (override-next-handler (decode-bits ?value
                                                        (dynamic-get mask)
                                                        0)))


(defmessage-handler MAIN::register put-mask after
                    "update the value after updating the mask!"
                    (?mask)
                    ; when we initialize this class, the slot for value is nil
                    ; so we have to make sure that we've initialized correctly before hand!
                    (bind ?old-value
                          (dynamic-get value))
                    (if (integerp ?old-value) then
                      (dynamic-put value
                                   (decode-bits ?old-value
                                                ?mask))))

(defmessage-handler MAIN::register decode primary
                    (?mask $?shift)
                    (decode-bits (dynamic-get value)
                                 ?mask
                                 (expand$ (first$ ?shift))))

(defmessage-handler MAIN::register encode primary
                    (?new-value ?mask $?shift)
                    (decode-bits (dynamic-get value)
                                 ?new-value
                                 ?mask
                                 (expand$ (first$ ?shift))))
