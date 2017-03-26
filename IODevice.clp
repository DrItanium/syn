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
(defclass MAIN::io-device
  (is-a USER)
  (slot index
        (type INTEGER)
        (range 0 ?VARIABLE)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot length
        (type INTEGER)
        (range 1 ?VARIABLE)
        (storage local)
        (visibility public)
        (default-dynamic 1))
  (message-handler responds-to primary)
  (message-handler read primary)
  (message-handler write primary))
(defmessage-handler MAIN::io-device responds-to primary
                    (?address)
                    (and (<= ?self:index
                             ?address)
                         (< ?address
                            (+ ?self:index
                               (dynamic-get length)))))

(defmessage-handler MAIN::io-device read primary
                    (?address)
                    0)

(defmessage-handler MAIN::io-device write primary
                    (?address ?value))

(defclass MAIN::native-io-device
  (is-a io-device)
  (slot native-reference
        (type EXTERNAL-ADDRESS)
        (storage local)
        (visibility public))
  (slot native-type
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (default PLEASE-REIMPLEMENT-THIS))
  (message-handler native-call primary)
  (message-handler native-type primary)
  (message-handler write primary)
  (message-handler read primary)
  (message-handler get-native-construction-args primary)
  (message-handler init after)
  (message-handler delete before))

(defmessage-handler MAIN::native-io-device native-call primary
                    (?operation $?args)
                    (call ?self:native-reference
                          ?operation
                          (expand$ ?args)))

(defmessage-handler MAIN::native-io-device native-type primary
                    ()
                    (call ?self:native-reference
                          type))
(defmessage-handler MAIN::native-io-device read primary
                    (?address)
                    (call ?self:native-reference
                          read
                          (- ?address
                             (dynamic-get index))))
(defmessage-handler MAIN::native-io-device write primary
                    (?address ?value)
                    (call ?self:native-reference
                          write
                          (- ?address
                             (dynamic-get index))
                          ?value))
(defmessage-handler MAIN::native-io-device init after
                    ()
                    (call (bind ?self:native-reference
                                (new (dynamic-get native-type)
                                     (expand$ (send ?self
                                                    get-native-construction-args))))
                          initialize))

(defmessage-handler MAIN::native-io-device get-native-construction-args primary
                    ()
                    (create$))
(defmessage-handler MAIN::native-io-device delete before
                    ()
                    (call ?self:native-reference
                          shutdown))
