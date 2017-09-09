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

(defclass MAIN::random-number-generator
  (is-a io-device)
  (slot range-start
        (type INTEGER
              SYMBOL)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE)
        (default-dynamic FALSE))
  (slot range-end
        (type INTEGER
              SYMBOL)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE)
        (default-dynamic FALSE))
  (message-handler clear-range primary)
  (message-handler seed primary)
  (message-handler skip primary)
  (message-handler next primary))

(defmessage-handler random-number-generator seed primary
                    (?value)
                    (seed ?value))

(defmessage-handler random-number-generator skip primary
                    (?count)
                    (loop-for-count ?count 
                                    (random)))
(defmessage-handler random-number-generator next primary
                    ()
                    (if (and ?self:range-start
                             ?self:range-end) then
                      (random ?self:range-start
                              ?self:range-end)
                      else
                      (random)))

(defmessage-handler random-number-generator clear-range primary
                    ()
                    (bind ?self:range-start 
                          FALSE)
                    (bind ?self:range-end
                          FALSE))
