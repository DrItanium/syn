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
; CoreBase.clp - base classes for execution cores and common concepts
;------------------------------------------------------------------------------
(defgeneric MAIN::core-initialize)
(defgeneric MAIN::core-shutdown)
(defgeneric MAIN::core-cycle)
(defgeneric MAIN::core-run)
(defclass MAIN::core
  "Concept of an execution core"
  (is-a external-address-wrapper)
  (role abstract)
  (pattern-match non-reactive)
  (slot backing-type
        (source composite)
        (storage shared)
        (default UNKNOWN-CORE!))
  (slot initialized
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (message-handler init after)
  (message-handler delete before)
  (message-handler initialize primary)
  (message-handler shutdown primary)
  (message-handler cycle primary)
  (message-handler run-for primary)
  (message-handler run primary))

(defmessage-handler MAIN::core initialize primary
                    ()
                    (if (not ?self:initialized) then
                      (bind ?self:initialized
                            (core-initialize ?self:backing-store))))
(defmessage-handler MAIN::core shutdown primary
                    ()
                    (if ?self:initialized then
                      (bind ?self:initialized
                            FALSE)
                      (core-shutdown ?self:backing-store)))

(defmessage-handler MAIN::core init after
                    ()
                    (send ?self
                          initialize))

(defmessage-handler MAIN::core delete before
                    ()
                    (send ?self
                          shutdown))


(defmessage-handler MAIN::core cycle primary
                    ()
                    (core-cycle ?self:backing-store))

(defmessage-handler MAIN::core run-for primary
                    (?count)
                    (core-cycle ?self:backing-store
                                ?count))

(defmessage-handler MAIN::core run primary
                    ()
                    (core-run ?self:backing-store))

(defmethod MAIN::core-initialize
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        initialize))

(defmethod MAIN::core-shutdown
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        shutdown))
(defmethod MAIN::core-cycle
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        cycle))
(defmethod MAIN::core-run
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        run))

(defmethod MAIN::core-cycle
  ((?core EXTERNAL-ADDRESS)
   (?count INTEGER))
  (if (<= ?count 0) then
    FALSE
    else
    (loop-for-count (?i 1 ?count) do
                    (core-cycle ?core))
    TRUE))

