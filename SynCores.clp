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
  (message-handler cycle primary)
  (message-handler run primary)
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
(defmessage-handler MAIN::core cycle primary
                    ()
                    (call ?self:reference
                          cycle))

(defmessage-handler MAIN::core run primary
                    ()
                    (call ?self:reference
                          run))
(defclass MAIN::register-core
  (is-a core)
  (message-handler get-register primary)
  (message-handler set-register primary)
  (message-handler read-memory primary)
  (message-handler write-memory primary))

(defmessage-handler MAIN::register-core get-register primary
                    (?index)
                    (call ?self:reference
                          get-register
                          ?index))
(defmessage-handler MAIN::register-core set-register primary
                    (?index ?value)
                    (call ?self:reference
                          set-register
                          ?index
                          ?value))

(defmessage-handler MAIN::register-core read-memory primary
                    (?address)
                    (call ?self:reference
                          read-memory
                          ?address))
(defmessage-handler MAIN::register-core write-memory primary
                    (?address ?value)
                    (call ?self:reference
                          write-memory
                          ?address
                          ?value))


(defclass MAIN::cisc0-core
  (is-a register-core)
  (slot type
        (source composite)
        (default cisc0-core)))

(defclass MAIN::iris-core
  (is-a register-core)
  (slot type
        (source composite)
        (default iris-core))
  (message-handler get-predicate-register primary)
  (message-handler set-predicate-register primary)
  (message-handler read-memory primary)
  (message-handler write-memory primary)
  (message-handler read-io-memory primary)
  (message-handler write-io-memory primary)
  (message-handler read-data-memory primary)
  (message-handler write-data-memory primary)
  (message-handler read-code-memory primary)
  (message-handler write-code-memory primary)
  (message-handler read-stack-memory primary)
  (message-handler write-stack-memory primary))

(defmessage-handler MAIN::iris-core read-memory primary
                    (?address)
                    (call ?self:reference 
                          read-data-memory
                          ?address))


(defmessage-handler MAIN::iris-core write-memory primary
                    (?address ?value)
                    (call ?self:reference 
                          write-data-memory
                          ?address
                          ?value))

(defmessage-handler MAIN::iris-core read-data-memory primary
                    (?address)
                    (call ?self:reference 
                          read-data-memory
                          ?address))


(defmessage-handler MAIN::iris-core write-data-memory primary
                    (?address ?value)
                    (call ?self:reference 
                          write-data-memory
                          ?address
                          ?value))
(defmessage-handler MAIN::iris-core read-stack-memory primary
                    (?address)
                    (call ?self:reference 
                          read-stack-memory
                          ?address))


(defmessage-handler MAIN::iris-core write-stack-memory primary
                    (?address ?value)
                    (call ?self:reference 
                          write-stack-memory
                          ?address
                          ?value))
(defmessage-handler MAIN::iris-core read-io-memory primary
                    (?address)
                    (call ?self:reference 
                          read-io-memory
                          ?address))


(defmessage-handler MAIN::iris-core write-io-memory primary
                    (?address ?value)
                    (call ?self:reference 
                          write-io-memory
                          ?address
                          ?value))

(defmessage-handler MAIN::iris-core read-code-memory primary
                    (?address)
                    (call ?self:reference 
                          read-code-memory
                          ?address))


(defmessage-handler MAIN::iris-core write-code-memory primary
                    (?address ?value)
                    (call ?self:reference 
                          write-code-memory
                          ?address
                          ?value))

(defmessage-handler MAIN::iris-core get-predicate-register primary
                    (?address)
                    (call ?self:reference 
                          get-predicate-register
                          ?address))


(defmessage-handler MAIN::iris-core set-predicate-register primary
                    (?address ?value)
                    (call ?self:reference 
                          set-predicate-register
                          ?address
                          ?value))
