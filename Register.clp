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
(deffunction MAIN::register:set-mask
             (?id ?mask)
             (call ?id
                   set-mask
                   ?mask))
(deffunction MAIN::register:get-mask
             (?id)
             (call ?id
                   get-mask))
(deffunction MAIN::register:write
             (?id ?value)
             (call ?id
                   set
                   ?value))
(deffunction MAIN::register:read
             (?id)
             (call ?id
                   get))
(defgeneric MAIN::register:increment)
(defmethod MAIN::register:increment
  ((?id EXTERNAL-ADDRESS)
   (?value INTEGER))
  (call ?id
        increment
        ?value))
(defmethod MAIN::register:increment
  ((?id EXTERNAL-ADDRESS))
  (call ?id
        increment))
(defgeneric MAIN::register:decrement)
(defmethod MAIN::register:decrement
  ((?id EXTERNAL-ADDRESS)
   (?value INTEGER))
  (call ?id
        decrement
        ?value))
(defmethod MAIN::register:decrement
  ((?id EXTERNAL-ADDRESS))
  (call ?id
        decrement))
(defgeneric MAIN::register:decode)
(defmethod MAIN::register:decode
  ((?id EXTERNAL-ADDRESS)
   (?mask INTEGER)
   (?shift INTEGER))
  (call ?id
        decode
        ?mask
        ?shift))
(defmethod MAIN::register:decode
  ((?id EXTERNAL-ADDRESS)
   (?mask INTEGER))
  (register:decode ?id
                   ?mask
                   0))

(defgeneric MAIN::register:encode)
(defmethod MAIN::register:encode
  ((?id EXTERNAL-ADDRESS)
   (?value INTEGER)
   (?mask INTEGER)
   (?shift INTEGER))
  (call ?id
        encode
        ?value
        ?mask
        ?shift))
(defmethod MAIN::register:encode
  ((?id EXTERNAL-ADDRESS)
   (?value INTEGER)
   (?mask INTEGER))
  (register:encode ?id
                   ?value
                   ?mask
                   0))


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
                    (register:set-mask (dynamic-get backing-store)
                                       ?mask))
(defmessage-handler MAIN::register get-mask primary
                    ()
                    (register:get-mask (dynamic-get backing-store)
                                       ?mask))

(defmessage-handler MAIN::register write primary
                    (?value)
                    (register:write (dynamic-get backing-store)
                                    ?value))
(defmessage-handler MAIN::register read primary
                    ()
                    (register:read (dynamic-get backing-store)))

(defmessage-handler MAIN::register increment primary
                    ($?count)
                    (register:increment (dynamic-get backing-store)
                                        (expand$ (first$ ?count))))
(defmessage-handler MAIN::register decrement primary
                    ($?count)
                    (register:decrement (dynamic-get backing-store)
                                        (expand$ (first$ ?count))))
(defmessage-handler MAIN::register decode primary
                    (?mask $?shift)
                    (register:decode (dynamic-get backing-store)
                                     ?mask
                                     (expand$ (first$ ?shift))))
(defmessage-handler MAIN::register encode primary
                    (?value ?mask $?shift)
                    (register:encode (dynamic-get backing-store)
                                     ?value
                                     ?mask
                                     (expand$ (first$ ?shift))))
