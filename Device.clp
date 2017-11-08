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
(defclass MAIN::device
  "An interface for providing read and write functionality"
  (is-a USER)
  (role abstract)
  (pattern-match non-reactive)
  (message-handler read primary)
  (message-handler write primary))

(defclass MAIN::external-device
  (is-a external-address-wrapper
        device)
  (role abstract)
  (pattern-match non-reactive))


(defgeneric MAIN::move
            "Move a value from one place to another, meant for multiple devices")
(defgeneric MAIN::swap
            "swap a value from one place to another, meant for multiple devices")

(defmethod MAIN::move
  "transfer data from one device to another"
  ((?src device)
   (?src-address INTEGER)
   (?dest device
          (neq ?src
               ?current-argument))
   (?dest-address INTEGER))
  (send ?dest
        write
        ?dest-address
        (send ?src
              read
              ?src-address)))

(defmethod MAIN::move
  "call move when the two devices are the same"
  ((?src device)
   (?src-address INTEGER)
   (?dest device
          (eq ?src
              ?current-argument))
   (?dest-address INTEGER))
  (send ?src
        move
        ?src-address
        ?dest-address))

(defmethod MAIN::move
  "transfer data from one device to another"
  ((?src EXTERNAL-ADDRESS)
   (?src-address INTEGER)
   (?dest EXTERNAL-ADDRESS
          (neq ?src
               ?current-argument))
   (?dest-address INTEGER))
  (call ?dest
        write
        ?dest-address
        (call ?src
              read
              ?src-address)))

(defmethod MAIN::move
  "call move when the two devices are the same"
  ((?src EXTERNAL-ADDRESS)
   (?src-address INTEGER)
   (?dest EXTERNAL-ADDRESS
          (eq ?src
              ?current-argument))
   (?dest-address INTEGER))
  (call ?src
        move
        ?src-address
        ?dest-address))


(defmethod MAIN::swap
  "transfer data from one device to another"
  ((?src EXTERNAL-ADDRESS)
   (?src-address INTEGER)
   (?dest EXTERNAL-ADDRESS
          (neq ?src
               ?current-argument))
   (?dest-address INTEGER))
  (bind ?dest-value
        (call ?dest
              read
              ?dest-address))
  (call ?dest
        write
        ?dest-address
        (call ?src
              read
              ?src-address))
  (call ?src
        write
        ?src-address
        ?dest-value))

(defmethod MAIN::swap
  "call swap when the two devices are the same"
  ((?src EXTERNAL-ADDRESS)
   (?src-address INTEGER)
   (?dest EXTERNAL-ADDRESS
          (eq ?src
              ?current-argument))
   (?dest-address INTEGER))
  (call ?src
        swap
        ?src-address
        ?dest-address))

(defmethod MAIN::swap
  "swap data between two different memory blocks"
  ((?src device)
   (?src-address INTEGER)
   (?dest device
          (neq ?src
               ?current-argument))
   (?dest-address INTEGER))
  (bind ?dest-value
        (send ?dest
              read
              ?dest-address))
  (send ?dest
        write
        ?dest-address
        (send ?src
              read
              ?src-address))
  (send ?src
        write
        ?src-address
        ?dest-value))

(defmethod MAIN::swap
  "call swap when the two devices are the same"
  ((?src device)
   (?src-address INTEGER)
   (?dest device
          (eq ?src
              ?current-argument))
   (?dest-address INTEGER))
  (send ?src
        swap
        ?src-address
        ?dest-address))

(defgeneric MAIN::load-value)
(defgeneric MAIN::store-value)

(defmethod MAIN::load-value
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER))
  (call ?memory
        read
        ?address))
(defmethod MAIN::load-value
  ((?memory device)
   (?address INTEGER))
  (send ?memory
        read
        ?address))

(defmethod MAIN::store-value
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER)
   (?value INTEGER))
  (call ?memory
        write
        ?address
        ?value))

(defmethod MAIN::store-value
  ((?memory device)
   (?address INTEGER)
   (?value INTEGER))
  (send ?memory
        write
        ?address
        ?value))

