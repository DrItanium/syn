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

(defclass MAIN::memory-block
  (is-a external-device)
  (role concrete)
  (pattern-match reactive)
  (slot backing-type
        (storage shared)
        (access read-only)
        (create-accessor read)
        (source composite)
        (default memory-block))
  (slot capacity
        (type INTEGER)
        (range 0
               ?VARIABLE)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler init after)
  (message-handler read primary)
  (message-handler write primary)
  (message-handler populate primary)
  (message-handler move primary)
  (message-handler swap primary)
  (message-handler size primary)
  (message-handler decrement primary)
  (message-handler increment primary))

(defmessage-handler MAIN::memory-block init after
                    ()
                    (bind ?self:constructor-args
                          (dynamic-get capacity)))

(defmessage-handler MAIN::memory-block read primary
                    (?addr)
                    (send ?self
                          call
                          read
                          ?addr))
(defmessage-handler MAIN::memory-block write primary
                    (?addr ?value)
                    (send ?self
                          call
                          write
                          ?addr
                          ?value))
(defmessage-handler MAIN::memory-block populate primary
                    (?value)
                    (send ?self
                          call
                          populate
                          ?value))

(defmessage-handler MAIN::memory-block move primary
                    (?from ?to)
                    (send ?self
                          call
                          move
                          ?from
                          ?to))
(defmessage-handler MAIN::memory-block swap primary
                    (?addr0 ?addr1)
                    (send ?self
                          call
                          swap
                          ?addr0
                          ?addr1))

(defmessage-handler MAIN::memory-block decrement primary
                    (?addr)
                    (send ?self
                          call
                          decrement
                          ?addr))
(defmessage-handler MAIN::memory-block increment primary
                    (?addr)
                    (send ?self
                          call
                          increment
                          ?addr))
(defmessage-handler MAIN::memory-block size primary
                    ()
                    (send ?self
                          call
                          size))

(defgeneric MAIN::move
            "Move a value from one place to another, meant for multiple memory-blocks")
(defgeneric MAIN::swap
            "swap a value from one place to another, meant for multiple memory-blocks")
(defgeneric MAIN::zero
            "Zero the contents of the memory block")

(defmethod MAIN::move
  "transfer data from one memory-block to another"
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
  "call move when the two memory-blocks are the same"
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

(defmethod MAIN::move
  "transfer data from one memory-block to another"
  ((?src memory-block)
   (?src-address INTEGER)
   (?dest memory-block
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
  "call move when the two memory-blocks are the same"
  ((?src memory-block)
   (?src-address INTEGER)
   (?dest memory-block
          (eq ?src
              ?current-argument))
   (?dest-address INTEGER))
  (send ?src
        move
        ?src-address
        ?dest-address))

(defmethod MAIN::swap
  "transfer data from one memory-block to another"
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
  "call swap when the two memory-blocks are the same"
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
  ((?src memory-block)
   (?src-address INTEGER)
   (?dest memory-block
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
  "call swap when the two memory-blocks are the same"
  ((?src memory-block)
   (?src-address INTEGER)
   (?dest memory-block
          (eq ?src
              ?current-argument))
   (?dest-address INTEGER))
  (send ?src
        swap
        ?src-address
        ?dest-address))
(defmethod MAIN::zero
  ((?mem EXTERNAL-ADDRESS))
  (call ?mem
        populate
        0))
(defmethod MAIN::zero
  ((?mem memory-block))
  (send ?mem
        populate
        0))
(defclass MAIN::register-file
  "Represents a bunch of fast memory"
  (is-a memory-block)
  (message-handler init around))
(defmessage-handler MAIN::register-file init around
                    ()
                    (call-next-handler)
                    ; now we can zero out the block of memory
                    (zero (dynamic-get backing-store)))

(defgeneric MAIN::load-value)
(defgeneric MAIN::store-value)

(defmethod MAIN::load-value
 ((?memory EXTERNAL-ADDRESS)
  (?address INTEGER))
 (call ?memory
       read
       ?address))
(defmethod MAIN::load-value
 ((?memory memory-block)
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
 ((?memory memory-block)
  (?address INTEGER)
  (?value INTEGER))
 (send ?memory
       write
       ?address
       ?value))
