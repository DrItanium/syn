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
  (is-a device)
  (slot backing-type
        (storage shared)
        (default memory-block)
        (access read-only)
        (create-accessor read)
        (source composite))
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
  (message-handler clear primary)
  (message-handler populate primary)
  (message-handler move primary)
  (message-handler swap primary)
  (message-handler size primary)
  (message-handler decrement primary)
  (message-handler increment primary))

(defmessage-handler MAIN::memory-block init after
                    ()
                    (bind ?self:constructor-args
                          ?self:capacity))

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
(defmessage-handler MAIN::memory-block clear primary
                    ()
                    (send ?self
                          call
                          clear))
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
