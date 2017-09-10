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

(batch* lib/cortex.clp)

(defmodule MAIN
           (import cortex
                   ?ALL)
           (export ?ALL))

(set-current-module MAIN)

(batch* lib/target/IODevice.clp)
(batch* lib/target/RandomNumberDevice.clp)
(batch* lib/target/ExternalAddressWrapper.clp)
(batch* lib/target/AssemblerBase.clp)
(batch* lib/target/CoreBase.clp)
(batch* lib/target/iris/Base.clp)
(batch* lib/target/cisc0/Base.clp)

(defclass MAIN::memory-block
  (is-a external-address-wrapper)
  (role concrete)
  (pattern-match reactive)
  (slot backing-type
        (source composite)
        (storage shared)
        (default memory-block))
  (slot length
        (type INTEGER)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler read primary)
  (message-handler write primary)
  (message-handler get-constructor-args primary))

(defmessage-handler memory-block get-constructor-args primary
                    ()
                    (create$ (dynamic-get length)))
(defmessage-handler memory-block read primary
                    (?address)
                    (send ?self
                          call
                          get
                          ?address))

(defmessage-handler memory-block write primary
                    (?address ?value)
                    (send ?self
                          call
                          set
                          ?address
                          ?value))

(defglobal MAIN
           ?*current-core* = FALSE)

(definstances MAIN::cores
              ([primary0] of cisc0-core-model1)
              ([dma] of iris-core)
              ([memory0] of memory-block
                         (length (hex->int 0x00400000))))

(deffacts MAIN::memory-map
          ; we have a 64-bit memory space with 64-bit words
          (map [primary0] from 0x0000000000000000 to 0x000000003FFFFFFF)
          (map [memory0] from  0x0000000000000000 to 0x0000000000400000)
          (map [dma] from      0x0000000000400000 to 0x0000000000402000) 
          )

(deffunction MAIN::read-from-io-address
             (?address)
             (assert (read ?address)))

(deffunction MAIN::write-to-io-address
             (?address ?value)
             (assert (write ?address
                            ?value)))
