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
           ?*current-core* = FALSE
           ?*result* = FALSE)

(definstances MAIN::cores
              ([primary0] of cisc0-core-model1)
              ([dma] of iris-core)
              ([memory0] of memory-block
                         (length (hex->int 0x00400000))))
(deffunction MAIN::call-read
             (?target ?address)
             (send ?target
                   read
                   ?address))
(deffunction MAIN::call-write
             (?target ?address ?value)
             (send ?target
                   write
                   ?address
                   ?value))
(deffunction MAIN::write-iris-data
             (?target ?address ?value)
             (send ?target 
                   write-data-memory
                   ?address
                   ?value))
(deffunction MAIN::read-iris-data
             (?target ?address)
             (send ?target
                   read-data-memory
                   ?address))

; for this machine, assume that all 64-bit entries are actually 16-bit words even though they are not, this makes the code very simple
; with the tradeoff being wasted space for the time being
(deffacts MAIN::memory-map
          ; we have a 64-bit memory space with 16-bit words stored in 64-bit signed words :D
          (map [memory0] from  0x0000000000000000 to 0x0000000000400000)
          (map [dma] from      0x0000000000400000 to 0x0000000000402000 read read-iris-data write write-iris-data)
          ; TODO: comeup with a way to describe engine mappings easily and efficiently :D
          ;(map [dma] from      0x0000000000402000 to 0x0000000000404000 read read-iris-io write write-iris-io
          )
(defclass memory-map-entry
  (is-a USER)
  (slot target
        (type INSTANCE)
        (storage local)
        (default ?NONE))
  (slot from
        (type INTEGER)
        (storage local)
        (default ?NONE))
  (slot to
        (type INTEGER)
        (storage local)
        (default ?NONE))
  (slot on-read
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic call-read))
  (slot on-write
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic call-write))
  (message-handler write primary)
  (message-handler read primary))

(defmessage-handler memory-map-entry read primary
                    (?address)
                    (funcall ?self:on-read
                             ?self:target
                             ?address))
(defmessage-handler memory-map-entry write primary
                    (?address ?value)
                    (funcall ?self:on-write
                             ?self:target
                             ?address
                             ?value))

(defrule MAIN::make-memory-map-entry
         ?f <- (map ?target from ?start to ?end)
         =>
         (retract ?f)
         (make-instance of memory-map-entry
                        (target ?target)
                        (from (hex->int ?start))
                        (to (hex->int ?end))))

(defrule MAIN::make-memory-map-entry-with-custom-actions
         ?f <- (map ?target from ?start to ?end read ?on-read write ?on-write)
         =>
         (retract ?f)
         (make-instance of memory-map-entry
                        (target ?target)
                        (from (hex->int ?start))
                        (to (hex->int ?end))
                        (on-read ?on-read)
                        (on-write ?on-write)))

(deffunction MAIN::process-io-event
             ()
             (run)
             (if (not ?*result*) then
               (halt)
               else
               ?*result*))

(deffunction MAIN::read-from-io-address
             (?address)
             ; okay, we have an address to read from
             (assert (read ?*current-core*
                           ?address))
             (process-io-event))

(deffunction MAIN::write-to-io-address
             (?address ?value)
             (assert (write ?*current-core*
                            ?address
                            ?value))
             (process-io-event))

(deffunction MAIN::main-cycle
             (?core)
             (bind ?*current-core*
                   ?core)
             (send ?*current-core*
                   cycle))

(deffunction MAIN::doit
             ()
             (while TRUE do
                    (if (not (main-cycle [primary0])) then
                      (break))
                    ; run the dma engine for 4 cycles for every primary cpu cycle!
                    (bind ?run
                          TRUE)
                    (loop-for-count 4 do
                                    (bind ?run
                                          (main-cycle [dma]))
                                    (if (not ?run) then
                                      (break)))
                    (if (not ?run) then
                      (break))))

