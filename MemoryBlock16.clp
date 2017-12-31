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
; MemoryBlock16.clp - a separate device process meant to respond on
; named pipes to requests
;------------------------------------------------------------------------------
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* ExternalAddressWrapper.clp)
(batch* Device.clp)
(batch* MemoryBlock.clp)
(batch* order.clp)
(batch* SimpleServer.clp)
(defglobal MAIN
           ?*memory-block16-mask* = (hex->int 0xFFFFFF)
           ?*memory-block16-capacity* = (+ ?*memory-block16-mask*
                                           1)
           ?*memory-manager-space* = (hex->int 0xFFFFFFF)
           ?*memory-manager-mask* = (hex->int 0xF000000)
           ?*memory-manager-shift* = 24)
(defclass MAIN::memory-map-entry
  (is-a USER)
  (slot backing-store
        (type INSTANCE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot start-address
        (type INTEGER)
        (range 0 ?VARIABLE)
        (default ?NONE))
  (slot end-address
        (type INTEGER)
        (range 0 ?VARIABLE)
        (default ?NONE))
  (message-handler visible-to-internal primary)
  (message-handler read primary)
  (message-handler write primary)
  (message-handler map-write primary))

(defmessage-handler memory-map-entry visible-to-internal primary
                    (?address)
                    ?address)
(defmessage-handler memory-map-entry read primary
                    (?address)
                    (send ?self:backing-store
                          read
                          (send ?self
                                visible-to-internal
                                ?address)))
(defmessage-handler memory-map-entry write primary
                    (?address ?value)
                    (send ?self:backing-store
                          write
                          (send ?self
                                visible-to-internal
                                ?address)
                          ?value))

(defmessage-handler memory-map-entry map-write primary
                    (?start $?values)
                    (progn$ (?value ?values)
                            (if (send ?self
                                      write 
                                      ?start
                                      ?value) then
                              (bind ?start
                                    (+ ?start
                                       1))
                              else
                              (return FALSE)))
                    TRUE)


(defclass MAIN::memory-map-entry:memory-block
  (is-a memory-map-entry)
  (slot backing-store
        (source composite)
        (allowed-classes memory-block))
  (slot internal-start-address
        (type INTEGER)
        (range 0 ?VARIABLE)
        (default ?NONE))
  (slot internal-end-address
        (type INTEGER)
        (range 0 ?VARIABLE)
        (default ?NONE))
  (message-handler map-write primary)
  (message-handler visible-to-internal primary))

(defmessage-handler memory-map-entry:memory-block map-write primary
                    (?start $?values)
                    (send (dynamic-get backing-store)
                          map-write
                          ?start
                          (expand$ ?values)))

(defmessage-handler memory-map-entry:memory-block visible-to-internal primary
                    (?address)
                    (+ (dynamic-get internal-start-address)
                       (- ?address
                          (dynamic-get start-address))))

(definstances MAIN::memory-blocks
              (mem0 of memory-block
                    (capacity ?*memory-block16-capacity*))
              (mem1 of memory-block
                    (capacity ?*memory-block16-capacity*))
              (mem2 of memory-block
                    (capacity ?*memory-block16-capacity*))
              (mem3 of memory-block
                    (capacity ?*memory-block16-capacity*))
              (of memory-map-entry:memory-block 
                  (backing-store [mem0])
                  (start-address 0)
                  (end-address ?*memory-block16-mask*)
                  (internal-start-address 0)
                  (internal-end-address ?*memory-block16-mask*))
              (of memory-map-entry:memory-block 
                  (backing-store [mem1])
                  (start-address ?*memory-block16-capacity*)
                  (end-address (+ ?*memory-block16-capacity*
                                          ?*memory-block16-mask*))
                  (internal-start-address 0)
                  (internal-end-address ?*memory-block16-mask*))
              (of memory-map-entry:memory-block 
                  (backing-store [mem2])
                  (start-address (* 2 ?*memory-block16-capacity*))
                  (end-address (+ (* 2 ?*memory-block16-capacity*)
                                          ?*memory-block16-mask*))
                  (internal-start-address 0)
                  (internal-end-address ?*memory-block16-mask*))
              (of memory-map-entry:memory-block 
                  (backing-store [mem3])
                  (start-address (* 3 ?*memory-block16-capacity*))
                  (end-address (+ (* 3 ?*memory-block16-capacity*)
                                          ?*memory-block16-mask*))
                  (internal-start-address 0)
                  (internal-end-address ?*memory-block16-mask*)))

; TODO: add support for restarting execution
;----------------------------------------------------------------
; Commands are - read, write, shutdown
(defrule MAIN::read-memory
         (stage (current dispatch))
         ?k <- (action read ?address callback ?callback)
         ?obj <- (object (is-a memory-map-entry)
                         (start-address ?start&:(<= ?start
                                                    ?address))
                         (end-address ?end&:(>= ?end
                                                ?address)))
         =>
         (retract ?k)
         (assert (command-writer (target ?callback)
                                 (command (send ?obj
                                                read
                                                ?address)))))

(defrule MAIN::write-memory
         (declare (salience 1))
         (stage (current dispatch))
         ?k <- (action write ?address ?value callback ?callback)
         ?obj <- (object (is-a memory-map-entry)
                         (start-address ?start&:(<= ?start
                                                    ?address))
                         (end-address ?end&:(>= ?end 
                                                ?address)))
         =>
         (retract ?k)
         (assert (command-writer (target ?callback)
                                 (command (send ?obj
                                                write
                                                ?address
                                                ?value)))))
(defrule MAIN::write-memory-set
         "VLIW style decoupling to maintain write ordering"
         (stage (current dispatch))
         ?k <- (action write set: ?address ?value $?cmds callback ?callback)
         =>
         (retract ?k)
         (assert (action write set: $?cmds callback ?callback)
                 (action write ?address ?value callback ?callback)))

(defrule MAIN::write-memory-set:retract
         (stage (current dispatch))
         ?k <- (action write set: callback ?)
         =>
         (retract ?k))

(defrule MAIN::write-memory-map:same-entry
         "if the first and last address map to the same entry then we're good, go for it :D"
         (stage (current dispatch))
         ?k <- (action write map: ?address $?values callback ?callback)
         ?start <- (object (is-a memory-map-entry)
                           (start-address ?a-start&:(<= ?a-start ?address))
                           (end-address ?a-end&:(>= ?a-end ?address)))
         ?end <- (object (is-a memory-map-entry)
                         (start-address ?b-start&:(<= ?b-start (+ ?address
                                                                  (length$ ?values))))
                         (end-address ?b-end&:(>= ?b-end (+ ?address
                                                            (length$ ?values)))))
         (test (eq ?start
                   ?end))
         =>
         (retract ?k)
         (assert (command-writer (target ?callback)
                                 (command (send ?start
                                                map-write
                                                ?address
                                                (expand$ ?values))))))

(defrule MAIN::write-memory-map:different-entries
         "We have hit a case where our request spans many different (more than 1) entries"
         (stage (current dispatch))
         ?k <- (action write map: ?address $?values callback ?callback)
         ?start <- (object (is-a memory-map-entry)
                           (start-address ?a-start&:(<= ?a-start ?address))
                           (end-address ?a-end&:(>= ?a-end ?address)))
         ?end <- (object (is-a memory-map-entry)
                         (start-address ?b-start&:(<= ?b-start (+ ?address
                                                                  (length$ ?values))))
                         (end-address ?b-end&:(>= ?b-end (+ ?address
                                                            (length$ ?values)))))
         (test (neq ?start
                    ?end))
         =>
         (retract ?k)
         ; need to break this design up into multiple facts, but we first need to slice the front
         ; off into a separate fact
         ; need to compute the end position of our collection
         (bind ?sub-section-length
               (- ?a-end
                  ?address))
         (if (send ?start
                   map-write
                   ?address
                   (subseq$ ?values
                            1
                            ?sub-section-length)) then
          (assert (action write 
                          map: (+ ?a-end 1) 
                          (subseq$ ?values
                                   (+ ?sub-section-length 1)
                                   (length$ ?values))
                          callback ?callback))
          else
          (assert (command-writer (target ?callback)
                                  (command FALSE)))))


(deffacts MAIN::legal-commands
          (make legal-commands read ->)
          (make legal-commands write
                "write set:"
                "write map:" ->))
