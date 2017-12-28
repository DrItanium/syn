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
(batch* Paragraph.clp)
(batch* SimpleServer.clp)
(defglobal MAIN
           ?*memory-block16-mask* = (hex->int 0xFFFFFF)
           ?*memory-block16-capacity* = (+ ?*memory-block16-mask*
                                           1)
           ?*memory-manager-space* = (hex->int 0xFFFFFFF)
           ?*memory-manager-mask* = (hex->int 0xF000000)
           ?*memory-manager-shift* = 24)
(defclass MAIN::memory-block16
  (is-a memory-block)
  (slot capacity
        (source composite)
        (default ?*memory-block16-capacity*))
  (message-handler write primary)
  (message-handler read primary))
(defmessage-handler MAIN::memory-block16 read primary
                    (?address)
                    (override-next-handler (binary-and ?*memory-block16-mask*
                                                       ?address)))
(defmessage-handler MAIN::memory-block16 write primary
                    (?address ?value)
                    (override-next-handler (binary-and ?*memory-block16-mask*
                                                       ?address)
                                           ?value))

(defclass MAIN::memory-manager
  (is-a encyclopedia-container)
  (role concrete)
  (pattern-match reactive)
  (message-handler compute-child-address primary))

(defmessage-handler MAIN::memory-manager compute-child-address primary
                    (?address)
                    (decode-bits ?address
                                 ?*memory-manager-mask*
                                 ?*memory-manager-shift*))

(deffacts MAIN::setup-watcher
          (make memory-manager)
          (make memory-block16 count: 4))

(defrule MAIN::construct-memory-manager
         (stage (current system-init))
         ?f <- (make memory-manager)
         =>
         (retract ?f)
         (make-instance of memory-manager
                        (children)))

(defrule MAIN::add-memory-block16
         (stage (current system-init))
         ?f <- (make memory-block16 count: ?x&:(> ?x 0))
         ?m <- (object (is-a memory-manager)
                       (children $?children))
         =>
         (retract ?f)
         (assert (make memory-block16 count: (- ?x 1)))
         (bind ?block
               (make-instance of memory-block16))
         (modify-instance ?m
                          (children ?children
                                    ?block)))
(defrule MAIN::add-memory-block16:retract
         (stage (current system-init))
         ?f <- (make memory-block16 count ?a&:(<= ?a 0))
         =>
         (retract ?f))

(defrule MAIN::add-memory-location-to-command
         (stage (current read))
         ?f <- (inspect action)
         ?k <- (action $?body)
         (object (is-a memory-manager)
                 (name ?manager))
         =>
         (retract ?f 
                  ?k)
         (assert (action ?body from ?manager)))

; TODO: add support for restarting execution
;----------------------------------------------------------------
; Commands are - read, write, shutdown

(defrule MAIN::read-memory
         (stage (current dispatch))
         ?k <- (action read ?address callback ?callback from ?target)
         =>
         (retract ?k)
         (assert (command-writer (target ?callback)
                                 (command (send ?target
                                                read
                                                ?address)))))

(defrule MAIN::write-memory
         (declare (salience 1))
         (stage (current dispatch))
         ?k <- (action write ?address ?value callback ?callback from ?target)
         =>
         (retract ?k)
         (assert (command-writer (target ?callback)
                                 (command (send ?target
                                                write
                                                ?address
                                                ?value)))))
(defrule MAIN::write-memory-set
         "VLIW style decoupling to maintain write ordering"
         (stage (current dispatch))
         ?k <- (action write set: ?address ?value $?cmds callback ?callback from ?target)
         =>
         (retract ?k)
         (assert (action write set: $?cmds callback ?callback from ?target)
                 (action write ?address ?value callback ?callback from ?target)))

(defrule MAIN::write-memory-map
         (stage (current dispatch))
         ?k <- (action write map: ?address $?values callback ?callback from ?target)
         =>
         (retract ?k)
         (bind ?result
               TRUE)
         (progn$ (?value $?values)
                 (bind ?result
                       (and (send ?target
                                  write
                                  (+ ?address
                                     (- ?value-index
                                        1))
                                  ?value)
                            ?result)))
         (assert (command-writer (target ?callback)
                                 (command ?result))))

(defrule MAIN::write-memory-set:retract
         (stage (current dispatch))
         ?k <- (action write set: callback ? from ?)
         =>
         (retract ?k))
