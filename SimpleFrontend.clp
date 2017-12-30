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
; SimpleFrontend.clp - A program to control various other devices
;------------------------------------------------------------------------------
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* ExternalAddressWrapper.clp)
(batch* Device.clp)
(batch* MemoryBlock.clp)
(batch* Paragraph.clp)
(batch* order.clp)

(deffunction MAIN::setup
             (?device)
             (system (format nil
                             "mkdir /tmp/syn"))
             (system (format nil
                             "rm -f %s" 
                             ?device))
             (set-socket-name ?device)
             (setup-connection)
             (printout t "Listening on socket " ?device crlf)
             (printout t tab "Before exiting run (shutdown-connection)" crlf))
(deffunction MAIN::stop-connection
             ()
             (shutdown-connection))

(deffunction fake-dma-test
             "seed memory with random numbers"
             (?seed ?size ?rng ?device ?fdev)
             ; first seed the rng
             (write-command ?rng
                            (format nil
                                    "seed %d"
                                    ?seed))
             ; skip the first three
             (write-command ?rng skip)
             (write-command ?rng skip)
             (write-command ?rng skip)
             ; start at address zero
             (bind ?read-rng
                   (str-cat "read to " ?fdev))

             (loop-for-count (?i 0 ?size) do
                             (write-command ?rng
                                            ?read-rng)
                             (bind ?k 
                                   (explode$ (read-command)))
                             (bind ?cmd
                                   (str-cat "write " 
                                            ?i
                                            " "
                                            (nth$ 1
                                                  ?k)
                                            " callback "
                                            ?fdev))
                             (write-command ?device
                                            ?cmd)
                             ; ditch the result
                             (read-command)
                             ; skip two random numbers
                             (write-command ?rng
                                            skip)
                             (write-command ?rng
                                            skip)))

(deffunction fake-dma-test2
             "seed memory with random numbers from local"
             (?seed ?size ?device ?fdev)
             ; first seed the rng
             (seed ?seed)
             ; skip the first three
             (random)
             (random)
             (random)
             ; start at address zero
             (loop-for-count (?i 0 ?size) do
                             (bind ?k
                                   (integer (random)))
                             (bind ?cmd
                                   (format nil
                                           "write %d %d callback %s"
                                           ?i
                                           (integer (random))
                                           ?fdev))
                             (write-command ?device
                                            ?cmd)
                             ; ditch the result
                             (read-command)
                             ; skip two random numbers
                             (random)
                             (random)))

(deffunction fake-dma-test2
             "seed memory with random numbers from local"
             (?seed ?size ?device ?fdev)
             ; first seed the rng
             (seed ?seed)
             ; skip the first three
             (random)
             (random)
             (random)
             ; start at address zero
             (loop-for-count (?i 0 ?size) do
                             (write-command ?device
                                            (format nil
                                                    "write %d %d callback %s"
                                                    ?i
                                                    (integer (random))
                                                    ?fdev))
                             ; ditch the result
                             (read-command)
                             ; skip two random numbers
                             (random)
                             (random)))

(deffunction fake-dma-test3
             "seed memory with random numbers from local writing multiple entries at a time"
             (?seed ?size ?device ?fdev)
             ; first seed the rng
             (seed ?seed)
             ; skip the first three
             (random)
             (random)
             (random)
             ; start at address zero
             (bind ?i 
                   0)
             (while (< ?i ?size) do
                    (write-command ?device
                                   (format nil
                                           "write %d %d callback %s"
                                           ?i
                                           (integer (random))
                                           ?fdev))
                    ; ditch the result
                    ; skip two random numbers
                    (random)
                    (random)
                    (write-command ?device
                                   (format nil
                                           "write %d %d callback %s"
                                           (+ ?i 1)
                                           (integer (random))
                                           ?fdev))
                    (random)
                    (random)
                    (bind ?i
                          (+ ?i 2))
                    (read-command)
                    (read-command)))

(deffunction fake-dma-test4
             "seed memory with random numbers from local writing multiple entries at a time (4)"
             (?seed ?size ?device ?fdev)
             ; first seed the rng
             (seed ?seed)
             ; skip the first three
             (random)
             (random)
             (random)
             ; start at address zero
             (bind ?i 
                   0)
             (while (< ?i ?size) do
                    (write-command ?device
                                   (format nil
                                           "write %d %d callback %s"
                                           ?i
                                           (integer (random))
                                           ?fdev))
                    ; ditch the result
                    ; skip two random numbers
                    (random)
                    (random)
                    (write-command ?device
                                   (format nil
                                           "write %d %d callback %s"
                                           (+ ?i 1)
                                           (integer (random))
                                           ?fdev))
                    (random)
                    (random)
                    (write-command ?device
                                   (format nil
                                           "write %d %d callback %s"
                                           (+ ?i 2)
                                           (integer (random))
                                           ?fdev))
                    (random)
                    (random)
                    (write-command ?device
                                   (format nil
                                           "write %d %d callback %s"
                                           (+ ?i 3)
                                           (integer (random))
                                           ?fdev))
                    (random)
                    (random)
                    (bind ?i
                          (+ ?i 4))
                    (read-command)
                    (read-command)
                    (read-command)
                    (read-command)))
(deffunction MAIN::set-write
             (?callback $?values)
             (format nil
                     "write set: %s callback %s"
                     (implode$ ?values)
                     ?callback))
(deffunction MAIN::map-write
             (?callback ?base-address $?values)
             (format nil
                     "write map: %d %s callback %s"
                     ?base-address
                     (implode$ ?values)
                     ?callback))
(deffunction MAIN::irandom
             ()
             (integer (random)))
(deffunction fake-dma-test5
             "seed memory with random numbers from local writing multiple entries at a time compacted into one message"
             (?seed ?size ?device ?fdev)
             ; first seed the rng
             (seed ?seed)
             ; skip the first three
             (random)
             (random)
             (random)
             ; start at address zero
             (bind ?i 
                   0)
             (while (< ?i ?size) do
                    (write-command ?device
                                   (set-write ?fdev
                                              ?i (irandom)
                                              (+ ?i 1) (irandom)
                                              (+ ?i 2) (irandom)
                                              (+ ?i 3) (irandom)))

                    ; skip multiple random numbers
                    (random)
                    (random)
                    (random)
                    (random)
                    (random)
                    (random)
                    (random)
                    (random)
                    (bind ?i
                          (+ ?i 4))
                    (read-command)
                    (read-command)
                    (read-command)
                    (read-command)))
(deffunction fake-dma-test6
             "seed memory with random numbers from local writing multiple entries at a time compacted into one message"
             (?seed ?size ?device ?fdev)
             ; first seed the rng
             (seed ?seed)
             ; skip the first three
             (random)
             (random)
             (random)
             ; start at address zero
             (bind ?i 
                   0)
             (while (< ?i ?size) do
                    (write-command ?device
                                   (set-write ?fdev
                                              ?i (irandom)
                                              (+ ?i 1) (irandom)
                                              (+ ?i 2) (irandom)
                                              (+ ?i 3) (irandom)
                                              (+ ?i 4) (irandom)
                                              (+ ?i 5) (irandom)
                                              (+ ?i 6) (irandom)
                                              (+ ?i 7) (irandom)))

                    ; skip multiple random numbers
                    (random) (random)
                    (random) (random)
                    (random) (random)
                    (random) (random)
                    (random) (random)
                    (random) (random)
                    (random) (random)
                    (random) (random)
                    (bind ?i
                          (+ ?i 8))
                    (read-command) (read-command) (read-command) (read-command)
                    (read-command) (read-command) (read-command) (read-command)
                    ))

(deffunction fake-dma-test7
             "seed memory with random numbers from local writing multiple entries at a time compacted into one message, tweakable"
             (?seed ?size ?device ?fdev ?message-size)
             ; first seed the rng
             (seed ?seed)
             ; skip the first three
             (random)
             (random)
             (random)
             ; start at address zero
             (bind ?i 
                   0)
             (while (< ?i ?size) do
                    (bind ?contents
                          (create$))
                    (loop-for-count (?j 0 (- ?message-size 1)) do
                                    (bind ?contents
                                          ?contents
                                          (+ ?i ?j)
                                          (irandom))
                                    (random)
                                    (random))
                    (write-command ?device
                                   (set-write ?fdev
                                              ?contents))

                    ; skip multiple random numbers
                    (bind ?i
                          (+ ?i 
                             ?message-size))
                    (loop-for-count (?j 1 ?message-size) do (read-command))))

(deffunction vliw-sendoff
             (?size ?outdev ?callback)
             (timer (fake-dma-test7 (irandom) 
                                    (integer (- (** 2 24) 1)) 
                                    ?outdev
                                    ?callback
                                    ?size)))

(deffunction fake-dma-test8
             "seed memory with random numbers from local writing multiple entries at a time compacted into one message, tweakable and using the map command instead of set"
             (?seed ?size ?device ?fdev ?message-size)
             ; first seed the rng
             (seed ?seed)
             ; skip the first three
             (random)
             (random)
             (random)
             ; start at address zero
             (bind ?i 
                   0)
             (while (< ?i ?size) do
                    (bind ?contents
                          (create$))
                    (loop-for-count (?j 1 ?message-size) do
                                    (bind ?contents
                                          ?contents
                                          (irandom))
                                    (random)
                                    (random))
                    (write-command ?device
                                   (map-write ?fdev
                                              ?i
                                              ?contents))

                    ; skip multiple random numbers
                    (bind ?i
                          (+ ?i 
                             ?message-size))
                    (read-command)))
(deffunction vliw-map-test
             (?size ?outdev ?callback)
             (timer (fake-dma-test8 (irandom)
                                    (integer (- (** 2 24) 1))
                                    ?outdev
                                    ?callback
                                    ?size)))
(defgeneric MAIN::list-commands)
(defmethod MAIN::list-commands
  ((?device LEXEME)
   (?router SYMBOL))
  (printout ?router
            "List of supported commands for: " ?device crlf)
  (if (write-command ?device
                     (format nil
                             "list-commands callback %s"
                             (get-socket-name))) then
    (progn$ (?a (explode$ (read-command))) do
            (printout ?router
                      tab "- " ?a crlf))
    else
    (printout ?router
              "Unable to get list of supported commands!" crlf)))
(defmethod MAIN::list-commands
  ((?device LEXEME))
  (list-commands ?device
                 t))

(deffunction MAIN::shutdown-device
             (?device)
             (write-command ?device
                            shutdown))

(deffunction MAIN::generic-command
             (?device $?args)
             (if (write-command ?device
                                (format nil
                                        "%s callback %s"
                                        (implode$ ?args)
                                        (get-socket-name))) then
               (explode$ (read-command))))

(deffunction MAIN::memory-command
             ($?parameters)
             (generic-command /tmp/syn/memory
                              ?parameters))

(deffunction MAIN::read-memory
             (?address)
             (bind ?x
                   (memory-command read
                                   ?address))
             (if (multifieldp ?x) then
               (if (> (length$ ?x) 1) then
                 ?x
                 else
                 (nth$ 1 
                       ?x))
               else
               ?x))

(deffunction MAIN::write-memory
             (?address ?value)
             (if (multifieldp (bind ?x
                                    (memory-command write
                                                    ?address
                                                    ?value))) then
               (if (> (length$ ?x) 1) then
                 ?x
                 else
                 (nth$ 1 ?x))
               else
               ?x))

(deffunction MAIN::gpr-command
             ($?args)
             (generic-command /tmp/syn/gpr
                              ?args))

(deffunction MAIN::get-register
             (?address)
             (nth$ 1 (gpr-command load
                                  ?address)))
(deffunction MAIN::set-register
             (?address ?value)
             (nth$ 1 (gpr-command store
                                  ?address
                                  ?value)))
(deffunction MAIN::increment-register
             (?address)
             (nth$ 1 (gpr-command ++
                                  ?address)))
(deffunction MAIN::decrement-register
             (?address)
             (nth$ 1 (gpr-command -- 
                                  ?address)))

(deffunction MAIN::blu-command
             ($?command)
             (generic-command /tmp/syn/blu
                              ?command))

(deffunction MAIN::op:binary-and
             (?a ?b)
             (blu-command and 
                          ?a
                          ?b))
(deffunction MAIN::op:binary-or
             (?a ?b)
             (blu-command or
                          ?a
                          ?b))
(deffunction MAIN::op:binary-xor
             (?a ?b)
             (blu-command xor
                          ?a
                          ?b))
(deffunction MAIN::op:binary-nor
             (?a ?b)
             (blu-command nor
                          ?a
                          ?b))
(deffunction MAIN::op:binary-nand
             (?a ?b)
             (blu-command nand
                          ?a
                          ?b))
(deffunction MAIN::op:binary-not
             (?a)
             (blu-command not
                          ?a))

(deffunction MAIN::alu-command
             ($?args)
             (generic-command /tmp/syn/alu
                              ?args))

(deffunction MAIN::op:add
             (?a ?b)
             (alu-command add
                          ?a
                          ?b))

(deffunction MAIN::op:add3
             (?a ?b ?c)
             (op:add (op:add ?a
                             ?b)
                     ?c))

(deffunction MAIN::op:sub
             (?a ?b)
             (alu-command sub
                          ?a
                          ?b))
(deffunction MAIN::op:mul
             (?a ?b)
             (alu-command mul
                          ?a
                          ?b))
(deffunction MAIN::op:div
             (?a ?b)
             ; TODO: add support for detecting divide by zero
             (alu-command div
                          ?a
                          ?b))
(deffunction MAIN::op:rem
             (?a ?b)
             ; TODO: add support for detecting rem by zero
             (alu-command rem
                          ?a
                          ?b))
(deffunction MAIN::op:shift-right
             (?a ?b)
             (alu-command shift-right
                          ?a
                          ?b))
(deffunction MAIN::op:shift-left
             (?a ?b)
             (alu-command shift-left
                          ?a
                          ?b))
(deffunction MAIN::op:incr
             (?a)
             (op:add ?a 
                     1))

(deffunction MAIN::op:decr
             (?a)
             (op:sub ?a
                     1))
(deffunction MAIN::op:double
             (?a)
             (op:shift-left ?a
                            1))
(deffunction MAIN::op:halve
             (?a)
             (op:shift-right ?a
                             1))

(deffunction MAIN::op:square
             (?a)
             (op:mul ?a
                     ?a))

(deffunction MAIN::op:cube
             (?a)
             (op:mul (op:mul ?a 
                             ?a)
                     ?a))
; wrappers around different units
(deffunction MAIN::op:generic
             (?operation $?values)
             (funcall (sym-cat op:
                               ?operation)
              (expand$ ?values)))
(deffunction MAIN::op:register-register
             (?operation ?r0 ?r1)
             (op:generic ?operation
                         (get-register ?r0)
                         (get-register ?r1)))

(deffunction MAIN::op:register-immediate
             (?operation ?r0 ?imm)
             (op:generic ?operation
                         (get-register ?r0)
                         ?imm))

(deffunction MAIN::op:immediate-immediate
             (?operation ?imm0 ?imm1)
             (op:generic ?operation
                         ?imm0
                         ?imm1))

(deffunction MAIN::op:immediate-unary
             (?operation ?imm)
             (op:generic ?operation
                         ?imm))
(deffunction MAIN::op:register-unary
             (?operation ?r0)
             (op:generic ?operation
              (get-register ?r0)))

(deffunction MAIN::gpr->index
             (?register)
             (string-to-field (sub-string 2 
                                          (str-length ?register)
                                          ?register)))
(deffunction MAIN::index->gpr
             (?index)
             (sym-cat r
                      ?index))

(deffunction MAIN::op:mac
             (?a ?b ?c)
             (op:add (op:mul ?a 
                             ?b)
                     ?c))

(deffunction MAIN::op:register3
             (?operation ?r0 ?r1 ?r2)
             (op:generic ?operation
                         (get-register ?r0)
                         (get-register ?r1)
                         (get-register ?r2)))


(deffunction MAIN::op:register2-imm
             (?operation ?r0 ?r1 ?imm)
             (op:generic ?operation
                         (get-register ?r0)
                         (get-register ?r1)
                         ?imm))

