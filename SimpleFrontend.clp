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

(defgeneric MAIN::register)
(defgeneric MAIN::list-commands)
(defgeneric MAIN::list-registers)

(deffacts MAIN::initialization
          (stage (current init)
                 (rest)))
(defglobal MAIN
           ?*rng-device* = /tmp/syn/rng
           ?*alu-device* = /tmp/syn/alu
           ?*memory-device* = /tmp/syn/memory
           ?*gpr-device* = /tmp/syn/gpr
           ?*blu-device* = /tmp/syn/blu
           ?*cmp-device* = /tmp/syn/cmp)

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

(deffunction MAIN::irandom
             ()
             (integer (random)))

(deffunction MAIN::irandom64
             ()
             (binary-or (left-shift (irandom)
                                    32)
                        (irandom)))


(deffunction MAIN::generic-command
             (?device $?args)
             (if (write-command ?device
                                (format nil
                                        "%s callback %s"
                                        (implode$ ?args)
                                        (get-socket-name))) then
               (explode$ (read-command))))

(deffunction MAIN::gpr->index
             (?register)
             (string-to-field (sub-string 2 
                                          (str-length ?register)
                                          ?register)))
(deffunction MAIN::index->gpr
             (?index)
             (sym-cat r
                      ?index))

(defmethod MAIN::register
  ((?value SYMBOL))
  (gpr->index ?value))

(defmethod MAIN::register
  ((?value INTEGER))
  ?value)

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
             (generic-command ?device
                              shutdown))

(deffunction MAIN::memory-command
             ($?parameters)
             (generic-command ?*memory-device*
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
             (generic-command ?*gpr-device*
                              ?args))
(deffunction MAIN::get-register
             (?address)
             (nth$ 1 (gpr-command load
                                  (register ?address))))
(deffunction MAIN::set-register
             (?address ?value)
             (nth$ 1 (gpr-command store
                                  (register ?address)
                                  ?value)))
(deffunction MAIN::increment-register
             (?address)
             (nth$ 1 (gpr-command ++
                                  (register ?address))))
(deffunction MAIN::decrement-register
             (?address)
             (nth$ 1 (gpr-command -- 
                                  (register ?address))))
(deffunction MAIN::get-register-count
             ()
             (nth$ 1 (gpr-command size)))


(deffunction MAIN::blu-command
             ($?command)
             (generic-command ?*blu-device*
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
             (generic-command ?*alu-device*
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

(deffunction MAIN::cmp-command 
             ($?args)
             (generic-command ?*cmp-device*
                              ?args))

(deffunction MAIN::op:neq
             (?a ?b)
             (cmp-command neq
                          ?a 
                          ?b))
(deffunction MAIN::op:eq
             (?a ?b)
             (cmp-command eq
                          ?a 
                          ?b))
(deffunction MAIN::op:lt
             (?a ?b)
             (cmp-command lt
                          ?a
                          ?b))
(deffunction MAIN::op:gt
             (?a ?b)
             (cmp-command gt
                          ?a 
                          ?b))
(deffunction MAIN::op:le
             (?a ?b)
             (cmp-command le
                          ?a
                          ?b))
(deffunction MAIN::op:ge
             (?a ?b)
             (cmp-command ge
                          ?a
                          ?b))
;------------------------------------------------------------------------------
; startup rules 
;------------------------------------------------------------------------------
(defrule MAIN::setup-connection
         (stage (current init))
         ?f <- (setup connection ?name)
         =>
         (retract ?f)
         (setup (sym-cat /tmp/syn/ 
                         ?name)))

(defrule MAIN::populate-rng
         (stage (current init))
         ?f <- (setup rng ?path)
         =>
         (retract ?f)
         (bind ?*rng-device*
               ?path))

(defrule MAIN::populate-memory
         (stage (current init))
         ?f <- (setup memory ?path)
         =>
         (retract ?f)
         (bind ?*memory-device*
               ?path))
(defrule MAIN::populate-alu
         (stage (current init))
         ?f <- (setup alu ?path)
         =>
         (retract ?f)
         (bind ?*alu-device*
               ?path))
(defrule MAIN::populate-gpr
         (stage (current init))
         ?f <- (setup gpr ?path)
         =>
         (retract ?f)
         (bind ?*gpr-device*
               ?path))
(defrule MAIN::populate-blu
         (stage (current init))
         ?f <- (setup blu ?path)
         =>
         (retract ?f)
         (bind ?*blu-device*
               ?path))
(defrule MAIN::populate-cmp
         (stage (current init))
         ?f <- (setup cmp ?path)
         =>
         (retract ?f)
         (bind ?*cmp-device*
               ?path))
;------------------------------------------------------------------------------
; Combination of operations
;------------------------------------------------------------------------------
(deffunction MAIN::jump
             (?register ?address)
             (set-register ?register
                           ?address))

(deffunction MAIN::advance-pc
             (?register)
             (increment-register ?register))


(deffunction MAIN::push-value
             "Push a value onto the stack"
             (?register ?value)
             (increment-register ?register)
             (write-memory (get-register ?register)
                           ?value))

(deffunction MAIN::pop-value
             "Pop a value off the stack"
             (?register)
             (bind ?result
                   (read-memory (get-register ?register)))
             (decrement-register ?register)
             ?result)

(deffunction MAIN::indirect-load
             "Perform an indirect load from memory (use the address stored at the provided address to load from memory)"
             (?address)
             (read-memory
               (read-memory ?address)))

(deffunction MAIN::indirect-store
             "Perform in indirect store into memory (use the address stored at the provided address as the address to store to"
             (?address ?value)
             (write-memory (read-memory ?address)
                           ?value))

(deffunction MAIN::register-load
             "Use the contents of the given register to load from main memory"
             (?index)
             (read-memory (get-register ?index)))

(deffunction MAIN::register-store
             "Use the contents of two registers to store into memory"
             (?dest ?value)
             (write-memory (get-register ?dest)
                           (get-register ?value)))

(deffunction MAIN::jump-and-link
             (?address ?link ?pc)
             (set-register ?link
                           (+ (get-register ?pc)
                              1))
             (jump ?pc
                   ?address))

(deffunction MAIN::jump-to-register
             (?link ?pc)
             (set-register ?pc
                           (get-register ?link)))
(deffunction MAIN::clear-register
             (?register)
             (set-register ?register
                           0))
;------------------------------------------------------------------------------
; Print Register Contents
;------------------------------------------------------------------------------
(deffunction MAIN::print-register
             (?index ?router)
             (printout ?router
                       tab (format nil
                                   "r%d = 0x%x"
                                   ?index
                                   (get-register ?index))
                       crlf))

(defmethod MAIN::list-registers
  ((?router SYMBOL))
  (printout ?router
            "Registers: " crlf)
  (loop-for-count (?i 0 
                      (- (get-register-count) 
                         1)) do
                  (print-register ?i
                                  ?router)))

(defmethod MAIN::list-registers
  ()
  (list-registers t))

