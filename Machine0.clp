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
; Machine0.clp - First architecture using the new ucoded techniques
;------------------------------------------------------------------------------
; The machine0 core is a 64-bit signed integer architecture with a 64-bit word
; and a 27-bit memory space. If the addresses are expanded to the byte level then it would
; be a 30-bit memory space!
; This 27 bit memory space is divided up into 8 16 mega word sections!
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* ExternalAddressWrapper.clp)
(batch* Device.clp)
(batch* MemoryBlock.clp)
(batch* Register.clp)
(batch* order.clp)
(defclass MAIN::machine0-memory-block
  (is-a memory-block)
  (slot capacity
        (source composite)
        (storage shared)
        (default (+ (hex->int 0x00FFFFFF) 
                    1)))
  (slot last-address
        (storage shared)
        (visibility public)
        (access read-only)
        (create-accessor read)
        (default (hex->int 0x00FFFFFF))))

(defclass MAIN::register-file
  (is-a memory-block)
  (slot capacity
        (source composite)
        (storage shared)
        (default 16)))

(defclass MAIN::memory-map-entry
  (is-a thing)
  (slot base-address 
        (type INTEGER)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot last-address 
        (type INTEGER)
        (storage local)
        (visibility public))
  (message-handler put-base-address after)
  (message-handler init after)
  (message-handler read primary)
  (message-handler write primary))
(defmessage-handler MAIN::memory-map-entry put-base-address after
                    (?addr)
                    (bind ?self:last-address
                          (+ ?addr
                             (send ?self:parent
                                   get-last-address))))
(defmessage-handler MAIN::memory-map-entry init after
                    ()
                    (bind ?self:last-address
                          (+ ?self:base-address
                             (send ?self:parent
                                   get-last-address))))

(defmessage-handler MAIN::memory-map-entry read primary
                    (?addr)
                    (send ?self:parent
                          read
                          (- ?addr
                             ?self:base-address)))
(defmessage-handler MAIN::memory-map-entry write primary
                    (?address ?value)
                    (send ?self:parent
                          write
                          (- ?address
                             ?self:base-address)
                          ?value))

; There are 8 memory spaces in this machine setup for a total of 1 gigabyte or 128 megawords
(deffacts MAIN::make-memory-blocks
          (make memory-block named space0)
          (make memory-block named space1)
          (make memory-block named space2)
          (make memory-block named space3)
          ;(make memory-block named space4)
          ;(make memory-block named space5)
          ;(make memory-block named space6)
          ;(make memory-block named space7)
          )
; The instruction pointer register is 27-bits wide or having a mask of 0x07FFFFFF 
; this applies to the stack register as well. All bits above the mask must be zero to maintain
; backwards compatibility
(defglobal MAIN
           ?*address-mask* = (hex->int 0x00FFFFFF)
           ?*address-mask27* = (hex->int 0x07FFFFFF))
; Internally, machine
(deffacts MAIN::make-registers
          ;(terminate at 128 cycles)
          ;(terminate at ?*address-mask* cycles)
          (terminate at (hex->int 0xFFFF) cycles)
          (make register named ip with mask ?*address-mask*)
          (make register named sp with mask ?*address-mask*)
          (make register named cs with mask ?*address-mask*)
          (make register named dictionary with mask ?*address-mask*)
          (make register named t0)
          (make register named t1)
          (make register named t2))

(defglobal MAIN
           ?*execution-cycle-stages* = (create$ read ; load the instruction from memory
                                                eval ; evaluate instruction input and construct the ucode sequence
                                                print ; invoke the execution unit and save the result as needed
                                                advance ; advance ip
                                                loop ; perform checks to see if we should loop or terminate
                                                ))
(deffacts MAIN::cycles
          (stage (current startup)
                 (rest initialize
                       execute
                       ?*execution-cycle-stages*
                       shutdown)))
; the cpu bootstrap process requires lower priority because it always exists in the background and dispatches 
; cycles as we go along. This is how we service interrupts and other such things. 
(defrule MAIN::boostrap-startup
         (declare (salience ?*priority:first*)) 
         (stage (current startup))
         =>
         (printout t 
                   "Machine0 System boot" crlf
                   "Starting up .... please wait" crlf))

(defrule MAIN::initialize:describe-phase
         (declare (salience ?*priority:first*))
         (stage (current initialize))
         =>
         (printout t
                   "Initializing memory space ... please wait" crlf))

(defrule MAIN::initialize:make-memory-block
         (stage (current initialize))
         ?f <- (make memory-block named ?name)
         =>
         (retract ?f)
         (printout t 
                   "Bringing up memory block: " ?name " .... ")
         (make-instance ?name of machine0-memory-block)
         (printout t 
                   Done crlf
                   tab (format nil
                               "size: 0x%x words"
                               (send (symbol-to-instance-name ?name)
                                     size)) crlf))
(defrule MAIN::initialize:make-register-file
         (stage (current initialize))
         ?f <- (make register-file named ?name)
         =>
         (retract ?f)
         (printout t
                   "Bringing up register-file: " ?name " .... ")
         (make-instance ?name of register-file)
         (printout t
                   Done crlf
                   tab (format nil
                               "register count: %d"
                               (send (symbol-to-instance-name ?name)
                                     size)) crlf))
(defrule MAIN::initialize:make-register-with-mask
         (stage (current initialize))
         ?f <- (make register named ?name with mask ?mask)
         =>
         (retract ?f)
         (printout t
                   "Bringing up register: " ?name " .... ")
         (make-instance ?name of register 
                        (mask ?mask))
         (printout t 
                   Done crlf
                   tab (format nil 
                               "mask: 0x%x"
                               ?mask) crlf))
(defrule MAIN::initialize:make-register-default
         (stage (current initialize))
         ?f <- (make register named ?name)
         =>
         (retract ?f)
         (printout t
                   "Bringing up register: " ?name " .... ")
         (make-instance ?name of register)
         (printout t
                   Done crlf))

(defrule MAIN::shutdown:print-phase
         (declare (salience ?*priority:first*))
         (stage (current shutdown))
         =>
         (printout t
                   "Shutting down machine0 system!" crlf))
(defrule MAIN::shutdown:delete-memory-block
         (stage (current shutdown))
         (object (is-a machine0-memory-block)
                 (name ?name))
         =>
         (assert (delete "memory block" 
                         ?name)))

(defrule MAIN::shutdown:delete-register-file
         (stage (current shutdown))
         (object (is-a register-file)
                 (name ?name))
         =>
         (assert (delete "register file"
                         ?name)))

(defrule MAIN::shutdown:delete-register
         (stage (current shutdown))
         (object (is-a register)
                 (name ?name))
         =>
         (assert (delete register ?name)))
(defrule MAIN::shutdown:delete-thingy
         (stage (current shutdown))
         ?f <- (delete ?title ?name)
         =>
         (retract ?f)
         (printout t
                   "Bringing down " ?title ": " (instance-name-to-symbol ?name) " .... ")
         (unmake-instance ?name)
         (printout t
                   Done crlf))

(defrule MAIN::shutdown:shutdown-complete
         (declare (salience -9000))
         (stage (current shutdown))
         =>
         (printout t
                   "shutdown complete .... bye" crlf))


(defrule MAIN::execute:generate-cycle-execute
         "setup the execution cycle!"
         (stage (current execute))
         =>
         (printout t
                   "Setting up the execution cycle!" crlf))

; The layout of the instruction is pretty simple, if the number is negative then it is
; a branch instruction
(deffunction MAIN::branch-instructionp
             "The most significant bit of an instruction signifies if it is a branch instruction or not"
             (?value)
             ; in this case it is bit 63,
             ; the proper code is:
             ; (<> (decode-bits ?value
             ;                 (hex->int 0x8000000000000000)
             ;                 62)
             ;    0))
             ; but since CLIPSIntegers are signed then we can just imply twos compliment
             (< ?value 
                0))

; The next 7 bits (56-62) are the group bits, the upper most 8 bits have the same purpose in 
; jumps and everything else!
(deffunction MAIN::get-group-bits
             (?value)
             (decode-bits ?value
                          (hex->int 0x7F00000000000000)
                          56))

(deffunction MAIN::instruction-volatile-bits
             (?value)
             (decode-bits ?value
                          (hex->int 0x00FFFFFFFFFFFFFF)
                          0))

(defrule MAIN::execute:execution-cycle:read-from-memory
         (stage (current read))
         (object (is-a register)
                 (name [ip])
                 (value ?addr))
         ?ms0 <- (object (is-a memory-map-entry)
                         (base-address ?ba&:(>= ?addr ?ba))
                         (last-address ?la&:(<= ?addr ?la)))
         =>
         (assert (instruction ?addr
                              (bind ?value
                                    (send ?ms0
                                          read
                                          ?addr)))))

; When dealing with non branch instructions, we have further bits defined for operations, the next
; 8 bits define the operation category
(deffunction MAIN::extract-operation-field
             (?value)
             (decode-bits ?value
                          (hex->int 0x00FF000000000000)
                          48))

(deftemplate MAIN::operation
             (slot address
                   (type INTEGER)
                   (default ?NONE))
             (slot original-value
                   (type INTEGER)
                   (default ?NONE))
             (slot type
                   (type SYMBOL
                         INSTANCE)
                   (allowed-symbols FALSE)
                   (default-dynamic FALSE))
             (multislot arguments))

; use a system of objects that are fixed in concept to describe the different fields that
; make up the instruction. 
(defclass MAIN::primary-class-descriptor
  "Is the instruction a branch or non-branch instruction?"
  (is-a thing)
  (slot parent
        (source composite)
        (default FALSE))
  (slot matches-with
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default ?NONE))
  (slot title
        (type SYMBOL)
        (allowed-symbols branch
                         non-branch)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default ?NONE)))

(definstances MAIN::primary-class-descriptors
              ([primary-class:branch] of primary-class-descriptor
                                      (matches-with TRUE)
                                      (title branch))
              ([primary-class:non-branch] of primary-class-descriptor
                                          (matches-with FALSE)
                                          (title non-branch)))
(defclass MAIN::bits-descriptor
  (is-a thing)
  (slot matches-with
        (type INTEGER)
        (range 0 127)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default ?NONE))
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default ?NONE)))

(defclass MAIN::group-bits-descriptor
  (is-a bits-descriptor)
  (slot matches-with
        (source composite)
        (range 0 127)))

(defclass MAIN::branch-group-bits-descriptor
  (is-a group-bits-descriptor)
  (slot parent
        (source composite)
        (default [primary-class:branch])))

(definstances MAIN::branch-group-ops
              ([group-branch:immediate-direct] of branch-group-bits-descriptor
                                               (matches-with 0)
                                               (title immediate))
              ([group-branch:immediate-indirect] of branch-group-bits-descriptor
                                                 (matches-with 1)
                                                 (title immediate-indirect))
              ([group-branch:register] of branch-group-bits-descriptor
                                       (matches-with 2)
                                       (title register))
              ([group-branch:register-indirect] of branch-group-bits-descriptor
                                                (matches-with 3)
                                                (title register-indirect))
              ([group-branch:register-computed] of branch-group-bits-descriptor
                                                (matches-with 4)
                                                (title register-computed))
              ([group-branch:conditional-immediate] of branch-group-bits-descriptor
                                                    (matches-with 5)
                                                    (title conditional-immediate))
              ([group-branch:conditional-immediate-indirect] of branch-group-bits-descriptor
                                                             (matches-with 6)
                                                             (title conditional-immediate-indirect))
              ([group-branch:conditional-register] of branch-group-bits-descriptor
                                                   (matches-with 7)
                                                   (title conditional-register))
              ([group-branch:conditional-register-indirect] of branch-group-bits-descriptor 
                                                            (matches-with 8)
                                                            (title conditional-register-indirect)))
(defclass MAIN::non-branch-group-bits-descriptor
  (is-a group-bits-descriptor)
  (slot parent 
        (source composite)
        (default [primary-class:non-branch])))

(definstances MAIN::non-branch-group-ops
              ([group-non-branch:basic] of non-branch-group-bits-descriptor
                                        (matches-with 0)
                                        (title basic))
              ; TODO: define more of these
              ([group-non-branch:arithmetic] of non-branch-group-bits-descriptor
                                             (matches-with 1)
                                             (title arithmetic)))

(defclass MAIN::operation-bits-descriptor
  (is-a bits-descriptor)
  (slot matches-with
        (source composite)
        (range 0 255)))

(defclass MAIN::basic-operation-bits-descriptor
  (is-a operation-bits-descriptor)
  (slot parent
        (source composite)
        (default [group-non-branch:basic])))
(definstances MAIN::basic-operations-descs
              ([basic-op:nop] of basic-operation-bits-descriptor
                              (matches-with 0)
                              (title nop)))

(defrule MAIN::eval:get-more-information
         (stage (current eval))
         ?f <- (instruction ?addr
                            ?value)
         =>
         (retract ?f)
         (assert (operation (address ?addr)
                            (original-value ?value))))

; These are constant fields that we know about so we should keep them around
(defrule MAIN::eval:mark-primary-descriptor
         (stage (current eval))
         ?f <- (operation (original-value ?value)
                          (type FALSE))
         (object (is-a primary-class-descriptor)
                 (matches-with =(branch-instructionp ?value))
                 (name ?descriptor))
         =>
         (modify ?f
                 (type ?descriptor)))
(defrule MAIN::eval:mark-group-bits-descriptor
         (stage (current eval))
         ?f <- (operation (type ?primary)
                          (original-value ?value))
         (object (is-a primary-class-descriptor)
                 (name ?primary))
         (object (is-a group-bits-descriptor)
                 (parent ?primary)
                 (matches-with =(get-group-bits ?value))
                 (name ?group))
         =>
         (modify ?f
                 (type ?group)))
(defrule MAIN::eval:mark-operation-bits-descriptor
         (stage (current eval))
         ?f <- (operation (type ?group)
                          (original-value ?value))
         (object (is-a group-bits-descriptor)
                 (name ?group)
                 (parent ?primary))
         (object (is-a primary-class-descriptor)
                 (name ?primary)
                 (matches-with FALSE))
         (object (is-a operation-bits-descriptor)
                 (parent ?group)
                 (matches-with =(extract-operation-field ?value))
                 (name ?operation))
         =>
         (modify ?f 
                 (type ?operation)))


(defrule MAIN::print:drop-nops
         "Regardless of the nop style, we should drop it from execution immediately"
         (declare (salience ?*priority:first*))
         (stage (current print))
         ?f <- (operation (type ?operation))
         (object (is-a operation-bits-descriptor)
                 (name ?operation)
                 (title nop))
         =>
         (retract ?f))





(defrule MAIN::execute:execution-cycle:advance:next-address
         "If we didn't update the instruction pointer then make sure we do that now!"
         (stage (current advance))
         (not (check [ip]))
         ?ip <- (object (is-a register)
                        (name [ip]))
         =>
         (assert (check [ip]))
         (send ?ip 
               increment))

(defrule MAIN::loop:restart-cycle
         ?f <- (stage (current loop)
                      (rest $?rest))
         ?f2 <- (check ?ip)
         (object (is-a register)
                 (name ?ip)
                 (value ?value))
         (terminate at ?addr cycles)
         (test (<> ?value 
                   ?addr))
         =>
         (retract ?f2)
         (modify ?f 
                 (rest ?*execution-cycle-stages*
                       $?rest)))


(deffacts MAIN::memory-map
          ; dumb memory map description, start at address zero and fill this out
          ; until we hit the end of the memory map
          (make memory-map-entry parent [space0] base 0)
          (make memory-map-entry parent [space1] follows [space0])
          (make memory-map-entry parent [space2] follows [space1])
          (make memory-map-entry parent [space3] follows [space2]))

(defrule MAIN::initialize:construct-memory-map-entry
         (stage (current initialize))
         ?f <- (make memory-map-entry parent ?space base ?base)
         (object (is-a machine0-memory-block)
                 (name ?space))
         =>
         (retract ?f)
         (make-instance of memory-map-entry 
                        (parent ?space)
                        (base-address ?base)))
(defrule MAIN::initialize:concat-memory-map
         "Use the previous memory map entry to identify where to place this one"
         (stage (current initialize))
         ?f <- (make memory-map-entry parent ?space follows ?other-space)
         (object (is-a memory-map-entry)
                 (parent ?other-space)
                 (last-address ?b))
         (object (is-a machine0-memory-block)
                 (name ?space))
         =>
         (retract ?f)
         (assert (make memory-map-entry parent ?space base (+ ?b 1))))
