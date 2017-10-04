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
(defglobal MAIN
           ?*address24bit* = (hex->int 0x00FFFFFF))
(defclass MAIN::machine0-memory-block
  (is-a memory-block)
  (slot capacity
        (source composite)
        (storage shared)
        (default (+ ?*address24bit*
                    1)))
  (slot last-address
        (storage shared)
        (visibility public)
        (access read-only)
        (create-accessor read)
        (default ?*address24bit*)))

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
                          (send ?self
                                compute-last-address
                                ?addr)))
(defmessage-handler MAIN::memory-map-entry compute-last-address primary
                    (?addr)
                    (+ ?addr
                       (send ?self:parent
                             get-last-address)))
(defmessage-handler MAIN::memory-map-entry init after
                    ()
                    (bind ?self:last-address
                          (send ?self
                                compute-last-address
                                ?self:base-address)))

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

(defclass MAIN::keyboard-controller
  "An abstraction layer over the keyboard, reading and writing to its MMIO does a getc and putc respectively"
  (is-a memory-map-entry)
  (slot parent
        (source composite)
        (default-dynamic FALSE))
  (slot router
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default-dynamic t))
  (message-handler compute-last-address primary)
  (message-handler read primary)
  (message-handler write primary))
(defmessage-handler MAIN::keyboard-controller compute-last-address primary
                    (?addr)
                    ; only one cell so don't do anything goofy
                    ?addr)
(defmessage-handler MAIN::keyboard-controller read primary
                    (?address)
                    (get-char ?self:router))
(defmessage-handler MAIN::keyboard-controller write primary
                    (?address ?value)
                    (put-char ?self:router
                              ?value))

(defclass MAIN::random-number-generator
  (is-a memory-map-entry)
  (slot parent
        (source composite)
        (default-dynamic FALSE))
  (slot seed
        (type INTEGER)
        (visibility public)
        (storage local)
        (default-dynamic (integer (time))))
  (slot current-value
        (type INTEGER)
        (visibility public)
        (create-accessor read)
        (storage local))
  (message-handler init after)
  (message-handler put-seed after)
  (message-handler compute-last-address primary)
  (message-handler read primary)
  (message-handler write primary))

(defmessage-handler MAIN::random-number-generator init after
                    ()
                    (seed (dynamic-get seed))
                    (dynamic-put current-value
                                 (random)))

(defmessage-handler MAIN::random-number-generator put-seed after
                    (?seed)
                    (seed ?seed))

(defmessage-handler MAIN::random-number-generator compute-last-address primary
                    (?addr)
                    (+ ?addr 1))
(defmessage-handler MAIN::random-number-generator read primary
                    (?address)
                    (switch (- ?address
                               (dynamic-get base-address))
                            ; random number port
                            (case 0 then
                              (bind ?result
                                    (dynamic-get current-value))
                              (dynamic-put current-value
                                           (random))
                              ?result)
                            ; seed port
                            (case 1 then
                              (dynamic-get seed))
                            (default 0)))
(defmessage-handler MAIN::random-number-generator write primary
                    (?address ?value)
                    (switch (- ?address
                               (dynamic-get base-address))
                            ; random number port, skip the current entry
                            (case 0 then
                              (dynamic-put current-value
                                           (random)))
                            (case 1 then
                              (dynamic-put seed
                                           ?value)
                              (seed ?value))
                            (default 0)))

(defclass MAIN::runlevel-controller
  "An abstraction layer over basic system runlevels and such things!"
  (is-a memory-map-entry)
  (slot parent
        (source composite)
        (default-dynamic FALSE))
  (message-handler compute-last-address primary)
  (message-handler read primary)
  (message-handler write primary))
(defmessage-handler MAIN::runlevel-controller compute-last-address primary
                    (?addr)
                    (+ ?addr 1))
(defmessage-handler MAIN::runlevel-controller write primary
                    (?address ?value)
                    (switch (- ?address
                               (dynamic-get base-address))
                            (case 0 then
                              (halt)
                              0)
                            (case 1 then
                              (assert (should shutdown))
                              0)
                            (default
                              0)))
(defmessage-handler MAIN::runlevel-controller read primary
                    (?addr)
                    0)

; There are 8 memory spaces in this machine setup for a total of 1 gigabyte or 128 megawords
(deffacts MAIN::make-memory-blocks
          (make memory-block named space0)
          (make memory-block named space1)
          (make memory-block named space2)
          (make memory-block named space3)
          (make memory-block named space4)
          (make memory-block named space5)
          (make memory-block named space6)
          ;(make memory-block named space7)
          )
; The instruction pointer register is 27-bits wide or having a mask of 0x07FFFFFF
; this applies to the stack register as well. All bits above the mask must be zero to maintain
; backwards compatibility
(defglobal MAIN
           ?*address-mask27* = (hex->int 0x07FFFFFF)
           ?*address-mask* = ?*address-mask27*
           ?*execution-cycle-stages* = (create$ read ; load the instruction from memory
                                                eval ; evaluate instruction input and construct the ucode sequence
                                                print ; invoke the execution unit and save the result as needed
                                                advance ; advance ip
                                                loop ; perform checks to see if we should loop or terminate
                                                ))
(defgeneric MAIN::address-mask)
(defgeneric MAIN::execution-cycle-stages)
(defmethod MAIN::address-mask () ?*address-mask*)
(defmethod MAIN::execution-cycle-stages () ?*execution-cycle-stages*)
; Internally, machine
(deffacts MAIN::make-registers
          ;(terminate at 128 cycles)
          ;(terminate at ?*address-mask* cycles)
          (terminate at (hex->int 0xFFFF) cycles)
          (make register named ip with mask (address-mask))
          (make register named sp with mask (address-mask))
          (make register named cs with mask (address-mask))
          (make register named dictionary with mask (address-mask))
          (make register named t0)
          (make register named t1)
          (make register named t2))

(deffacts MAIN::cycles
          (stage (current startup)
                 (rest initialize
                       check
                       execute
                       (execution-cycle-stages)
                       shutdown)))
(deffunction MAIN::note-bring-up
             (?router ?type ?name)
             (printout ?router
                       "Bringing up " ?type ": " ?name " .... "))
(deffunction MAIN::done-with-bring-up
             (?router $?extra)
             (printout ?router
                       Done crlf
                       (expand$ ?extra)))
(deffunction MAIN::request-future-delete
             (?type ?instance)
             (assert (delete ?type
                             ?instance)))

(deffunction MAIN::error-message
             ($?contents)
             (printout werror
                       "ERROR: " (expand$ ?contents)))



; the cpu bootstrap process requires lower priority because it always exists in the background and dispatches
; cycles as we go along. This is how we service interrupts and other such things.
(defrule MAIN::bootstrap-startup
         (declare (salience ?*priority:first*))
         (stage (current startup))
         =>
         (printout t
                   "Machine0 System boot" crlf
                   "Starting up .... please wait" crlf))
(defrule MAIN::bootstrap-set-address-mask
         (declare (salience 9999))
         (stage (current startup))
         (not (address-mask is ?))
         =>
         (assert (address-mask is (address-mask))))

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
         (note-bring-up t
                        "memory block"
                        ?name)
         (request-future-delete "memory block"
                                (make-instance ?name of machine0-memory-block))
         (done-with-bring-up t
                             tab "size: "
                             (int->hex (send (symbol-to-instance-name ?name)
                                             size)) crlf))
(defrule MAIN::initialize:make-register-file
         (stage (current initialize))
         ?f <- (make register-file named ?name)
         =>
         (retract ?f)
         (note-bring-up t
                        register-file
                        ?name)
         (request-future-delete "register file"
                                (make-instance ?name of register-file))
         (done-with-bring-up t
                             tab "register count: "
                             (send (symbol-to-instance-name ?name)
                                   size) crlf))

(defrule MAIN::initialize:make-register-with-mask
         (stage (current initialize))
         ?f <- (make register named ?name with mask ?mask)
         =>
         (retract ?f)
         (note-bring-up t
                        register
                        ?name)
         (request-future-delete register
                                (make-instance ?name of register
                                               (mask ?mask)))
         (done-with-bring-up t
                             tab "mask: " (int->hex ?mask) crlf))

(defrule MAIN::initialize:make-register-default
         (stage (current initialize))
         ?f <- (make register named ?name)
         =>
         (retract ?f)
         (note-bring-up t
                        register
                        ?name)
         (request-future-delete register
                                (make-instance ?name of register))
         (done-with-bring-up t))

(defrule MAIN::shutdown:print-phase
         (declare (salience ?*priority:first*))
         (stage (current shutdown))
         =>
         (printout t
                   "Shutting down machine0 system!" crlf))

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

(defrule MAIN::loop:restart-cycle:terminate-execution
         (stage (current loop))
         ?f <- (check ?ip)
         ?f2 <- (should shutdown)
         =>
         (retract ?f ?f2))
(defrule MAIN::loop:restart-cycle:terminate-execution-on-address
         (stage (current loop))
         ?f <- (check ?ip)
         (object (is-a register)
                 (name ?ip)
                 (value ?value))
         (terminate at ?addr cycles)
         (test (= ?value
                  ?addr))
         =>
         (retract ?f))


(defrule MAIN::loop:restart-cycle:continue-execution
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
                 (rest (execution-cycle-stages)
                       $?rest)))

(deftemplate MAIN::mmap
             "describes an mmap action to perform"
             (slot type
                   (type SYMBOL)
                   (default ?NONE))
             (slot parent
                   (type SYMBOL
                         INSTANCE)
                   (default-dynamic FALSE))
             (slot named
                   (type SYMBOL
                         INSTANCE-NAME)
                   (default-dynamic (gensym*)))
             (slot base
                   (type INTEGER
                         SYMBOL)
                   (allowed-symbols FALSE)
                   (default-dynamic FALSE))
             (slot follows
                   (type INSTANCE-NAME
                         SYMBOL)
                   (allowed-symbols FALSE)
                   (default-dynamic FALSE)))
(deffacts MAIN::memory-map
          ; dumb memory map description, start at address zero and fill this out
          ; until we hit the end of the memory map
          (mmap (type memory-map-entry) (parent [space0]) (base 0))
          (mmap (type memory-map-entry) (parent [space1]) (follows [space0]))
          (mmap (type memory-map-entry) (parent [space2]) (follows [space1]))
          (mmap (type memory-map-entry) (parent [space3]) (follows [space2]))
          (mmap (type memory-map-entry) (parent [space4]) (follows [space3]))
          (mmap (type memory-map-entry) (parent [space5]) (follows [space4]))
          (mmap (type memory-map-entry) (parent [space6]) (follows [space5]))
          (mmap (type keyboard-controller) (named [kc]) (parent FALSE) (follows [space6]))
          (mmap (type runlevel-controller) (named [rlc]) (parent FALSE) (follows [kc]))
          (mmap (type random-number-generator) (named [rng0]) (parent FALSE) (follows [rlc])))

(defrule MAIN::initialize:make-mmap-type
         (stage (current initialize))
         ?f <- (mmap (type ?type)
                     (follows FALSE)
                     (parent ?parent)
                     (named ?name&~FALSE)
                     (base ?base))
         =>
         (retract ?f)
         (bind ?k
               (make-instance ?name of ?type
                              (parent ?parent)
                              (base-address ?base)))
         (assert (delete ?type ?k)
                 (mapped ?type ?k)))

(defrule MAIN::initialize:concat-memory-map
         "Use the previous memory map entry to identify where to place this one"
         (stage (current initialize))
         ?f <- (mmap (follows ?other&~FALSE)
                     (base FALSE))
         (object (is-a memory-map-entry)
                 (parent ?other)
                 (last-address ?b))
         =>
         (modify ?f
                 (follows FALSE)
                 (base (+ ?b 1))))

(defrule MAIN::initialize:concat-memory-map:previous-is-memory-map
         "Use the previous memory map entry to identify where to place this one"
         (stage (current initialize))
         ?f <- (mmap (follows ?other&~FALSE)
                     (base FALSE))
         (object (is-a memory-map-entry)
                 (name ?other)
                 (last-address ?b))
         =>
         (modify ?f
                 (follows FALSE)
                 (base (+ ?b 1))))
(defrule MAIN::check:error-out-on-bogus-mmap-assertion
         (stage (current check))
         ?f <- (mmap (follows FALSE)
                     (base FALSE))
         =>
         (halt)
         (error-message "found a mmap request which does not follow anything and does not have a base address!" crlf))


(defrule MAIN::report-mapping
         (stage (current initialize))
         ?f <- (mapped ?type ?instance)
         (object (is-a ?type)
                 (name ?instance)
                 (parent ?block)
                 (base-address ?start)
                 (last-address ?end))
         =>
         (retract ?f)
         (printout t
                   "Mapped " ?type " named "
                   (instance-name-to-symbol (if ?block then ?block else ?instance))
                   " to the address range [" ?start ", " ?end "]" crlf))

(defrule MAIN::halt-on-mmap-equality
         "Every mmap entry should not have an overlap at system boot!"
         (stage (current check))
         (object (is-a memory-map-entry)
                 (base-address ?base)
                 (last-address ?last)
                 (name ?mme0))
         (object (is-a memory-map-entry)
                 (name ?mme1&~?mme0)
                 (base-address ?base)
                 (last-address ?last))
         =>
         (halt)
         (error-message "memory map entries: " ?mme0 " and " ?mme1 " occupy the exact same space!" crlf))

(defrule MAIN::halt-on-mmap-overlap
         "Every mmap entry should not have an overlap at system boot!"
         (stage (current check))
         (object (is-a memory-map-entry)
                 (base-address ?base0)
                 (last-address ?last0)
                 (name ?mme0))
         (object (is-a memory-map-entry)
                 (name ?mme1&~?mme0)
                 (base-address ?base1)
                 (last-address ?last1))
         (test (or (<= ?base0 ?base1 ?last0)
                   (<= ?base0 ?last1 ?last0)
                   (<= ?base1 ?base0 ?last1)
                   (<= ?base1 ?last0 ?last1)))
         =>
         (halt)
         (error-message "memory map entries: " ?mme0 " and " ?mme1 " overlap!" crlf))

(defrule MAIN::mmap-beyond-address-mask
         (stage (current check))
         (object (is-a memory-map-entry)
                 (base-address ?b)
                 (last-address ?l)
                 (name ?mme0))
         (address-mask is ?addr-mask)
         (test (<= ?b ?addr-mask ?l))
         =>
         (halt)
         (error-message "memory map entry " ?mme0 " goes beyond the address mask!" crlf))
