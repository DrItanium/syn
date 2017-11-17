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
; X8.clp - First architecture using the new ucoded techniques
;------------------------------------------------------------------------------
; The x8 core is a 12-bit signed integer architecture which is a reimplementation
; of the DEC PDP-8e architecture :D. I did this so I have a variety of software for
; testing purposes
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* InterruptHandler.clp)
(set-current-module MAIN)
(batch* ExternalAddressWrapper.clp)
(batch* Device.clp)
(batch* MemoryBlock.clp)
(batch* Register.clp)
(batch* mmu.clp)
(batch* keyboard.clp)
(batch* rng.clp)
(batch* RunLevelController.clp)
(batch* order.clp)
(defglobal MAIN
           ?*address12bit* = (hex->int 0x00000FFF))
(defclass MAIN::x8-memory-block
  (is-a memory-block)
  (slot capacity
        (source composite)
        (storage shared)
        (default (+ ?*address12bit*
                    1)))
  (slot last-address
        (storage shared)
        (visibility public)
        (access read-only)
        (create-accessor read)
        (default ?*address12bit*)))

; There is one memory space
(deffacts MAIN::make-memory-blocks
          (make memory-block named space))
; The instruction pointer register is 27-bits wide or having a mask of 0x07FFFFFF
; this applies to the stack register as well. All bits above the mask must be zero to maintain
; backwards compatibility
(defglobal MAIN
           ?*address-mask* = ?*address12bit*
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
(defclass MAIN::x8-register
  (is-a register)
  (slot mask
        (source composite)
        (default-dynamic ?*address-mask*))
  (message-handler init after))

(defclass MAIN::x8-program-counter
  (is-a x8-register)
  (message-handler init after)
  (message-handler increment primary))
(defmessage-handler x8-program-counter increment primary
                    ()
                    (dynamic-put value
                                 (binary-and (hex->int 0xFFF)
                                             (+ (dynamic-get value)
                                                1))))
(defclass MAIN::x8-accumulator
  (is-a x8-register)
  (message-handler get-link-bit primary))
(defmessage-handler MAIN::x8-accumulator get-link-bit primary
                    ()
                    (decode-bits (dynamic-get value)
                                 (hex->int 0x1000)
                                 12))
; Internally, machine
(deffacts MAIN::make-registers
          ;(terminate at 128 cycles)
          ;(terminate at ?*address-mask* cycles)
          ;(terminate at (hex->int 0xFFFF) cycles)
          (make register named tmp)
          (make register named mbr)
          (make register named mar))
(definstances MAIN::make-specific-registers
              (ac of x8-accumulator)
              (pc of x8-program-counter))



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

(defmessage-handler MAIN::x8-register init after
                    ()
                    (note-bring-up t
                                   register
                                   ?name)
                    (request-future-delete register
                                           (instance-name ?self)))

(deffunction MAIN::get-link-bit
             ()
             (get-bit (send [ac]
                            get-value)
                      12))

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
        (type INTEGER)
        (range 0 7)
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

(defclass MAIN::bits-descriptor
  (is-a thing)
  (slot matches-with
        (type INTEGER)
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

(definstances MAIN::basic-instructions
              ([op:and] of primary-class-descriptor
                        (matches-with 0)
                        (title and))
              ([op:tad] of primary-class-descriptor
                        (matches-with 1)
                        (title tad))
              ([op:isz] of primary-class-descriptor
                        (matches-with 2)
                        (title isz))
              ([op:dca] of primary-class-descriptor
                        (matches-with 3)
                        (title dca))
              ([op:jms] of primary-class-descriptor
                        (matches-with 4)
                        (title jms))
              ([op:jmp] of primary-class-descriptor
                        (matches-with 5)
                        (title jmp))
              ([op:iot] of primary-class-descriptor
                        (matches-with 6)
                        (title iot))
              ([op:opr] of primary-class-descriptor
                        (matches-with 7)
                        (title opr)))


(deffunction MAIN::get-operation-bits
             (?value)
             (decode-bits ?value
                          (hex->int 0x7)
                          0))
(deffunction MAIN::get-indirect-bit
             (?value)
             (get-bit ?value
                      3))
(deffunction MAIN::get-clear-bit
             (?value)
             (get-bit ?value
                      4))

(deffunction MAIN::get-offset
             (?value)
             (decode-bits ?value
                          (hex->int 0xFE0)
                          5))

(deffunction MAIN::get-iot-device-id
             (?value)
             (decode-bits ?value
                          (hex->int 0xFF0)
                          4))
(deffunction MAIN::get-iot-function-code
             (?value)
             (decode-bits ?value
                          (hex->int 0x700)
                          8))
; TODO: add support for OPR instruction from PDP8
(deffunction MAIN::get-opr-bit4
             (?value)
             (get-bit ?value
                      3))
(defgeneric MAIN::get-opr-group)
(defmethod MAIN::get-opr-group
  ((?value INTEGER
           (not (get-opr-bit4 ?current-argument))))
  0)
(defmethod MAIN::get-opr-group
  ((?value INTEGER
           (and (get-opr-bit4 ?current-argument)
                (not (get-bit ?current-argument
                              11))
                (not (get-bit ?current-argument
                              8)))))
  ; type 2, or group becomes code 1
  1)

(defmethod MAIN::get-opr-group
  ((?value INTEGER
           (and (get-opr-bit4 ?current-argument)
                (not (get-bit ?current-argument
                              11))
                (get-bit ?current-argument
                              8))))
  ; type 2, and group becomes code 2
  2)

(defmethod MAIN::get-opr-group
  ((?value INTEGER
           (and (get-opr-bit4 ?current-argument)
                (get-bit ?current-argument
                         11))))
  3)

(deffunction MAIN::return-on-bit-true
             (?value ?index ?ret)
             (if (get-bit ?value
                          ?index) then
               ?ret
               else
               (create$)))
(defgeneric MAIN::decode-opr-bits)
(defmethod MAIN::decode-opr-bits
  ((?group INTEGER
           (= ?current-argument
              0))
   (?value INTEGER))
  (create$ (return-on-bit-true ?value
                               4
                               CLA)
           (return-on-bit-true ?value
                               5
                               CLL)
           (return-on-bit-true ?value
                               6
                               CMA)
           (return-on-bit-true ?value
                               7
                               CML)
           (return-on-bit-true ?value
                               8
                               RAR)
           (return-on-bit-true ?value
                               9
                               RAL)
           (return-on-bit-true ?value
                               10
                               BSW)
           (return-on-bit-true ?value
                               11
                               IAC)))

(defmethod MAIN::decode-opr-bits
  ((?group INTEGER
           (= ?current-argument
              1))
   (?value INTEGER))
  (create$ (return-on-bit-true ?value
                               4
                               CLA)
           (return-on-bit-true ?value
                               5
                               SMA)
           (return-on-bit-true ?value
                               6
                               SZA)
           (return-on-bit-true ?value
                               7
                               SNL)))

(defmethod MAIN::decode-opr-bits
  ((?group INTEGER
           (= ?current-argument
              2))
   (?value INTEGER))
  (create$ (return-on-bit-true ?value
                               4
                               CLA)
           (return-on-bit-true ?value
                               5
                               SPA)
           (return-on-bit-true ?value
                               6
                               SNA)
           (return-on-bit-true ?value
                               7
                               SZL)
           ;according to http://homepage.divms.uiowa.edu/~jones/pdp8/man/micro.html
           ; privileged ucode instructions are only allowed in the group 2, and group
           (return-on-bit-true ?value
                               9
                               OSR)
           (return-on-bit-true ?value
                               10
                               HLT)))

(defmethod MAIN::decode-opr-bits
  ((?group INTEGER
           (= ?current-argument
              3))
   (?value INTEGER))
  (create$ (return-on-bit-true ?value
                               4
                               CLA)
           (return-on-bit-true ?value
                               5
                               MQA)
           (return-on-bit-true ?value
                               6
                               SCA)
           (return-on-bit-true ?value
                               7
                               MQL)
           ;according to http://homepage.divms.uiowa.edu/~jones/pdp8/man/micro.html
           ; the code field is three bits wide
           (decode-bits ?value
                        (hex->int 0x700)
                        8)))

(deffunction MAIN::get-current-page
             ()
             (decode-bits (send [pc]
                                get-value)
                          (hex->int 0xFF0000)
                          0))
(deffunction MAIN::perform-initial-startup
             ()
             (printout t
                       "X8 System boot" crlf
                       "Starting up .... please wait" crlf)))
(deffunction MAIN::final-shutdown
             ()
             (printout t
                       "shutdown complete .... bye" crlf))
(defmessage-handler MAIN::x8-accumulator clear-accumulator primary
                    ()
                    ; save the link register bit or the portion above :D
                    (dynamic-put value
                                 (decode-bits (dynamic-get value)
                                              (binary-not (hex->int 0xFFF))
                                              0)))
(defmessage-handler MAIN::x8-accumulator clear-link primary
                    ()
                    (dynamic-put value
                                 (decode-bits (dynamic-get value)
                                              (hex->int 0xFFF)
                                              0)))
(defmessage-handler MAIN::x8-accumulator complement-accumulator primary
                    ()
                    (bind ?accumulator-bits
                          (decode-bits (dynamic-get value)
                                       (hex->int 0xFFF)
                                       0))
                    (bind ?rest
                          (decode-bits (dynamic-get value)
                                       (binary-not (hex->int 0xFFF))
                                       0))
                    ; make sure that all of the bits are cleared correctly
                    (dynamic-put value
                                 (binary-or (binary-and (hex->int 0xFFF)
                                                        (binary-not ?accumulator-bits))
                                            ?rest)))
(defmessage-handler MAIN::x8-accumulator complement-link primary
                    ()
                    (bind ?accumulator-bits
                          (decode-bits (dynamic-get value)
                                       (hex->int 0xFFF)
                                       0))
                    (bind ?link
                          (decode-bits (dynamic-get value)
                                       (binary-not (hex->int 0xFFF))
                                       12))
                    (dynamic-put value
                                 (binary-or ?accumulator-bits
                                            (right-shift (binary-not ?link)
                                                         12))))
(defmessage-handler MAIN::x8-accumulator byte-swap primary
                    "byte swap the upper and lower 6 bits of the value"
                    ()
                    (bind ?accumulator-bits
                          (decode-bits (dynamic-get value)
                                       (hex->int 0xFFF)
                                       0))
                    (bind ?link
                          (decode-bits (dynamic-get value)
                                       (binary-not (hex->int 0xFFF))
                                       0))
                    (bind ?lower
                          (decode-bits ?accumulator-bits
                                       (hex->int 0x3F)
                                       0))
                    (bind ?upper
                          (decode-bits ?accumulator-bits
                                       (hex->int 0xFC0)
                                       6))
                    (dynamic-put value
                                 (binary-or ?link
                                            (binary-or (right-shift ?lower
                                                                    6)
                                                       ?upper))))
(defmessage-handler MAIN::x8-accumulator rotate-accumulator-right
                    ()
                    ; need to extract the least significant bit
                    (bind ?new-link
                          (right-shift (decode-bits (dynamic-get value)
                                                    (hex->int 0x1)
                                                    0)
                                       12))


                    (bind ?contents
                          (right-shift (binary-and (dynamic-get value)
                                                   (hex->int 0x1FFFF))
                                       1))
                    (dynamic-put value
                                 (binary-or ?new-link
                                            ?contents)))

(defmessage-handler MAIN::x8-accumulator rotate-accumulator-left
                    ()
                    (bind ?old-link
                          (decode-bits (dynamic-get value)
                                       (hex->int 0x1000)
                                       12))
                    (bind ?new-accumulator
                          (right-shift (dynamic-get value)
                                       1))
                    (dynamic-put value
                                 (binary-or (binary-and ?new-accumulator
                                                        (hex->int 0x1FFF))
                                            ?old-link)))




;TOOD: implement more of the microcoded operations

; microcoded internal operations!
(deffunction MAIN::clear-accumulator
             ()
             (send [ac]
                   clear-accumulator))
(deffunction MAIN::clear-link
             ()
             (send [ac]
                   clear-link))
(deffunction MAIN::complement-accumulator
             ()
             (send [ac]
                   complement-accumulator))
(deffunction MAIN::complement-link
             ()
             (send [ac]
                   complement-link))
(deffunction MAIN::byte-swap
             ()
             (send [ac]
                   byte-swap))
(deffunction MAIN::increment-accumulator
             ()
             (send [ac]
                   increment))
(deffunction MAIN::rotate-accumulator-right
             ()
             (send [ac]
                   rotate-accumulator-right))
(deffunction MAIN::rotate-accumulator-right-twice
             ()
             (rotate-accumulator-right)
             (rotate-accumulator-right))
(deffunction MAIN::rotate-accumulator-left
             ()
             (send [ac]
                   rotate-accumulator-left))
(deffunction MAIN::rotate-accumulator-left-twice
             ()
             (rotate-accumulator-left)
             (rotate-accumulator-left))
(deffunction MAIN::skip-if-zero-accumulator
             ()
             (if (= (send [ac]
                           get-value)
                     0) then
                 (send [pc]
                       increment)))
(deffunction MAIN::skip-if-minus-accumulator
             ()
             (if (= (decode-bits (send [ac]
                                    get-value)
                              (hex->int 0x800)
                              11)
                    1) then
               (send [pc]
                     increment)))
(deffunction MAIN::skip-if-nonzero-link
             ()
             (if (<> (send [ac]
                          get-link-bit)
                    0) then
               (send [pc]
                     increment)))

(deffunction MAIN::skip-if-positive-accumulator
             ()
             (if (= (decode-bits (send [ac]
                                    get-value)
                              (hex->int 0x800)
                              11)
                    0) then
               (send [pc]
                     increment)))

(deffunction MAIN::skip-if-nonzero-accumulator
             ()
             (if (<> (send [ac]
                           get-value)
                     0) then
                 (send [pc]
                       increment)))

(deffunction MAIN::skip-if-zero-link
             ()
             (if (= (send [ac]
                          get-link-bit)
                    0) then
               (send [pc]
                     increment)))
(deffunction MAIN::switch-register
             ()
             ; switch the contents of the front panel register with the accumulator
             )
(deffunction MAIN::halt-execution
             ()
             ; invoke the cpu halt state
             )
;TODO: add support for the MQA and MQL instructions from group three

;-----------------------------------------------------------------------------
; !RULES
;-----------------------------------------------------------------------------
; the cpu bootstrap process requires lower priority because it always exists in the background and dispatches
; cycles as we go along. This is how we service interrupts and other such things.
(defrule MAIN::bootstrap-startup
         (declare (salience ?*priority:first*))
         (stage (current startup))
         =>
         (perform-initial-startup))

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
                                (make-instance ?name of x8-memory-block))
         (done-with-bring-up t
                             tab "size: "
                             (int->hex (send (symbol-to-instance-name ?name)
                                             size)) crlf))

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
                   "Shutting down x8 system!" crlf))

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
         (final-shutdown))


(defrule MAIN::execute:generate-cycle-execute
         "setup the execution cycle!"
         (stage (current execute))
         =>
         (printout t
                   "Setting up the execution cycle!" crlf))

(defrule MAIN::execute:execution-cycle:read-from-memory
         (stage (current read))
         (object (is-a register)
                 (name [ip])
                 (value ?addr))
         ?ms0 <- (object (is-a memory-map-entry)
                         (base-address ?ba&:(>= ?addr
                                                ?ba))
                         (last-address ?la&:(<= ?addr
                                                ?la)))
         =>
         (assert (instruction ?addr
                              (bind ?value
                                    (send ?ms0
                                          read
                                          ?addr)))))


(defrule MAIN::eval:get-more-information
         (stage (current eval))
         ?f <- (instruction ?addr
                            ?value)
         =>
         (retract ?f)
         (assert (operation (address ?addr)
                            (original-value ?value))))

(defrule MAIN::eval:mark-primary-descriptor
         (stage (current eval))
         ?f <- (operation (original-value ?value)
                          (type FALSE))
         (object (is-a primary-class-descriptor)
                 (matches-with =(get-operation-bits ?value))
                 (name ?descriptor))
         =>
         (modify ?f
                 (type ?descriptor)))

(defrule MAIN::decode-arguments
         (stage (current eval))
         ?f <- (operation (type ?p)
                          (arguments)
                          (original-value ?value))
         (object (is-a primary-class-descriptor)
                 (name ?p)
                 (matches-with ?k&:(< ?k 6)))
         =>
         (modify ?f
                 (arguments (if (get-indirect-bit ?value) then
                           indirect
                           else
                           direct)
                       (if (get-clear-bit ?value) then
                           zero-page
                           else
                           any-page)
                       (get-offset ?value))))

(defrule MAIN::decode-iot-argument
         (stage (current eval))
         ?f <- (operation (type ?p)
                          (arguments)
                          (original-value ?value))
         ; iot
         (object (is-a primary-class-descriptor)
                 (name ?p)
                 (title iot))
         =>
         (modify ?f
                 (arguments (get-iot-device-id ?value)
                            (get-iot-function-code ?value))))

(defrule MAIN::decode-opr-argument
         (stage (current eval))
         ?f <- (operation (type ?p)
                          (arguments)
                          (original-value ?value))
         (object (is-a primary-class-descriptor)
                 (name ?p)
                 (title opr))
         =>
         (bind ?group
               (get-opr-group ?value))
         (modify ?f
                 (arguments ?group
                       (decode-opr-bits ?group
                                        ?value))))

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
;(defrule MAIN::invoke-operation:direct-bit
;         (declare (salience 1))
;         ?f <- (operation (arguments direct
;
;                                ?rest
;                                ?address))
;         =>
;         (modify ?f
;                 (arguments
(defrule MAIN::invoke-operation:and,direct,any-page
         (stage (current print))
         ?f <- (operation (type ?p)
                          (arguments direct
                                any-page
                                ?
                                ?address))
         (object (is-a primary-class-descriptor)
                 (name ?p)
                 (title and))
         =>
         (retract ?f)
         (send [tmp]
               put-value
               (binary-or (get-current-page)
                          (send [space]
                                read
                                ?address)))
         (send [ac]
               put-value
               (binary-and [tmp]
                           [ac])))

(defrule MAIN::invoke-operation:and,direct,zero-page
         (stage (current print))
         ?f <- (operation (type ?p)
                          (arguments direct
                                zero-page
                                ?address))
         (object (is-a primary-class-descriptor)
                 (name ?p)
                 (title and))
         =>
         (retract ?f)
         (send [tmp]
               put-value
               (send [space]
                     read
                     ?address))
         (send [ac]
               put-value
               (binary-and [tmp]
                           [ac])))

(defrule MAIN::invoke-operation:and,indirect,zero-page
         (stage (current print))
         ?f <- (operation (type ?p)
                          (arguments indirect
                                zero-page
                                ?address))
         (object (is-a primary-class-descriptor)
                 (name ?p)
                 (title and))
         =>
         (retract ?f)
         (send [tmp]
               put-value
               (load-value [space]
                           ?address))
         (send [tmp]
               put-value
               (load-value [space]
                           [tmp]))
         (send [ac]
               put-value
               (binary-and [tmp]
                           [ac])))
(defrule MAIN::invoke-operation:and,indirect,any-page
         (stage (current print))
         ?f <- (operation (type ?p)
                          (arguments indirect
                                any-page
                                ?address))
         (object (is-a primary-class-descriptor)
                 (name ?p)
                 (title and))
         =>
         (retract ?f)
         (send [tmp]
               put-value
               (binary-or (get-current-page)
                          (send [space]
                                read
                                ?address)))
         (send [tmp]
               put-value
               (load-value [space]
                           [tmp]))
         (send [ac]
               put-value
               (binary-and [tmp]
                           [ac])))
