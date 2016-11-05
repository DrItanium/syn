
(defmodule iris16
           (import cortex
                   ?ALL)
           (import ucode
                   ?ALL)
           (export defgeneric 
                   encode-instruction
                   decode-instruction
                   new-core)
           (export defclass
                   core))
(defglobal iris16
           ?*stack-pointer* = 253
           ?*link-register* = 254
           ?*instruction-pointer* = 255)

(defclass iris16::core
  (is-a thing)
  (slot registers
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot data
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot code
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot stack
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot data-capacity
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot code-capacity
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot stack-capacity
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler init after)
  (message-handler init before)
  (message-handler startup primary))

(defmessage-handler iris16::core init after
                    ()
                    (memory-zero 
                      (bind ?self:registers
                            (alloc word16u 
                                   256)))
                    ; setup the stack pointer
                    (memory-store ?self:registers
                                  (dynamic-get stack-pointer-index)
                                  (hex->int 0xFFFF))
                    (memory-zero
                      (bind ?self:data
                            (alloc word16u
                                   ?self:data-capacity)))
                    (memory-zero
                      (bind ?self:stack
                            (alloc word16u
                                   ?self:stack-capacity)))
                    (memory-zero
                      (bind ?self:code
                            (alloc word32u
                                   ?self:code-capacity))))

(deffunction iris16::init
             "microcode startup"
             ()
             (if (not ?*init-called*) then
               (bind ?*init-called*
                     TRUE)
               (bind ?fmt
                     "(deffunction iris16::%s-with-offset 
                                   (?r ?i) 
                                   (memory-load-op-store-with-offset ?r 
                                                                     ?i 
                                                                     %s))")
                     (bind ?immediate-forms
                           (create$))
                     (bind ?arithmetics
                           (create$))
                     (progn$ (?op ?*dual-arithmetic-operations*)
                             ; build immediate operations
                             (build (format nil
                                            "(deffunction iris16::%s-with-offset
                                                          (?r ?i)
                                                          (memory-load-op-store=-with-offset ?r 
                                                                                             ?i 
                                                                                             %s))"
                                            ?op 
                                            ?op))
                             (bind ?immediate-forms
                                   ?immediate-forms
                                   (sym-cat ?op
                                            -with-offset))
                             ; build basic operations
                             (build (format nil
                                            "(deffunction iris16::call-%s
                                                          (?r ?i)
                                                          (register-binary-operation ?r 
                                                                                     ?i 
                                                                                     %s))"
                                            ?op
                                            ?op))
                                   (bind ?arithmetics
                                         ?arithmetics
                                         (sym-cat call- ?op)))
                             (bind ?*arithmetic-operations*
                                   ?arithmetics
                                   ?*binary-manipulation-operations*
                                   ?immediate-forms)))

(defmessage-handler iris16::core init before
                    "Make sure that the backing code has been initialized before initializing"
                    () 
                    (init))
(defgeneric iris16::new-core)

(defmethod iris16::new-core
  ((?data-cap INTEGER)
   (?code-cap INTEGER)
   (?stack-cap INTEGER))
  (make-instance of core
                 (parent FALSE)
                 (data-capacity ?data-cap)
                 (code-capacity ?code-cap)
                 (stack-capacity ?stack-cap)))
(defmethod iris16::new-core
  ()
  (new-core (hex->int 0xFFFF)
            (hex->int 0xFFFF)
            (hex->int 0xFFFF)))
; instruction decoding routines
(deffunction iris16::decode-group
             (?value)
             (decode-bits ?value 
                          (hex->int 0x00000007) 
                          0))
(deffunction iris16::decode-operation
             (?value)
             (decode-bits ?value
                          (hex->int 0x000000F8)
                          3))
(deffunction iris16::decode-immediate
             (?value)
             (decode-bits ?value
                          (hex->int 0xFFFF0000)
                          16))
(deffunction iris16::decode-half-immediate
             (?value)
             (decode-bits ?value
                          (hex->int 0xFF000000)
                          24))
(deffunction iris16::decode-destination
             (?value)
             (decode-bits ?value
                          (hex->int 0x0000FF00)
                          8))
(deffunction iris16::decode-source0
             (?value)
             (decode-bits ?value
                          (hex->int 0x00FF0000)
                          16))
(deffunction iris16::decode-source1
             (?value)
             (decode-bits ?value
                          (hex->int 0xFF000000)
                          24))

(deffunction iris16::decode-control
             (?value)
             (decode-bits ?value
                          (hex->int 0x000000FF)
                          0))
(deffunction iris16::encode-control
             (?value ?control)
             (encode-bits ?value
                          ?control
                          (hex->int 0x000000FF)
                          0))
(deffunction iris16::encode-half-immediate
             (?value ?immediate)
             (encode-bits ?value
                          ?immediate
                          (hex->int 0xFF000000)
                          24))
(deffunction iris16::encode-immediate
             (?value ?immediate)
             (encode-bits ?value
                          ?immediate
                          (hex->int 0xFFFF0000)
                          16))
(deffunction iris16::encode-group
             (?value ?group)
             (encode-bits ?value
                          ?group
                          (hex->int 0x00000007)
                          0))

(deffunction iris16::encode-destination
             (?value ?destination)
             (encode-bits ?value
                          ?destination
                          (hex->int 0x0000FF00)
                          8))

(deffunction iris16::encode-source0
             (?value ?source0)
             (encode-bits ?value
                          ?source0
                          (hex->int 0x00FF0000)
                          16))
(deffunction iris16::encode-source1
             (?value ?source1)
             (encode-bits ?value
                          ?source1
                          (hex->int 0xFF000000)
                          24))
(deffunction iris16::encode-operation
             (?value ?op)
             (encode-bits ?value
                          ?op
                          (hex->int 0x000000F8)
                          3))
(deffunction iris16::decode-upper8
             (?value)
             (decode-bits ?value
                          (hex->int 0x0000FF00)
                          8))
(deffunction iris16::decode-lower8
             (?value)
             (decode-bits ?value
                          (hex->int 0x000000FF)
                          0))
(defgeneric iris16::encode-instruction)
(defgeneric iris16::decode-instruction)
(defmethod iris16::decode-instruction
  ((?value INTEGER))
  (bind ?casted 
        (cast word32u
              ?value))
  (create$ (decode-group ?casted)
           (decode-operation ?casted)
           (decode-destination ?casted)
           (decode-source0 ?casted)
           (decode-source1 ?casted)
           (decode-immediate ?casted)))
(defmethod iris16::encode-instruction
  ((?group INTEGER)
   (?op INTEGER)
   (?destination INTEGER)
   (?source0 INTEGER)
   (?source1 INTEGER))
  (encode-source1
    (encode-source0
      (encode-destination 
        (encode-operation
          (encode-group 0 ?group)
          ?op)
        ?destination)
      ?source0)
    ?source1))
(defmethod iris16::encode-instruction
  ((?group INTEGER)
   (?op INTEGER)
   (?destination INTEGER)
   (?immediate INTEGER))
  (encode-instruction ?group
                      ?op
                      ?destination
                      (decode-lower8 ?immediate)
                      (decode-upper8 ?immediate)))
(deffunction iris16::register-binary-operation
             (?register-file ?instruction ?op)
             (memory-load-binary-op-store ?register-file
                                          (decode-destination ?instruction)
                                          (decode-source0 ?instruction)
                                          (decode-source1 ?instruction)
                                          ?op))
(deffunction iris16::memory-load-op-store-with-offset
             (?register-file ?instruction ?op)
             (memory-store ?register-file
                           (decode-destination ?instruction)
                           (funcall ?op
                                    (memory-load ?register-file
                                                 (decode-source0 ?instruction))
                                    (decode-half-immediate ?instruction))))
(defglobal iris16
           ?*init-called* = FALSE
           ?*dual-arithmetic-operations* = (create$ add-word16u
                                                    sub-word16u
                                                    mul-word16u
                                                    div-word16u
                                                    rem-word16u
                                                    left-shift
                                                    right-shift)
           ?*binary-manipulation-operations* = (create$ binary-and
                                                        binary-op
                                                        binary-not
                                                        binary-xor)
           ?*arithmetic-operations* = (create$))




(defmethod iris16::binary-not
  ((?first INTEGER)
   (?unused INTEGER))
  (binary-not ?first))


(deffunction iris16::arithmetic-operation
             (?register-file ?instruction)
             (funcall (nth$ (decode-operation ?instruction)
                            ?*arithmetic-operations*)
                      ?register-file
                      ?instruction))
(deffunction iris16::get-register
             (?register ?index)
             (memory-load ?register
                          ?index))
(deffunction iris16::set-register
             (?register ?index ?value)
             (memory-store ?register
                           ?index
                           ?value))
(deffunction iris16::swap-registers
             (?register ?a ?b)
             (memory-swap ?register
                          ?a 
                          ?b))

(deffunction iris16::store-code
             (?registers ?code ?instruction)
             (memory-load-binary-op-store ?code
                                          (get-register ?registers
                                                        (decode-destination ?instruction))
                                          ?registers
                                          (decode-source0 ?instruction)
                                          ?registers
                                          (decode-source1 ?instructions)
                                          make-word32u))
(deffunction iris16::load-code
             (?registers ?code ?instruction)
             (bind ?value
                   (memory-load ?code
                                (get-register ?registers
                                              (decode-destination ?instruction))))
             (set-register ?registers
                           (decode-source0 ?instruction)
                           (word32u-lower-half ?value))
             (set-register ?registers
                           (decode-source1 ?instruction)
                           (word32u-upper-half ?value)))
(deffunction iris16::set-immediate
             (?register ?instruction)
             (set-register ?register
                           (decode-destination ?instruction)
                           (decode-immediate ?instruction)))
(deffunction iris16::swap-operation
             (?register ?instruction)
             (swap-registers ?register
                             (decode-destination ?instruction)
                             (decode-source0 ?instruction)))
(deffunction iris16::move-operation
             (?register ?instruction)
             (set-register ?register
                           (decode-destination ?instruction)
                           (get-register ?register
                                         (decode-source0 ?instruction))))
(deffunction iris16::load-operation
             (?register ?data ?instruction)
             (set-register ?register
                           (decode-destination ?instruction)
                           (memory-load ?data
                                        (get-register ?register
                                                      (decode-source0 ?instruction)))))
(deffunction iris16::load-immediate-operation
             (?register ?data ?instruction)
             (set-register ?register
                           (decode-destination ?instruction)
                           (memory-load ?data
                                        (decode-immediate ?instruction))))
(deffunction iris16::store-operation
             (?register ?data ?instruction)
             (memory-store ?data
                           (get-register ?register
                                         (decode-destination ?instruction))
                           (get-register ?register
                                         (decode-source0 ?instruction))))
(deffunction iris16::store-immediate-operation
             (?register ?data ?instruction)
             (memory-store ?data
                           (get-register ?register
                                         (decode-destination ?instruction))
                           (decode-immediate ?instruction)))
(deffunction iris16::get-stack-pointer
             (?register-file)
             (get-register ?register-file
                           ?*stack-pointer*))

(deffunction iris16::push-generic
             (?registers ?stack ?value)
             ; two operations in one, add followed by a store
             (memory-increment ?registers
                               ?*stack-pointer*)
             (memory-store ?stack
                           (get-stack-pointer ?registers)
                           ?value))

(deffunction iris16::push-operation
             (?registers ?stack ?instruction)
             (push-generic ?registers
                           ?stack
                           (get-register ?registers
                                         (decode-destination ?instruction))))
(deffunction iris16::push-immediate-operation
             (?registers ?stack ?instruction)
             (push-generic ?registers
                           ?stack
                           (decode-immediate ?instruction)))

(deffunction iris16::pop-operation
             (?registers ?stack ?instruction)
             (set-register ?registers
                           (decode-destination ?instruction)
                           (memory-load ?stack
                                        (get-stack-pointer ?registers)))
             (memory-decrement ?self:registers
                               ?*stack-pointer*))
(defglobal iris16
           ?*move-operations* = (create$ move-operation
                                         set-immediate 
                                         swap-operation
                                         load-operation
                                         load-immediate-operation
                                         store-operation
                                         store-immediate-operation
                                         push-operation
                                         push-immediate-operation
                                         pop-operation
                                         load-code
                                         store-code))

(deffunction iris16::move-group-operation
             (?registers ?code ?data ?stack ?instruction)
             (switch (nth$ (decode-operation ?instruction)
                           ?*move-operation*)
                     (case move-operation then
                       (move-operation ?registers
                                       ?instruction))
                     (case set-immediate then
                       (set-immediate ?registers
                                      ?instruction))
                     (case swap-operation then
                       (swap-operation ?registers
                                       ?instruction))
                     (case load-operation then
                       (load-operation ?registers
                                       ?data
                                       ?instruction))
                     (case load-immediate-operation then
                       (load-immediate-operation ?registers
                                                 ?data
                                                 ?instruction))
                     (case store-operation then
                       (store-operation ?registers
                                        ?data
                                        ?instruction))
                     (case store-immediate-operation then
                       (store-immediate-operation ?registers
                                                  ?data
                                                  ?instruction))
                     (case push-operation then
                       (push-operation ?registers
                                       ?stack
                                       ?instruction))
                     (case push-immediate-operation then
                       (push-immediate-operation ?registers
                                                 ?stack
                                                 ?instruction))
                     (case pop-operation then
                       (pop-operation ?registers
                                      ?stack
                                      ?instruction))
                     (case load-code then
                       (load-code ?registers
                                  ?code
                                  ?instruction))
                     (case store-code then
                       (store-code ?registers
                                   ?code
                                   ?instruction))
                     (default (printout werror "Illegal move group operation " (decode-operation ?instruction) crlf))))


(deffunction iris16::set-instruction-pointer
             (?registers ?value)
             (set-register ?registers
                           ?*instruction-pointer*
                           ?value))

(deffunction iris16::get-instruction-pointer
             (?registers)
             (get-register ?registers
                           ?*instruction-pointer*))

(deffunction iris16::jump-operation
             (?registers ?instruction)
             ; the operation field has a total of 5 bits which we use to determine what kind of operation we are looking at
             ; 0 - if then else
             ; 1 - conditional
             ; 2 - if-false
             ; 3 - immediate
             ; 4 - link
             (bind ?if-then-else
                   (decode-bits ?instruction
                                (binary->int 0b00001)
                                0))
             (bind ?conditional
                   (decode-bits ?instruction
                                (binary->int 0b00010)
                                1))
             (bind ?iffalse
                   (decode-bits ?instruction
                                (binary->int 0b00100)
                                2))
             (bind ?immediate
                   (decode-bits ?instruction
                                (binary->int 0b01000)
                                3))
             (bind ?link
                   (decode-bits ?instruction
                                (binary->int 0b10000)
                                4))
             (bind ?ip 
                   (get-instruction-pointer ?registers))
             (bind ?cond 
                   TRUE)
             (set-instruction-pointer ?registers
                                      (if ?conditional then
                                        (bind ?dest
                                              (get-register ?registers
                                                            (decode-destination ?instruction)))
                                        (bind ?cond 
                                              (if ?iffalse then
                                                (= ?dest 0)
                                                else
                                                (<> ?dest 0)))
                                        (if ?ifthenelse then
                                          (get-register ?registers
                                                        (if ?cond then 
                                                          (decode-source0 ?instruction)
                                                          else
                                                          (decode-source1 ?instruction)))
                                          else
                                          (if ?cond then
                                            (if ?immediate then
                                              (decode-immediate ?instruction)
                                              else
                                              (get-register ?registers
                                                            (decode-source0 ?instruction)))
                                            else
                                            (+ ?ip 
                                               1)))
                                        else
                                        (if ?immediate then 
                                          (decode-immediate ?instruction)
                                          else
                                          (get-register ?registers
                                                        (decode-destination ?instruction)))))
             (if (and ?link 
                      ?cond) then
               (set-register ?registers
                             ?*link-register*
                             (+ ?ip 1))))

