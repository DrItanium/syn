
(defmodule iris16
           (import cortex
                   ?ALL)
           (import ucode
                   ?ALL)
           (export defgeneric 
                   new-core)
           (export defclass
                   core))

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
  (slot stack-pointer-index
        (type INTEGER)
        (visibility public)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default 253))
  (slot link-register-index
        (type INTEGER)
        (visibility public)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default 254))
  (slot instruction-pointer-index
        (type INTEGER)
        (visibility public)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default 255))
  (message-handler get-register primary)
  (message-handler set-register primary)
  (message-handler get-instruction-pointer primary)
  (message-handler set-instruction-pointer primary)
  (message-handler push-value primary)
  (message-handler init after)
  (message-handler startup primary))
(defmessage-handler iris16::core get-instruction-pointer primary
                    ()
                    (memory-load ?self:registers
                                 (dynamic-get instruction-pointer-index)))
(defmessage-handler iris16::core set-instruction-pointer primary
                    (?value)
                    (memory-store ?self:registers
                                  (dynamic-get instruction-pointer-index)
                                  ?value))
(defmessage-handler iris16::core get-register primary
                    (?index)
                    (memory-load ?self:registers
                                 ?index))
(defmessage-handler iris16::core set-register primary
                    (?index ?value)
                    (memory-store ?self:registers
                                  ?index
                                  ?value))

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

(defmessage-handler iris16::core push-value primary
                    (?value)
                    ; increment memory first
                    (memory-increment ?self:registers
                                      (dynamic-get stack-pointer-index))
                    (memory-store ?self:stack
                                  (memory-load ?self:registers
                                               (dynamic-get stack-pointer-index))
                                  ?value))
(defmessage-handler iris16::core pop-value primary
                    ()
                    (bind ?value
                          (memory-load ?self:stack
                                       (memory-load ?self:registers
                                                    (dynamic-get stack-pointer-index))))
                    (memory-decrement ?self:registers
                                      (dynamic-get stack-pointer-index))
                    ?value)
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
(defmethod iris16::decode-instruction
