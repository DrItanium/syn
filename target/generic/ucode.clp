; Basic routines that act as ucode operations
(defmodule ucode
           (import cortex
                   ?ALL)
           (export ?ALL))
(defgeneric ucode::registerp
            "Is the given symbol a register?")
(defgeneric ucode::register-to-index
            "convert the given register to an index value")
(defgeneric ucode::memory-load
            "Load the given value from the specified address in memory and return it")

(defgeneric ucode::memory-store
            "Store the given value at the specified address into memory")
(defgeneric ucode::memory-load-op-store
            "Load a value from memory, perform an operation on it, and then store it back into memory")
(defgeneric ucode::memory-load-op
            "Load a value from memory, perform an operation on it, and then return it");
(defgeneric ucode::alloc
            "Allocate a new memory buffer")
(defgeneric ucode::cast
            "Convert the given value to the specified word size")
(defgeneric ucode::memory-zero
            "Zero the given memory space")
(defgeneric ucode::memory-increment
            "Load a given value from memory, increment the value, and then return it")
(defgeneric ucode::memory-decrement
            "Load a given value from memory, decrement the value, and then return it")
(defgeneric ucode::memory-load-indirect 
            "Use the value stored at the given address in memory as another address and load the value stored there!")
(defgeneric ucode::memory-store-indirect
            "Use the value stored at the given address in memory as another address and store the given value stored there!")
(defgeneric ucode::memory-swap
            "Swap the contents of two memory addresses")
(defgeneric ucode::memory-size
            "Size of the memory space in words!")
(defgeneric ucode::memory-type
            "Get the type associated with the given memory space")
(defgeneric ucode::memory-copy
            "Load a given value from memory and store it to another address")
(defgeneric ucode::memory-load-binary-op-store
            "Load two values from the memory, perform and operation, and then store it back into memory at another address")

(defmethod ucode::memory-type
  ((?memory EXTERNAL-ADDRESS))
  (call ?memory
        type))

(defmethod ucode::memory-size
  ((?memory EXTERNAL-ADDRESS))
  (call ?memory
        size))
(defmethod ucode::memory-swap
  "Swap the contents of two memory addresses within the same memory space!"
  ((?memory EXTERNAL-ADDRESS)
   (?address0 INTEGER)
   (?address1 INTEGER))
  (call ?memory
        swap
        ?address0
        ?address1))
(defmethod ucode::memory-swap
  "Swap the contents of two memory addresses across two different memory spaces!"
  ((?mem0 EXTERNAL-ADDRESS)
   (?addr0 INTEGER)
   (?mem1 EXTERNAL-ADDRESS)
   (?addr1 INTEGER))
  (bind ?a 
        (memory-load ?mem0
                     ?addr0))
  (memory-store ?mem0
                ?addr0
                (memory-load ?mem1
                             ?addr1))
  (memory-store ?mem1
                ?addr1
                ?a))


(defmethod ucode::memory-zero
  ((?memory EXTERNAL-ADDRESS))
  (call ?memory
        clear))

(defmethod ucode::memory-zero
  "Zero out a specific cell!"
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER))
  (memory-store ?memory
                ?address
                0))

(defmethod ucode::memory-zero
  "Clear a range of memory, [?from, ?to]"
  ((?memory EXTERNAL-ADDRESS)
   (?from INTEGER)
   (?to INTEGER
        (>= ?to 
            ?from)))
  (loop-for-count (?index ?from ?to) do
                  (memory-zero ?memory
                               ?index)))

(defmethod ucode::cast
  ((?type SYMBOL)
   (?value INTEGER))
  (funcall (sym-cat add- 
                    ?type)
           ?value
           0))

(defmethod ucode::alloc
  ((?type SYMBOL)
   (?capacity INTEGER))
  (new ?type
       ?capacity))

(defmethod ucode::memory-load-op
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER)
   (?op SYMBOL))
  (funcall ?op
           (memory-load ?memory
                        ?address)))


(defmethod ucode::memory-load-op-store
  ((?from EXTERNAL-ADDRESS)
   (?addr0 INTEGER)
   (?to EXTERNAL-ADDRESS)
   (?addr1 INTEGER)
   (?op SYMBOL))
  (memory-store ?to
                ?addr1
                (memory-load-op ?from
                                ?addr0
                                ?op)))

(defmethod ucode::memory-load-op-store
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER)
   (?op SYMBOL))
  (memory-store ?memory
                ?address
                (memory-load-op ?memory
                                ?address
                                ?op)))

(defmethod ucode::memory-copy
  ((?mem EXTERNAL-ADDRESS)
   (?from INTEGER)
   (?to INTEGER))
  (memory-store ?mem
                ?to
                (memory-load ?mem
                             ?from)))

(defmethod ucode::memory-copy
  ((?from-mem EXTERNAL-ADDRESS)
   (?from-addr INTEGER)
   (?to-mem EXTERNAL-ADDRESS)
   (?to-addr INTEGER))
  (memory-store ?to-mem
                ?to-addr
                (memory-load ?from-mem
                             ?from-addr)))

(defmethod ucode::memory-copy
  ((?mem EXTERNAL-ADDRESS)
   (?from INTEGER)
   (?to INTEGER)
   (?mask INTEGER))
  (memory-store ?mem
                ?to
                (decode-bits (memory-load ?mem
                                          ?from)
                             ?mask
                             0)))

(defmethod ucode::memory-copy
  ((?from-mem EXTERNAL-ADDRESS)
   (?from-addr INTEGER)
   (?to-mem EXTERNAL-ADDRESS)
   (?to-addr INTEGER)
   (?mask INTEGER))
  (memory-store ?to-mem
                ?to-addr
                (decode-bits (memory-load ?from-mem
                                          ?from-addr)
                             ?mask
                             0)))

(defmethod ucode::memory-load
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER))
  (call ?memory
        get
        ?address))

(defmethod ucode::memory-store
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER)
   (?value INTEGER))
  (call ?memory
        set 
        ?address
        ?value))

(defmethod ucode::memory-increment
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER))
  (memory-load-op-store ?memory
                        ?address
                        increment))

(defmethod ucode::memory-decrement
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER))
  (memory-load-op-store ?memory
                        ?address
                        decrement))
(defmethod ucode::memory-load-indirect
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER))
  (memory-load ?memory
               (memory-load ?memory
                            ?address)))

(defmethod ucode::memory-store-indirect
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER)
   (?value INTEGER))
  (memory-store ?memory
                (memory-load ?memory
                             ?address)
                ?value))

(defclass ucode::memory-space
  "A very thin wrapper over the memory space, should not modify it's contents once initialization is complete!"
  (is-a thing)
  (slot type
        (type SYMBOL)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default ?NONE))
  (slot capacity
        (type INTEGER)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default ?NONE))
  (slot raw-pointer
        (type EXTERNAL-ADDRESS)
        (storage local)
        (visibility public)
        (access initialize-only))
  (message-handler init after))
(defmessage-handler ucode::memory-space init after
                    ()
                    (bind ?self:raw-pointer
                          (alloc ?self:type
                                 ?self:capacity)))



(defmethod ucode::memory-load-binary-op-store
  ((?mem EXTERNAL-ADDRESS)
   (?dest INTEGER)
   (?source0 INTEGER)
   (?source1 INTEGER)
   (?op SYMBOL))
  (memory-store ?mem
                ?dest
                (funcall ?op
                         (memory-load ?mem
                                      ?source0)
                         (memory-load ?mem
                                      ?source1))))

(defmethod ucode::memory-load-binary-op-store
  ((?mem EXTERNAL-ADDRESS)
   (?dest INTEGER)
   (?src0-mem EXTERNAL-ADDRESS)
   (?source0 INTEGER)
   (?src1-mem EXTERNAL-ADDRESS)
   (?source1 INTEGER)
   (?op SYMBOL))
  (memory-store ?mem
                ?dest
                (funcall ?op
                         (memory-load ?src0-mem
                                      ?source0)
                         (memory-load ?src1-mem
                                      ?source1))))

(defgeneric ucode::make-word64u)
(defgeneric ucode::make-word32u)
(defgeneric ucode::make-word16u)
(defmethod ucode::make-word16u
  ((?lower INTEGER)
   (?upper INTEGER))
  (encode-bits
    (encode-bits 0
                 ?lower
                 (hex->int 0x00FF)
                 0)
    ?upper
    (hex->int 0xFF00)
    8))
(defmethod ucode::make-word32u
  ((?lower INTEGER)
   (?upper INTEGER))
  (encode-bits 
    (encode-bits 0
                 ?lower
                 (hex->int 0x0000FFFF)
                 0)
    ?upper
    (hex->int 0xFFFF0000)
    16))
(defmethod ucode::make-word32u
  ((?lowest INTEGER)
   (?lower-upper INTEGER)
   (?upper-lower INTEGER)
   (?upper INTEGER))
  (make-word32u (make-word16u ?lowest
                              ?lower-upper)
                (make-word16u ?upper-lower
                              ?upper)))

(defmethod ucode::make-word64u
  ((?lower INTEGER)
   (?upper INTEGER))
  (encode-bits 
    (encode-bits 0
                 ?lower
                 (hex->int 0x00000000FFFFFFFF)
                 0)
    ?upper
    (hex->int 0xFFFFFFFF00000000)
    32))

(defmethod ucode::make-word64u
  ((?lowest INTEGER)
   (?lower-upper INTEGER)
   (?upper-lower INTEGER)
   (?upper INTEGER))
  (make-word64u (make-word32u ?lowest
                              ?lower-upper)
                (make-word32u ?upper-lower
                              ?upper)))
(defmethod ucode::make-word64u
  ((?f0 INTEGER)
   (?f1 INTEGER)
   (?f2 INTEGER)
   (?f3 INTEGER)
   (?f4 INTEGER)
   (?f5 INTEGER)
   (?f6 INTEGER)
   (?f7 INTEGER))
  (make-word64u (make-word16u ?f0
                              ?f1)
                (make-word16u ?f2
                              ?f3)
                (make-word16u ?f4
                              ?f5)
                (make-word16u ?f6
                              ?f7)))

(deffunction ucode::word32u-lower-half
             (?value)
             (decode-bits ?value
                          (hex->int 0x0000FFFF)
                          0))
(deffunction ucode::word32u-upper-half
             (?value)
             (decode-bits ?value
                          (hex->int 0xFFFF0000)
                          16))

(defmethod ucode::registerp
  ((?sym SYMBOL))
  (and (has-prefix ?sym r)
       (integerp 
         (string-to-field
           (sub-string 2 (length$ ?sym)
                       ?sym)))))

(defmethod ucode::register-to-index
  ((?sym SYMBOL
         (registerp ?sym)))
  (string-to-field 
    (sub-string 2 (length$ ?sym)
                ?sym)))

