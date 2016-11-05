; Basic routines that act as ucode operations
(defmodule ucode
           (export ?ALL))
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
(defgeneric ucode::increment
            "Increment the given number by 1")
(defgeneric ucode::decrement
            "Decrement the given number by 1")
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


(defmethod ucode::increment 
  ((?value INTEGER))
  (+ ?value
     1))

(defmethod ucode::decrement
  ((?value INTEGER))
  (- ?value
     1))

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
  ((?memory EXTERNAL-ADDRESS)
   (?address INTEGER)
   (?op SYMBOL))
  (memory-store ?memory
                ?address
                (memory-load-op ?memory
                                ?address
                                ?op)))

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

(deffunction ucode::swap-memory
             (?mem ?addr0 ?addr1)
             (call ?mem
                   swap
                   ?addr0
                   ?addr1))

(deffunction ucode::memory-op
             "Load two addresses, perform an operation on the values, and then store it in a given address"
             (?operation ?mDest ?addressDest ?mSrc0 ?addressSrc0 ?mSrc1 ?addressSrc1)
             (store-memory ?mDest
                           ?addressDest
                           (funcall ?operation
                                    (load-memory ?mSrc0
                                                 ?addressSrc0)
                                    (load-memory ?mSrc1
                                                 ?addressSrc1))))

(deffunction ucode::memory-op-same-memory
             (?operation ?mem ?destination ?source0 ?source1)
             (memory-op ?operation
                        ?mem
                        ?destination
                        ?mem
                        ?source0
                        ?mem
                        ?source1))

(deffunction ucode::two-address-memory-op
             (?operation ?mDest ?addressDest ?mSrc0 ?addressSrc0)
             (memory-op ?operation
                        ?mDest ?addressDest
                        ?mDest ?addressDest
                        ?mSrc0 ?addressSrc0))

(deffunction ucode::load-dword
             "Load two words worth of data and return it as a multifield"
             (?memory ?address)
             (create$ (load-memory ?memory
                                   ?address)
                      (load-memory ?memory
                                   (+ ?address 1))))

(deffunction ucode::store-dword
             (?memory ?address ?value0 ?value1)
             (store-memory ?memory
                           ?address
                           ?value0)
             (store-memory ?memory
                           (+ ?address 1)
                           ?value1))

(deffunction ucode::register-style-op
             "Load values from two addresses and perform an operation on it, the result is not saved to memory but returned instead!"
             (?operation ?mSrc0 ?src0 ?mSrc1 ?src1)
             (funcall ?operation
                      (load-memory ?mSrc0
                                   ?src0)
                      (load-memory ?mSrc1
                                   ?src1)))

(deffunction ucode::memory-op-with-immediate
             "Perform a memory op where the second argument is an immediate"
             (?operation ?mDest ?dest ?mSrc0 ?src0 ?immediate)
             (store-memory ?mDest
                           ?dest
                           (funcall ?operation
                                    (load-memory ?mSrc0
                                                 ?src0)
                                    ?immediate)))
(deffunction ucode::register-style-op-with-immediate
             (?operation ?mSrc0 ?src0 ?immediate)
             (funcall ?operation
                      (load-memory ?mSrc0
                                   ?src0)
                      ?immediate))

