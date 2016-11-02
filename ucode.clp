; Basic routines that act as ucode operations
(defmodule ucode
           (export ?ALL))
(deffunction ucode::alloc-word16u
             (?capacity)
             (new word16u
                  ?capacity))
(deffunction ucode::to-word16u
             (?value)
             (add-word16u ?value
                          0))
(deffunction ucode::zero-memory
             (?mem)
             (call ?mem
                   clear))
(deffunction ucode::load-memory
             (?mem ?address)
             (call ?mem
                   get
                   ?address))
(deffunction ucode::store-memory
             (?mem ?address ?value)
             (call ?mem
                   set
                   ?address
                   ?value))

(deffunction ucode::load-memory16u
             (?mem ?index)
             (to-word16u (load-memory ?mem
                                      ?index)))

(deffunction ucode::store-memory16u
             (?mem ?index ?value)
             (store-memory ?mem
                           ?index
                           (to-word16u ?value)))

(deffunction ucode::increment-memory
             "Load the given memory cell add one and then store it again"
             (?mem ?address)
             (store-memory ?mem
                           ?address
                           (+ (load-memory ?mem
                                           ?address)
                              1)))

(deffunction ucode::decrement-memory
             (?mem ?address)
             (store-memory ?mem
                           ?address
                           (- (load-memory ?mem
                                           ?address)
                              1)))
(deffunction ucode::load-modify-store-memory
             (?mem ?address ?operation)
             (store-memory ?mem
                           ?address
                           (funcall ?operation
                                    (load-memory ?mem
                                                 ?address))))

(deffunction ucode::indirect-load
             "Load the value stored at the address stored at the provided address"
             (?mem ?address)
             (load-memory ?mem
                          (load-memory ?mem
                                       ?address)))
(deffunction ucode::indirect-store
             (?mem ?address ?value)
             (store-memory ?mem
                           (load-memory ?mem
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
