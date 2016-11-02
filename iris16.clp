(defmodule native
           (export ?ALL))
(deffunction native::alloc-word16u
             (?capacity)
             (new word16u
                  ?capacity))
(deffunction native::to-word16u
             (?value)
             (add-word16u ?value
                          0))
(deffunction native::zero-memory
             (?mem)
             (call ?mem
                   clear))
(deffunction native::load-memory
             (?mem ?address)
             (call ?mem
                   get
                   ?address))
(deffunction native::store-memory
             (?mem ?address ?value)
             (call ?mem
                   set
                   ?address
                   ?value))

(deffunction native::load-memory16u
             (?mem ?index)
             (to-word16u (load-memory ?mem
                                      ?index)))

(deffunction native::store-memory16u
             (?mem ?index ?value)
             (store-memory ?mem
                           ?index
                           (to-word16u ?value)))

(deffunction native::increment-memory
             "Load the given memory cell add one and then store it again"
             (?mem ?address)
             (store-memory ?mem
                           ?address
                           (bind ?output
                                 (+ (load-memory ?mem
                                                 ?address)
                                    1))))

(deffunction native::decrement-memory
             (?mem ?address)
             (store-memory ?mem
                           ?address
                           (- (load-memory ?mem
                                           ?address)
                              1)))
(deffunction native::indirect-load
             "Load the value stored at the address stored at the provided address"
             (?mem ?address)
             (load-memory ?mem
                          (load-memory ?mem
                                       ?address)))
(deffunction native::indirect-store
             (?mem ?address ?value)
             (store-memory ?mem
                           (load-memory ?mem
                                        ?address)
                           ?value))
(deffunction native::swap-memory
             (?mem ?addr0 ?addr1)
             (call ?mem
                   swap
                   ?addr0
                   ?addr1))

(deffunction native::memory-op
             "Load two addresses, perform an operation on the values, and then store it in a given address"
             (?operation ?mDest ?addressDest ?mSrc0 ?addressSrc0 ?mSrc1 ?addressSrc1)
             (store-memory ?mDest
                           ?addressDest
                           (funcall ?operation
                                    (load-memory ?mSrc0
                                                 ?addressSrc0)
                                    (load-memory ?mSrc1
                                                 ?addressSrc1))))

(deffunction native::memory-op-same-memory
             (?operation ?mem ?destination ?source0 ?source1)
             (memory-op ?operation
                        ?mem
                        ?destination
                        ?mem
                        ?source0
                        ?mem
                        ?source1))
(deffunction native::register-style-op
             "Load values from two addresses and perform an operation on it, the result is not saved to memory but returned instead!"
             (?operation ?mSrc0 ?src0 ?mSrc1 ?src1)
             (funcall ?operation
                      (load-memory ?mSrc0
                                   ?src0)
                      (load-memory ?mSrc1
                                   ?src1)))
(deffunction native::memory-op-with-immediate
             "Perform a memory op where the second argument is an immediate"
             (?operation ?mDest ?dest ?mSrc0 ?src0 ?immediate)
             (store-memory ?mDest
                           ?dest
                           (funcall ?operation
                                    (load-memory ?mSrc0
                                                 ?src0)
                                    ?immediate)))
(deffunction native::register-style-op-with-immediate
             (?operation ?mSrc0 ?src0 ?immediate)
             (funcall ?operation
                      (load-memory ?mSrc0
                                   ?src0)
                      ?immediate))

(defmodule iris16
           (import native
                   ?ALL)
           (export deffunction
                   new-core)
           (export defclass
                   core))

(defclass iris16::core
  (is-a USER)
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
  (slot instruction-pointer-index
        (type INTEGER)
        (visibility public)
        (storage shared)
        (access read-only)
        (default 255))
  (message-handler get-register primary)
  (message-handler set-register primary)
  (message-handler get-instruction-pointer primary)
  (message-handler set-instruction-pointer primary)
  (message-handler init after)
  (message-handler startup primary))
(defmessage-handler iris16::core get-instruction-pointer primary
                    ()
                    (load-memory16u ?self:registers
                                    (dynamic-get instruction-pointer-index)))
(defmessage-handler iris16::core set-instruction-pointer primary
                    (?value)
                    (store-memory16u ?self:registers
                                     (dynamic-get instruction-pointer-index)
                                     ?value))
(defmessage-handler iris16::core get-register primary
                    (?index)
                    (load-memory16u ?self:registers
                                    ?index))
(defmessage-handler iris16::core set-register primary
                    (?index ?value)
                    (store-memory16u ?self:registers
                                     ?index
                                     ?value))

(defmessage-handler iris16::core init after
                    ()
                    (bind ?self:registers
                          (alloc-word16u 256))
                    (bind ?self:data
                          (alloc-word16u (to-word16u ?self:data-capacity)))
                    (bind ?self:stack
                          (alloc-word16u (to-word16u ?self:stack-capacity)))
                    (bind ?self:code
                          (alloc-word16u (to-word16u ?self:code-capacity))))

(deffunction iris16::new-core
             (?data-cap ?code-cap ?stack-cap)
             (make-instance of core
                            (data-capacity ?data-cap)
                            (code-capacity ?code-cap)
                            (stack-capacity ?stack-cap)))
