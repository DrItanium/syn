
(defmodule iris16
           (import ucode
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
