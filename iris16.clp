
(defmodule iris16
           (import cortex
                   ?ALL)
           (import ucode
                   ?ALL)
           (export deffunction
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
        (create-accessors read)
        (default 253))
  (slot link-register-index
        (type INTEGER)
        (visibility public)
        (storage shared)
        (access read-only)
        (create-accessors read)
        (default 254))
  (slot instruction-pointer-index
        (type INTEGER)
        (visibility public)
        (storage shared)
        (access read-only)
        (create-accessors read)
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
                            (alloc word16u
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
  ((?data-capacity INTEGER)
   (?code-capacity INTEGER)
   (?stack-capacity INTEGER))
  (make-instance of core
                 (data-capacity ?data-cap)
                 (code-capacity ?code-cap)
                 (stack-capacity ?stack-cap)))
