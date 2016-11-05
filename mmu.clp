; Defines a basic mmu
(defmodule memory-management
           (import cortex
                   ?ALL)
           (import ucode 
                   ?ALL)
           (export ?ALL))

(deftemplate memory-management::memory-request
             "Describes an action to be performed on memory"
             (slot action
                   (type SYMBOL)
                   (default ?NONE))
             (slot address
                   (type INTEGER)
                   (default ?NONE))
             (multislot arguments))
(defclass memory-management::memory-responder
  "Acts as a responder to a memory request"
  (is-a thing)
  (slot begin-address
        (type INTEGER)
        (range 0 ?VARIABLE)
        (default ?NONE))
  (slot end-address
        (type INTEGER)
        (range 0 ?VARIABLE)
        (default ?NONE)))
