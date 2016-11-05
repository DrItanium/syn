; Defines a basic mmu
(defmodule memory-management
           (import cortex
                   ?ALL)
           (import ucode 
                   ?ALL)
           (export ?ALL))

(defclass memory-management::memory-space
          (is-a USER)

(defclass memory-management::mmu 
          (is-a USER)
          (multislot children)
