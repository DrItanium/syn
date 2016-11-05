; Defines a basic mmu
(defmodule memory-management
           (import cortex
                   ?ALL)
           (import ucode 
                   ?ALL)
           (export ?ALL))



(defclass memory-management::mmu 
          (is-a thing-with-children))
