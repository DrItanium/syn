(defmodule cortex
           "Defines basic functions and such!"
           (export ?ALL))

(defgeneric cortex::increment
            "Increment the given number by 1")
(defgeneric cortex::decrement
            "Decrement the given number by 1")

(defmethod cortex::increment 
  ((?value INTEGER))
  (+ ?value
     1))

(defmethod cortex::decrement
  ((?value INTEGER))
  (- ?value
     1))

(defclass cortex::thing
  "Base class for everything that requires a parent relationship!"
  (is-a USER)
  (slot parent
        (type INSTANCE
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler parent-is primary))

(defmessage-handler cortex::thing parent-is primary
                    "Is the given name a parent of the current thing"
                    (?c)
                    (or (eq ?c 
                            ?self:parent)
                        (and (instancep ?self:parent)
                             (send ?self:parent 
                                   parent-is
                                   ?c))))

(defclass cortex::has-children
  "An interface that stipulates the given class has children!"
  (is-a USER)
  (multislot children
             (storage local)
             (visibility public)))

(defclass cortex::thing-with-children
  (is-a thing
        has-children))

(deffunction cortex::bool
             "Convert the number to a boolean value!"
             (?value)
             (<> ?value
                 0))

(defclass cortex::has-title
  "An object which has a title separate from its instance name"
  (is-a USER)
  (slot title
        (type LEXEME)
        (visibility public)
        (storage local)))

(defclass cortex::has-index
  (is-a USER)
  (slot index
        (type INTEGER)
        (visibility public)
        (storage local)))

(defclass cortex::indexed-thing
  (is-a thing
        has-index))

(defclass cortex::indexed-thing-with-children
  (is-a thing-with-children
        indexed-thing))

(defclass cortex::has-value
  (is-a USER)
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE)))
