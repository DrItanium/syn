(defmodule cortex
           "Defines basic functions and such!"
           (export ?ALL))
(defglobal cortex
           ?*priority:first* = 10000
           ?*priority:right-after-first* = 9999
           ?*priority:three* = 3
           ?*priority:two* = 2
           ?*priority:one* = 1
           ?*priority:last* = -9999
           ?*priority:dead-last* = -10000)
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
(deffunction cortex::int->bool
             "Convert the number to a boolean value!"
             (?value)
             (<> ?value
                 0))
(deffunction cortex::bool
 (?value)
 (int->bool ?value))

(deffunction cortex::bool->int
             (?value)
             (if ?value then 1 else 0))

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

(deftemplate cortex::fact
             (slot target
                   (default ?NONE))
             (slot description
                   (type SYMBOL)
                   (default ?NONE))
             (multislot data))


(defgeneric cortex::external-address-type)
(defmethod cortex::external-address-type
  ((?a EXTERNAL-ADDRESS))
  (call ?a
        type))


(defclass cortex::has-contents
  (is-a USER)
  (multislot contents
             (storage local)
             (visibility public)))


(deffunction cortex::number-list->bytes
             (?number-list)
             (map break-apart-number
                  (expand$ ?number-list)))



(defgeneric cortex::buildf)
(defmethod cortex::buildf
  ((?router SYMBOL)
   (?fmt STRING)
   (?args MULTIFIELD))
  (build (format ?router
                 ?fmt
                 (expand$ ?args))))
(defmethod cortex::buildf
  ((?router SYMBOL)
   (?fmt STRING)
   $?args)
  (buildf ?router
          ?fmt
          ?args))
(defmethod cortex::buildf
  ((?fmt STRING)
   (?args MULTIFIELD))
  (buildf nil
          ?fmt
          ?args))
(defmethod cortex::buildf
  ((?fmt STRING)
   $?args)
  (buildf ?fmt
          ?args))

(deffunction cortex::symbol->index
             (?symbol ?collection)
             (member$ ?symbol
                      ?collection))
(deffunction cortex::symbol->zero-index
             (?symbol ?collection)
             (- (symbol->index ?symbol
                               ?collection)
                1))
