; syn
; Copyright (c) 2013-2017, Joshua Scoggins and Contributors
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defmodule cortex
           (export ?ALL))
(defglobal cortex
           ?*priority:first* = 10000
           ?*priority:right-after-first* = 9999
           ?*priority:two-after-first* = 9998
           ?*priority:three* = 3
           ?*priority:two* = 2
           ?*priority:one* = 1
           ?*priority:last* = -9999
           ?*priority:dead-last* = -10000)
(defgeneric cortex::increment
            "Increment the given number by 1")
(defgeneric cortex::decrement
            "Decrement the given number by 1")
(defgeneric cortex::int->hex
            "Convert the given integer to a hex number!")
(defgeneric cortex::get-bit
            "Retrieves a single bit from a number as a boolean")

(defmethod cortex::int->hex
  ((?value INTEGER))
  (sym-cat (format nil
                   "0x%x"
                   ?value)))

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


(defgeneric cortex::type-of)
(defmethod cortex::type-of
  ((?a EXTERNAL-ADDRESS))
  (call ?a
        type))
(defmethod cortex::type-of
  ((?a PRIMITIVE))
  (class ?a))


(defclass cortex::has-contents
  (is-a USER)
  (multislot contents
             (storage local)
             (visibility public)))


(deffunction cortex::number-list->bytes
             (?number-list)
             (map$ break-apart-number
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
(deffunction cortex::build-stubbed-message-handler
             (?type ?op)
             (buildf "(defmessage-handler %s %s primary () ?self)"
                     ?type
                     ?op))
(deffunction cortex::little-endianp
             ()
             (eq (get-endian)
                 little))
(deffunction cortex::unknown-endianp
             ()
             (eq (get-endian)
                 unknown))
(deffunction cortex::big-endianp
             ()
             (eq (get-endian)
                 big))

(defmethod cortex::get-bit
  ((?value INTEGER)
   (?index INTEGER
           (<= 0 ?current-argument 63)))
  (<> (decode-bits ?value
                   (left-shift 1
                               ?index)
                   ?index)
      0))
(defmethod cortex::get-bit
  ((?value INTEGER)
   (?index INTEGER
           (not (<= 0 ?current-argument 63))))
  FALSE)



