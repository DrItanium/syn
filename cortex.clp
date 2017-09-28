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


(defglobal MAIN
           ?*priority:first* = 10000
           ?*priority:right-after-first* = 9999
           ?*priority:two-after-first* = 9998
           ?*priority:three* = 3
           ?*priority:two* = 2
           ?*priority:one* = 1
           ?*priority:last* = -9999
           ?*priority:dead-last* = -10000)
(defgeneric MAIN::increment
            "Increment the given number by 1")
(defgeneric MAIN::decrement
            "Decrement the given number by 1")
(defgeneric MAIN::int->hex
            "Convert the given integer to a hex number!")

(defmethod MAIN::int->hex
           ((?value INTEGER))
           (sym-cat (format nil
                            "0x%x"
                            ?value)))

(defmethod MAIN::increment
  ((?value INTEGER))
  (+ ?value
     1))

(defmethod MAIN::decrement
  ((?value INTEGER))
  (- ?value
     1))

(defclass MAIN::thing
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

(defmessage-handler MAIN::thing parent-is primary
                    "Is the given name a parent of the current thing"
                    (?c)
                    (or (eq ?c
                            ?self:parent)
                        (and (instancep ?self:parent)
                             (send ?self:parent
                                   parent-is
                                   ?c))))

(defclass MAIN::has-children
  "An interface that stipulates the given class has children!"
  (is-a USER)
  (multislot children
             (storage local)
             (visibility public)))

(defclass MAIN::thing-with-children
  (is-a thing
        has-children))
(deffunction MAIN::int->bool
             "Convert the number to a boolean value!"
             (?value)
             (<> ?value
                 0))
(deffunction MAIN::bool
 (?value)
 (int->bool ?value))

(deffunction MAIN::bool->int
             (?value)
             (if ?value then 1 else 0))

(defclass MAIN::has-title
  "An object which has a title separate from its instance name"
  (is-a USER)
  (slot title
        (type LEXEME)
        (visibility public)
        (storage local)))

(defclass MAIN::has-index
  (is-a USER)
  (slot index
        (type INTEGER)
        (visibility public)
        (storage local)))

(defclass MAIN::indexed-thing
  (is-a thing
        has-index))

(defclass MAIN::indexed-thing-with-children
  (is-a thing-with-children
        indexed-thing))

(defclass MAIN::has-value
  (is-a USER)
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE)))

(deftemplate MAIN::fact
             (slot target
                   (default ?NONE))
             (slot description
                   (type SYMBOL)
                   (default ?NONE))
             (multislot data))


(defgeneric MAIN::type-of)
(defmethod MAIN::type-of
  ((?a EXTERNAL-ADDRESS))
  (call ?a
        type))
(defmethod MAIN::type-of
  ((?a PRIMITIVE))
  (class ?a))


(defclass MAIN::has-contents
  (is-a USER)
  (multislot contents
             (storage local)
             (visibility public)))


(deffunction MAIN::number-list->bytes
             (?number-list)
             (map break-apart-number
                  (expand$ ?number-list)))



(defgeneric MAIN::buildf)
(defmethod MAIN::buildf
  ((?router SYMBOL)
   (?fmt STRING)
   (?args MULTIFIELD))
  (build (format ?router
                 ?fmt
                 (expand$ ?args))))
(defmethod MAIN::buildf
  ((?router SYMBOL)
   (?fmt STRING)
   $?args)
  (buildf ?router
          ?fmt
          ?args))
(defmethod MAIN::buildf
  ((?fmt STRING)
   (?args MULTIFIELD))
  (buildf nil
          ?fmt
          ?args))
(defmethod MAIN::buildf
  ((?fmt STRING)
   $?args)
  (buildf ?fmt
          ?args))

(deffunction MAIN::symbol->index
             (?symbol ?collection)
             (member$ ?symbol
                      ?collection))
(deffunction MAIN::symbol->zero-index
             (?symbol ?collection)
             (- (symbol->index ?symbol
                               ?collection)
                1))
(deffunction MAIN::build-stubbed-message-handler
             (?type ?op)
             (buildf "(defmessage-handler %s %s primary () ?self)"
                     ?type
                     ?op))
