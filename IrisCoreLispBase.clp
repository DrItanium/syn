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


(deffunction lower::mk-move-op
             (?parent ?destination ?source)
             (mk-list ?parent
                      move
                      ?destination
                      ?source))

(defclass lower::register
  (is-a node)
  (slot alias-to
        (type SYMBOL
              INSTANCE)
        (visibility public)
        (storage local)
        (allowed-symbols FALSE))
  (message-handler resolve primary))

(defmessage-handler lower::register resolve primary
                    ()
                    (if (instancep ?self:alias-to) then
                      (send ?self:alias-to
                            resolve)
                      else
                      (instance-name-to-symbol (instance-name ?self))))



(defclass lower::section
  (is-a node
        has-body)
  (slot section
        (type SYMBOL)
        (allowed-symbols code
                         data)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler resolve primary))

(defmessage-handler lower::section resolve primary
                    ()
                    (create$ (str-cat .
                                      ?self:section)
                             (map call-resolve
                                  (expand$ ?self:body))))

(defclass lower::label
  (is-a node
        has-title
        has-body)
  (message-handler resolve primary))

(defmessage-handler lower::label resolve primary
                    ()
                    (create$ (str-cat ".label "
                                      (dynamic-get title))
                             (map call-resolve
                                  (expand$ ?self:body))))

(defclass lower::org
  (is-a node
        has-body)
  (slot address
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler resolve primary))

(defmessage-handler lower::org resolve primary
                    ()
                    (create$ (str-cat ".org "
                                      (send (dynamic-get address)
                                            resolve))
                             (map call-resolve
                                  (expand$ ?self:body))))

(defclass lower::word
  (is-a node)
  (slot value
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler resolve primary))

(defmessage-handler lower::word resolve primary
                    ()
                    (str-cat ".word "
                             (dynamic-get value)))

(defclass lower::instruction
  (is-a node
        has-title)
  (message-handler resolve-arguments primary)
  (message-handler resolve primary))

(defmessage-handler lower::instruction resolve primary
                    ()
                    (str-cat (dynamic-get title)
                             " "
                             (send ?self
                                   resolve-arguments)))

(defmessage-handler lower::instruction resolve-arguments primary
                    ()
                    "")

(defclass lower::zero-argument-instruction
  (is-a instruction))

(defclass lower::instruction-with-destination
  (is-a instruction)
  (slot destination-register
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmessage-handler lower::instruction-with-destination resolve-arguments primary
                    ()
                    (str-cat (send ?self:destination-register
                                   resolve)))

(defclass lower::one-argument-instruction
  (is-a instruction-with-destination))

(defclass lower::instruction-with-destination-and-source0
  (is-a instruction-with-destination)
  (slot source-register0
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmessage-handler lower::instruction-with-destination-and-source0 resolve-arguments primary
                    ()
                    (str-cat (call-next-handler)
                             " "
                             (send ?self:source-register0
                                   resolve)))

(defclass lower::two-argument-instruction
  (is-a instruction-with-destination-and-source0))

(defclass lower::instruction-with-destination-source0-and-source1
  (is-a instruction-with-destination-and-source0)
  (slot source-register1
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmessage-handler lower::instruction-with-destination-source0-and-source1 resolve-arguments primary
                    ()
                    (str-cat (call-next-handler)
                             " "
                             (send ?self:source-register1
                                   resolve)))

(defclass lower::three-argument-instruction
  (is-a instruction-with-destination-source0-and-source1))

(defclass lower::instruction-with-destination-source0-source1-and-source2
  (is-a instruction-with-destination-source0-and-source1)
  (slot source-register2
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmessage-handler lower::instruction-with-destination-source0-source1-and-source2 resolve-arguments primary
                    ()
                    (str-cat (call-next-handler)
                             " "
                             (send ?self:source-register2
                                   resolve)))

(defclass lower::four-argument-instruction
  (is-a instruction-with-destination-source0-source1-and-source2))

(defclass lower::simple-container
  (is-a node
        has-body))

(deffunction lower::mk-container
             (?name ?parent $?body)
             (make-instance ?name of simple-container
                            (parent ?parent)
                            (body ?body)))

(defmessage-handler lower::simple-container resolve primary
                    ()
                    (map call-resolve
                         (expand$ ?self:body)))

(defrule lower::construct-alias
         (declare (salience ?*priority:first*))
         ?f <- (object (is-a list)
                       (contents alias
                                 ?target
                                 as
                                 ?alias&:(symbolp ?alias))
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?alias of register
                        (parent ?parent)
                        (alias-to (if (symbolp ?target) then
                                    (symbol-to-instance-name ?target)
                                    else
                                    ?target))))

(defrule lower::mark-register
         (declare (salience 100))
         ?f <- (object (is-a list)
                       (contents $?a
                                 ?register&:(symbolp ?register)
                                 $?b))
         (object (is-a register)
                 (name =(symbol-to-instance-name ?register)))
         =>
         (modify-instance ?f
                          (contents ?a
                                    (symbol-to-instance-name ?register)
                                    ?b)))

(defrule lower::construct-section
         ?f <- (object (is-a list)
                       (contents section
                                 ?section&:(not (neq ?section
                                                     code
                                                     data))
                                 $?body)
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of section
                        (body ?body)
                        (section ?section)
                        (parent ?p)))

(defrule lower::make-org
         ?f <- (object (is-a list)
                       (contents org
                                 ?address
                                 $?body)
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of org
                        (address ?address)
                        (parent ?p)
                        (body ?body)))
(defrule lower::make-label
         ?f <- (object (is-a list)
                       (contents label
                                 ?title
                                 $?body)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of label
                        (parent ?p)
                        (title ?title)
                        (body ?body)))



(defrule lower::make-word
         ?f <- (object (is-a list)
                       (contents word
                                 ?value)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of word
                        (parent ?p)
                        (value ?value)))

(defrule lower::construct-output-string
         (declare (salience -1000))
         (object (is-a file)
                 (name ?file))
         ?f <- (object (is-a section)
                       (parent ?file))
         =>
         (progn$ (?l (send ?f resolve))
                 (printout t ?l crlf)))


(defrule lower::construct-one-register-instruction
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?destination)
                       (name ?n)
                       (parent ?p))
         (one-register-operation ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of one-argument-instruction
                        (parent ?p)
                        (title ?operation)
                        (destination-register ?destination)))
(defrule lower::construct-zero-register-instruction
         ?f <- (object (is-a list)
                       (contents ?operation)
                       (name ?n)
                       (parent ?p))
         (zero-register-operation ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of zero-argument-instruction
                        (parent ?p)
                        (title ?operation)))

(defrule lower::construct-two-register-instruction
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?destination
                                 ?source0)
                       (name ?n)
                       (parent ?p))
         (two-register-operation ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (title ?operation)
                        (destination-register ?destination)
                        (source-register0 ?source0)))

(defrule lower::construct-three-register-instruction
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?destination
                                 ?source0
                                 ?source1)
                       (name ?n)
                       (parent ?p))
         (three-register-operation ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of three-argument-instruction
                        (parent ?p)
                        (title ?operation)
                        (destination-register ?destination)
                        (source-register0 ?source0)
                        (source-register1 ?source1)))

(defrule lower::construct-four-register-instruction
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?destination
                                 ?source0
                                 ?source1
                                 ?source2)
                       (name ?n)
                       (parent ?p))
         (four-register-operation ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of four-argument-instruction
                        (parent ?p)
                        (title ?operation)
                        (destination-register ?destination)
                        (source-register0 ?source0)
                        (source-register1 ?source1)
                        (source-register2 ?source2)))
;---------------------------------------------------------
; macros and shorthand operations
;---------------------------------------------------------

(defrule lower::parse-set-operation-alternate-style
         ?f <- (object (is-a list)
                       (contents set
                                 ?target
                                 to
                                 ?value)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (title set)
                        (destination-register ?target)
                        (source-register0 ?value)))

(defrule lower::parse-call-unconditional-operation-register-version
         ?f <- (object (is-a list)
                       (contents call
                                 ?register))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f
                          (contents bl
                                    ?register)))

(defrule lower::parse-call-unconditional-operation-immediate-version
         ?f <- (object (is-a list)
                       (contents call
                                 ?target&:(or (symbolp ?target)
                                              (numberp ?target))))
         =>
         (modify-instance ?f
                          (contents bil
                                    ?target)))

(defrule lower::parse-return-instruction
         ?f <- (object (is-a list)
                       (contents ret))
         =>
         (modify-instance ?f
                          (contents blr)))

(defrule lower::parse-call-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents call lr))
         =>
         (modify-instance ?f
                          (contents blrl)))

(defrule lower::parse-move-to-lr-alt
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 lr
                                 ?register))
         =>
         (modify-instance ?f
                          (contents mtlr
                                    ?register)))
(defrule lower::parse-move-from-lr-alt
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 ?register
                                 lr))
         =>
         (modify-instance ?f
                          (contents mflr
                                    ?register)))

(defrule lower::parse-move-to-ip-alt
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 ip
                                 ?register))
         =>
         (modify-instance ?f
                          (contents mtip
                                    ?register)))
(defrule lower::parse-move-from-ip-alt
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 ?register
                                 ip))
         =>
         (modify-instance ?f
                          (contents mfip
                                    ?register)))

(deffacts lower::nary-register-macros
          (nary-register-macro add)
          (nary-register-macro sub)
          (nary-register-macro mul)
          (nary-register-macro div)
          (nary-register-macro rem)
          (nary-register-macro shl)
          (nary-register-macro shr)
          (nary-register-macro and)
          (nary-register-macro or)
          (nary-register-macro xor)
          (nary-register-macro nand)
          (nary-register-macro nor)
          (nary-register-macro min)
          (nary-register-macro max))

(defrule lower::parse-nary-argument-macro
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?destination
                                 ?source0
                                 ?source1
                                 $?rest
                                 ?sourceN)
                       (name ?name)
                       (parent ?p))
         (nary-register-macro ?operation)
         =>
         (unmake-instance ?f)
         (mk-container ?name
                       ?p
                       (mk-list ?name
                                ?operation
                                iv0
                                ?source0
                                ?source1
                                $?rest)
                       (mk-list ?name
                                ?operation
                                ?destination
                                iv0
                                ?sourceN)))



(defrule lower::parse-using-block
         ?f <- (object (is-a list)
                       (contents using
                                 ?stack-info
                                 ?registers-to-preserve
                                 $?body)
                       (name ?n)
                       (parent ?p))
         ?f2 <- (object (is-a list)
                        (name ?stack-info)
                        (contents save-to
                                  ?stack))
         ?f3 <- (object (is-a list)
                        (name ?registers-to-preserve)
                        (contents $?registers))
         =>
         (unmake-instance ?f
                          ?f2
                          ?f3)
         (bind ?pre
               (create$))
         (bind ?post
               (create$))
         (progn$ (?a $?registers)
                 (bind ?pre
                       ?pre
                       (mk-list ?n
                                push
                                ?stack
                                ?a))
                 (bind ?post
                       (mk-list ?n
                                pop
                                ?a
                                ?stack)
                       ?post))
         (mk-container ?n
                       ?p
                       ?pre
                       ?body
                       ?post))

(defrule lower::parse-func
         ?f <- (object (is-a list)
                       (contents func
                                 ?name
                                 $?body)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of label
                        (parent ?p)
                        (title ?name)
                        (body ?body
                              (mk-list ?n
                                       ret))))
(defrule lower::generate-nop-from-tautology-swap-or-move
         (declare (salience ?*priority:right-after-first*))
         ?f <- (object (is-a list)
                       (contents swap|move
                                 ?same
                                 ?same)
                       (name ?n)
                       (parent ?p))
         =>
         (modify-instance ?f
                          (contents nop)))

