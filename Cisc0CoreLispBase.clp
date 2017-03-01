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


(defmessage-handler NUMBER resolve primary
                    ()
                    ?self)
(defmessage-handler LEXEME resolve primary
                    ()
                    ?self)
(deffunction lower::call-resolve
             (?obj)
             (send ?obj
                   resolve))
(deffunction lower::mk-list
             (?parent $?contents)
             (make-instance of list
                            (parent ?parent)
                            (contents ?contents)))
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


(defclass lower::data-storage-declaration
  (is-a node)
  (slot decl
        (type LEXEME)
        (storage shared)
        (visibility public)
        (default "Please Override In Subclasses"))
  (slot value
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler resolve primary))

(defmessage-handler lower::data-storage-declaration resolve primary
                    ()
                    (str-cat (dynamic-get decl)
                             " "
                             (dynamic-get value)))

(defclass lower::word
  (is-a data-storage-declaration)
  (slot decl
        (source composite)
        (default .word)))

(defclass lower::dword
  (is-a data-storage-declaration)
  (slot decl
        (source composite)
        (default .dword)))


(defclass lower::instruction
  (is-a node)
  (slot group
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot flags
             (storage local)
             (visibility public)
             (default ?NONE))
  (message-handler resolve-arguments primary)
  (message-handler resolve primary))

(defmessage-handler lower::instruction resolve primary
                    ()
                    (str-cat (dynamic-get group)
                             " "
                             (implode$ (dynamic-get flags))
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
(defclass lower::bitmask
  (is-a node)
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler resolve primary))
(defmessage-handler lower::bitmask resolve primary
                    ()
                    (dynamic-get value))

(deffacts lower::legal-bitmasks
          (bitmask 0m0000)
          (bitmask 0m0001)
          (bitmask 0m0010)
          (bitmask 0m0011)
          (bitmask 0m0100)
          (bitmask 0m0101)
          (bitmask 0m0110)
          (bitmask 0m0111)
          (bitmask 0m1000)
          (bitmask 0m1001)
          (bitmask 0m1010)
          (bitmask 0m1011)
          (bitmask 0m1100)
          (bitmask 0m1101)
          (bitmask 0m1110)
          (bitmask 0m1111))

(defrule lower::build-bitmasks
         (declare (salience ?*priority:first*))
         ?f <- (bitmask ?title)
         =>
         (retract ?f)
         (make-instance ?title of bitmask
                        (parent FALSE)
                        (value ?title)))

(defrule lower::tag-bitmasks
         (declare (salience 2000))
         ?f <- (object (is-a list)
                       (contents $?pre
                                 ?bitmask&:(symbolp ?bitmask)
                                 $?post)
                       (name ?n))
         (object (is-a bitmask)
                 (name =(symbol-to-instance-name ?bitmask)))
         =>
         (modify-instance ?f
                          (contents ?pre
                                    (symbol-to-instance-name ?bitmask)
                                    ?post)))

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
(defrule lower::make-dword
         ?f <- (object (is-a list)
                       (contents dword
                                 ?value)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of dword
                        (parent ?p)
                        (value /value)))


(defrule lower::construct-output-string
         (declare (salience -1000))
         (object (is-a file)
                 (name ?file))
         ?f <- (object (is-a section)
                       (parent ?file))
         =>
         (progn$ (?l (send ?f resolve))
                 (printout t ?l crlf)))


(deffacts lower::legal-shift-operations
          (operation shift right)
          (operation shift left))

(defrule lower::construct-shift-instruction:with-immediate
         ?f <- (object (is-a list)
                       (contents shift
                                 ?direction
                                 immediate
                                 ?dest
                                 ?src)
                       (name ?n)
                       (parent ?p))
         (operation shift
                    ?direction)
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group shift)
                        (flags ?direction
                               immediate)
                        (destination ?dest)
                        (source0 ?src)))

(defrule lower::construct-shift-instruction
         ?f <- (object (is-a list)
                       (contents shift
                                 ?direction
                                 ?dest
                                 ?src)
                       (name ?n)
                       (parent ?p))
         (operation shift
                    ?direction)
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group shift)
                        (flags ?direction)
                        (destination ?dest)
                        (source0 ?src)))

(deffacts lower::legal-logical-operations
          (operation logical
                     and)
          (operation logical
                     or)
          (operation logical
                     not)
          (operation logical
                     xor)
          (operation logical
                     nand))
(defrule lower::construct-logical-instruction
         ?f <- (object (is-a list)
                       (contents logical
                                 ?operation
                                 ?dest
                                 ?src)
                       (name ?n)
                       (parent ?p))
         (operation logical
                    ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group logical)
                        (operation ?operation)
                        (flags)
                        (destination ?dest)
                        (source0 ?src)))


(defrule lower::construct-logical-instruction:immediate
         ?f <- (object (is-a list)
                       (contents logical
                                 ?operation
                                 immediate
                                 ?bitmask
                                 ?destination
                                 ?value)
                       (name ?n)
                       (parent ?p))
         (operation logical
                    ?operation)
         (object (is-a bitmask)
                 (name ?bitmask))
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group logical)
                        (operation ?operation)
                        (flags immediate
                               ?bitmask)
                        (destination ?destination)
                        (source0 ?value)))
(deffacts lower::memory-operations
          (operation memory
                     load)
          (operation memory
                     store)
          (operation memory
                     push)
          (operation memory
                     pop)
          (stack-operation push)
          (stack-operation pop)
          (load-store-operation load)
          (load-store-operation store)
          (indirect-or-direct-flag indirect)
          (indirect-or-direct-flag direct))

(defrule lower::construct-memory-operation:load-store-operation
         ?f <- (object (is-a list)
                       (contents memory
                                 ?operation
                                 ?bitmask
                                 ?direct
                                 ?immediate-value)
                       (name ?n)
                       (parent ?p))
         (operation memory
                    ?operation)
         (object (is-a bitmask)
                 (name ?bitmask))
         (load-store-operation ?operation)
         (indirect-or-direct-flag ?direct)
         =>
         (unmake-instance ?f)
         (make-instance ?n of one-argument-instruction
                        (parent ?p)
                        (group memory)
                        (operation ?operation)
                        (flags ?bitmask
                               ?direct)
                        (destination ?immediate-value)))

(defrule lower::construct-memory-operation::stack-operation
         ?f <- (object (is-a list)
                       (contents memory
                                 ?operation
                                 ?bitmask
                                 ?destination)
                       (name ?n)
                       (parent ?p))
         (operation memory
                    ?operation)
         (stack-operation ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of one-argument-instruction
                        (parent ?p)
                        (group memory)
                        (operation ?operation)
                        (flags ?bitmask)
                        (destination ?destination)))
(deffacts lower::arithmetic-operations
          (operation arithmetic
                     add)
          (operation arithmetic
                     sub)
          (operation arithmetic
                     mul)
          (operation arithmetic
                     div)
          (operation arithmetic
                     rem))

(defrule lower::construct-arithmetic-operation
         ?f <- (object (is-a list)
                       (contents arithmetic
                                 ?operation
                                 ?destination
                                 ?source)
                       (name ?n)
                       (parent ?p))
         (operation arithmetic
                    ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group arithmetic)
                        (operation ?operation)
                        (flags)
                        (destination ?destination)
                        (source0 ?source)))

(defrule lower::construct-arithmetic-operation
         ?f <- (object (is-a list)
                       (contents arithmetic
                                 ?operation
                                 immediate
                                 ?destination
                                 ?source)
                       (name ?n)
                       (parent ?p))
         (operation arithmetic
                    ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group arithmetic)
                        (operation ?operation)
                        (flags immediate)
                        (destination ?destination)
                        (source0 ?source)))
(defrule lower::construct-swap-operation
         ?f <- (object (is-a list)
                       (contents swap
                                 ?dest
                                 ?src0)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group swap)
                        (operation "")
                        (flags)
                        (destination ?dest)
                        (source0 ?src0)))

(defrule lower::construct-move-operation
         ?f <- (object (is-a list)
                       (contents move
                                 ?bitmask
                                 ?dest
                                 ?src0)
                       (name ?n)
                       (parent ?p))
         (object (is-a bitmask)
                 (name ?bitmask))
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group move)
                        (operation "")
                        (flags ?bitmask)
                        (destination ?dest)
                        (source0 ?src0)))

(defrule lower::construct-set-operation
         ?f <- (object (is-a list)
                       (contents set
                                 ?bitmask
                                 ?dest
                                 ?src0)
                       (name ?n)
                       (parent ?p))
         (object (is-a bitmask)
                 (name ?bitmask))
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group set)
                        (operation "")
                        (flags ?bitmask)
                        (destination ?dest)
                        (source0 ?src0)))

(deffacts lower::compare-operations
          (operation compare
                     ==)
          (operation compare
                     !=)
          (operation compare
                     <)
          (operation compare
                     <=)
          (operation compare
                     >)
          (operation compare
                     >=))
(defrule lower::construct-compare-operation
         ?f <- (object (is-a list)
                       (contents compare
                                 ?operation
                                 ?dest
                                 ?src0)
                       (name ?n)
                       (parent ?p))
         (operation compare
                    ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group compare)
                        (operation ?operation)
                        (flags)
                        (destination ?dest)
                        (source0 ?src0)))

(defrule lower::construct-compare-operation:immediate
         ?f <- (object (is-a list)
                       (contents compare
                                 ?operation
                                 immediate
                                 ?dest
                                 ?src0)
                       (name ?n)
                       (parent ?p))
         (operation compare
                    ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group compare)
                        (operation ?operation)
                        (flags immediate)
                        (destination ?dest)
                        (source0 ?src0)))
(deffacts lower::legal-complex-operations
          (operation complex
                     encoding)
          (operation encoding
                     decode)
          (operation encoding
                     encode)
          (operation encoding
                     bitset)
          (operation encoding
                     bitunset))
(defrule lower::construct-complex-encoding-operation
         ?f <- (object (is-a list)
                       (contents complex
                                 encoding
                                 ?sub-operation)
                       (name ?n)
                       (parent ?p))
         (operation encoding
                    ?sub-operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of zero-argument-instruction
                        (parent ?p)
                        (group complex)
                        (operation encoding)
                        (flags ?sub-operation)))
(deffacts lower::legal-branch-operations
          (call-or-no-call-flag call)
          (call-or-no-call-flag nocall)
          (conditional-or-unconditional-flag conditional)
          (conditional-or-unconditional-flag unconditional))
(defrule lower::construct-branch-encoding:if
         ?f <- (object (is-a list)
                       (contents branch
                                 if
                                 ?call-flag
                                 ?r0
                                 ?r1)
                       (name ?n)
                       (parent ?p))
         (call-or-no-call-flag ?call-flag)
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group branch)
                        (operation if)
                        (flags ?call-flag)
                        (destination ?r0)
                        (source0 ?r1)))

(defrule lower::construct-branch-encoding:call
         ?f <- (object (is-a list)
                       (contents branch
                                 call
                                 ?r0)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of one-argument-instruction
                        (parent ?p)
                        (group branch)
                        (operation call)
                        (flags)
                        (destination ?r0)))

(defrule lower::construct-branch-encoding:call-immediate
         ?f <- (object (is-a list)
                       (contents branch
                                 call
                                 immediate
                                 ?r0)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of one-argument-instruction
                        (parent ?p)
                        (group branch)
                        (operation call)
                        (flags immediate)
                        (destination ?r0)))

(defrule lower::construct-branch-encoding:jump
         ?f <- (object (is-a list)
                       (contents branch
                                 ?cond
                                 ?r0)
                       (name ?n)
                       (parent ?p))
         (conditional-or-unconditional-flag ?cond)
         =>
         (unmake-instance ?f)
         (make-instance ?n of one-argument-instruction
                        (parent ?p)
                        (group branch)
                        (operation ?cond)
                        (flags)
                        (destination ?r0)))

(defrule lower::construct-branch-encoding:jump-immediate
         ?f <- (object (is-a list)
                       (contents branch
                                 ?cond
                                 immediate
                                 ?r0)
                       (name ?n)
                       (parent ?p))
         (conditional-or-unconditional-flag ?cond)
         =>
         (unmake-instance ?f)
         (make-instance ?n of one-argument-instruction
                        (parent ?p)
                        (group branch)
                        (operation ?cond)
                        (flags immediate)
                        (destination ?r0)))
;------------------------------------------------------------------------------
; Macros
;------------------------------------------------------------------------------
(deffacts lower::zero-argument-simple-macros
          "Simple macros which take in no arguments inside the instruction itself"
          (simple-macro 0 nop -> swap r0 r0)
          (simple-macro 0 istore -> indirect-store)
          (simple-macro 0 iload -> indirect-load)
          (simple-macro 0 store -> direct-store)
          (simple-macro 0 load -> direct-load)
          (simple-macro 0 bitset -> complex encoding bitset)
          (simple-macro 0 bitunset -> complex encoding bitunset)
          (simple-macro 0 encode -> complex encoding encode)
          (simple-macro 0 decode -> complex encoding decode)
          (simple-macro 0 return -> pop ip)
          (simple-macro 0 indirect-store -> memory store 0m1111 indirect 0x00)
          (simple-macro 0 indirect-load -> memory load 0m1111 indirect 0x00)
          (simple-macro 0 direct-store -> memory store 0m1111 direct 0x00)
          (simple-macro 0 direct-load -> memory load 0m1111 direct 0x00))

(defrule lower::zero-argument-simple-macro-modify
         "In the cases where the instruction has no arguments and is a single
         symbol then we can define facts in place of special rules to handle
         the replacement!"
         ?f <- (object (is-a list)
                       (contents ?target))
         (simple-macro 0
                       ?target -> $?replacement)
         =>
         (modify-instance ?f
                          (contents $?replacement)))

(deffacts lower::simple-one-arg-macros
          (simple-macro 1 push -> push32)
          (simple-macro 1 pop -> pop32)
          ; not sure if these are going to match correctly!
          ; they should since they are distinct!
          (simple-macro 1 load -> direct-load)
          (simple-macro 1 store -> direct-store)
          (simple-macro 1 iload -> indirect-load)
          (simple-macro 1 istore -> indirect-store)
          (simple-macro 1 direct-load -> memory load 0m1111 direct)
          (simple-macro 1 direct-store -> memory store 0m1111 direct)
          (simple-macro 1 indirect-load -> memory load 0m1111 indirect)
          (simple-macro 1 indirect-store -> memory store 0m1111 indirect)
          (simple-macro 1 push16u -> push16 upper)
          (simple-macro 1 push16l -> push16 lower)
          ; TODO: merge the hand written pop16 rules into this fact set
          (simple-macro 1 pop16u -> pop16 upper)
          (simple-macro 1 pop16l -> pop16 lower)
          (simple-macro 1 pop32 -> memory pop 0m1111)
          (simple-macro 1 push32 -> memory push 0m1111))

(defrule lower::translate-simple-one-arg-macro
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?argument))
         (simple-macro 1 ?operation -> $?replacement)
         =>
         (modify-instance ?f
                          (contents ?replacement
                                    ?argument)))

(deffacts lower::two-argument-simple-macros
          (simple-macro 2 set32 -> set 0m1111)
          (simple-macro 2 set24 -> set24l)
          (simple-macro 2 set24l -> set 0m0111)
          (simple-macro 2 set24u -> set 0m1110)
          (simple-macro 2 set8 -> set8ll)
          (simple-macro 2 set8ll -> set 0m0001)
          (simple-macro 2 set8lu -> set 0m0010)
          (simple-macro 2 set8ul -> set 0m0100)
          (simple-macro 2 set8uu -> set 0m1000)
          (simple-macro 2 set16 -> set16l)
          (simple-macro 2 set16l -> set 0m0011)
          (simple-macro 2 set16u -> set 0m1100))

(defrule lower::handle-two-argument-simple-macros
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?arg0
                                 ?arg1))
         (simple-macro 2 ?operation -> $?replacement)
         =>
         (modify-instance ?f
                          (contents $?replacement
                                    ?arg0
                                    ?arg1)))
(defrule lower::not-self-macro
         ?f <- (object (is-a list)
                       (contents not
                                 ?register))
         =>
         (modify-instance ?f
                          (contents logical
                                    not
                                    ?register
                                    ?register)))

(defrule lower::incr-macro
         ?f <- (object (is-a list)
                       (contents incr
                                 ?register))
         =>
         (modify-instance ?f
                          (contents arithmetic
                                    add
                                    immediate
                                    ?register
                                    0x1)))

(defrule lower::decr-macro
         ?f <- (object (is-a list)
                       (contents decr
                                 ?register))
         =>
         (modify-instance ?f
                          (contents arithmetic
                                    sub
                                    immediate
                                    ?register
                                    0x1)))


(defrule lower::clear-register-macro
         ?f <- (object (is-a list)
                       (contents clear
                                 ?register))
         =>
         (modify-instance ?f
                          (contents set
                                    0m0000
                                    ?register
                                    0x00000000)))


(deffacts lower::pop/push16-types
          (flag pop16
                upper
                0m1100)
          (flag pop16
                lower
                0m0011)
          (flag push16
                upper
                0m1100)
          (flag push16
                lower
                0m0011))
(defrule lower::pop16-macro
         ?f <- (object (is-a list)
                       (contents pop16
                                 ?half
                                 ?register))
         (flag pop16
               ?half
               ?bitmask)
         =>
         (modify-instance ?f
                          (contents memory
                                    pop
                                    ?bitmask
                                    ?register)))
(defrule lower::pop16-macro
         ?f <- (object (is-a list)
                       (contents push16
                                 ?half
                                 ?register))
         (flag push16
               ?half
               ?bitmask)
         =>
         (modify-instance ?f
                          (contents memory
                                    push
                                    ?bitmask
                                    ?register)))

(definstances lower::registers
              (r0 of register
                  (parent FALSE))
              (r1 of register
                  (parent FALSE))
              (r2 of register
                  (parent FALSE))
              (r3 of register
                  (parent FALSE))
              (r4 of register
                  (parent FALSE))
              (r5 of register
                  (parent FALSE))
              (r6 of register
                  (parent FALSE))
              (r7 of register
                  (parent FALSE))
              (r8 of register
                  (parent FALSE))
              (r9 of register
                  (parent FALSE))
              (r10 of register
                   (parent FALSE))
              (r11 of register
                   (parent FALSE))
              (r12 of register
                   (parent FALSE))
              (r13 of register
                   (parent FALSE))
              (r14 of register
                   (parent FALSE))
              (r15 of register
                   (parent FALSE))
              (addr of register
                    (parent FALSE))
              (ip of register
                  (parent FALSE))
              (sp of register
                  (parent FALSE))
              (value of register
                     (parent FALSE))
              (mask of register
                    (parent FALSE))
              (shift of register
                     (parent FALSE))
              (field of register
                     (parent FALSE)))

(defrule lower::change-stack-pointer-block
         ?f <- (object (is-a list)
                       (contents change-stack-pointer
                                 ?target
                                 $?rest)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                swap
                                sp
                                ?target)
                       $?rest
                       (mk-list ?n
                                swap
                                sp
                                ?target)))

(defrule lower::save-registers-block
         ?f <- (object (is-a list)
                       (contents use-registers
                                 ?registers
                                 $?body)
                       (name ?n)
                       (parent ?p))
         ?f2 <- (object (is-a list)
                        (name ?registers)
                        (contents $?regs))
         =>
         (unmake-instance ?f
                          ?f2)
         (bind ?pre
               (create$))
         (bind ?post
               (create$))
         (progn$ (?reg $?regs)
                 (bind ?pre
                       ?pre
                       (mk-list ?n
                                push
                                ?reg))
                 (bind ?post
                       (mk-list ?n
                                pop
                                ?reg)
                       ?post))
         (mk-container ?n
                       ?p
                       ?pre
                       ?body
                       ?post))

