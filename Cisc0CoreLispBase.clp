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




(defclass lower::section
  (is-a node
        has-body)
  (slot section
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler resolve primary))

(defmessage-handler lower::section resolve primary
                    ()
                    (create$ (str-cat "; Section "
                                      (dynamic-get section))
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

(deffacts lower::construct-data-storage-declaration
          (data-storage-decl word -> word)
          (data-storage-decl dword -> dword))


(defclass lower::instruction
  (is-a node)
  (slot group
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot operation
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
                    (bind ?rflags
                          (create$))
                    (progn$ (?flag (dynamic-get flags))
                            (bind ?rflags
                                  ?rflags
                                  (send ?flag
                                        resolve)))

                    (str-cat (dynamic-get group)
                             " "
                             (dynamic-get operation)
                             " "
                             (implode$ ?rflags)
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

(defclass lower::instruction-with-dest-src0-and-src1
  (is-a instruction-with-destination-and-source0)
  (slot source-register1
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmessage-handler lower::instruction-with-dest-src0-and-src1 resolve-arguments primary
                    ()
                    (str-cat (call-next-handler)
                             " "
                             (send ?self:source-register1
                                   resolve)))
(defclass lower::three-argument-instruction
  (is-a instruction-with-dest-src0-and-src1))

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

(deftemplate lower::macro-with-constant-immediate
             (slot match
                   (type SYMBOL)
                   (default ?NONE))
             (multislot replacement
                        (default ?NONE))
             (multislot constant
                        (default ?NONE)))

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

(defglobal lower
           ?*bitmask0* = 0m0000
           ?*bitmask32* = 0m1111
           ?*bitmask24l* = 0m0111
           ?*bitmask24u* = 0m1110
           ?*bitmask8ll* = 0m0001
           ?*bitmask8lu* = 0m0010
           ?*bitmask8ul* = 0m0100
           ?*bitmask8uu* = 0m1000
           ?*bitmask16l* = 0m0011
           ?*bitmask16u* = 0m1100)

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
                                 ?section&:(symbolp ?section)
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


(defrule lower::make-data-storage-declaration
         ?f <- (object (is-a list)
                       (contents ?decl
                                 ?value)
                       (name ?n)
                       (parent ?p))
         (data-storage-decl ?decl -> ?class)
         =>
         (unmake-instance ?f)
         (make-instance ?n of ?class
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

(defrule lower::convert-operations-fact-to-mass-operation-facts
         "Construct many operation facts from a single operations fact!"
         (declare (salience ?*priority:first*))
         ?f <- (operations ?group
                           $?sub-ops)
         =>
         (retract ?f)
         (progn$ (?sub-op $?sub-ops)
                 (assert (operation ?group
                                    ?sub-op))))
(deffacts lower::legal-shift-operations
          (operations shift
                      left
                      right))

(defrule lower::construct-shift-simple-macros
         (declare (salience ?*priority:first*))
         (operation shift
                    ?direction)
         =>
         (bind ?base-outcome
               shift
               ?direction)
         (assert (simple-macro 2 (bind ?base-title
                                       (sym-cat shift-
                                                ?direction)) -> ?base-outcome)
                 (simple-macro 2 (sym-cat ?base-title
                                          -immediate) -> ?base-outcome immediate)))

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
                        (operation ?direction)
                        (flags immediate)
                        (destination-register ?dest)
                        (source-register0 ?src)))

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
                        (operation ?direction)
                        (flags)
                        (destination-register ?dest)
                        (source-register0 ?src)))
(deffunction lower::print-message-about-offending-object
             (?name)
             (create$ tab "See " ?name " for more information!"))

(defrule lower::illegal-shift-operation
         (object (is-a list)
                 (contents shift
                           ?direction
                           $?rest)
                 (name ?name))
         (not (operation shift
                         ?direction))
         =>
         (printout werror
                   "ERROR: Shift instruction has illegal direction: " ?direction "!" crlf
                   (print-message-about-offending-object ?name) crlf)

         (halt))

(deffacts lower::legal-logical-operations
          (operations logical
                      and
                      or
                      not
                      xor
                      nand))

(defrule lower::construct-logical-group-simple-macros
         (declare (salience ?*priority:first*))
         (operation logical
                    ?op)
         =>
         (bind ?base-immediate
               (sym-cat ?op i))
         ; The immediate versions accept three arguments (bitmask, destination, immediate)
         ; We can construct extra simple macros which are two arguments which fix the bitmask
         (assert (simple-macro 2 ?op -> logical ?op)
                 (simple-macro 3 ?base-immediate -> logical ?op immediate)
                 (simple-macro 2 (sym-cat ?base-immediate 8ll) -> ?base-immediate ?*bitmask8ll*)
                 (simple-macro 2 (sym-cat ?base-immediate 8lu) -> ?base-immediate ?*bitmask8lu*)
                 (simple-macro 2 (sym-cat ?base-immediate 8ul) -> ?base-immediate ?*bitmask8ul*)
                 (simple-macro 2 (sym-cat ?base-immediate 8uu) -> ?base-immediate ?*bitmask8uu*)
                 (simple-macro 2 (sym-cat ?base-immediate 16l) -> ?base-immediate ?*bitmask16l*)
                 (simple-macro 2 (sym-cat ?base-immediate 16u) -> ?base-immediate ?*bitmask16u*)
                 (simple-macro 2 (sym-cat ?base-immediate 24l) -> ?base-immediate ?*bitmask24l*)
                 (simple-macro 2 (sym-cat ?base-immediate 24u) -> ?base-immediate ?*bitmask24u*)
                 (simple-macro 2 (sym-cat ?base-immediate 32) -> ?base-immediate ?*bitmask32*)))

(defrule lower::handle-simple-macro-replacement
         ?f <- (object (is-a list)
                       (contents ?title
                                 $?rest))
         (simple-macro =(length$ ?rest)
                       ?title -> $?replacement)
         =>
         (modify-instance ?f
                          (contents ?replacement ?rest)))

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
                        (destination-register ?dest)
                        (source-register0 ?src)))


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
                        (destination-register ?destination)
                        (source-register0 ?value)))
(defrule lower::illegal-logical-instruction
         ?f <- (object (is-a list)
                       (contents logical
                                 ?operation
                                 $?rest)
                       (name ?name))
         (not (operation logical
                         ?operation))
         =>
         (printout werror
                   "ERROR: logical group instruction has illegal operation: " ?operation "!" crlf
                   (print-message-about-offending-object ?name) crlf)
         (halt))

(deffacts lower::memory-operations
          (operations memory
                      load
                      store
                      push
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
                        (destination-register ?immediate-value)))

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
                        (destination-register ?destination)))
(defrule lower::illegal-memory-operation
         ?f <- (object (is-a list)
                       (contents memory
                                 ?operation
                                 $?)
                       (name ?name))
         (not (operation memory
                         ?operation))
         =>
         (printout werror
                   "ERROR: illegal memory operation " ?operation "!" crlf
                   (print-message-about-offending-object ?name) crlf)
         (halt))

(deffacts lower::arithmetic-operations
          (operations arithmetic
                      add
                      sub
                      mul
                      div
                      rem))

(defrule lower::generate-simple-macro-arithmetic-cmd
         "Generate the simple-macros automatically on startup"
         (declare (salience ?*priority:first*))
         (operation arithmetic
                    ?id)
         =>
         (assert (simple-macro 2 ?id -> arithmetic ?id)
                 (simple-macro 2 (sym-cat ?id i) -> arithmetic ?id immediate)))

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
                        (destination-register ?destination)
                        (source-register0 ?source)))

(defrule lower::construct-arithmetic-operation:immediate
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
                        (destination-register ?destination)
                        (source-register0 ?source)))
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
                        (destination-register ?dest)
                        (source-register0 ?src0)))
(deffacts lower::simple-operation-with-bitmask
          (operations simple-operation-with-bitmask
                      move
                      set))

(defrule lower::construct-simple-operation-with-bitmask
         ?f <- (object (is-a list)
                       (contents ?operation
                                 ?bitmask
                                 ?dest
                                 ?src0)
                       (name ?n)
                       (parent ?p))
         (object (is-a bitmask)
                 (name ?bitmask))
         (operation simple-operation-with-bitmask
                    ?operation)
         =>
         (unmake-instance ?f)
         (make-instance ?n of two-argument-instruction
                        (parent ?p)
                        (group ?operation)
                        (operation "")
                        (flags ?bitmask)
                        (destination-register ?dest)
                        (source-register0 ?src0)))

(deffacts lower::compare-operations
          (operations compare
                      ==
                      !=
                      <
                      <=
                      >
                      >=))
(deffacts lower::compare-translation-macros
          (simple-macro 2 eq -> ==)
          (simple-macro 2 eqi -> ==i)
          (simple-macro 2 neq -> !=)
          (simple-macro 2 neqi -> !=i)
          (simple-macro 2 lt -> <)
          (simple-macro 2 lti -> <i)
          (simple-macro 2 gt -> >)
          (simple-macro 2 gti -> >i)
          (simple-macro 2 le -> <=)
          (simple-macro 2 lei -> <=i)
          (simple-macro 2 ge -> >=)
          (simple-macro 2 gei -> >=i)
          (simple-macro 2 equals -> eq)
          (simple-macro 2 equals-imm -> eqi)
          (simple-macro 2 not-equals -> neq)
          (simple-macro 2 not-equals-imm -> neqi)
          (simple-macro 2 less-than -> lt)
          (simple-macro 2 less-than-imm -> lti)
          (simple-macro 2 greater-than -> gt)
          (simple-macro 2 greater-than-imm -> gti)
          (simple-macro 2 less-than-or-equal-to -> le)
          (simple-macro 2 less-than-or-equal-to-imm -> lei)
          (simple-macro 2 greater-than-or-equal-to -> ge)
          (simple-macro 2 greater-than-or-equal-to-imm -> gei))

(defrule lower::generate-compare-operation-simple-macro
         (declare (salience ?*priority:first*))
         (operation compare
                    ?id)
         =>
         (assert (simple-macro 2 ?id -> compare ?id)
                 (simple-macro 2 (sym-cat ?id i) -> compare ?id immediate)))

(deffacts lower::macros-with-constant-immediates
          (macro-with-constant-immediate (match ==0)
                                         (replacement eqi)
                                         (constant 0x0))
          (macro-with-constant-immediate (match !=0)
                                         (replacement neqi)
                                         (constant 0x0))
          (macro-with-constant-immediate (match <0)
                                         (replacement lti)
                                         (constant 0x0))
          (macro-with-constant-immediate (match >0)
                                         (replacement gti)
                                         (constant 0x0))
          (macro-with-constant-immediate (match >=0)
                                         (replacement gei)
                                         (constant 0x0))
          (macro-with-constant-immediate (match <=0)
                                         (replacement lei)
                                         (constant 0x0))
          (simple-macro 1 greater-than-zero -> >0)
          (simple-macro 1 less-than-zero -> <0)
          (simple-macro 1 greater-than-or-equal-to-zero -> >=0)
          (simple-macro 1 less-than-or-equal-to-zero -> <=0)
          (simple-macro 1 equal-to-zero -> ==0)
          (simple-macro 1 not-equal-to-zero -> !=0)
          (simple-macro 1 eqz -> equal-to-zero)
          (simple-macro 1 neqz -> not-equal-to-zero)
          (simple-macro 1 ltz -> less-than-zero)
          (simple-macro 1 gtz -> greater-than-zero)
          (simple-macro 1 gez -> greater-than-or-equal-to-zero)
          (simple-macro 1 lez -> less-than-or-equal-to-zero))

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
                        (destination-register ?dest)
                        (source-register0 ?src0)))

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
                        (destination-register ?dest)
                        (source-register0 ?src0)))

(deffacts lower::legal-complex-operations
          (operations complex
                      encoding)
          (operations encoding
                      decode
                      encode
                      bitset
                      bitunset))

(defrule lower::generate-encoding-simple-macros
         (declare (salience ?*priority:first*))
         (operation encoding
                    ?op)
         =>
         (assert (simple-macro 0 ?op -> complex encoding ?op)))

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
                        (destination-register ?r0)
                        (source-register0 ?r1)))

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
                        (destination-register ?r0)))

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
                        (destination-register ?r0)))

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
                        (destination-register ?r0)))

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
                        (destination-register ?r0)))
(deffacts lower::branch-simple-macros
          (simple-macro 1 branch-unconditional-immediate -> branch unconditional immediate)
          (simple-macro 1 bui -> branch-unconditional-immediate)
          (simple-macro 1 branch-conditional-immediate -> branch conditional immediate)
          (simple-macro 1 bci -> branch-conditional-immediate))
;------------------------------------------------------------------------------
; Macros
;------------------------------------------------------------------------------
(deffacts lower::zero-argument-simple-macros
          "Simple macros which take in no arguments inside the instruction itself"
          (simple-macro 0 nop -> swap r0 r0)
          (simple-macro 0 return -> pop ip))


(deffacts lower::simple-one-arg-macros
          (simple-macro 1 push -> push ?*bitmask32*)
          (simple-macro 1 pop -> pop ?*bitmask32*)
          (simple-macro 1 push16u -> push ?*bitmask16u*)
          (simple-macro 1 push16l -> push ?*bitmask16l*)
          ; TODO: merge the hand written pop16 rules into this fact set
          (simple-macro 1 pop16u -> pop ?*bitmask16u*)
          (simple-macro 1 pop16l -> pop ?*bitmask16l*)
          (simple-macro 1 pop32 -> pop ?*bitmask32*)
          (simple-macro 1 push32 -> push ?*bitmask32*))

(deffacts lower::load-store-fact-macros
          ; not sure if these are going to match correctly!
          ; they should since they are distinct!
          (simple-macro 1 direct-load32 -> direct-load ?*bitmask32*)
          (simple-macro 1 direct-store32 -> direct-store ?*bitmask32*)
          (simple-macro 1 indirect-load32 -> indirect-load ?*bitmask32*)
          (simple-macro 1 indirect-store32 -> indirect-store ?*bitmask32*)
          (simple-macro 0 indirect-store32 -> indirect-store32 0x00)
          (simple-macro 0 indirect-load32 -> indirect-load32 0x00)
          (simple-macro 0 direct-store32 -> direct-store32 0x00)
          (simple-macro 0 direct-load32 -> direct-load32 0x00)
          (simple-macro 1 dload32 -> direct-load32)
          (simple-macro 1 dstore32 -> direct-store32)
          (simple-macro 1 iload32 -> indirect-load32)
          (simple-macro 1 istore32 -> indirect-store32)
          (simple-macro 0 istore32 -> indirect-store32)
          (simple-macro 0 iload32 -> indirect-load32)
          (simple-macro 0 dstore32 -> direct-store32)
          (simple-macro 0 dload32 -> direct-load32)
          (simple-macro 1 direct-load16u -> direct-load ?*bitmask16u*)
          (simple-macro 1 direct-store16u -> direct-store ?*bitmask16u*)
          (simple-macro 1 indirect-load16u -> indirect-load ?*bitmask16u*)
          (simple-macro 1 indirect-store16u -> indirect-store ?*bitmask16u*)
          (simple-macro 0 indirect-store16u -> indirect-store16u 0x00)
          (simple-macro 0 indirect-load16u -> indirect-load16u 0x00)
          (simple-macro 0 direct-store16u -> direct-store16u 0x00)
          (simple-macro 0 direct-load16u -> direct-load16u 0x00)
          (simple-macro 1 dload16u -> direct-load16u)
          (simple-macro 1 dstore16u -> direct-store16u)
          (simple-macro 1 iload16u -> indirect-load16u)
          (simple-macro 1 istore16u -> indirect-store16u)
          (simple-macro 0 istore16u -> indirect-store16u)
          (simple-macro 0 iload16u -> indirect-load16u)
          (simple-macro 0 dstore16u -> direct-store16u)
          (simple-macro 0 dload16u -> direct-load16u)
          (simple-macro 1 direct-load16l -> direct-load ?*bitmask16l*)
          (simple-macro 1 direct-store16l -> direct-store ?*bitmask16l*)
          (simple-macro 1 indirect-load16l -> indirect-load ?*bitmask16l*)
          (simple-macro 1 indirect-store16l -> indirect-store ?*bitmask16l*)
          (simple-macro 0 indirect-store16l -> indirect-store16l 0x00)
          (simple-macro 0 indirect-load16l -> indirect-load16l 0x00)
          (simple-macro 0 direct-store16l -> direct-store16l 0x00)
          (simple-macro 0 direct-load16l -> direct-load16l 0x00)
          (simple-macro 1 dload16l -> direct-load16l)
          (simple-macro 1 dstore16l -> direct-store16l)
          (simple-macro 1 iload16l -> indirect-load16l)
          (simple-macro 1 istore16l -> indirect-store16l)
          (simple-macro 0 istore16l -> indirect-store16l)
          (simple-macro 0 iload16l -> indirect-load16l)
          (simple-macro 0 dstore16l -> direct-store16l)
          (simple-macro 0 dload16l -> direct-load16l)
          (simple-macro 1 direct-load24u -> direct-load ?*bitmask24u*)
          (simple-macro 1 direct-store24u -> direct-store ?*bitmask24u*)
          (simple-macro 1 indirect-load24u -> indirect-load ?*bitmask24u*)
          (simple-macro 1 indirect-store24u -> indirect-store ?*bitmask24u*)
          (simple-macro 0 indirect-store24u -> indirect-store24u 0x00)
          (simple-macro 0 indirect-load24u -> indirect-load24u 0x00)
          (simple-macro 0 direct-store24u -> direct-store24u 0x00)
          (simple-macro 0 direct-load24u -> direct-load24u 0x00)
          (simple-macro 1 dload24u -> direct-load24u)
          (simple-macro 1 dstore24u -> direct-store24u)
          (simple-macro 1 iload24u -> indirect-load24u)
          (simple-macro 1 istore24u -> indirect-store24u)
          (simple-macro 0 istore24u -> indirect-store24u)
          (simple-macro 0 iload24u -> indirect-load24u)
          (simple-macro 0 dstore24u -> direct-store24u)
          (simple-macro 0 dload24u -> direct-load24u)
          (simple-macro 1 direct-load24l -> direct-load ?*bitmask24l*)
          (simple-macro 1 direct-store24l -> direct-store ?*bitmask24l*)
          (simple-macro 1 indirect-load24l -> indirect-load ?*bitmask24l*)
          (simple-macro 1 indirect-store24l -> indirect-store ?*bitmask24l*)
          (simple-macro 0 indirect-store24l -> indirect-store24l 0x00)
          (simple-macro 0 indirect-load24l -> indirect-load24l 0x00)
          (simple-macro 0 direct-store24l -> direct-store24l 0x00)
          (simple-macro 0 direct-load24l -> direct-load24l 0x00)
          (simple-macro 1 dload24l -> direct-load24l)
          (simple-macro 1 dstore24l -> direct-store24l)
          (simple-macro 1 iload24l -> indirect-load24l)
          (simple-macro 1 istore24l -> indirect-store24l)
          (simple-macro 0 istore24l -> indirect-store24l)
          (simple-macro 0 iload24l -> indirect-load24l)
          (simple-macro 0 dstore24l -> direct-store24l)
          (simple-macro 0 dload24l -> direct-load24l))

(defrule lower::handle-direct-load-macro
         ?f <- (object (is-a list)
                       (contents direct-load
                                 ?bitmask
                                 ?offset))
         =>
         (modify-instance ?f
                          (contents load ?bitmask direct ?offset)))

(defrule lower::handle-direct-store-macro
         ?f <- (object (is-a list)
                       (contents direct-store
                                 ?bitmask
                                 ?offset))
         =>
         (modify-instance ?f
                          (contents store ?bitmask direct ?offset)))


(defrule lower::handle-indirect-load-macro
         ?f <- (object (is-a list)
                       (contents indirect-load
                                 ?bitmask
                                 ?offset))
         =>
         (modify-instance ?f
                          (contents load ?bitmask indirect ?offset)))

(defrule lower::handle-indirect-store-macro
         ?f <- (object (is-a list)
                       (contents indirect-store
                                 ?bitmask
                                 ?offset))
         =>
         (modify-instance ?f
                          (contents store ?bitmask indirect ?offset)))
(deffacts lower::base-memory-simple-macros
          (simple-macro 2 push -> memory push)
          (simple-macro 2 pop -> memory pop)
          (simple-macro 3 load -> memory load)
          (simple-macro 3 store -> memory store))

(deffacts lower::two-argument-simple-macros
          (simple-macro 2 set32 -> set ?*bitmask32*)
          (simple-macro 2 set24 -> set24l)
          (simple-macro 2 set24l -> set ?*bitmask24l*)
          (simple-macro 2 set24u -> set ?*bitmask24u*)
          (simple-macro 2 set8 -> set8ll)
          (simple-macro 2 set8ll -> set ?*bitmask8ll*)
          (simple-macro 2 set8lu -> set ?*bitmask8lu*)
          (simple-macro 2 set8ul -> set ?*bitmask8ul*)
          (simple-macro 2 set8uu -> set ?*bitmask8uu*)
          (simple-macro 2 set16 -> set16l)
          (simple-macro 2 set16l -> set ?*bitmask16l*)
          (simple-macro 2 set16u -> set ?*bitmask16u*))

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

(deffacts lower::macros-with-constant-immediates
          (macro-with-constant-immediate (match decrement-value)
                                         (replacement subi)
                                         (constant 0x1))
          (macro-with-constant-immediate (match increment-value)
                                         (replacement addi)
                                         (constant 0x1))
          (macro-with-constant-immediate (match double-value)
                                         (replacement muli)
                                         (constant 0x2))
          (macro-with-constant-immediate (match halve-value)
                                         (replacement divi)
                                         (constant 0x2))
          (macro-with-constant-immediate (match mod2-value)
                                         (replacement remi)
                                         (constant 0x2))
          (macro-with-constant-immediate (match clear-register)
                                         (replacement set ?*bitmask0*)
                                         (constant 0x00000000))
          (simple-macro 1 incr -> increment)
          (simple-macro 1 decr -> decrement)
          (simple-macro 1 1+ -> increment)
          (simple-macro 1 1- -> decrement)
          (simple-macro 1 increment -> increment-value)
          (simple-macro 1 decrement -> decrement-value)
          (simple-macro 1 2* -> double)
          (simple-macro 1 double -> double-value)
          (simple-macro 1 2/ -> halve)
          (simple-macro 1 halve -> halve-value)
          (simple-macro 1 2% -> mod2)
          (simple-macro 1 mod2 -> mod2-value)
          (simple-macro 1 clear -> clear-register)
          (simple-macro 1 clr -> clear))

(defrule lower::translate-constant-immediate-macro
         "Macros like increment and decrement are really another operation with a constant second argument.
         Instead of having to write a new rule for each macro I want, it is easier to just declare facts which
         make the code far more flexible in generation (it also allows runtime generation of new macros!)"
         ?f <- (object (is-a list)
                       (contents ?title
                                 ?register))
         (macro-with-constant-immediate (match ?title)
                                        (replacement $?replacement)
                                        (constant $?constant))
         =>
         (modify-instance ?f
                          (contents ?replacement
                                    ?register
                                    ?constant)))

(deffacts lower::base-registers
          (base-register r0)
          (base-register r1)
          (base-register r2)
          (base-register r3)
          (base-register r4)
          (base-register r5)
          (base-register r6)
          (base-register r7)
          (base-register r8)
          (base-register r9)
          (base-register r10)
          (base-register r11)
          (base-register r12)
          (base-register r13)
          (base-register r14)
          (base-register r15))

(defrule lower::parse-hardcoded-alias-facts:front
         (declare (salience ?*priority:two*))
         ?f <- (alias ?a <- ?b <- $?rest)
         =>
         (retract ?f)
         (assert (alias ?a <- ?b)
                 (alias ?b <- $?rest)))

(defrule lower::retract-hardcoded-alias-facts
         (declare (salience ?*priority:two*))
         ?f <- (alias ? <-)
         =>
         (retract ?f))

(defrule lower::parse-hardcoded-alias-facts
         (declare (salience ?*priority:two*))
         ?f <- (alias ?a <- ?b)
         =>
         (retract ?f)
         (mk-list FALSE
                  alias
                  ?a
                  as
                  ?b))

(deffacts lower::aliases
          (alias r15 <- instruction-pointer <- ip)
          (alias instruction-pointer <- program-counter <- pc)
          (alias r14 <- stack-pointer <- sp)
          (alias r13 <- condition-register <- cond)
          (alias r12 <- address-register <- addr)
          (alias r11 <- value-register <- value)
          (alias r10 <- mask-register <- mask)
          (alias r9 <- shift-register <- shift)
          (alias r9 <- field-register <- field))


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

(deffacts lower::move-macros
          "common action macros!"
          (simple-macro 2 move32 -> move ?*bitmask32*)
          (simple-macro 2 move16u -> move ?*bitmask16u*)
          (simple-macro 2 move16l -> move ?*bitmask16l*)
          (simple-macro 2 move16 -> move16l)
          (simple-macro 2 move8ll -> move ?*bitmask8ll*)
          (simple-macro 2 move8lu -> move ?*bitmask8lu*)
          (simple-macro 2 move8ul -> move ?*bitmask8ul*)
          (simple-macro 2 move8uu -> move ?*bitmask8uu*)
          (simple-macro 2 move8 -> move8ll)
          (simple-macro 2 move24u -> move ?*bitmask24u*)
          (simple-macro 2 move24l -> move ?*bitmask24l*)
          (simple-macro 2 move24 -> move24l)
          (simple-macro 2 copy -> move32)
          (simple-macro 2 copy-lower16 -> move16l)
          (simple-macro 2 copy16 -> move16)
          (simple-macro 2 copy-upper16 -> move16u)
          (simple-macro 2 copy-upper24 -> move24u)
          (simple-macro 2 copy-lower24 -> move24l)
          (simple-macro 2 cast16 -> move16)
          (simple-macro 2 cast8 -> move8)
          (simple-macro 2 cast24 -> move24)
          (simple-macro 2 make-data-address -> move24))

(deffunction lower::mk-use-registers-block
             (?name ?p ?registers $?contents)
             (mk-list-with-title ?name
                                 ?p
                                 use-registers
                                 (mk-list ?name
                                          ?registers)
                                 $?contents))



;------------------------------------------------------------------------------
; Simple macro declaration inside a program!
;------------------------------------------------------------------------------
(defrule lower::def-simple-macro
         (declare (salience ?*priority:first*))
         ?f <- (object (is-a list)
                       (contents defsimplemacro
                                 ?title
                                 ?count
                                 ?replacement))
         ?f2 <- (object (is-a list)
                        (name ?replacement)
                        (contents replace-with
                                  $?symbols))
         =>
         (unmake-instance ?f
                          ?f2)
         (assert (simple-macro ?count ?title -> $?symbols)))

(defrule lower::defunc-basic
         ?f <- (object (is-a list)
                       (contents defunc-basic
                                 ?title
                                 $?body)
                       (name ?name))
         =>
         (modify-instance ?f
                          (contents label
                                    ?title
                                    $?body
                                    (mk-list ?name
                                             return))))

(deffacts lower::assembler-temporary-declaration
          (alias r8 <- assembler-temporary <- at)
          (alias assembler-temporary <- asm-temp)
          (alias assembler-temporary <- atmp))

(defrule lower::loop-macro-default
         "Construct a sub section with a body and a branch back to the target label!"
         ?f <- (object (is-a list)
                       (contents loop
                                 ?loop-title
                                 $?body)
                       (name ?name))
         =>
         (modify-instance ?f
                          (contents label
                                    ?loop-title
                                    $?body
                                    (mk-list ?name
                                             bui
                                             ?loop-title))))


(defrule lower::loop-macro-gensym*
         "Generate a loop body without caring about the title of the loop, this means that you can only restart the loop at the bottom!"
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents loop
                                 ?loop-title
                                 $?body)
                       (name ?name))
         (object (is-a list)
                 (name ?loop-title)
                 (contents gensym*))
         =>
         ; delete it
         (unmake-instance ?loop-title)
         (modify-instance ?f
                          (contents loop
                                    (gensym*)
                                    $?body)))
(defrule lower::parse-ascii-constant-immediate-check
         (declare (salience ?*priority:first*))
         ?f <- (ascii-check-macro ?title ?value)
         =>
         (retract ?f)
         (assert (macro-with-constant-immediate (match ?title)
                                                (replacement eqi)
                                                (constant ?value))))
(deffacts lower::ascii-check-macros
          (ascii-check-macro is-left-paren 0x28)
          (ascii-check-macro is-right-paren 0x29)
          (ascii-check-macro is-space 0x20)
          (ascii-check-macro is-new-line 0x0a)
          (ascii-check-macro is-null-char 0x00)
          (simple-macro 1 is-lparen -> is-left-paren)
          (simple-macro 1 is-rparent -> is-right-paren)
          (simple-macro 1 is-open-paren -> is-lparen)
          (simple-macro 1 is-close-parent -> is-rparen)
          (simple-macro 1 lparenp -> is-lparen)
          (simple-macro 1 rparenp -> is-rparen)
          (simple-macro 1 open-parenp -> is-open-paren)
          (simple-macro 1 close-parenp -> is-close-paren)
          (simple-macro 1 is-null -> is-null-char))

(defrule lower::parse-multiply-accumulate-operation
         ?f <- (object (is-a list)
                       (contents multiply-accumulate
                                 ?dest
                                 ?src0
                                 ?src1)
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?name
                       ?p
                       (mk-list ?name
                                mul
                                ?dest
                                ?src0)
                       (mk-list ?name
                                add
                                ?dest
                                ?src1)))
(deffacts lower::multiply-accumulate-macros
          (simple-macro 3 mac -> multiply-accumulate))


(deffacts lower::lower-eight-registers:parameter-passing-conventions
          (alias r7 <- result-register <- result)
          (alias r6 <- temporary-register0 <- temporary0 <- temp0)
          (alias r5 <- temporary-register1 <- temporary1 <- temp1)
          (alias r4 <- temporary-register2 <- temporary2 <- temp2)
          (alias r3 <- temporary-register3 <- temporary3 <- temp3)
          (alias r2 <- temporary-register4 <- temporary4 <- temp4)
          (alias r1 <- temporary-register5 <- temporary5 <- temp5)
          (alias r0 <- temporary-register6 <- temporary6 <- temp6))

(deffacts lower::special-set-macros
          (simple-macro 1 set-address-register -> set32 addr)
          (simple-macro 1 set-addr -> set-address-register)
          (simple-macro 1 move-to-address-register -> move32 addr)
          (simple-macro 1 move-to-addr -> move-to-address-register)
          (simple-macro 1 mtaddr -> move-to-addr))


; Pass arguments through registers! Four registers for arguments with three
; temporaries.

(deffacts lower::load-argument-macros
          (alias temp0 <- argument-register0 <- arg0)
          (alias temp1 <- argument-register1 <- arg1)
          (alias temp2 <- argument-register2 <- arg2)
          (alias temp3 <- argument-register3 <- arg3 <- rest-param)
          (alias temp4 <- local0)
          (alias temp5 <- local1)
          (alias temp6 <- local2))

;-----------------------------------------------------------------------------
; Bootstrap rules, makes it possible to manipulate the set of simple macros
; easily at runtime.
;-----------------------------------------------------------------------------
(defrule lower::runtime-generate-simple-macros
         ?f <- (object (is-a list)
                       (contents simple-macro
                                 ?count
                                 ?name
                                 ->
                                 $?replacement))
         =>
         (unmake-instance ?f)
         (assert (simple-macro ?count ?name -> $?replacement)))

(defrule lower::runtime-ungenerate-simple-macros
         ?f <- (object (is-a list)
                       (contents delete
                                 simple-macro
                                 ?count
                                 ?name))
         ?f2 <- (simple-macro ?count ?name -> $?)
         =>
         (retract ?f2)
         (unmake-instance ?f))
(defrule lower::runtime-ungenerate-simple-macros:no-exist
         ?f <- (object (is-a list)
                       (contents delete
                                 simple-macro
                                 ?count
                                 ?name))
         (not (simple-macro ?count ?name -> $?))
         =>
         (printout werror
                   "ERROR: couldn't delete simple-macro " ?count " " ?name " because it doesn't exist!" crlf)
         (halt))

(defrule lower::runtime-make-alias
         ?f <- (object (is-a list)
                       (contents aliases
                                 $?aliases))
         =>
         (unmake-instance ?f)
         (assert (alias $?aliases)))

(defrule lower::make-basic-register-declaration
         ?f <- (object (is-a list)
                       (contents basic-register
                                 ?title))
         =>
         (unmake-instance ?f)
         (assert (basic-register ?title)))
(defrule lower::mass-basic-register-declarations
         ?f <- (object (is-a list)
                       (contents basic-registers
                                 $?registers))
         =>
         (unmake-instance ?f)
         (progn$ (?register $?registers)
                 (assert (basic-register ?register))))
