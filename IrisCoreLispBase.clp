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

(defrule lower::parse-increment-macro
         ?f <- (object (is-a list)
                       (contents incr
                                 ?destination
                                 ?register))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f
                          (contents addi
                                    ?destination
                                    ?register
                                    1)))

(defrule lower::parse-decrement-macro
         ?f <- (object (is-a list)
                       (contents decr
                                 ?destination
                                 ?register))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f
                          (contents subi
                                    ?destination
                                    ?register
                                    1)))
(defrule lower::parse-double-macro
         ?f <- (object (is-a list)
                       (contents double
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents add
                                    ?destination
                                    ?register
                                    ?register)))

(defrule lower::parse-triple-macro
         ?f <- (object (is-a list)
                       (contents triple
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents muli
                                    ?destination
                                    ?register
                                    3)))

(defrule lower::parse-halve-macro
         ?f <- (object (is-a list)
                       (contents halve
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents divi
                                    ?destination
                                    ?register
                                    2)))
(defrule lower::parse-square-macro
         ?f <- (object (is-a list)
                       (contents square
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents mul
                                    ?destination
                                    ?register
                                    ?register)))
(defrule lower::parse-cube-macro
         ?f <- (object (is-a list)
                       (contents cube
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents mul
                                    ?destination
                                    ?register
                                    ?register
                                    ?register)))


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






(defrule lower::memswap-data
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents memswap
                                 ?register0
                                 ?register1)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                ld
                                iv0
                                ?register0)
                       (mk-list ?n
                                ld
                                iv1
                                ?register1)
                       (mk-list ?n
                                st
                                ?register1
                                iv0)
                       (mk-list ?n
                                st
                                ?register0
                                iv1)))

(defrule lower::terminate
         ?f <- (object (is-a list)
                       (contents terminate)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                set
                                iv0
                                0x0000)
                       (mk-list ?n
                                stio
                                iv0
                                iv0)))

(defrule lower::putc
         ?f <- (object (is-a list)
                       (contents putc
                                 ?register)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                set
                                iv0
                                0x0001)
                       (mk-list ?n
                                stio
                                iv0
                                ?register)))

(defrule lower::getc
         ?f <- (object (is-a list)
                       (contents getc
                                 ?register)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                set
                                iv0
                                0x0001)
                       (mk-list ?n
                                ldio
                                ?register
                                iv0)))


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

(defrule lower::nop-macro
         ?f <- (object (is-a list)
                       (contents nop))
         =>
         (modify-instance ?f
                          (contents addi
                                    iv0
                                    iv0
                                    0)))
(defrule lower::zero-register-macro
         ?f <- (object (is-a list)
                       (contents zero|clear
                                 ?register))
         =>
         (modify-instance ?f
                          (contents set
                                    ?register
                                    0x0000))
