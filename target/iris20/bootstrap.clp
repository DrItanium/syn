(deffunction target-architecture
             ()
             (funcall current-target-architecture))
(deffunction current-target-architecture
             ()
             iris20)
(deffunction enum->int
             (?symbol ?collection)
             (symbol->zero-index ?symbol
                                 ?collection))
(deffunction section-descriptor->int
             (?symbol)
             (enum->int ?symbol
                        ?*iris20-enumSectionType*))
(deffunction operation->int
             (?id)
             (enum->int ?id
                        ?*iris20-enumOperation*))

(defmethod iris20-encode-Operation
  ((?value INTEGER)
   (?field SYMBOL))
  (iris20-encode-Operation ?value
                           (operation->int ?field)))
(defgeneric encode)
(defgeneric decode)


(deffunction generic-encode-decode-operation
             (?action ?section $?args)
             (funcall (sym-cat (target-architecture)
                               -
                               ?action
                               -
                               ?section)
                      (expand$ ?args)))

(defmethod encode
  ((?section SYMBOL)
   (?value INTEGER)
   (?field INTEGER
           SYMBOL))
  (generic-encode-decode-operation encode
                                   ?section
                                   ?value
                                   ?field))

(defmethod decode
  ((?section SYMBOL)
   (?value INTEGER))
  (generic-encode-decode-operation decode
                                   ?section
                                   ?value))

(defgeneric encode-register)
(defgeneric decode-register)

(defmethod encode-register
  ((?index INTEGER)
   (?section INTEGER))
  (encode SectionIndex
          (encode SectionDescriptor
                  0
                  ?section)
          ?index))



(defmethod encode-register
  ((?index INTEGER)
   (?section SYMBOL))
  (encode-register ?index
                   (section-descriptor->int ?section)))

(defmethod decode-register
  ((?register INTEGER))
  (create$ (decode SectionDescriptor
                   ?register)
           (decode SectionIndex
                   ?register)))

(deffunction stack
             (?i)
             (encode-register ?i
                              Stack))
(deffunction register
             (?i)
             (encode-register ?i
                              Register))
(deffunction memory
             (?i)
             (encode-register ?i
                              Memory))
(defmessage-handler INTEGER encode primary
                    ()
                    ?self)
(defglobal MAIN
           ?*addr-max* = (hex->int 0x03FFFFFF)
           ?*space-size* = (hex->int 0x00FFFFFF)
           ?*half-space* = (div ?*space-size* 2)
           ?*stack-bottom* = ?*addr-max*
           ?*stack-top* = (- ?*stack-bottom*
                             ?*half-space*)
           ?*call-stack-bottom* = (- ?*stack-top* 1)
           ?*call-stack-top* = (- ?*call-stack-bottom*
                                  ?*half-space*)
           ?*memory1-end* = (- ?*stack-top* 1)
           ?*memory1-start* = (- ?*memory1-end*
                                 ?*space-size*)
           ?*memory0-end* = (- ?*memory1-start* 1)
           ?*memory0-start* = (- ?*memory0-end*
                                 ?*space-size*)
           ?*addr-table-end* = (- ?*memory0-start* 1)
           ?*addr-table-begin* = (- ?*addr-table-end*
                                    (hex->int 0xFFFF))
           ?*code-end* = (- ?*addr-table-begin* 1)
           ?*code-start* = 0
           ?*register-count* = 64
           ?*register-ip* = (- ?*register-count* 1)
           ?*register-lr* = (- ?*register-count* 2)
           ?*register-sp* = (- ?*register-count* 3)
           ?*register-stack-pointer-top* = (- ?*register-count* 4)
           ?*register-stack-pointer-bottom* = (- ?*register-count* 5)
           ?*register-memory-space0-start* = (- ?*register-count* 6)
           ?*register-memory-space0-end* = (- ?*register-count* 7)
           ?*register-memory-space1-start* = (- ?*register-count* 8)
           ?*register-memory-space1-end* = (- ?*register-count* 9)
           ?*register-call-stack-bottom* = (- ?*register-count* 10)
           ?*register-call-stack-top* = (- ?*register-count* 11)
           ?*register-code-end* = (- ?*register-count* 12)
           ?*register-code-start* = (- ?*register-count* 13)
           ?*register-call-stack-pointer* = (- ?*register-count* 14)
           ?*register-address-table-base* = (- ?*register-count* 15)
           ?*register-address-table-pointer* = (- ?*register-count* 16)
           ?*register-temp0* = (- ?*register-count* 17)
           ?*register-temp1* = (- ?*register-count* 18)
           ?*register-temp2* = (- ?*register-count* 19)
           ?*register-temp3* = (- ?*register-count* 20)
           ?*register-temp4* = (- ?*register-count* 21)
           ?*register-temp5* = (- ?*register-count* 22)
           ?*register-temp6* = (- ?*register-count* 23))

(deffunction link-register () ?*register-lr*)
(deffunction instruction-pointer () ?*register-ip*)
(deffunction stack-pointer () ?*register-sp*)

(deffunction has-register-prefix
             (?title)
             (str-index "register-"
                        ?title))
(deffunction build-register-operation
             (?title)
             (buildf "(deffunction register:%s () ?*%s*)"
                     (sub-string (+ (str-length "register-")
                                    1)
                                 (str-length ?title)
                                 ?title)
                     ?title))

(map build-register-operation
     (expand$ (filter has-register-prefix
                      (expand$ (get-defglobal-list MAIN)))))
(loop-for-count (?i 0 63) do
                (buildf "(deffunction register:%s () %d)"
                        (sym-cat r
                                 ?i)
                        ?i))

(deffunction register:
             (?name)
             (funcall (sym-cat register: ?name)))


(defgeneric output-bytes-to-router)
(defmethod output-bytes-to-router
  ((?bytes MULTIFIELD)
   (?router SYMBOL))
  (progn$ (?byte ?bytes)
          (put-char ?router
                    ?byte)))
(defmethod output-bytes-to-router
  ((?bytes MULTIFIELD))
  (output-bytes-to-router ?bytes
                          t))


(defgeneric return-from-register)
(defgeneric make:atom)
(defgeneric make:molecule)
(defgeneric make:word-container)
(defgeneric return-from-stack)



(defclass instruction
  (is-a USER)
  (role abstract)
  (slot operation
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot dest
        (type INTEGER)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot src0
        (type INTEGER)
        (visibility public)
        (storage local))
  (slot src1
        (type INTEGER)
        (visibility public)
        (storage local))
  (slot immediate
        (type INTEGER
              INSTANCE)
        (allowed-classes label)
        (storage local)
        (visibility public)
        (default-dynamic 0))
  (slot operation-suffix
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default Operation))
  (slot dest-suffix
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot src0-suffix
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot src1-suffix
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler encode primary))
(defclass atom
  (is-a instruction)
  (role concrete)
  (pattern-match reactive)
  (slot dest-suffix
        (source composite)
        (default Destination))
  (slot src0-suffix
        (source composite)
        (default Source0))
  (slot src1-suffix
        (source composite)
        (default Source1))
  (message-handler encode primary))
(defclass molecule
  (is-a instruction)
  (role concrete)
  (pattern-match reactive)
  (slot dest-suffix
        (source composite)
        (default MoleculeDestination))
  (slot src0-suffix
        (source composite)
        (default MoleculeSource0))
  (slot src1-suffix
        (source composite)
        (default MoleculeSource1))
  (message-handler encode primary))

(defmethod make:molecule
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (make-instance of molecule
                 (operation ?operation)
                 (dest ?dest)
                 (src0 ?src0)
                 (src1 ?src1)))
(defmethod make:molecule
  ((?operation SYMBOL)
   (?dest INTEGER))
  (make:molecule ?operation
                 ?dest
                 0
                 0))
(defmethod make:molecule
  ((?operation SYMBOL))
  (make:molecule ?operation
                 0))

(defmethod make:molecule
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER))
  (make:molecule ?operation
                 ?dest
                 ?src0
                 0))

(defmethod make:atom
  ((?operation SYMBOL))
  (make:atom ?operation
             0))

(defmethod make:atom
  ((?operation SYMBOL)
   (?destination INTEGER))
  (make:atom ?operation
             ?destination
             0))

(defmethod make:atom
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER))
  (make:atom ?operation
             ?dest
             ?src0
             0))

(defmethod make:atom
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (make-instance of atom
                 (operation ?operation)
                 (dest ?dest)
                 (src0 ?src0)
                 (src1 ?src1)))


(defclass word-container
  (is-a USER)
  (multislot contents
             (storage local)
             (visibility public)
             (default ?NONE))
  (message-handler encode primary))
(defclass label
  (is-a USER)
  (slot title
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot address
        (type SYMBOL
              INTEGER)
        (allowed-symbols FALSE)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default-dynamic FALSE)))

(defgeneric .label)
(defmethod .label
  ((?title SYMBOL
           (not (instance-existp (symbol-to-instance-name ?current-argument)))))
  (make-instance ?title of label
                 (title ?title)))
(defmethod .label
  ((?title SYMBOL
           (instance-existp (symbol-to-instance-name ?current-argument))))
  (symbol-to-instance-name ?title))

(defmethod make:word-container
  ((?first INTEGER
           atom)
   (?second INTEGER
            atom))
  (make-instance of word-container
                 (contents ?first
                           ?second)))
(defmethod make:word-container
  ((?wide-instruction INTEGER
                      molecule))
  (make-instance of word-container
                 (contents ?wide-instruction)))
(defmethod encode
  ((?thing molecule
           atom))
  (send ?thing
        encode))
(defmethod encode
  ((?first atom)
   (?second atom))
  (encode SecondAtom
          (encode FirstAtom
                  0
                  (encode ?first))
          (encode ?second)))
(defmessage-handler word-container encode primary
                    ()
                    (encode MoleculeContainsOneInstruction
                            (encode (expand$ ?self:contents))
                            (switch (length$ ?self:contents)
                                    (case 1 then 1)
                                    (case 2 then 0)
                                    (default (format werror
                                                     "ERROR: word-container contains %d instructions!%n"
                                                     (length$ ?self:contents))
                                             (create$)))))

(defmethod return-instruction
  ((?register INTEGER))
  (make:atom BranchUnconditionalRegister
             ?register))
(defmethod return-from-stack
  ((?sp INTEGER))
  (return-instruction (stack ?sp)))
(defmethod return-from-stack
  ()
  (return-from-stack (stack-pointer)))
(deffunction return-from-memory
             (?r)
             (return-instruction (memory ?r)))
(deffunction return-to-register
             (?r)
             (return-instruction (register ?r)))
(deffunction return-to-link-register
             ()
             (return-to-register (link-register)))
(defgeneric number-of-args)
(defgeneric operation-to-call)
(deffunction make:special-defmethod
             (?title ?symbol ?value)
             (buildf "(defmethod %s
                        ((?a SYMBOL
                             (not (neq ?current-argument
                                       %s
                                       %s))))
                        %s)"
                     ?title
                     ?symbol
                     (lowcase ?symbol)
                     (str-cat ?value)))
(deffunction immediatep
             (?symbol)
             (has-suffix ?symbol
                         Immediate))
(deffunction make:operation-to-call
             (?symbol ?func)
             (make:special-defmethod operation-to-call
                                     ?symbol
                                     ?func))

(deffunction make:arg-count-function
             (?op ?count)
             (make:special-defmethod number-of-args
                                     ?op
                                     ?count))

(deffunction setoperationp
             (?symbol)
             (has-prefix ?symbol
                         Set))
(deffunction build-generic
             (?title)
             (buildf "(defgeneric %s)"
                     ?title))
(deffunction make-operation-title
             (?title)
             (sym-cat op:
                      (lowcase ?title)))

(deffunction make-stack-operation-title
             (?title)
             (sym-cat (make-operation-title ?title)
                      :stack))

(deffunction build-specific-operation-three-arg
             (?operation)
             (build-generic (bind ?title
                                  (make-operation-title ?operation)))
             (build-generic (bind ?stack-title
                                  (make-stack-operation-title ?operation)))
             (make:arg-count-function ?operation
                                      3)
             (make:operation-to-call ?operation
                                     ?title)
             (buildf "(defmethod %s ((?dest INTEGER) (?src0 INTEGER) (?src1 INTEGER)) (make:atom %s ?dest ?src0 ?src1))"
                     ?title
                     ?operation)
             (buildf "(defmethod %s ((?dest INTEGER) (?src0 INTEGER) (?src1 INTEGER)) (%s (stack ?dest) (stack ?src0) (stack ?src1) FALSE))"
                     ?stack-title
                     ?title)
             (buildf "(defmethod %s ((?stack INTEGER)) (%s ?stack ?stack ?stack))"
                     ?stack-title
                     ?stack-title))

(deffunction build-specific-operation-two-arg
             (?operation)
             (build-generic (bind ?title
                                  (make-operation-title ?operation)))
             (build-generic (bind ?stack-title
                                  (make-stack-operation-title ?operation)))
             (make:arg-count-function ?operation
                                      2)
             (make:operation-to-call ?operation
                                     ?title)
             (buildf "(defmethod %s ((?dest INTEGER) (?src0 INTEGER)) (make:atom %s ?dest ?src0))"
                     ?title
                     ?operation)
             (buildf "(defmethod %s ((?dest INTEGER) (?src0 INTEGER)) (%s (stack ?dest) (stack ?src0)))"
                     ?stack-title
                     ?title)
             (buildf "(defmethod %s ((?stack INTEGER)) (%s ?stack ?stack))"
                     ?stack-title
                     ?stack-title))

(deffunction build-specific-operation-one-arg
             (?operation)
             (build-generic (bind ?title
                                  (make-operation-title ?operation)))
             (build-generic (bind ?stack-title
                                  (make-stack-operation-title ?operation)))
             (make:arg-count-function ?operation
                                      1)
             (make:operation-to-call ?operation
                                     ?title)
             (buildf "(defmethod %s ((?dest INTEGER)) (make:atom %s ?dest))"
                     ?title
                     ?operation)
             (buildf "(defmethod %s ((?dest INTEGER)) (%s (stack ?dest)))"
                     ?stack-title
                     ?title))
(defclass branch-immediate-atom
  (is-a atom)
  (slot immediate
        (source composite)
        (default ?NONE))
  (message-handler encode primary))
(deffunction encode-branch-operation
             (?operation-suffix ?operation ?imm-suffix ?immediate ?dest-suffix ?dest)
             (encode ?operation-suffix
                     (encode ?imm-suffix
                             (if (= (number-of-args ?operation)
                                    2) then
                               (encode ?dest-suffix
                                       0
                                       ?dest)
                               else
                               0)
                             ?immediate)
                     ?operation))
(defmessage-handler branch-immediate-atom encode primary
                    ()
                    (encode-branch-operation ?self:operation-suffix
                                             ?self:operation
                                             Immediate
                                             ?self:immediate
                                             Destination
                                             ?self:dest))


(defclass branch-immediate-molecule
  (is-a molecule)
  (slot width
        (type INTEGER)
        (allowed-integers 32
                          48)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot immediate
        (source composite)
        (default ?NONE))
  (message-handler encode primary))
(defmessage-handler branch-immediate-molecule encode primary
                    ()
                    (encode-branch-operation ?self:operation-suffix
                                             ?self:operation
                                             (sym-cat Immediate
                                                      ?self:width)
                                             ?self:immediate
                                             MoleculeDestination
                                             ?self:dest))

(deffunction make:link-version
             (?op ?link)
             (if ?link then (sym-cat ?op Link) else ?op))
(deffunction branch-unconditional-immediate
             (?address ?link)
             (make-instance of branch-immediate-atom
                            (operation (make:link-version BranchUnconditionalImmediate
                                                          ?link))
                            (dest 0)
                            (immediate ?address)))
(deffunction branch-conditional-immediate
             (?dest ?address ?check-false ?link)
             (bind ?base-op
                   (if ?check-false then
                     BranchConditionalFalseImmediate
                     else
                     BranchConditionalTrueImmediate))
             (make-instance of branch-immediate-atom
                            (dest ?dest)
                            (operation (make:link-version ?base-op
                                                          ?link))
                            (immediate ?address)))
(deffunction branch-unconditional-immediate32
             (?address ?link)
             (make-instance of branch-immediate-molecule
                            (width 32)
                            (operation (make:link-version BranchUnconditionalImmediate32
                                                          ?link))
                            (dest 0)
                            (immediate ?address)))

(deffunction branch-unconditional-immediate48
             (?address ?link)
             (make-instance of branch-immediate-molecule
                            (width 48)
                            (operation (make:link-version BranchUnconditionalImmediate48
                                                          ?link))
                            (dest 0)
                            (immediate ?address)))

(deffunction branch-conditional-immediate-molecule
             (?dest ?address ?check-false ?link ?width)
             (bind ?base-op
                   (make:link-version (sym-cat BranchConditional
                                               (if ?check-false then
                                                 False
                                                 else
                                                 True)
                                               Immediate
                                               ?width)
                                      ?link))
             (make-instance of branch-immediate-molecule
                            (operation ?base-op)
                            (width ?width)
                            (destination ?dest)
                            (immediate ?address)))

(deffunction branch-conditional-immediate32
             (?dest ?address ?check-false ?link)
             (branch-conditional-immediate-molecule ?dest
                                                    ?address
                                                    ?check-false
                                                    ?link
                                                    32))

(deffunction branch-conditional-immediate48
             (?dest ?address ?check-false ?link)
             (branch-conditional-immediate-molecule ?dest
                                                    ?address
                                                    ?check-false
                                                    ?link
                                                    48))

(deffunction encode-set-operation
             (?op-suffix ?op ?dest-suffix ?dest ?imm-suffix ?imm)
             (encode ?op-suffix
                     (encode ?dest-suffix
                             (encode ?imm-suffix
                                     0
                                     (send ?imm
                                           encode))
                             ?dest)
                     ?op))
(defclass set16-instruction
  (is-a atom)
  (slot operation
        (source composite)
        (default-dynamic Set16))
  (slot immediate
        (source composite)
        (default ?NONE))
  (message-handler encode primary))
(defmessage-handler set16-instruction encode primary
                    ()
                    (encode-set-operation ?self:operation-suffix
                                          ?self:operation
                                          ?self:dest-suffix
                                          ?self:dest
                                          Immediate
                                          ?self:immediate))

(defclass wide-set-instruction
  (is-a molecule)
  (slot immediate
        (source composite)
        (default ?NONE))
  (message-handler encode primary))
(defmethod encode
  ((?operation SYMBOL
               (eq ?current-argument
                   Set32))
   (?value INTEGER)
   (?field INTEGER
           SYMBOL))
  (encode Immediate32
          ?value
          ?field))
(defmethod encode
  ((?operation SYMBOL
               (eq ?current-argument
                   Set48))
   (?value INTEGER)
   (?field INTEGER
           SYMBOL))
  (encode Immediate48
          ?value
          ?field))

(defmessage-handler wide-set-instruction encode primary
                    ()
                    (encode-set-operation ?self:operation-suffix
                                          ?self:operation
                                          MoleculeDestination
                                          ?self:dest
                                          ?self:operation
                                          ?self:immediate))

(deffunction make:wide-set-instruction
             (?dest ?i ?op)
             (make:word-container
               (make-instance of wide-set-instruction
                              (operation ?op)
                              (immediate ?i)
                              (dest ?dest))))


(deffunction set16
             (?dest ?i)
             (make-instance of set16-instruction
                            (dest ?dest)
                            (immediate ?i)))
(deffunction set32
             (?dest ?i)
             (make:wide-set-instruction ?dest
                                        ?i
                                        Set32))

(deffunction set48
             (?dest ?i)
             (make:wide-set-instruction ?dest
                                        ?i
                                        Set48))
(deffunction make:two-argument-set-count
             (?t)
             (make:arg-count-function ?t
                                      2))
(deffunction make:operation-to-call:set
             (?t)
             (make:operation-to-call ?t
                                     (lowcase ?t)))
(deffunction make:set-operation:to-call-and-arg-count
             (?t)
             (make:operation-to-call:set ?t)
             (make:two-argument-set-count ?t))


(map make:set-operation:to-call-and-arg-count
     Set16
     Set32
     Set48)

(map build-specific-operation-one-arg
     BranchUnconditionalRegister
     BranchUnconditionalRegisterLink)
(map build-specific-operation-two-arg
     Move
     Swap
     BinaryNot
     BranchConditionalTrueRegister
     BranchConditionalFalseRegister
     BranchConditionalTrueRegisterLink
     BranchConditionalFalseRegisterLink)
(map build-specific-operation-three-arg
     SystemCall
     Add
     Sub
     Mul
     Div
     Rem
     ShiftLeft
     ShiftRight
     BinaryAnd
     BinaryOr
     BinaryXor
     AddImmediate
     SubImmediate
     MulImmediate
     DivImmediate
     RemImmediate
     ShiftLeftImmediate
     ShiftRightImmediate
     Eq
     EqImmediate
     Neq
     NeqImmediate
     LessThan
     LessThanImmediate
     GreaterThan
     GreaterThanImmediate
     LessThanOrEqualTo
     LessThanOrEqualToImmediate
     GreaterThanOrEqualTo
     GreaterThanOrEqualToImmediate
     BinaryAndImmediate
     BinaryOrImmediate
     BinaryXorImmediate
     BinaryNand
     BinaryNandImmediate
     BranchIfThenElseLinkPredTrue
     BranchIfThenElseLinkPredFalse
     BranchIfThenElseNormalPredTrue
     BranchIfThenElseNormalPredFalse)


(deffunction push
             (?sp ?value)
             (op:move (stack ?sp)
                      ?value))
(deffunction pop
             (?sp ?dest)
             (op:move ?dest
                      (stack ?sp)))

(deffunction op:load
             (?dest ?src)
             (op:move ?dest
                      (memory ?src)))
(deffunction op:store
             (?dest ?src)
             (op:move (memory ?dest)
                      ?src))
(deffunction nop
             ()
             (op:swap (register:r0)
                      (register:r0)))

(defgeneric push16)
(defmethod push16
  ((?immediate INTEGER
               label)
   (?sp INTEGER))
  (set16 (stack ?sp)
         ?immediate))
(defmethod push16
  ((?immediate INTEGER))
  (push16 ?immediate
          (stack-pointer)))


(deffunction store16
             (?address ?imm)
             (set16 (memory ?address)
                    ?imm))

(defgeneric op:increment)
(defgeneric op:decrement)
(defgeneric op:double)
(defgeneric op:halve)

(defmethod op:increment
  ((?dest INTEGER)
   (?src INTEGER))
  (op:add ?dest ?src 1 TRUE))

(defmethod op:decrement
  ((?dest INTEGER)
   (?src INTEGER))
  (op:sub ?dest ?src 1 TRUE))

(defmethod op:double
  ((?dest INTEGER)
   (?src INTEGER))
  (op:mul ?dest ?src 2 TRUE))

(defmethod op:halve
  ((?dest INTEGER)
   (?src INTEGER))
  (op:div ?dest ?src 2 TRUE))

(deffunction stack-store
             (?sp)
             (bind ?rtemp0
                   (register (register:temp0)))
             (make:word-container (pop ?sp
                                       ?rtemp0)
                                  (op:store ?rtemp0
                                            (stack ?sp))))

(deffunction stack-load
             (?sp ?dest)
             (make:word-container (pop ?sp
                                       (register ?dest))
                                  (op:load (register ?dest)
                                           ?dest)))

(defgeneric set64)
(defmethod set64
  ((?dest INTEGER)
   (?value INTEGER)
   (?left-over INTEGER
               atom))
  (bind ?rtemp0
        (register (register:temp0)))
  (create$ (make:word-container (set16 ?rtemp0
                                       (decode-bits ?value
                                                    (hex->int 0xFFFF000000000000)
                                                    48))
                                (op:shiftleftimmediate ?rtemp0
                                                       ?rtemp0
                                                       48))
           (set48 ?dest
                  (decode-bits ?value
                               (hex->int 0x0000FFFFFFFFFFFF)
                               0))
           (make:word-container (op:add ?dest
                                        ?dest
                                        ?rtemp0)
                                ?left-over)))

(defmethod set64
  ((?dest INTEGER)
   (?value INTEGER))
  (set64 ?dest
         ?value
         (nop)))

(defmessage-handler instruction encode primary
                    ()
                    (bind ?result
                          (encode (dynamic-get operation-suffix)
                                  0
                                  ?self:operation))
                    (bind ?arg-count
                          (number-of-args ?self:operation))
                    (if (>= ?arg-count 1) then
                      (bind ?result
                            (encode (dynamic-get dest-suffix)
                                    ?result
                                    ?self:dest)))
                    (if (>= ?arg-count 2) then
                      (bind ?result
                            (encode (dynamic-get src0-suffix)
                                    ?result
                                    ?self:src0)))
                    (if (>= ?arg-count 3) then
                      (bind ?result
                            (encode (dynamic-get src1-suffix)
                                    ?result
                                    ?self:src1)))
                    ?result)

;--------------------------------------------------------------------------------
(deffunction stack-init-code
             ()
             (create$ (set64 (register:stack-pointer-bottom)
                             ?*stack-bottom*
                             (op:move (stack-pointer)
                                      (register:stack-pointer-bottom)))
                      (set64 (register:stack-pointer-top)
                             ?*stack-top*)
                      (set64 (register:call-stack-bottom)
                             ?*call-stack-bottom*
                             (op:move (register:call-stack-pointer)
                                      (register:call-stack-bottom)))
                      (set64 (register:call-stack-top)
                             ?*call-stack-top*)))
(deffunction setup-start-end-pair
             (?start ?start-addr ?end ?end-addr)
             (create$ (set64 (register ?start)
                             ?start-addr)
                      (set64 (register ?end)
                             ?end-addr)))

(deffunction memory-block-code
             ()
             (create$
               (setup-start-end-pair (register:memory-space0-start)
                                     ?*memory0-start*
                                     (register:memory-space0-end)
                                     ?*memory0-end*)
               (setup-start-end-pair (register:memory-space1-start)
                                     ?*memory1-start*
                                     (register:memory-space1-end)
                                     ?*memory1-end*)
               (setup-start-end-pair (register:code-start)
                                     ?*code-start*
                                     (register:code-end)
                                     ?*code-end*)
               (set64 (register (register:address-table-base))
                      ?*addr-table-begin*
                      (op:move (register:address-table-pointer)
                               (register:address-table-base)))))

(defgeneric func)
(defmethod func
  ((?title SYMBOL)
   (?single-atom INTEGER))
  (create$ (.label ?title)
           (make:word-container ?single-atom
                                (return-from-register (link-register)))))

(defgeneric stack-func)
(defmethod stack-func
  ((?title SYMBOL)
   (?operation SYMBOL)
   (?sp INTEGER))
  (func ?title
        (funcall (sym-cat op:
                          ?operation
                          :stack)
                 ?sp)))
(defmethod stack-func
  ((?title SYMBOL)
   (?operation SYMBOL))
  (stack-func ?title
              ?operation
              (stack-pointer)))
(defmethod stack-func
  ((?title SYMBOL))
  (stack-func ?title
              ?title))

(deffunction setup-read-eval-print-loop
             ()
             (create$ (make:word-container (op:add (memory (register:address-table-base))
                                                   (register (instruction-pointer))
                                                   1
                                                   TRUE)
                                           (nop))
                      (.label EvalBase)
                      ; loop body goes here
                      (make:word-container (nop)
                                           (return-from-memory (register:address-table-base)))))


(deffunction setup-simple-funcs
             ()
             (create$ (map stack-func
                           eq
                           neq
                           add
                           sub
                           div
                           rem)
                      (stack-func shift-left
                                  shiftleft)
                      (stack-func shift-right
                                  shiftright)
                      (stack-func lt
                                  lessthan)
                      (stack-func gt
                                  greaterthan)
                      (stack-func le
                                  lessthanorequalto)
                      (stack-func ge
                                  greaterthanorequalto)
                      (func incr
                            (op:increment (stack (stack-pointer))
                                          (stack (stack-pointer))))
                      (func decr
                            (op:decrement (stack (stack-pointer))
                                          (stack (stack-pointer))))
                      (func halve
                            (op:halve (stack (stack-pointer))
                                   (stack (stack-pointer))))
                      (func double
                            (op:double (stack (stack-pointer))
                                    (stack (stack-pointer))))))


(deffunction labelp
             (?l)
             (and (instancep ?l)
                  (eq (class ?l)
                      label)))
(deffunction word-containerp
             (?m)
             (integerp ?m))

(deffunction transmute-list
             (?input)
             (if (labelp ?input) then
               label
               else
               (if (word-containerp ?input) then
                 word-container
                 else
                 unknown)))
(deffunction strip-label
             (?input)
             (if (labelp ?input) then (create$) else ?input))

(deffunction strip-labels
             (?input)
             (map strip-label
                  (expand$ ?input)))
(deffunction compute-address
             (?input)
             (length$ (strip-labels ?input)))

(deffunction encode-thing
             (?thing)
             (send ?thing
                   encode))
(deffunction encode-list
             (?input)
             (map encode-thing
                  (expand$ ?input)))


