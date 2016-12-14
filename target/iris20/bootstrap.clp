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
             (bind ?reg-len
                   (str-length "register-"))
             (build (format nil "(deffunction register:%s () ?*%s*)"
                            (sub-string (+ ?reg-len 1) (str-length ?title)
                                        ?title)
                            ?title)))

(map build-register-operation
     (expand$ (filter has-register-prefix
                      (expand$ (get-defglobal-list MAIN)))))
(loop-for-count (?i 0 63) do
                (build (format nil
                               "(deffunction register:%s () %d)"
                               (sym-cat r ?i)
                               ?i)))
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


(deffunction output-bytes-to-file
             (?bytes ?router)
             (progn$ (?byte ?bytes)
                     (put-char ?router
                               ?byte)))

(defgeneric return-from-register)
(defgeneric make-atom)
(defgeneric make-molecule)
(defgeneric make-word-container)
(defgeneric return-from-stack)

(deffunction enum->int
             (?symbol ?collection)
             (- (member$ ?symbol
                         ?collection) 1))
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

(deffunction construct-register-operation
             "Tag the type of operation to perform on the given register index"
             (?action ?index)
             (iris20-encode-SectionDescriptor (iris20-encode-SectionIndex 0
                                                                          ?index)
                                              (section-descriptor->int ?action)))
(deffunction stack-operation (?i) (construct-register-operation Stack ?i))
(deffunction register-operation (?i) (construct-register-operation Register ?i))
(deffunction memory-operation (?i) (construct-register-operation Memory ?i))

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
  (message-handler encode primary))
(defclass atom
  (is-a instruction)
  (role concrete)
  (pattern-match reactive)
  (message-handler encode primary))
(defclass molecule
  (is-a instruction)
  (role concrete)
  (pattern-match reactive)
  (message-handler encode primary))

(defmethod make-molecule
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (make-instance of molecule
                 (operation ?operation)
                 (dest ?dest)
                 (src0 ?src0)
                 (src1 ?src1)))
(defmethod make-molecule
  ((?operation SYMBOL)
   (?dest INTEGER))
  (make-molecule ?operation
                 ?dest
                 0
                 0))
(defmethod make-molecule
  ((?operation SYMBOL))
  (make-molecule ?operation
                 0))

(defmethod make-molecule
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER))
  (make-molecule ?operation
                 ?dest
                 ?src0
                 0))

(defmethod make-atom
  ((?operation SYMBOL))
  (make-atom ?operation
             0))

(defmethod make-atom
  ((?operation SYMBOL)
   (?destination INTEGER))
  (make-atom ?operation
             ?destination
             0))

(defmethod make-atom
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER))
  (make-atom ?operation
             ?dest
             ?src0
             0))

(defmethod make-atom
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

(defmethod make-word-container
  ((?first INTEGER
           atom)
   (?second INTEGER
            atom))
  (make-instance of word-container
                 (contents ?first
                           ?second)))
(defmethod make-word-container
  ((?wide-instruction INTEGER
                      molecule))
  (make-instance of word-container
                 (contents ?wide-instruction)))
(defmessage-handler word-container encode primary
                    ()
                    (switch (length$ ?self:contents)
                            (case 0 then
                              (printout werror
                                        "ERROR: word-container contains no instructions!"
                                        crlf)
                              (halt)
                              (create$))
                            (case 1 then (iris20-encode-MoleculeContainsOneInstruction
                                           (send (nth$ 1 ?self:contents) encode)
                                           1))
                            (case 2 then
                              (iris20-encode-MoleculeContainsOneInstruction
                                (iris20-encode-SecondAtom
                                  (iris20-encode-FirstAtom 0
                                                           (send (nth$ 1
                                                                       ?self:contents)
                                                                 encode))
                                  (send (nth$ 2
                                              ?self:contents)
                                        encode))
                                0))
                            (default (printout werror
                                               "ERROR: word-container contains too many instructions!"
                                               crlf)
                                     (halt)
                                     (create$))))

(defmethod return-instruction
  ((?register INTEGER))
  (make-atom BranchUnconditionalRegister
             ?register))
(defmethod return-from-stack
  ((?sp INTEGER))
  (return-instruction (stack-operation ?sp)))
(defmethod return-from-stack
  ()
  (return-from-stack (stack-pointer)))
(deffunction return-from-memory
             (?r)
             (return-instruction (memory-operation ?r)))
(deffunction return-to-register
             (?r)
             (return-instruction (register-operation ?r)))
(deffunction return-to-link-register
             ()
             (return-to-register (link-register)))
(defgeneric number-of-args)
(defgeneric operation-to-call)
(deffunction make-operation-to-call
             (?symbol ?func)
             (build (format nil
                            "(defmethod operation-to-call ((?a SYMBOL (not (neq ?current-argument %s %s)))) %s)"
                            ?symbol
                            (lowcase ?symbol)
                            ?func)))

(deffunction immediatep
             (?symbol)
             (has-suffix ?symbol
                         Immediate))
(deffunction make-arg-count-function
             (?op ?count)
             (build (format nil
                            "(defmethod number-of-args ((?a SYMBOL (not (neq ?current-argument %s %s)))) %d)"
                            ?op
                            (lowcase ?op)
                            ?count)))

(deffunction setoperationp
             (?symbol)
             (has-prefix ?symbol
                         Set))

(deffunction build-specific-operation-three-arg
             (?operation)
             (bind ?title
                   (sym-cat (lowcase ?operation)
                            -op))
             (bind ?stack-title
                   (str-cat ?title
                            ":stack"))
             (build (format nil
                            "(defgeneric %s)"
                            ?title))
             (build (format nil
                            "(defgeneric %s)"
                            ?stack-title))
             (make-arg-count-function ?operation
                                      3)
             (make-operation-to-call ?operation
                                     ?title)
             (build (format nil
                            "(defmethod %s ((?dest INTEGER) (?src0 INTEGER) (?src1 INTEGER)) (make-atom %s ?dest ?src0 ?src1))"
                            ?title
                            ?operation))
             (build (format nil
                            "(defmethod %s ((?dest INTEGER) (?src0 INTEGER) (?src1 INTEGER)) (%s (stack-operation ?dest) (stack-operation ?src0) (stack-operation ?src1) FALSE))"
                            ?stack-title
                            ?title))
             (build (format nil
                            "(defmethod %s ((?stack INTEGER)) (%s ?stack ?stack ?stack))"
                            ?stack-title
                            ?stack-title)))

(deffunction build-specific-operation-two-arg
             (?operation)
             (bind ?title
                   (sym-cat (lowcase ?operation)
                            -op))
             (make-arg-count-function ?operation
                                      2)
             (make-operation-to-call ?operation
                                     ?title)
             (bind ?stack-title
                   (str-cat ?title
                            ":stack"))
             (build (format nil
                            "(defgeneric %s)"
                            ?title))
             (build (format nil
                            "(defgeneric %s)"
                            ?stack-title))
             (build (format nil
                            "(defmethod %s ((?dest INTEGER) (?src0 INTEGER)) (make-atom %s ?dest ?src0))"
                            ?title
                            ?operation))
             (build (format nil
                            "(defmethod %s ((?dest INTEGER) (?src0 INTEGER)) (%s (stack-operation ?dest) (stack-operation ?src0)))"
                            ?stack-title
                            ?title))
             (build (format nil
                            "(defmethod %s ((?stack INTEGER)) (%s ?stack ?stack))"
                            ?stack-title
                            ?stack-title)))

(deffunction build-specific-operation-one-arg
             (?operation)
             (make-arg-count-function ?operation
                                      1)
             (bind ?title
                   (sym-cat (lowcase ?operation)
                            -op))
             (make-operation-to-call ?operation
                                     ?title)
             (bind ?stack-title
                   (str-cat ?title
                            ":stack"))
             (build (format nil
                            "(defgeneric %s)"
                            ?title))
             (build (format nil
                            "(defgeneric %s)"
                            ?stack-title))
             (build (format nil
                            "(defmethod %s ((?dest INTEGER)) (make-atom %s ?dest))"
                            ?title
                            ?operation))
             (build (format nil
                            "(defmethod %s ((?dest INTEGER)) (%s (stack-operation ?dest)))"
                            ?stack-title
                            ?title))
             )
(defclass branch-immediate-atom
  (is-a atom)
  (slot immediate
        (source composite)
        (default ?NONE))
  (message-handler encode primary))
(defmessage-handler branch-immediate-atom encode primary
                    ()
                    (iris20-encode-Operation
                      (iris20-encode-Immediate
                        (if (= (number-of-args ?self:operation) 2) then
                          (iris20-encode-Destination 0
                                                     ?self:dest)
                          else
                          0)
                        ?self:immediate)
                      ?self:operation))

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
                    (funcall (sym-cat iris20-encode-Immediate
                                      ?self:width)
                             (iris20-encode-Operation
                               (if (= (number-of-args ?self:operation) 2) then
                                 (iris20-encode-MoleculeDestination 0
                                                                    ?self:dest)
                                 else
                                 0)
                               ?self:operation)
                             ?self:immediate))

(deffunction make-link-version
             (?op ?link)
             (if ?link then (sym-cat ?op Link) else ?op))
(deffunction branch-unconditional-immediate
             (?address ?link)
             (make-instance of branch-immediate-atom
                            (operation (make-link-version BranchUnconditionalImmediate
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
                            (operation (make-link-version ?base-op
                                                          ?link))
                            (immediate ?address)))
(deffunction branch-unconditional-immediate32
             (?address ?link)
             (make-instance of branch-immediate-molecule
                            (width 32)
                            (operation (make-link-version BranchUnconditionalImmediate32
                                                          ?link))
                            (dest 0)
                            (immediate ?address)))

(deffunction branch-unconditional-immediate48
             (?address ?link)
             (make-instance of branch-immediate-molecule
                            (width 48)
                            (operation (make-link-version BranchUnconditionalImmediate48
                                                          ?link))
                            (dest 0)
                            (immediate ?address)))

(deffunction branch-conditional-immediate-molecule
             (?dest ?address ?check-false ?link ?width)
             (bind ?base-op
                   (make-link-version (sym-cat BranchConditional
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
                    (iris20-encode-Operation
                      (iris20-encode-Destination
                        (iris20-encode-Immediate 0
                                                 ?self:immediate)
                        ?self:dest)
                      ?self:operation))
(defclass wide-set-instruction
  (is-a molecule)
  (slot immediate
        (source composite)
        (default ?NONE))
  (message-handler encode primary))
(defmessage-handler wide-set-instruction encode primary
                    ()
                    (iris20-encode-Operation
                      (iris20-encode-MoleculeDestination
                        (funcall (switch ?self:operation
                                         (case Set32 then iris20-encode-Immediate32)
                                         (case Set48 then iris20-encode-Immediate48)
                                         (default (sym-cat UNKNOWN_OPERATION
                                                           ?self:operation)))
                                 0
                                 (send ?self:immediate
                                       encode))
                        ?self:dest)
                      ?self:operation))

(deffunction make-wide-set-instruction
             (?dest ?i ?op)
             (make-word-container
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
             (make-wide-set-instruction ?dest
                                        ?i
                                        Set32))

(deffunction set48
             (?dest ?i)
             (make-wide-set-instruction ?dest
                                        ?i
                                        Set48))

(make-arg-count-function Set16
                         2)
(make-arg-count-function Set32
                         2)
(make-arg-count-function Set48
                         2)
(make-operation-to-call Set16
                        (lowcase Set16))
(make-operation-to-call Set32
                        (lowcase Set32))
(make-operation-to-call Set48
                        (lowcase Set48))

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
             (move-op (stack-operation ?sp)
                      ?value))
(deffunction pop
             (?sp ?dest)
             (move-op ?dest
                      (stack-operation ?sp)))

(deffunction load-op
             (?dest ?src)
             (move-op ?dest
                      (memory-operation ?src)))
(deffunction store-op
             (?dest ?src)
             (move-op (memory-operation ?dest)
                      ?src))
(deffunction nop
             ()
             (swap-op (register:r0)
                      (register:r0)))

(defgeneric push16)
(defmethod push16
  ((?immediate INTEGER)
   (?sp INTEGER))
  (set16 (stack-operation ?sp)
         ?immediate))
(defmethod push16
  ((?immediate INTEGER))
  (push16 ?immediate
          (stack-pointer)))

(deffunction store16
             (?address ?imm)
             (set16 (memory-operation ?address)
                    ?imm))

(defgeneric increment-op)
(defgeneric decrement-op)
(defgeneric double)
(defgeneric halve)

(defmethod increment-op
  ((?dest INTEGER)
   (?src INTEGER))
  (add-op ?dest ?src 1 TRUE))

(defmethod decrement-op
  ((?dest INTEGER)
   (?src INTEGER))
  (sub-op ?dest ?src 1 TRUE))

(defmethod double
  ((?dest INTEGER)
   (?src INTEGER))
  (mul-op ?dest ?src 2 TRUE))

(defmethod halve
  ((?dest INTEGER)
   (?src INTEGER))
  (div-op ?dest ?src 2 TRUE))

(deffunction stack-store
             (?sp)
             (bind ?rtemp0
                   (register-operation (register:temp0)))
             (make-word-container (pop ?sp
                                       ?rtemp0)
                                  (store-op ?rtemp0
                                            (stack-operation ?sp))))

(deffunction stack-load
             (?sp ?dest)
             (make-word-container (pop ?sp
                                       (register-operation ?dest))
                                  (load-op (register-operation ?dest)
                                           ?dest)))

(defgeneric set64)
(defmethod set64
  ((?dest INTEGER)
   (?value INTEGER)
   (?left-over INTEGER
               atom))
  (bind ?rtemp0
        (register-operation (register:temp0)))
  (create$ (make-word-container (set16 ?rtemp0
                                       (decode-bits ?value
                                                    (hex->int 0xFFFF000000000000)
                                                    48))
                                (shiftleftimmediate-op ?rtemp0
                                                       ?rtemp0
                                                       48))
           (set48 ?dest
                  (decode-bits ?value
                               (hex->int 0x0000FFFFFFFFFFFF)
                               0))
           (make-word-container (add-op ?dest
                                        ?dest
                                        ?rtemp0)
                                ?left-over)))

(defmethod set64
  ((?dest INTEGER)
   (?value INTEGER))
  (set64 ?dest
         ?value
         (nop)))


(defmessage-handler atom encode primary
                    ()
                    (bind ?result
                          (iris20-encode-Operation 0
                                                   ?self:operation))
                    (bind ?arg-count
                          (number-of-args ?self:operation))
                    (if (>= ?arg-count 1) then
                      (bind ?result
                            (iris20-encode-Destination ?result
                                                       ?self:dest)))
                    (if (>= ?arg-count 2) then
                      (bind ?result
                            (iris20-encode-Source0 ?result
                                                   ?self:src0)))
                    (if (>= ?arg-count 3) then
                      (bind ?result
                            (iris20-encode-Source1 ?result
                                                   ?self:src1)))
                    ?result)

(defmessage-handler molecule encode primary
                    ()
                    (bind ?result
                          (iris20-encode-Operation 0
                                                   ?self:operation))
                    (bind ?arg-count
                          (number-of-args ?self:operation))
                    (if (>= ?arg-count 1) then
                      (bind ?result
                            (iris20-encode-MoleculeDestination ?result
                                                               ?self:dest)))
                    (if (>= ?arg-count 2) then
                      (bind ?result
                            (iris20-encode-MoleculeSource0 ?result
                                                           ?self:src0)))
                    (if (>= ?arg-count 3) then
                      (bind ?result
                            (iris20-encode-MoleculeSource1 ?result
                                                           ?self:src1)))
                    ; if we have more than 3 then we have to handle those
                    ; specially
                    ?result)


;--------------------------------------------------------------------------------
(deffunction stack-init-code
             ()
             (create$ (set64 (register:stack-pointer-bottom)
                             ?*stack-bottom*
                             (move-op (stack-pointer)
                                      (register:stack-pointer-bottom)))
                      (set64 (register:stack-pointer-top)
                             ?*stack-top*)
                      (set64 (register:call-stack-bottom)
                             ?*call-stack-bottom*
                             (move-op (register:call-stack-pointer)
                                      (register:call-stack-bottom)))
                      (set64 (register:call-stack-top)
                             ?*call-stack-top*)))
(deffunction setup-start-end-pair
             (?start ?start-addr ?end ?end-addr)
             (create$ (set64 (register-operation ?start)
                             ?start-addr)
                      (set64 (register-operation ?end)
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
               (set64 (register-operation (register:address-table-base))
                      ?*addr-table-begin*
                      (move-op (register:address-table-pointer)
                               (register:address-table-base)))))

(defgeneric func)
(defmethod func
  ((?title SYMBOL)
   (?single-atom INTEGER))
  (create$ (.label ?title)
           (make-word-container ?single-atom
                                (return-from-register (link-register)))))

(defgeneric stack-func)
(defmethod stack-func
  ((?title SYMBOL)
   (?operation SYMBOL)
   (?sp INTEGER))
  (func ?title
        (funcall (sym-cat ?operation -op:stack)
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
             (create$ (make-word-container (add-op (memory-operation (register:address-table-base))
                                                   (register-operation (instruction-pointer))
                                                   1
                                                   TRUE)
                                           (nop))
                      (.label EvalBase)
                      ; loop body goes here
                      (make-word-container (nop)
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
                            (increment-op (stack-operation (stack-pointer))
                                          (stack-operation (stack-pointer))))
                      (func decr
                            (decrement-op (stack-operation (stack-pointer))
                                          (stack-operation (stack-pointer))))
                      (func halve
                            (halve (stack-operation (stack-pointer))
                                   (stack-operation (stack-pointer))))
                      (func double
                            (double (stack-operation (stack-pointer))
                                    (stack-operation (stack-pointer))))))


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
