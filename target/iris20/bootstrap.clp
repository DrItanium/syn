(defglobal MAIN
           ?*addr-max* = (hex->int 0x03FFFFFF)
           ?*space-size* = (hex->int 0x00FFFFFF)
           ?*half-space* = (/ ?*space-size* 2)
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

(deffunction temporary-register0 () ?*register-temp0*)
(deffunction temporary-register1 () ?*register-temp1*)
(deffunction temporary-register2 () ?*register-temp2*)
(deffunction temporary-register3 () ?*register-temp3*)
(deffunction temporary-register4 () ?*register-temp4*)
(deffunction temporary-register5 () ?*register-temp5*)
(deffunction temporary-register6 () ?*register-temp6*)

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

(defgeneric molecule)
(defgeneric return-from-register)
(defgeneric make-atom)
(defgeneric make-molecule-instruction)
(defgeneric make-molecule)
(defgeneric return-from-stack)

(deffunction link-register () ?*register-lr*)
(deffunction instruction-pointer () ?*register-ip*)
(deffunction stack-pointer () ?*register-sp*)
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

(deffunction construct-register-operation
             "Tag the type of operation to perform on the given register index"
             (?action ?index)
             (iris20-encode-SectionDescriptor (iris20-encode-SectionIndex 0 
                                                                          ?index)
                                              (section-descriptor->int ?action)))
(deffunction stack-operation (?i) (construct-register-operation Stack ?i))
(deffunction register-operation (?i) (construct-register-operation Register ?i))
(deffunction memory-operation (?i) (construct-register-operation Memory ?i))

(defmethod make-molecule-instruction
  ((?operation SYMBOL))
  (iris20-encode-MoleculeOperation 0 
                                   (operation->int ?operation)))

(defmethod make-molecule-instruction
  ((?operation SYMBOL)
   (?destination INTEGER))
  (iris20-encode-MoleculeDestination (make-molecule-instruction ?operation)
                                     ?destination))
(defmethod make-molecule-instruction
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER))
  (iris20-encode-MoleculeSource0 (make-molecule-instruction ?operation
                                                            ?dest)
                                 ?src0))
(defmethod make-molecule-instruction
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (iris20-encode-MoleculeSource1 (make-molecule-instruction ?operation
                                                            ?dest
                                                            ?src0)
                                 ?src1))

(defmethod make-atom
  ((?operation SYMBOL))
  (iris20-encode-Operation 0
                           (operation->int ?operation)))

(defmethod make-atom
  ((?operation SYMBOL)
   (?destination INTEGER))
  (iris20-encode-Destination (make-atom ?operation)
                             ?destination))
(defmethod make-atom
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER))
  (iris20-encode-Source0 (make-atom ?operation
                                    ?dest)
                         ?src0))
(defmethod make-atom
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (iris20-encode-Source1 (make-atom ?operation
                                    ?dest
                                    ?src0)
                         ?src1))
(defmethod make-molecule
  ((?first INTEGER)
   (?second INTEGER))
  (iris20-encode-MoleculeContainsOneInstruction (iris20-encode-SecondAtom 
                                                  (iris20-encode-FirstAtom 0
                                                                           ?first)
                                                  ?second)
                                                0))
(defmethod make-molecule
  ((?wide-instruction INTEGER))
  (iris20-encode-MoleculeContainsOneInstruction ?wide-instruction 
                                                1))
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
             (build (format nil
                            "(defmethod %s ((?dest INTEGER) (?src0 INTEGER) (?src1 INTEGER) (?immediate SYMBOL (not (neq ?current-argument FALSE TRUE)))) (make-atom (if ?immediate then %sImmediate else %s) ?dest ?src0 ?src1))"
                            ?title 
                            ?operation
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
             (build (format nil
                            "(defmethod %s ((?dest INTEGER)) (make-atom %s ?dest))"
                            ?title 
                            ?operation))
             (build (format nil
                            "(defmethod %s ((?dest INTEGER)) (%s (stack-operation ?dest)))"
                            ?stack-title
                            ?title))
             )
(deffunction make-link-version
             (?op ?link)
             (if ?link then (sym-cat ?op Link) else ?op))
(deffunction branch-unconditional-immediate
             (?address ?link)
             (iris20-encode-Immediate (make-atom (make-link-version BranchUnconditionalImmediate 
                                                                    ?link))
                                      ?address))
(deffunction branch-unconditional-immediate32
             (?address ?link)
             (make-molecule (iris20-encode-Immediate32 (make-molecule-instruction (make-link-version BranchUnconditionalImmediate32
                                                                                                     ?link))
                                                       ?address)))

(deffunction branch-unconditional-immediate48
             (?address ?link)
             (make-molecule (iris20-encode-Immediate48 (make-molecule-instruction (make-link-version BranchUnconditionalImmediate48
                                                                                                     ?link))
                                                       ?address)))

(deffunction branch-conditional-immediate32
             (?dest ?address ?check-false ?link)
             (bind ?base-op
                   (if ?check-false then
                     BranchConditionalFalseImmediate32
                     else
                     BranchConditionalTrueImmediate32))
             (make-molecule (iris20-encode-Immediate32 (make-molecule-instruction (make-link-version ?base-op
                                                                                                     ?link)
                                                                                  ?dest)
                                                       ?address)))
(deffunction branch-conditional-immediate48
             (?dest ?address ?check-false ?link)
             (bind ?base-op
                   (if ?check-false then
                     BranchConditionalFalseImmediate48
                     else
                     BranchConditionalTrueImmediate48))
             (make-molecule (iris20-encode-Immediate48 (make-molecule-instruction (make-link-version ?base-op
                                                                                                     ?link)
                                                                                  ?dest)
                                                       ?address)))
(deffunction set16
             (?dest ?i)
             (iris20-encode-Immediate (make-atom Set16
                                                 ?dest)
                                      ?i))
(deffunction set32
             (?dest ?i)
             (make-molecule (iris20-encode-Immediate32 (make-molecule-instruction Set32
                                                                                  ?dest)
                                                       ?i)))

(deffunction set48
             (?dest ?i)
             (make-molecule (iris20-encode-Immediate48 (make-molecule-instruction Set48
                                                                                  ?dest)
                                                       ?i)))

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
             (swap-op 0 
                      0))

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
             (make-molecule (pop ?sp 
                                 (register-operation (temporary-register0)))
                            (store-op (temporary-register0)
                                   (stack-operation ?sp))))

(deffunction stack-load
             (?sp ?dest)
             (make-molecule (pop ?sp 
                                 (register-operation ?dest))
                            (load-op (register-operation ?dest)
                                  ?dest)))

(defgeneric set64)
(defmethod set64
  ((?dest INTEGER)
   (?value INTEGER)
   (?left-over INTEGER))
  (create$ (make-molecule (set16 (register-operation (temporary-register0))
                                 (decode-bits ?value 
                                              (hex->int 0xFFFF000000000000)
                                              48))
                          (shiftleft-op (register-operation (temporary-register0))
                                        (register-operation (temporary-register0))
                                        48
                                        TRUE))
           (set48 ?dest 
                  (decode-bits ?value
                               (hex->int 0x0000FFFFFFFFFFFF)
                               0))
           (make-molecule (add-op ?dest 
                                  ?dest 
                                  (temporary-register0)
                                  FALSE)
                          ?left-over)))

(defmethod set64
  ((?dest INTEGER)
   (?value INTEGER))
  (set64 ?dest
         ?value
         (nop)))


