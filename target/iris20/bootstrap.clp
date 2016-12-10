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





(defgeneric defunc)
(defgeneric molecule)
(defgeneric return-from-register)
(defgeneric make-atom)
(defgeneric make-molecule-instruction)
(defgeneric make-molecule)
(defgeneric return-from-stack)

(deffunction link-register () ?*register-lr*)
(deffunction instruction-pointer () ?*register-ip*)
(deffunction stack-pointer () ?*register-stack-pointer*)
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
             (iris20-encode-SectionDescriptor (iris20-encode-SectionIndex ?index)
                                              (section-descriptor->int ?action)))
(deffunction stack-operation (?i) (operation Stack ?i))
(deffunction register-operation (?i) (operation Register ?i))
(deffunction memory-operation (?i) (operation Memory ?i))

(defmethod make-molecule-instruction
  ((?operation SYMBOL))
  (iris20-encode-MoleculeOperation (operation->int ?operation)))

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
  (iris20-encodeOperation (operation->int ?operation)))
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
                                                  (iris20-encode-FirstAtom ?first)
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
(defmethod defunc
  ((?title LEXEME)
   (?atom INTEGER))
  (create$ ?title
           (molecule ?atom
                     (return-from-register (link-register)))))


