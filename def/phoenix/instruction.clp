(deffunction generate-bit-versions
             (?symbol)
             (bind ?output
                   (create$))
             (progn$ (?width (create$ 64 32 16 8 4 2 1))
                     (bind ?output
                           ?output
                           (sym-cat ?symbol 
                                    ?width)))
             ?output)

(deffacts phoenix
          (input-type InstructionFragment)
          (title _phoenix_decl)
          (namespace phoenix)
          (field (name FieldIsOperand)
                 (mask 0b0000000000000001)
                 (shift 0)
                 (input-type InstructionFragment)
                 (output-type bool))
          (field (name RawOperation)
                 (mask 0b1111111111111110)
                 (shift 1)
                 (output-type InstructionFragment))
          (field (name OperandWidth)
                 (mask 0b1110000000000000)
                 (shift 13)
                 (output-type OperationWidth))
          (field (name OperationIsImmediate)
                 (mask 0b0001000000000000)
                 (shift 12)
                 (output-type bool))
          (field (name Operation)
                 (mask 0b0000000001111110)
                 (shift 1)
                 (output-type OperationType))
          (field (name RawOperand)
                 (mask 0b1111111111111110)
                 (shift 1)
                 (output-type uint16_t))
          (field (name RegisterIndex64)
                 (mask 0b1111111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name RegisterIndex32)
                 (mask 0b1111111110000000)
                 (shift 7)
                 (output-type RegisterIndex))
          (field (name RegisterIndex16)
                 (mask 0b1111111111000000)
                 (shift 6)
                 (output-type RegisterIndex))
          (field (name RegisterIndex8)
                 (mask 0b1111111111100000)
                 (shift 5)
                 (output-type RegisterIndex))
          (field (name RegisterIndex4)
                 (mask 0b1111111111110000)
                 (shift 4)
                 (output-type RegisterIndex))
          (field (name RegisterIndex2)
                 (mask 0b1111111111111000)
                 (shift 3)
                 (output-type RegisterIndex))
          (field (name RegisterIndex1)
                 (mask 0b1111111111111100)
                 (shift 2)
                 (output-type RegisterIndex))
          (field (name IndirectBit)
                 (mask 0b0000000000000010)
                 (shift 1)
                 (output-type bool))
          (enum (cast-to byte)
                (max-size 8)
                (name OperationWidth)
                (children (generate-bit-versions Width)))
          (enum (cast-to uint16_t)
                (max-size "ArchitectureConstants::MaxOperations")
                (name OperationType)
                (children Nop
                          Add
                          Sub
                          Mul
                          Div
                          Rem
                          ShiftLeft
                          ShiftRight
                          BinaryAnd
                          BinaryOr
                          BinaryNot
                          BinaryXor
                          BinaryNand
                          BinaryNor
                          Push
                          Pop
                          Load
                          Store
                          Move
                          Set
                          Swap
                          Eq
                          Neq
                          LessThan
                          GreaterThan
                          LessThanOrEqualTo
                          GreaterThanOrEqualTo
                          Call
                          BranchSelect
                          Branch
                          ConditionalBranch)))

