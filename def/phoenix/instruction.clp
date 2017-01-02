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
          (input-type uint16_t)
          (title _phoenix_decl)
          (namespace phoenix)
          (field (name FieldIsOperand)
                 (mask 0b0000000000000001)
                 (shift 0)
                 (input-type uint16_t)
                 (output-type bool))
          (field (name RawOperation)
                 (mask 0b1111111111111110)
                 (shift 1)
                 (output-type uint16_t))
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
                 (output-type uint16_t))
          (field (name RegisterIndex16)
                 (mask 0b1111111111000000)
                 (shift 6)
                 (output-type uint16_t))
          (field (name RegisterIndex8)
                 (mask 0b1111111111100000)
                 (shift 5)
                 (output-type uint16_t))
          (field (name RegisterIndex4)
                 (mask 0b1111111111110000)
                 (shift 4)
                 (output-type uint16_t))
          (field (name RegisterIndex2)
                 (mask 0b1111111111111000)
                 (shift 3)
                 (output-type uint16_t))
          (field (name RegisterIndex1)
                 (mask 0b1111111111111100)
                 (shift 2)
                 (output-type uint16_t))
          (field (name IndirectBit)
                 (mask 0b0000000000000010)
                 (shift 1)
                 (output-type bool))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name Operation)
                (children (map generate-bit-versions
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
                               BinaryAndImmediate
                               BinaryOrImmediate
                               BinaryXorImmediate
                               BinaryNandImmediate
                               BinaryNorImmediate
                               AddImmediate
                               SubImmediate
                               MulImmediate
                               DivImmediate
                               RemImmediate
                               ShiftLeftImmediate
                               ShiftRightImmediate
                               Push
                               Pop
                               Load
                               LoadWithOffset
                               Store
                               StoreWithOffset
                               Move
                               Set
                               Swap
                               Eq
                               Neq
                               LessThan
                               GreaterThan
                               LessThanOrEqualTo
                               GreaterThanOrEqualTo
                               EqImmediate
                               NeqImmediate
                               LessThanImmediate
                               GreaterThanImmediate
                               LessThanOrEqualToImmediate
                               GreaterThanOrEqualToImmediate
                               CallImmediate
                               CallRegister
                               BranchSelect
                               BranchRegister
                               ConditionalBranchImmediate
                               ConditionalBranchRegister)))

