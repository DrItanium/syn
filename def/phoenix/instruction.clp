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
          (flag (name FieldIsImmediate)
                (mask 0b1000000000000000)
                (shift 15))
          (field (name FragmentType)
                 (mask 0b0111000000000000)
                 (shift 12)
                 (output-type WordTag))

          (field (name ImmediateValue)
                 (mask 0b0111111111111111)
                 (shift 0)
                 (output-type InstructionFragment))
          (field (name FragmentBits)
                 (mask 0b0000111111111111)
                 (shift 0)
                 (output-type InstructionFragment))
          (field (name Operation)
                 (mask 0b0000111111000000)
                 (shift 6)
                 (output-type OperationType))
          (field (name UnusedOperationBits)
                 (mask 0b0000000000111111)
                 (shift 0)
                 (output-type byte))
          (field (name FullIndex)
                 (mask 0b0000000000111111)
                 (shift 0)
                 (output-type RegisterIndex))
          (field (name FullFlags)
                 (mask 0b0000111111000000)
                 (shift 6)
                 (output-type byte))
          (field (name HalfIndex)
                 (mask 0b0000000001111111)
                 (shift 0)
                 (output-type RegisterIndex))
          (field (name HalfFlags)
                 (mask 0b0000111110000000)
                 (shift 7)
                 (output-type byte))
          (field (name QuarterIndex)
                 (mask 0b0000000011111111)
                 (shift 0)
                 (output-type RegisterIndex))
          (field (name QuarterFlags)
                 (mask 0b0000111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name ByteIndex)
                 (mask 0b0000000111111111)
                 (shift 0)
                 (output-type RegisterIndex))
          (field (name ByteFlags)
                 (mask 0b0000111000000000)
                 (shift 9)
                 (output-type byte))

          (field (name NybbleIndex)
                 (mask 0b0000001111111111)
                 (shift 0)
                 (output-type RegisterIndex))
          (field (name NybbleFlags)
                 (mask 0b0000110000000000)
                 (shift 10)
                 (output-type byte))
          (field (name ImmediateValue)
                 (mask 0b0000111111111111)
                 (shift 0)
                 (output-type InstructionFragment))
          (field (name PredicateIndex)
                 (mask 0b0000111111111111)
                 (shift 0)
                 (output-type RegisterIndex))
          (enum (cast-to byte)
                (max-size 8)
                (name WordTag)
                (children Full
                          Half
                          Quarter
                          Byte
                          Nybble
                          Predicate
                          Control))
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

