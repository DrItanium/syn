; several types of actions: flags, register, and custom
(deffacts instruction-description
          (input-type Word)
          (title _iris19_decl)
          (enum (name Operation)
                (children Arithmetic
                          Shift
                          Logical
                          Compare
                          Branch
                          Move)
                (cast-to byte)
                (max-size "ArchitectureConstants::MaxInstructionCount"))
          (enum (name MoveOperation)
                (children Move
                          Set
                          Swap
                          SystemCall)
                (cast-to byte)
                (max-size 4))
          (enum (name ArithmeticOps)
                (children Add
                          Sub
                          Mul
                          Div
                          Rem)
                (cast-to byte)
                (max-size 8))
          (enum (name CompareCombine)
                (children None
                          And
                          Or
                          Xor)
                (max-size 4)
                (cast-to byte))
          (enum (name CompareStyle)
                (children Equals
                          NotEquals
                          LessThan
                          GreaterThan
                          LessThanOrEqualTo
                          GreaterThanOrEqualTo)
                (max-size 8)
                (cast-to byte))
          (enum (name LogicalOps)
                (children And
                          Or
                          Xor
                          Nand
                          Not)
                (max-size 8)
                (cast-to byte))
          (enum (name MemoryOperation)
                (children Load
                          Store
                          Push
                          Pop)
                (max-size 4)
                (cast-to byte))
          (field (name Control)
                 (output-type Operation)
                 (mask 0b00000000000000000000000000000111)
                 (shift 0))
          (field (name ImmediateFlag)
                 (mask 0b00000000000000000000000000001000)
                 (shift 3)
                 (output-type bool))
          (field (name DestinationIndex)
                 (mask 0b00000000000000001111111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name Source0Index)
                 (mask 0b00000000111111110000000000000000)
                 (shift 16)
                 (output-type byte))
          (field (name Source1Index)
                 (mask 0b11111111000000000000000000000000)
                 (shift 24)
                 (output-type byte))
          (field (name ShortImmediate)
                 (mask 0b11111111000000000000000000000000)
                 (shift 24)
                 (output-type byte))
          (field (name RawBitmask)
                 (mask 0b11111111000000000000000000000000)
                 (shift 24)
                 (output-type byte))
          (field (name CompareType)
                 (shift 4)
                 (mask 0b00000000000000000000000001110000)
                 (output-type byte))
          (field (name ArithmeticFlagType)
                 (shift 4)
                 (mask 0b00000000000000000000000001110000)
                 (output-type byte))
          (field (name LogicalFlagType)
                 (shift 4)
                 (mask 0b00000000000000000000000001110000)
                 (output-type byte))
          (field (name ShiftFlagLeft)
                 (shift 4)
                 (mask 0b00000000000000000000000000010000)
                 (output-type bool))
          (field (name BranchFlagIsCallForm)
                 (shift 4)
                 (mask 0b00000000000000000000000000010000)
                 (output-type bool))
          (field (name BranchFlagIsIfForm)
                 (shift 5)
                 (mask 0b00000000000000000000000000100000)
                 (output-type bool))
          (field (name BranchFlagIsConditional)
                 (shift 6)
                 (mask 0b00000000000000000000000001000000)
                 (output-type bool))
          (field (name MoveSubtype)
                 (shift 4)
                 (mask 0b00000000000000000000000000110000)
                 (output-type byte))

          )
