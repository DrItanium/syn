; several types of actions: flags, register, and custom
(deffacts instruction-description
          (input-type word)
          (enum (name Operation)
                (children Arithmetic
                          Shift
                          Logical
                          Compare
                          Branch
                          Move)
                (cast-to byte)
                (mask 0b00000000000000000000000000000111)
                (shift 0)
                (field-name Control)
                (max-size "ArchitectureConstants::MaxInstructionCount"))
          (field (name Immediate)
                 (mask 0b00000000000000000000000000001000)
                 (shift 3)
                 (output-type bool))
          (enum (name LogicalOps)
                (children And
                          Or
                          Xor
                          Nand
                          Not)
                (cast-to byte)
                (max-size 8)
                (field-name ArithmeticFlagType)
                (shift 4)
                (mask 0b00000000000000000000000001110000))
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
                 (output-type byte)))
