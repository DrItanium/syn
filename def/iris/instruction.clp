(deffacts iris16
          (input-type raw_instruction)
          (title _iris_decl)
          (namespace iris)
          (field (name Control)
                 (mask 0x000000FF)
                 (shift 0)
                 (output-type byte))
          (field (name Destination)
                 (mask 0x0000FF00)
                 (shift 8)
                 (output-type byte))
          (field (name Source0)
                 (mask 0x00FF0000)
                 (shift 16)
                 (output-type byte))
          (field (name Source1)
                 (mask 0xFF000000)
                 (shift 24)
                 (output-type byte))
          (field (name HalfImmediate)
                 (mask 0xFF000000)
                 (shift 24)
                 (output-type word))
          (field (name Immediate)
                 (mask 0xFFFF0000)
                 (shift 16)
                 (output-type word))
          (field (name Group)
                 (mask 0x00000007)
                 (shift 0)
                 (output-type byte))
          (field (name Operation)
                 (mask 0x000000F8)
                 (shift 3)
                 (output-type byte))
          (field (name PredicateResult)
                 (mask 0x00000F00)
                 (shift 8)
                 (output-type byte))
          (field (name PredicateInverseResult)
                 (mask 0x0000F000)
                 (shift 12)
                 (output-type byte))
          (field (name PredicateSource0)
                 (mask 0x000F0000)
                 (shift 16)
                 (output-type byte))
          (field (name PredicateSource1)
                 (mask 0x00F00000)
                 (shift 20)
                 (output-type byte))
          (field (name Lower4Bits)
                 (mask 0x0F)
                 (shift 0)
                 (input-type byte)
                 (output-type byte))
          (field (name Upper4Bits)
                 (mask 0xF0)
                 (shift 4)
                 (input-type byte)
                 (output-type byte))
          (field (name GroupByte)
                 (mask 0x00000007)
                 (shift 0)
                 (input-type byte)
                 (output-type byte))
          (field (name OperationByte)
                 (mask 0x000000F8)
                 (shift 3)
                 (input-type byte)
                 (output-type byte))
          (field (name DoubleOperation)
                 (mask 0b00000000000000000000001111111000)
                 (shift 3)
                 (output-type byte))
          (field (name DoubleDestination)
                 (mask 0b00000000000000111111100000000000)
                 (shift 11)
                 (output-type byte))
          (field (name DoubleSource0)
                 (mask 0b00000001111111000000000000000000)
                 (shift 18)
                 (output-type byte))
          (field (name DoubleSource1)
                 (mask 0b11111110000000000000000000000000)
                 (shift 25)
                 (output-type byte))
          (enum (name InstructionGroup)
                (children Arithmetic
                          Move
                          Jump
                          Compare
                          DoubleWord)
                (cast-to byte)
                (max-size "ArchitectureConstants::MaxGroups"))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name ArithmeticOp)
                (children Add
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
                          AddImmediate
                          SubImmediate
                          MulImmediate
                          DivImmediate
                          RemImmediate
                          ShiftLeftImmediate
                          ShiftRightImmediate
                          Min
                          Max))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name JumpOp)
                (children BranchUnconditionalImmediate
                          BranchUnconditionalImmediateLink
                          BranchUnconditional
                          BranchUnconditionalLink
                          BranchConditionalImmediate
                          BranchConditionalImmediateLink
                          BranchConditional
                          BranchConditionalLink
                          IfThenElse
                          IfThenElseLink
                          BranchUnconditionalLR
                          BranchUnconditionalLRAndLink
                          BranchConditionalLR
                          BranchConditionalLRAndLink
                          ))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name MoveOp)
                (children Move
                          Set
                          Swap
                          Load
                          LoadImmediate
                          LoadWithOffset
                          Store
                          Memset
                          StoreWithOffset
                          Push
                          PushImmediate
                          Pop
                          LoadCode
                          StoreCode
                          IOWrite
                          IORead
                          IOWriteWithOffset
                          IOReadWithOffset
                          MoveFromIP
                          MoveToIP
                          MoveFromLR
                          MoveToLR
                          ))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name CompareOp)
                (children Eq
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
                          SaveCRs
                          RestoreCRs
                          CRXor
                          CRNot
                          CRAnd
                          CROr
                          CRNand
                          CRNor
                          CRSwap
                          CRMove
                          ))
          (enum (name DoubleOperation)
                (cast-to byte)
                (max-size 128)
                (children Add
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
                          AddImmediate
                          SubImmediate
                          MulImmediate
                          DivImmediate
                          RemImmediate
                          ShiftLeftImmediate
                          ShiftRightImmediate
                          Min
                          Max
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
                          Move
                          Swap)))
