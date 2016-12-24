(deffacts iris16
          (input-type word)
          (title _iris16_decl)
          (namespace iris16)
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
                 (output-type byte))
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
          (enum (name InstructionGroup)
                (children Arithmetic
                          Move
                          Jump
                          Compare
                          Misc)
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
                          AddImmediate
                          SubImmediate
                          MulImmediate
                          DivImmediate
                          RemImmediate
                          ShiftLeftImmediate
                          ShiftRightImmediate))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name JumpOp)
                (children UnconditionalImmediate
                          UnconditionalImmediateLink
                          UnconditionalRegister
                          UnconditionalRegisterLink
                          ConditionalTrueImmediate
                          ConditionalTrueImmediateLink
                          ConditionalTrueRegister
                          ConditionalTrueRegisterLink
                          ConditionalFalseImmediate
                          ConditionalFalseImmediateLink
                          ConditionalFalseRegister
                          ConditionalFalseRegisterLink
                          IfThenElseNormalPredTrue
                          IfThenElseNormalPredFalse
                          IfThenElseLinkPredTrue
                          IfThenElseLinkPredFalse))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name MoveOp)
                (children Move
                          Set
                          Swap
                          Load
                          LoadImmediate
                          Store
                          Memset
                          Push
                          PushImmediate
                          Pop
                          LoadCode
                          StoreCode
                          IOWrite
                          IORead))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name CompareOp)
                (children Eq
                          EqImm
                          Neq
                          NeqImm
                          LessThan
                          LessThanImm
                          GreaterThan
                          GreaterThanImm
                          LessThanOrEqualTo
                          LessThanOrEqualToImm
                          GreaterThanOrEqualTo
                          GreaterThanOrEqualToImm)))

