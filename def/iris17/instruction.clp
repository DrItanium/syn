(deffacts iris17
          (input-type word)
          (title _iris17_decl)
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
                 (output-type hword))
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
                (max-size "ArchitectureConstants::MaxInstructionsPerGroup")
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
                (max-size "ArchitectureConstants::MaxInstructionsPerGroup")
                (name MiscOp)
                (children SystemCall))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxInstructionsPerGroup")
                (name JumpOp)
                (children UnconditionalRegister
                          UnconditionalRegisterLink
                          ConditionalTrueRegister
                          ConditionalTrueRegisterLink
                          ConditionalFalseRegister
                          ConditionalFalseRegisterLink
                          IfThenElseNormalPredTrue
                          IfThenElseNormalPredFalse
                          IfThenElseLinkPredTrue
                          IfThenElseLinkPredFalse))
          (enum (cast-to byte)
                (max-size 255)
                (name SystemCalls)
                (children Terminate
                          GetC
                          PutC))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxInstructionsPerGroup")
                (name MoveOp)
                (children Move
                          SetLower
                          SetUpper
                          Swap
                          Load
                          Store
                          Push
                          Pop))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxInstructionsPerGroup")
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

