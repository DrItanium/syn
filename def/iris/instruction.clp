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
          (field (name Destination32)
                 (mask 0x00007F00)
                 (shift 8)
                 (output-type byte))
          (field (name Source0_32)
                 (mask 0x007F0000)
                 (shift 16)
                 (output-type byte))
          (field (name Source1_32)
                 (mask 0x7F000000)
                 (shift 24)
                 (output-type byte))
          (field (name Destination64)
                 (mask 0x00003F00)
                 (shift 8)
                 (output-type byte))
          (field (name Source0_64)
                 (mask 0x003F0000)
                 (shift 16)
                 (output-type byte))
          (field (name Source1_64)
                 (mask 0x3F000000)
                 (shift 24)
                 (output-type byte))
          (enum (name InstructionGroup)
                (children Arithmetic
                          Move
                          Jump
                          Compare
                          ConditionalRegister
                          Operation32
                          Operation64)
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
                          IfThenElseLinkPredFalse
                          UnconditionalJumpLinkRegister
                          UnconditionalJumpLinkRegisterLink
                          ConditionalTrueJumpLinkRegister
                          ConditionalTrueJumpLinkRegisterLink
                          ConditionalFalseJumpLinkRegister
                          ConditionalFalseJumpLinkRegisterLink))
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
                          MoveFromLinkRegister
                          MoveToLinkRegister))
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
                          GreaterThanOrEqualToImm))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name ConditionRegisterOp)
                (children SaveCRs
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
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name Instruction32BitOp)
                (children Add32
                          Sub32
                          Mul32
                          Div32
                          Remainder32
                          ShiftLeft32
                          ShiftRight32
                          And32
                          Or32
                          Xor32
                          Not32
                          Load32
                          Store32
                          LoadCode
                          StoreCode
                          Push32
                          Pop32
                          Swap32
                          BranchConditionalImmediate32
                          BranchConditionalRegister32
                          BranchUnconditionalImmediate32
                          BranchUnconditionalRegister32
                          Eq32
                          Neq32
                          LessThan32
                          GreaterThan32
                          LessThanOrEqualTo32
                          GreaterThanOrEqualTo32))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name Instruction64BitOp)
                (children Add64
                          Sub64
                          Mul64
                          Div64
                          Remainder64
                          ShiftLeft64
                          ShiftRight64
                          And64
                          Or64
                          Xor64
                          Not64
                          Load64
                          Store64
                          LoadCode
                          StoreCode
                          Push64
                          Pop64
                          Swap64
                          BranchConditionalImmediate64
                          BranchConditionalRegister64
                          BranchUnconditionalImmediate64
                          BranchUnconditionalRegister64
                          Eq64
                          Neq64
                          LessThan64
                          GreaterThan64
                          LessThanOrEqualTo64
                          GreaterThanOrEqualTo64))

          )

