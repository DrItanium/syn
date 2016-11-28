(deffacts iris20
          (input-type InstructionAtom)
          (title _iris20_decl)
          (namespace iris20)
          (field (name MoleculeContainsOneInstruction)
                 (mask 0x0000000000000001)
                 (shift 0)
                 (input-type InstructionMolecule)
                 (output-type bool))
          (field (name FirstAtom)
                 (mask 0x00000000FFFFFFFF)
                 (shift 0)
                 (input-type InstructionMolecule)
                 (output-type InstructionAtom))
          (field (name SecondAtom)
                 (mask 0xFFFFFFFF00000000)
                 (shift 32)
                 (input-type InstructionMolecule)
                 (output-type InstructionAtom))
          (field (name Immediate48)
                 (mask 0xFFFFFFFFFFFF0000)
                 (shift 16)
                 (input-type InstructionMolecule)
                 (output-type word))
          (field (name Immediate32)
                 (mask 0xFFFFFFFF00000000)
                 (shift 32)
                 (input-type InstructionMolecule)
                 (output-type word))
          (field (name SectionDescriptor)
                 (mask 0b11000000)
                 (shift 6)
                 (input-type byte)
                 (output-type SectionType))
          (field (name SectionIndex)
                 (mask 0b00111111)
                 (shift 0)
                 (input-type byte)
                 (output-type byte))
          (field (name Control)
                 (mask 0x000000FF)
                 (shift 0)
                 (output-type byte))
          (field (name Destination)
                 (mask 0x000FF00)
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
                 (output-type InstructionImmediate))
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
          (enum (name SectionType)
                (cast-to byte)
                (max-size "4")
                (children Register
                          Stack
                          Memory
                          Undefined))
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
                (name MiscOp)
                (children SystemCall))
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
                (name SystemCalls)
                (children Terminate
                          GetC
                          PutC
                          InitializeXMem))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name MoveOp)
                (children Move
                          Set16
                          Set32
                          Set48
                          Swap))
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

