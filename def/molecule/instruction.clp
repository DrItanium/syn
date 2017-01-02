(deffacts molecule
          (input-type InstructionAtom)
          (title _molecule_decl)
          (namespace molecule)
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
          (field (name Operation)
                 (mask 0x000000FE)
                 (shift 1)
                 (output-type Operation))
          (field (name MoleculeOperation)
                 (mask 0x00000000000000FE)
                 (shift 1)
                 (output-type Operation)
                 (input-type InstructionMolecule))
          (field (name MoleculeDestination)
                 (mask 0x000000000000FF00)
                 (shift 8)
                 (output-type byte)
                 (input-type InstructionMolecule))
          (field (name MoleculeSource0)
                 (mask 0x0000000000FF0000)
                 (shift 16)
                 (output-type byte)
                 (input-type InstructionMolecule))
          (field (name MoleculeSource1)
                 (mask 0x00000000FF000000)
                 (shift 24)
                 (output-type byte)
                 (input-type InstructionMolecule))
          (enum (name SectionType)
                (cast-to byte)
                (max-size "4")
                (children Register
                          Stack
                          Memory
                          Undefined))
          (enum (cast-to byte)
                (max-size "ArchitectureConstants::MaxOperations")
                (name Operation)
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
                          ShiftRightImmediate
                          BranchUnconditionalImmediate
                          BranchUnconditionalImmediateLink
                          BranchUnconditionalRegister
                          BranchUnconditionalRegisterLink
                          BranchConditionalTrueImmediate
                          BranchConditionalTrueImmediateLink
                          BranchConditionalTrueRegister
                          BranchConditionalTrueRegisterLink
                          BranchConditionalFalseImmediate
                          BranchConditionalFalseImmediateLink
                          BranchConditionalFalseRegister
                          BranchConditionalFalseRegisterLink
                          BranchIfThenElseNormalPredTrue
                          BranchIfThenElseNormalPredFalse
                          BranchIfThenElseLinkPredTrue
                          BranchIfThenElseLinkPredFalse
                          Move
                          Set16
                          Set32
                          Set48
                          Swap
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
                          BinaryAndImmediate
                          BinaryOrImmediate
                          BinaryXorImmediate
                          BinaryNand
                          BinaryNandImmediate
                          BranchUnconditionalImmediate32
                          BranchUnconditionalImmediate32Link
                          BranchConditionalTrueImmediate32
                          BranchConditionalTrueImmediate32Link
                          BranchConditionalFalseImmediate32
                          BranchConditionalFalseImmediate32Link
                          BranchUnconditionalImmediate48
                          BranchUnconditionalImmediate48Link
                          BranchConditionalTrueImmediate48
                          BranchConditionalTrueImmediate48Link
                          BranchConditionalFalseImmediate48
                          BranchConditionalFalseImmediate48Link
                          )))

