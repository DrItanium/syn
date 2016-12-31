
(deffacts cisc0
          (input-type Word)
          (title _cisc0_decl)
          (namespace cisc0)
          (field (name Control) (mask 0b0000000000001111) (shift 0) (output-type Operation))
          (field (name Upper) (mask 0b1111111100000000) (shift 8) (output-type byte))
          (field (name Lower) (mask 0b0000000011111111) (shift 0) (output-type byte))
          (field (name CompareImmediateFlag) (mask 0b0010000000000000) (shift 0) (output-type bool))
          (field (name CompareCombineFlag) (mask 0b0001100000000000) (shift 11) (output-type CompareCombine))
          (field (name CompareType) (mask 0b0000011100000000) (shift 8) (output-type CompareStyle))
          (field (name CompareRegister0) (mask 0b0000000000001111) (shift 0) (output-type byte))
          (field (name CompareRegister1) (mask 0b0000000011110000) (shift 4) (output-type byte))
          (field (name CompareImmediate) (mask 0b1111111100000000) (shift 8) (output-type byte))
          (field (name ArithmeticFlagImmediate) (mask 0b0000000000010000) (shift 4) (output-type bool))
          (field (name ArithmeticFlagType) (mask 0b0000000011100000) (shift 5) (output-type ArithmeticOps))
          (field (name ArithmeticImmediate) (mask 0b1111000000000000) (shift 12) (output-type RegisterValue))
          (field (name ArithmeticDestination) (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name ArithmeticSource) (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name ArithmeticSignature) (mask 0b0000000011110000) (shift 4) (output-type byte))
          (field (name LogicalFlagImmediate) (mask 0b0000000000010000) (shift 4) (output-type bool))
          (field (name LogicalFlagImmediateType) (mask 0b0000000001100000) (shift 5) (output-type ImmediateLogicalOps))
          (field (name LogicalFlagImmediateMask) (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name LogicalImmediateDestination) (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name LogicalImmediateLower16) (mask 0b1111111111111111) (shift 0) (output-type Word))
          (field (name LogicalImmediateUpper16) (mask 0b1111111111111111) (shift 0) (output-type Word))
          (field (name LogicalFlagType) (mask 0b0000000011100000) (shift 4) (output-type LogicalOps))
          (field (name LogicalRegister0) (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name LogicalRegister1) (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name LogicalSignature) (mask 0b0000111111110000) (shift 4) (output-type byte))
          (field (name LogicalImmediateError) (mask 0b0000000010000000) (shift 7) (output-type bool))
          (field (name LogicalIndirectError) (mask 0b0000111100000000) (shift 8) (output-type bool))
          (field (name ShiftFlagLeft) (mask 0b0000000000010000) (shift 4) (output-type bool))
          (field (name ShiftFlagImmediate) (mask 0b0000000000100000) (shift 5) (output-type bool))
          (field (name ShiftImmediate) (mask 0b1111100000000000) (shift 11) (output-type RegisterValue))
          (field (name ShiftRegister0) (mask 0b0000011110000000) (shift 7) (output-type byte))
          (field (name ShiftRegister1) (mask 0b0111100000000000) (shift 11) (output-type byte))
          (field (name BranchFlags) (mask 0b0000000011110000) (shift 4) (output-type byte))
          (field (name BranchFlagIsConditional) (mask 0b0000000010000000) (shift 7) (output-type bool))
          (field (name BranchFlagIsIfForm) (mask 0b0000000001000000) (shift 6) (output-type bool))
          (field (name BranchFlagIsCallForm) (mask 0b0000000000100000) (shift 5) (output-type bool))
          (field (name BranchFlagIsImmediate) (mask 0b0000000000010000) (shift 4) (output-type bool))
          (field (name BranchIfOnTrue) (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name BranchIfOnFalse) (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name BranchIndirectDestination) (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name MemoryFlagType) (mask 0b0000000000110000) (shift 4) (output-type MemoryOperation))
          (field (name MemoryFlagBitmask) (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name MemoryFlagIndirect) (mask 0b0000000001000000) (shift 6) (output-type bool))
          (field (name MemoryFlagReadNextWord) (mask 0b0000000010000000) (shift 7) (output-type bool))
          (field (name MemoryOffset) (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name MemoryRegister) (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name MemorySignature) (mask 0b0000111111110000) (shift 4) (output-type byte))
          (field (name MemoryAddress) (mask 0b0000000000001111) (shift 0) (output-type byte))
          (field (name MemoryValue) (mask 0b0000000011110000) (shift 4) (output-type byte))
          (field (name MoveBitmask) (mask 0b0000000011110000) (shift 4) (output-type byte))
          (field (name MoveRegister0) (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name MoveRegister1) (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name MoveSignature) (mask 0b0000000011110000) (shift 4) (output-type byte))
          (field (name SetBitmask) (mask 0b0000000011110000) (shift 4) (output-type byte))
          (field (name SetDestination) (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name SetSignature) (mask 0b0000111111110000) (shift 4) (output-type byte))
          (field (name SwapDestination) (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name SwapSource) (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name SystemAction) (mask 0b0000001111110000) (shift 4) (output-type byte))
          (field (name SystemArg0) (mask 0b0011110000000000) (shift 8) (output-type byte))
          (field (name ComplexSubClass) (mask 0b0000000011110000) (shift 4) (output-type ComplexSubTypes))
          (field (name ComplexClassEncoding_Type) (mask 0b0000011100000000) (shift 8) (output-type EncodingOperation))
          (enum (name Operation) (children
                                   Memory
                                   Arithmetic
                                   Shift
                                   Logical
                                   Compare
                                   Branch
                                   SystemCall
                                   Move
                                   Set
                                   Swap
                                   Complex
                                   ) (cast-to byte) (max-size "ArchitectureConstants::MaxInstructionCount"))
          (enum (name ArithmeticOps) (children
                                       Add
                                       Sub
                                       Mul
                                       Div
                                       Rem
                                       ) (cast-to byte) (max-size "8"))
          (enum (name CompareCombine) (children
                                        None
                                        And
                                        Or
                                        Xor
                                        ) (cast-to byte) (max-size "4"))
          (enum (name CompareStyle) (children
                                      Equals
                                      NotEquals
                                      LessThan
                                      GreaterThan
                                      LessThanOrEqualTo
                                      GreaterThanOrEqualTo
                                      ) (cast-to byte) (max-size "8"))
          (enum (name ImmediateLogicalOps) (children
                                             And
                                             Or
                                             Xor
                                             Nand
                                             ) (cast-to byte) (max-size "4"))
          (enum (name LogicalOps) (children
                                    And
                                    Or
                                    Xor
                                    Nand
                                    Not
                                    ) (cast-to byte) (max-size "8"))
          (enum (name MemoryOperation) (children
                                         Load
                                         Store
                                         Push
                                         Pop
                                         ) (cast-to byte) (max-size "4"))
          (enum (name ComplexSubTypes) (children
                                         Encoding
                                         ) (cast-to byte) (max-size "16"))
          (enum (name EncodingOperation) (children
                                           Encode
                                           Decode
                                           BitSet
                                           BitUnset
                                           ) (cast-to byte) (max-size "8"))
          )
