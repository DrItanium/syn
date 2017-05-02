; syn
; Copyright (c) 2013-2017, Joshua Scoggins and Contributors
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



(deffacts cisc0
          (input-type Word)
          (title _cisc0_decl)
          (namespace cisc0)
          (field (name Control)
                 (mask 0b0000000000001111)
                 (shift 0)
                 (output-type Operation))
          (field (name Upper)
                 (mask 0b1111111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name Lower)
                 (mask 0b0000000011111111)
                 (shift 0)
                 (output-type byte))
          (field (name CompareImmediateFlag)
                 (mask 0b0010000000000000)
                 (shift 13)
                 (output-type bool))
          (field (name CompareType)
                 (mask 0b0000011100000000)
                 (shift 8)
                 (output-type CompareStyle))
          (field (name CompareRegister0)
                 (mask 0b0000000000001111)
                 (shift 0)
                 (output-type byte))
          (field (name CompareRegister1)
                 (mask 0b0000000011110000)
                 (shift 4)
                 (output-type byte))
          (field (name CompareImmediate)
                 (mask 0b1111111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name ArithmeticFlagImmediate)
                 (mask 0b0000000000010000)
                 (shift 4)
                 (output-type bool))
          (field (name ArithmeticFlagType)
                 (mask 0b0000000011100000)
                 (shift 5)
                 (output-type ArithmeticOps))
          (field (name ArithmeticImmediate)
                 (mask 0b1111000000000000)
                 (shift 12)
                 (output-type RegisterValue))
          (field (name ArithmeticDestination)
                 (mask 0b0000111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name ArithmeticSource)
                 (mask 0b1111000000000000)
                 (shift 12)
                 (output-type byte))
          (field (name ArithmeticSignature)
                 (mask 0b0000000011110000)
                 (shift 4)
                 (output-type byte))
          (field (name LogicalFlagImmediate)
                 (mask 0b0000000000010000)
                 (shift 4)
                 (output-type bool))
          (field (name LogicalFlagImmediateMask)
                 (mask 0b0000111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name LogicalImmediateDestination)
                 (mask 0b1111000000000000)
                 (shift 12)
                 (output-type byte))
          (field (name LogicalImmediateLower16)
                 (mask 0b1111111111111111)
                 (shift 0)
                 (output-type Word))
          (field (name LogicalImmediateUpper16)
                 (mask 0b1111111111111111)
                 (shift 0)
                 (output-type Word))
          (field (name LogicalFlagType)
                 (mask 0b0000000011100000)
                 (shift 4)
                 (output-type LogicalOps))
          (field (name LogicalRegister0)
                 (mask 0b0000111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name LogicalRegister1)
                 (mask 0b1111000000000000)
                 (shift 12)
                 (output-type byte))
          (field (name LogicalSignature)
                 (mask 0b0000111111110000)
                 (shift 4)
                 (output-type byte))
          (field (name LogicalImmediateError)
                 (mask 0b0000000010000000)
                 (shift 7)
                 (output-type bool))
          (field (name LogicalIndirectError)
                 (mask 0b0000111100000000)
                 (shift 8)
                 (output-type bool))
          (field (name ShiftFlagLeft)
                 (mask 0b0000000000010000)
                 (shift 4)
                 (output-type bool))
          (field (name ShiftFlagImmediate)
                 (mask 0b0000000000100000)
                 (shift 5)
                 (output-type bool))
          (field (name ShiftImmediate)
                 (mask 0b1111100000000000)
                 (shift 11)
                 (output-type RegisterValue))
          (field (name ShiftRegister0)
                 (mask 0b0000011110000000)
                 (shift 7)
                 (output-type byte))
          (field (name ShiftRegister1)
                 (mask 0b0111100000000000)
                 (shift 11)
                 (output-type byte))
          (field (name BranchFlags)
                 (mask 0b0000000011110000)
                 (shift 4)
                 (output-type byte))
          (field (name BranchFlagIsConditional)
                 (mask 0b0000000010000000)
                 (shift 7)
                 (output-type bool))
          (field (name BranchFlagIsIfForm)
                 (mask 0b0000000001000000)
                 (shift 6)
                 (output-type bool))
          (field (name BranchFlagIsCallForm)
                 (mask 0b0000000000100000)
                 (shift 5)
                 (output-type bool))
          (field (name BranchFlagIsImmediate)
                 (mask 0b0000000000010000)
                 (shift 4)
                 (output-type bool))
          (field (name BranchIfOnTrue)
                 (mask 0b0000111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name BranchIfOnFalse)
                 (mask 0b1111000000000000)
                 (shift 12)
                 (output-type byte))
          (field (name BranchIndirectDestination)
                 (mask 0b1111000000000000)
                 (shift 12)
                 (output-type byte))
          (field (name MemoryFlagType)
                 (mask 0b0000000000110000)
                 (shift 4)
                 (output-type MemoryOperation))
          (field (name MemoryFlagBitmask)
                 (mask 0b0000111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name MemoryFlagIndirect)
                 (mask 0b0000000001000000)
                 (shift 6)
                 (output-type bool))
          (field (name MemoryOffset)
                 (mask 0b1111000000000000)
                 (shift 12)
                 (output-type byte))
          (field (name MemoryRegister)
                 (mask 0b1111000000000000)
                 (shift 12)
                 (output-type byte))
          (field (name MemorySignature)
                 (mask 0b0000111111110000)
                 (shift 4)
                 (output-type byte))
          (field (name MemoryAddress)
                 (mask 0b0000000000001111)
                 (shift 0)
                 (output-type byte))
          (field (name MemoryValue)
                 (mask 0b0000000011110000)
                 (shift 4)
                 (output-type byte))
          (field (name MoveBitmask)
                 (mask 0b0000000011110000)
                 (shift 4)
                 (output-type byte))
          (field (name MoveRegister0)
                 (mask 0b0000111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name MoveRegister1)
                 (mask 0b1111000000000000)
                 (shift 12)
                 (output-type byte))
          (field (name MoveSignature)
                 (mask 0b0000000011110000)
                 (shift 4)
                 (output-type byte))
          (field (name SetBitmask)
                 (mask 0b0000000011110000)
                 (shift 4)
                 (output-type byte))
          (field (name SetDestination)
                 (mask 0b0000111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name SetSignature)
                 (mask 0b0000111111110000)
                 (shift 4)
                 (output-type byte))
          (field (name SwapDestination)
                 (mask 0b0000111100000000)
                 (shift 8)
                 (output-type byte))
          (field (name SwapSource)
                 (mask 0b1111000000000000)
                 (shift 12)
                 (output-type byte))
          (field (name ComplexSubClass)
                 (mask 0b0000000011110000)
                 (shift 4)
                 (output-type ComplexSubTypes))
          (field (name ComplexClassEncoding_Type)
                 (mask 0b0000011100000000)
                 (shift 8)
                 (output-type EncodingOperation))
          (enum (name Operation)
                (children Memory
                          Arithmetic
                          Shift
                          Logical
                          Compare
                          Branch
                          Move
                          Set
                          Swap
                          Complex)
                (cast-to byte)
                (max-size "ArchitectureConstants::MaxInstructionCount"))
          (enum (name ArithmeticOps)
                (children Add
                          Sub
                          Mul
                          Div
                          Rem)
                (cast-to byte)
                (max-size "8"))
          (enum (name CompareStyle)
                (children Equals
                          NotEquals
                          LessThan
                          GreaterThan
                          LessThanOrEqualTo
                          GreaterThanOrEqualTo)
                (cast-to byte)
                (max-size "8"))
          (enum (name LogicalOps)
                (children And
                          Or
                          Xor
                          Nand
                          Not)
                (cast-to byte)
                (max-size "8"))
          (enum (name MemoryOperation)
                (children Load
                          Store
                          Push
                          Pop)
                (cast-to byte)
                (max-size "4"))
          (enum (name ComplexSubTypes)
                (children Encoding)
                (cast-to byte)
                (max-size "16"))
          (enum (name EncodingOperation)
                (children Encode
                          Decode
                          BitSet
                          BitUnset)
                (cast-to byte)
                (max-size "8"))
          (include "ExecutionUnits.h")
          (using ALUOperation
                 syn::ALU::StandardOperations)
          (to-execution-unit ArithmeticOps Add ->
                             ALUOperation Add)
          (to-execution-unit ArithmeticOps Sub ->
                             ALUOperation Subtract)
          (to-execution-unit ArithmeticOps Mul ->
                             ALUOperation Multiply)
          (to-execution-unit ArithmeticOps Div ->
                             ALUOperation Divide)
          (to-execution-unit ArithmeticOps Rem ->
                             ALUOperation Remainder)
          (to-execution-unit LogicalOps Not ->
                             ALUOperation UnaryNot)
          (to-execution-unit LogicalOps Or ->
                             ALUOperation BinaryOr)
          (to-execution-unit LogicalOps And ->
                             ALUOperation BinaryAnd)
          (to-execution-unit LogicalOps Xor ->
                             ALUOperation BinaryXor)
          (to-execution-unit LogicalOps Nand ->
                             ALUOperation BinaryNand)
          (using CompareUnitOperation
                 syn::Comparator::StandardOperations)
          (to-execution-unit CompareStyle Equals ->
                             CompareUnitOperation Eq)
          (to-execution-unit CompareStyle NotEquals ->
                             CompareUnitOperation Neq)
          (to-execution-unit CompareStyle LessThan ->
                             CompareUnitOperation LessThan)
          (to-execution-unit CompareStyle LessThanOrEqualTo ->
                             CompareUnitOperation LessThanOrEqualTo)
          (to-execution-unit CompareStyle GreaterThan ->
                             CompareUnitOperation GreaterThan)
          (to-execution-unit CompareStyle GreaterThanOrEqualTo ->
                             CompareUnitOperation GreaterThanOrEqualTo)

          )
