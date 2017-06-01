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



(deffacts cisc0-base-decls
          (input-type Word)
          (title _cisc0_decl)
          (namespace cisc0))

(deffacts cisc0-core-fields
          (deffield Control 0b0000000000001111 0 Operation)
          (deffield Upper   0b1111111100000000 8 byte)
          (deffield Lower   0b0000000011111111 0 byte))

(deffacts cisc0-compare-fields
          (defbitfield CompareImmediateFlag 0b0010000000000000 13)
          (deffield CompareType             0b0000011100000000 8 CompareStyle)
          (deffield CompareRegister0        0b0000000000001111 0 byte)
          (deffield CompareRegister1        0b0000000011110000 4 byte)
          (deffield CompareImmediate        0b1111111100000000 8 byte))

(deffacts cisc0-arithmetic-fields
          (defbitfield ArithmeticFlagImmediate 0b0000000000010000 4)
          (deffield ArithmeticFlagType         0b0000000011100000 5 ArithmeticOps)
          (deffield ArithmeticImmediate        0b1111000000000000 12 RegisterValue)
          (deffield ArithmeticDestination      0b0000111100000000 8 byte)
          (deffield ArithmeticSource           0b1111000000000000 12 byte))

(deffacts cisc0-logical-fields
          (defbitfield LogicalFlagImmediate         0b0000000000010000 4)
          (defbitmask LogicalFlagImmediateMask      0b0000111100000000 8)
          (deffield LogicalImmediateDestination     0b1111000000000000 12 byte)
          (deffield LogicalFlagType                 0b0000000011100000 5 LogicalOps)
          (deffield LogicalRegister0                0b0000111100000000 8 byte)
          (deffield LogicalRegister1                0b1111000000000000 12 byte))

(deffacts cisc0-shift-fields
          (defbitfield ShiftFlagLeft        0b0000000000010000 4)
          (defbitfield ShiftFlagImmediate   0b0000000000100000 5)
          (deffield ShiftImmediate          0b1111100000000000 11 RegisterValue)
          (deffield ShiftRegister0          0b0000011110000000 7  byte)
          (deffield ShiftRegister1          0b0111100000000000 11 byte))

(deffacts cisc0-branch-fields
          (defbitfield BranchFlagIsConditional   0b0000000010000000 7)
          (defbitfield BranchFlagIsIfForm        0b0000000001000000 6)
          (defbitfield BranchFlagIsCallForm      0b0000000000100000 5)
          (defbitfield BranchFlagIsImmediate     0b0000000000010000 4)
          (deffield BranchIfOnTrue               0b0000111100000000 8  byte)
          (deffield BranchIfOnFalse              0b1111000000000000 12 byte)
          (deffield BranchIndirectDestination    0b1111000000000000 12 byte))

(deffacts cisc0-memory-fields
          (deffield MemoryFlagType         0b0000000000110000 4 MemoryOperation)
          (defbitmask MemoryFlagBitmask    0b0000111100000000 8)
          (defbitmask MemoryFlagIndirect   0b0000000001000000 6)
          (deffield MemoryOffset           0b1111000000000000 12 byte)
          (deffield MemoryRegister         0b1111000000000000 12 byte))

(deffacts cisc0-move-fields
          (defbitmask MoveBitmask 0b0000000011110000 4)
          (deffield MoveRegister0 0b0000111100000000 8 byte)
          (deffield MoveRegister1 0b1111000000000000 12 byte))

(deffacts cisc0-set-fields
          (defbitmask SetBitmask   0b0000000011110000 4)
          (deffield SetDestination 0b0000111100000000 8 byte))

(deffacts cisc0-swap-fields
          (deffield SwapDestination 0b0000111100000000 8 byte)
          (deffield SwapSource      0b1111000000000000 12 byte))

(deffacts cisc0-complex-fields
          (deffield ComplexSubClass           0b0000000011110000 4 ComplexSubTypes)
          (deffield ComplexClassEncoding_Type 0b0000011100000000 8 EncodingOperation)
          (deffield ComplexClassExtended_Type 0b0000011100000000 8 ExtendedOperation)
          (deffield ComplexClassExtended_Arg0 0b0111100000000000 11 byte))



(deffacts cisc0-enums
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
                          Rem
                          Min
                          Max)
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
                (children Encoding
                          Extended)
                (cast-to byte)
                (max-size "16"))
          (enum (name EncodingOperation)
                (children Encode
                          Decode
                          BitSet
                          BitUnset)
                (cast-to byte)
                (max-size "8"))
          (enum (name ExtendedOperation)
                (children PushValueAddr
                          PopValueAddr
                          PushRegisters
                          PopRegisters
                          IsEven)
                (cast-to byte)
                (max-size "8")))

(deffacts cisc0-file-layouts-and-requests
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
(defrule translate-flat-fact
         (declare (salience ?*priority:first*))
         ?f <- (deffield ?name
                         ?mask
                         ?shift
                         ?output-type)
         =>
         (retract ?f)
         (assert (field (name ?name)
                        (mask ?mask)
                        (shift ?shift)
                        (output-type ?output-type))))

(defrule translate-bit-fact
         (declare (salience ?*priority:first*))
         ?f <- (defbitfield ?name
                            ?mask
                            ?shift)
         =>
         (retract ?f)
         (assert (deffield ?name
                           ?mask
                           ?shift
                           bool)))

(defrule translate-bitmask-fact
         (declare (salience ?*priority:first*))
         ?f <- (defbitmask ?name
                           ?mask
                           ?shift)
         =>
         (retract ?f)
         (assert (deffield ?name
                           ?mask
                           ?shift
                           byte)))

