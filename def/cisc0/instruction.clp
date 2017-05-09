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
          (field (name ArithmeticFlagType)      (mask 0b0000000011100000) (shift 5) (output-type ArithmeticOps))
          (field (name ArithmeticImmediate)     (mask 0b1111000000000000) (shift 12) (output-type RegisterValue))
          (field (name ArithmeticDestination)   (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name ArithmeticSource)        (mask 0b1111000000000000) (shift 12) (output-type byte)))

(deffacts cisc0-logical-fields
          (field (name LogicalFlagImmediate)        (mask 0b0000000000010000) (shift 4) (output-type bool))
          (field (name LogicalFlagImmediateMask)    (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name LogicalImmediateDestination) (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name LogicalFlagType)             (mask 0b0000000011100000) (shift 4) (output-type LogicalOps))
          (field (name LogicalRegister0)            (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name LogicalRegister1)            (mask 0b1111000000000000) (shift 12) (output-type byte)))

(deffacts cisc0-shift-fields
          (field (name ShiftFlagLeft)      (mask 0b0000000000010000) (shift 4) (output-type bool))
          (field (name ShiftFlagImmediate) (mask 0b0000000000100000) (shift 5) (output-type bool))
          (field (name ShiftImmediate)     (mask 0b1111100000000000) (shift 11) (output-type RegisterValue))
          (field (name ShiftRegister0)     (mask 0b0000011110000000) (shift 7) (output-type byte))
          (field (name ShiftRegister1)     (mask 0b0111100000000000) (shift 11) (output-type byte)))

(deffacts cisc0-branch-fields
          (defbitfield BranchFlagIsConditional   0b0000000010000000 7)
          (defbitfield BranchFlagIsIfForm        0b0000000001000000 6)
          (defbitfield BranchFlagIsCallForm      0b0000000000100000 5)
          (defbitfield BranchFlagIsImmediate     0b0000000000010000 4)
          (deffield BranchIfOnTrue               0b0000111100000000 8  byte)
          (deffield BranchIfOnFalse              0b1111000000000000 12 byte)
          (deffield BranchIndirectDestination    0b1111000000000000 12 byte))

(deffacts cisc0-memory-fields
          (field (name MemoryFlagType)     (mask 0b0000000000110000) (shift 4) (output-type MemoryOperation))
          (field (name MemoryFlagBitmask)  (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name MemoryFlagIndirect) (mask 0b0000000001000000) (shift 6) (output-type bool))
          (field (name MemoryOffset)       (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name MemoryRegister)     (mask 0b1111000000000000) (shift 12) (output-type byte))
          (field (name MemoryAddress)      (mask 0b0000000000001111) (shift 0) (output-type byte))
          (field (name MemoryValue)        (mask 0b0000000011110000) (shift 4) (output-type byte)))

(deffacts cisc0-move-fields
          (field (name MoveBitmask)   (mask 0b0000000011110000) (shift 4) (output-type byte))
          (field (name MoveRegister0) (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name MoveRegister1) (mask 0b1111000000000000) (shift 12) (output-type byte)))

(deffacts cisc0-set-fields
          (field (name SetBitmask)     (mask 0b0000000011110000) (shift 4) (output-type byte))
          (field (name SetDestination) (mask 0b0000111100000000) (shift 8) (output-type byte)))

(deffacts cisc0-swap-fields
          (field (name SwapDestination) (mask 0b0000111100000000) (shift 8) (output-type byte))
          (field (name SwapSource)      (mask 0b1111000000000000) (shift 12) (output-type byte)))

(deffacts cisc0-complex-fields
          (field (name ComplexSubClass)           (mask 0b0000000011110000) (shift 4) (output-type ComplexSubTypes))
          (field (name ComplexClassEncoding_Type) (mask 0b0000011100000000) (shift 8) (output-type EncodingOperation)))


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
                (children Encoding)
                (cast-to byte)
                (max-size "16"))
          (enum (name EncodingOperation)
                (children Encode
                          Decode
                          BitSet
                          BitUnset)
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
         (assert (field (name ?name)
                        (mask ?mask)
                        (shift ?shift)
                        (output-type bool))))
