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


(deffacts iris-base-decls
          (input-type raw_instruction)
          (title _iris_decl)
          (namespace iris))
(deffacts iris-instruction-fields
          (field (name Group)
                 (mask 0x00000007)
                 (shift 0)
                 (output-type byte))
          (field (name Operation)
                 (mask 0x000000F8)
                 (shift 3)
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
                 (output-type byte)))
(deffacts iris-error-state
          (field (name StatusInError)
                 (mask 0b0000000000000001)
                 (shift 0)
                 (input-type word)
                 (output-type bool))
          (field (name StatusDivideByZero)
                 (mask 0b0000000000000010)
                 (shift 1)
                 (input-type word)
                 (output-type bool))
          (field (name StatusIllegalInstruction)
                 (mask 0b0000000000000100)
                 (shift 2)
                 (input-type word)
                 (output-type bool)))
(deffacts iris-enums
          (enum (name InstructionGroup)
                (children Arithmetic
                          Move
                          Jump
                          Compare
                          ConditionalRegister
                          Unused0
                          CustomInstructionReserved)
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
                          Max
                          ))
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
                          ReturnFromError))
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
                          StoreImmediate
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
                          SaveAllRegisters
                          RestoreAllRegisters))
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
                          GreaterThanOrEqualToImmediate))
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
          )
(defrule MAIN::perform-special-conversion:arithmetic->aluoperation
         (declare (salience ?*priority:first*))
         ?f <- (arithmetic->alu ?arith ?alu)
         =>
         (retract ?f)
         (assert (to-execution-unit ArithmeticOp ?arith ->
                                    ALUOperation ?alu)))

(defrule MAIN::perform-special-conversion:compare->compareop
         (declare (salience ?*priority:first*))
         ?f <- (compare->compareop ?cop ?comparator)
         =>
         (retract ?f)
         (assert (to-execution-unit CompareOp ?cop ->
                                    ComparatorOp ?comparator)))

(defrule MAIN::perform-special-conversion:predop->crop
         (declare (salience ?*priority:first*))
         ?f <- (predop->crop ?cop ?comparator)
         =>
         (retract ?f)
         (assert (to-execution-unit ConditionRegisterOp ?cop ->
                                    CRUnitOp ?comparator)))

(deffacts MAIN::execution-unit-conversion-routines
          (include "ExecutionUnits.h")
          (using ALUOperation
                 syn::ALU::StandardOperations)
          (arithmetic->alu Add Add)
          (arithmetic->alu AddImmediate Add)
          (arithmetic->alu Sub Subtract)
          (arithmetic->alu SubImmediate Subtract)
          (arithmetic->alu Mul Multiply)
          (arithmetic->alu MulImmediate Multiply)
          (arithmetic->alu Div Divide)
          (arithmetic->alu DivImmediate Divide)
          (arithmetic->alu Rem Remainder)
          (arithmetic->alu RemImmediate Remainder)
          (arithmetic->alu ShiftLeft ShiftLeft)
          (arithmetic->alu ShiftLeftImmediate ShiftLeft)
          (arithmetic->alu ShiftRight ShiftRight)
          (arithmetic->alu ShiftRightImmediate ShiftRight)
          (arithmetic->alu BinaryNot UnaryNot)
          (arithmetic->alu BinaryOr BinaryOr)
          (arithmetic->alu BinaryAnd BinaryAnd)
          (arithmetic->alu BinaryXor BinaryXor)
          (using ComparatorOp
                 syn::Comparator::StandardOperations)
          (compare->compareop LessThan
                              LessThan)
          (compare->compareop LessThanImmediate
                              LessThan)
          (compare->compareop LessThanOrEqualTo
                              LessThanOrEqualTo)
          (compare->compareop LessThanOrEqualToImmediate
                              LessThanOrEqualTo)
          (compare->compareop GreaterThan
                              GreaterThan)
          (compare->compareop GreaterThanImmediate
                              GreaterThan)
          (compare->compareop GreaterThanOrEqualTo
                              GreaterThanOrEqualTo)
          (compare->compareop GreaterThanOrEqualToImmediate
                              GreaterThanOrEqualTo)
          (compare->compareop Eq
                              Eq)
          (compare->compareop EqImmediate
                              Eq)
          (compare->compareop Neq
                              Neq)
          (compare->compareop NeqImmediate
                              Neq)
          (using CRUnitOp
                 syn::Comparator::BooleanOperations)
          (predop->crop CRAnd
                        BinaryAnd)
          (predop->crop CROr
                        BinaryOr)
          (predop->crop CRNand
                        BinaryNand)
          (predop->crop CRNor
                        BinaryNor)
          (predop->crop CRXor
                        BinaryXor)
          (predop->crop CRNot
                        UnaryNot))
