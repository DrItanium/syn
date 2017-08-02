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
          (deffield Group                  0x00000007 0  byte)
          (deffield Operation              0x000000F8 3  byte)
          (deffield Destination            0x0000FF00 8  byte)
          (deffield Source0                0x00FF0000 16 byte)
          (deffield Source1                0xFF000000 24 byte)
          (deffield HalfImmediate          0xFF000000 24 word)
          (deffield Immediate              0xFFFF0000 16 word)
          (deffield PredicateResult        0x00000F00 8  byte)
          (deffield PredicateInverseResult 0x0000F000 12 byte)
          (deffield PredicateSource0       0x000F0000 16 byte)
          (deffield PredicateSource1       0x00F00000 20 byte)
          (deffield Lower4Bits             0x0F       0  byte input-type: byte)
          (deffield Upper4Bits             0xF0       4  byte input-type: byte))
(deffacts iris-error-state
          (deffield StatusInError            0b0000000000000001 0 bool input-type: word)
          (deffield StatusDivideByZero       0b0000000000000010 1 bool input-type: word)
          (deffield StatusIllegalInstruction 0b0000000000000100 2 bool input-type: word))
(deffacts iris-enums-and-keyword-generators
          (defenum-and-translation InstructionGroup
                                   "ArchitectureConstants::MaxGroups"
                                   byte
                                   desc/iris/InstructionGroup.desc
                                   entries:
                                   arithmetic Arithmetic
                                   move Move
                                   jump Jump
                                   compare Compare
                                   predicate ConditionalRegister
                                   unused0 Unused0
                                   reserved CustomInstructionReserved)
          (defenum-and-translation:iris-operation-group ArithmeticOp
                                                        entries:
                                                        add Add
                                                        sub Sub
                                                        mul Mul
                                                        div Div
                                                        rem Rem
                                                        shl ShiftLeft
                                                        shr ShiftRight
                                                        and BinaryAnd
                                                        or BinaryOr
                                                        not BinaryNot
                                                        xor BinaryXor
                                                        nand BinaryNand
                                                        nor BinaryNor
                                                        addi AddImmediate
                                                        subi SubImmediate
                                                        muli MulImmediate
                                                        divi DivImmediate
                                                        remi RemImmediate
                                                        shli ShiftLeftImmediate
                                                        shri ShiftRightImmediate
                                                        min Min
                                                        max Max)
          (defenum-and-translation:iris-operation-group JumpOp
                                                        entries:
                                                        bi BranchUnconditionalImmediate
                                                        bil BranchUnconditionalImmediateLink
                                                        b BranchUnconditional
                                                        bl BranchUnconditionalLink
                                                        bci BranchConditionalImmediate
                                                        bcil BranchConditionalImmediateLink
                                                        bc BranchConditional
                                                        bcl BranchConditionalLink
                                                        blr BranchUnconditionalLR
                                                        blrl BranchUnconditionalLRAndLink
                                                        bclr BranchConditionalLR
                                                        bclrl BranchConditionalLRAndLink
                                                        rfe ReturnFromError)
          (defenum-and-translation:iris-operation-group MoveOp
                                                        entries:
                                                        move Move
                                                        set Set
                                                        swap Swap
                                                        ld Load
                                                        ldi LoadImmediate
                                                        ldwo LoadWithOffset
                                                        st Store
                                                        sti StoreImmediate
                                                        stwo StoreWithOffset
                                                        push Push
                                                        pushi PushImmediate
                                                        pop Pop
                                                        ldc LoadCode
                                                        stc StoreCode
                                                        ldio LoadIO
                                                        stio StoreIO
                                                        ldiowo LoadIOWithOffset
                                                        stiowo StoreIOWithOffset
                                                        mfip MoveFromIP
                                                        mtip MoveToIP
                                                        mflr MoveFromLR
                                                        mtlr MoveToLR
                                                        sregs SaveAllRegisters
                                                        rregs RestoreAllRegisters)
          (defenum-and-translation:iris-operation-group CompareOp
                                                        entries:
                                                        eq Eq
                                                        eqi EqImmediate
                                                        ne Neq
                                                        nei NeqImmediate
                                                        lt LessThan
                                                        lti LessThanImmediate
                                                        gt GreaterThan
                                                        gti GreaterThanImmediate
                                                        le LessThanOrEqualTo
                                                        lei LessThanOrEqualToImmediate
                                                        ge GreaterThanOrEqualTo
                                                        gei GreaterThanOrEqualToImmediate)
          (defenum-and-translation:iris-operation-group ConditionRegisterOp
                                                        entries:
                                                        psave SaveCRs
                                                        prestore RestoreCRs
                                                        pxor CRXor
                                                        pnot CRNot
                                                        pand CRAnd
                                                        por CROr
                                                        pnand CRNand
                                                        pnor CRNor
                                                        pswap CRSwap
                                                        pmove CRMove))




(defrule MAIN::translate-iris-operation-group
         (declare (salience ?*priority:first*))
         ?f <- (defenum-and-translation:iris-operation-group ?title
                                                             entries:
                                                             $?entries)
         =>
         (retract ?f)
         (assert (defenum-and-translation ?title
                                          "ArchitectureConstants::MaxOperations"
                                          byte
                                          (sym-cat desc/iris/ ?title .desc)
                                          entries:
                                          ?entries)))


(deffacts MAIN::execution-unit-conversion-routines
          (include "ExecutionUnits.h")
          (using ALUOperation
                 syn::ALU::StandardOperations)
          (using ComparatorOp
                 syn::Comparator::StandardOperations)
          (using CRUnitOp
                 syn::Comparator::BooleanOperations)
          (defspecial-execution-unit-converter arithmetic->alu
                                               ArithmeticOp
                                               ALUOperation)
          (defspecial-execution-unit-converter compare->compareop
                                               CompareOp
                                               ComparatorOp)
          (defspecial-execution-unit-converter predop->crop
                                               ConditionRegisterOp
                                               CRUnitOp)
          (arithmetic->alu Add
                           Add)
          (arithmetic->alu AddImmediate
                           Add)
          (arithmetic->alu Sub
                           Subtract)
          (arithmetic->alu SubImmediate
                           Subtract)
          (arithmetic->alu Mul
                           Multiply)
          (arithmetic->alu MulImmediate
                           Multiply)
          (arithmetic->alu Div
                           Divide)
          (arithmetic->alu DivImmediate
                           Divide)
          (arithmetic->alu Rem
                           Remainder)
          (arithmetic->alu RemImmediate
                           Remainder)
          (arithmetic->alu ShiftLeft
                           ShiftLeft)
          (arithmetic->alu ShiftLeftImmediate
                           ShiftLeft)
          (arithmetic->alu ShiftRight
                           ShiftRight)
          (arithmetic->alu ShiftRightImmediate
                           ShiftRight)
          (arithmetic->alu BinaryNot
                           UnaryNot)
          (arithmetic->alu BinaryOr
                           BinaryOr)
          (arithmetic->alu BinaryAnd
                           BinaryAnd)
          (arithmetic->alu BinaryXor
                           BinaryXor)
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
