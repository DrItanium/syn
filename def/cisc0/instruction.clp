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
          ; the first defines everything else we need to know
          (defbitfield CompareFlagImmediate 0b0000000010000000 7)
          (defsubtypefield CompareType      0b0000000001110000 4 CompareStyle)
          (deffield CompareDestination      0b0000111100000000 8 byte)
          (deffield CompareSource           0b1111000000000000 12 byte)
          (defbitmask CompareBitmask        0b1111000000000000 12))

(deffacts cisc0-arithmetic-fields
          (defbitfield ArithmeticFlagImmediate 0b0000000000010000 4)
          (defsubtypefield ArithmeticType      0b0000000011100000 5 ArithmeticOps)
          (deffield ArithmeticDestination      0b0000111100000000 8 byte)
          (deffield ArithmeticImmediate        0b1111000000000000 12 byte)
          (deffield ArithmeticSource           0b1111000000000000 12 byte))

(deffacts cisc0-logical-fields
          (defbitfield LogicalFlagImmediate      0b0000000000010000 4)
          (defsubtypefield LogicalType           0b0000000011100000 5 LogicalOps)
          (deffield LogicalDestination           0b0000111100000000 8 byte)
          (defbitmask LogicalBitmask             0b1111000000000000 12)
          (deffield LogicalSource                0b1111000000000000 12 byte))

(deffacts cisc0-shift-fields
          (defbitfield ShiftFlagLeft        0b0000000000010000 4)
          (defbitfield ShiftFlagImmediate   0b0000000000100000 5)
          (deffield ShiftDestination        0b0000011110000000 7  byte)
          (deffield ShiftImmediate          0b1111100000000000 11 byte)
          (deffield ShiftSource             0b0111100000000000 11 byte))

(deffacts cisc0-branch-fields
          (defbitfield BranchFlagIsConditional   0b0000000010000000 7)
          (defbitfield BranchFlagIsCallForm      0b0000000000100000 5)
          (defbitfield BranchFlagImmediate       0b0000000000010000 4)
          (deffield BranchDestination            0b0000111100000000 8  byte))

(deffacts cisc0-memory-fields
          (defsubtypefield MemoryType      0b0000000000110000 4 MemoryOperation)
          (defbitmask MemoryBitmask        0b0000111100000000 8)
          (defbitmask MemoryFlagIndirect   0b0000000001000000 6)
          (deffield MemoryDestination      0b1111000000000000 12 byte))

(deffacts cisc0-move-fields
          (defbitmask MoveBitmask   0b0000000011110000 4)
          (deffield MoveDestination 0b0000111100000000 8 byte)
          (deffield MoveSource      0b1111000000000000 12 byte))

(deffacts cisc0-set-fields
          (defbitmask SetBitmask    0b0000000011110000 4)
          (deffield SetDestination  0b0000111100000000 8 byte))

(deffacts cisc0-swap-fields
          (deffield SwapDestination 0b0000111100000000 8 byte)
          (deffield SwapSource      0b1111000000000000 12 byte))

(deffacts cisc0-complex-fields
          (defsubtypefield ComplexType              0b0000000011110000 4 ComplexSubTypes)
          (deffield EncodingComplexSubType          0b0000111100000000 8 EncodingOperation)
          (deffield ExtendedComplexSubType          0b0000111100000000 8 ExtendedOperation)
          (deffield ParsingComplexSubType           0b0000111100000000 8 ParsingOperation)
          (deffield ComplexClassExtendedDestination 0b1111000000000000 12 byte))



(deffacts cisc0-enums
          (defenum LegalRegisterNames
                   2
                   byte
                   entries:
                   Destination
                   Source)
          (defenum Operation
                   "ArchitectureConstants::MaxInstructionCount"
                   byte
                   entries:
                   Memory
                   Arithmetic
                   Shift
                   Logical
                   Compare
                   Branch
                   Move
                   Set
                   Swap
                   Return
                   Complex)
          (defenum ArithmeticOps
                   8
                   byte
                   entries:
                   Add
                   Sub
                   Mul
                   Div
                   Rem
                   Min
                   Max)
          (defenum CompareStyle
                   8
                   byte
                   entries:
                   Equals
                   NotEquals
                   LessThan
                   GreaterThan
                   LessThanOrEqualTo
                   GreaterThanOrEqualTo
                   MoveFromCondition
                   MoveToCondition)
          (defenum LogicalOps
                   8
                   byte
                   entries:
                   And
                   Or
                   Xor
                   Nand
                   Not)
          (defenum MemoryOperation
                   4
                   byte
                   entries:
                   Load
                   Store
                   Push
                   Pop)
          (defenum ComplexSubTypes
                   16
                   byte
                   entries:
                   Encoding
                   Extended
                   Parsing)
          (defenum EncodingOperation
                   16
                   byte
                   entries:
                   Encode
                   Decode
                   BitSet
                   BitUnset)
          (defenum ParsingOperation
                   16
                   byte
                   entries:
                   Hex8ToRegister
                   RegisterToHex8
                   MemCopy)
          (defenum ExtendedOperation
                   16
                   byte
                   entries:
                   PushValueAddr
                   PopValueAddr
                   IsOdd
                   IsEven
                   IncrementValueAddr
                   DecrementValueAddr
                   WordsBeforeFirstZero))

(deffacts cisc0-file-layouts-and-requests
          (include "ExecutionUnits.h")
          (using ALUOperation
                 syn::ALU::StandardOperations)
          (using CompareUnitOperation
                 syn::Comparator::StandardOperations)
          (defspecial-execution-unit-converter arithmetic->alu
                                               ArithmeticOps
                                               ALUOperation)
          (defspecial-execution-unit-converter logical->alu
                                               LogicalOps
                                               ALUOperation)
          (defspecial-execution-unit-converter compare->compareunit
                                               CompareStyle
                                               CompareUnitOperation)
          (arithmetic->alu Add Add)
          (arithmetic->alu Sub Subtract)
          (arithmetic->alu Mul Multiply)
          (arithmetic->alu Div Divide)
          (arithmetic->alu Rem Remainder)
          (logical->alu Not UnaryNot)
          (logical->alu Or BinaryOr)
          (logical->alu And BinaryAnd)
          (logical->alu Xor BinaryXor)
          (logical->alu Nand BinaryNand)
          (compare->compareunit Equals Eq)
          (compare->compareunit NotEquals Neq)
          (compare->compareunit LessThan LessThan)
          (compare->compareunit LessThanOrEqualTo LessThanOrEqualTo)
          (compare->compareunit GreaterThan GreaterThan)
          (compare->compareunit GreaterThanOrEqualTo GreaterThanOrEqualTo)
          (top-level-type ComplexSubTypes)
          (top-level-to-sub-type ComplexSubTypes Encoding -> EncodingOperation)
          (top-level-to-sub-type ComplexSubTypes Extended -> ExtendedOperation)
          (top-level-to-sub-type ComplexSubTypes Parsing   -> ParsingOperation)
          (top-level-type Operation)
          (top-level-to-sub-type Operation Arithmetic -> ArithmeticOps)
          (top-level-to-sub-type Operation Compare -> CompareStyle)
          (top-level-to-sub-type Operation Memory -> MemoryOperation)
          (top-level-to-sub-type Operation Logical -> LogicalOps)
          (top-level-to-sub-type Operation Complex -> ComplexSubTypes))
(defrule translate-defsubtype-decl
         (declare (salience ?*priority:first*))
         ?f <- (defsubtypefield ?name
                                ?mask
                                ?shift
                                ?output-type)
         =>
         (retract ?f)
         (assert (deffield ?name
                           ?mask
                           ?shift
                           ?output-type)
                 (sub-type-field ?name
                                 ?output-type)))
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

(defrule MAIN::generate-top-level-type-conversion-generic
         (declare (salience 1))
         ?f <- (top-level-type ?t)
         (constructed enum ?t)
         =>
         (retract ?f)
         (assert (made-top-level-type-conversion ?t))
         (printout t
                   (generic-struct (str-cat ?t
                                            ToSubType)
                                   (variable ?t
                                             value)
                                   (standard-using-decl type
                                                        ?t)) crlf))


(defrule MAIN::generate-top-level-type-conversion-specialization
         (declare (salience 1))
         ?f <- (top-level-to-sub-type ?top ?v -> ?sub-type)
         (made-top-level-type-conversion ?top)
         (constructed enum ?sub-type)
         =>
         (retract ?f)
         (assert (made-top-level-to-sub-type-specialization ?top ?v -> ?sub-type)
                 (specialized-on-top-level-type ?top))
         (printout t
                   (specialize-struct (str-cat ?top
                                               ToSubType)
                                      (explicit-enum ?top
                                                     ?v)
                                      (standard-using-decl type
                                                           ?sub-type)) crlf))

(defrule MAIN::generate-top-level-has-sub-type-function
         (declare (salience 1))
         (made-top-level-type-conversion ?t)
         (specialized-on-top-level-type ?t)
         (not (top-level-to-sub-type ?t ? -> ?))
         (not (made-top-level-to-sub-type-query ?t))
         =>
         (assert (made-top-level-to-sub-type-query ?t))
         (bind ?standard-decl
               (template-decl (variable ?t
                                        value)))
         (bind ?subtype-type
               (str-cat ?t ToSubType
                        (template-specialization value)))

         (printout t
                   ?standard-decl crlf
                   (standard-using-decl (sym-cat SubTypeOf
                                                 ?t)
                                        (explicit-enum (typename ?subtype-type)
                                                       type)) crlf
                   ?standard-decl crlf
                   (function-decl constexpr
                                  bool
                                  HasSubType
                                  FALSE
                                  ""
                                  noexcept
                                  (return-statement (explicit-enum ?subtype-type
                                                                   value))) crlf))




(defrule MAIN::generate-top-level-type-conversion-specialization-encoding-op:generic-case
         (declare (salience -2))
         (made-top-level-to-sub-type-specialization ?top ?v -> ?sub-type)
         (encoding-operation ?name
                             ?str
                             ?sub-type
                             ?full-type)
         (made-top-level-to-sub-type-query ?top)
         (not (generic encoding of sub type generated ?top))
         =>
         (assert (generic encoding of sub type generated ?top))
         (bind ?q
               (str-cat
                 (sym-cat SubTypeOf
                          ?top)
                 "<v>"))
         (bind ?q2
               (sym-cat EncodeSubType
                        ?top))
         (printout t
                   (template-decl (variable ?top v)) crlf
                   "struct " ?q2 " : syn::ConditionFulfillment<false> {" crlf
                   (standard-using-decl ReturnType
                                        ?full-type) crlf
                   (standard-using-decl CastTo
                                        ?q) crlf
                   (constexpr (function-signature ReturnType
                                                  encodeSubType
                                                  (create$ (variable ReturnType input)
                                                           (variable ?q data))
                                                  (noexcept)))
                   (scope-body (return-statement input)) crlf
                   "};" crlf))

(deffacts cisc0-destination-register-usage
          (defproperty-struct UsesDestination
                              Operation)
          (properties UsesDestination
                      Operation
                      Arithmetic
                      Swap
                      Move
                      Shift
                      Compare
                      Set
                      Memory
                      Logical)
          (defproperty-struct UsesSource
                              Operation)
          (properties UsesSource
                      Operation
                      Arithmetic
                      Shift
                      Compare
                      Move
                      Swap
                      Logical)
          (defproperty-struct HasBitmask
                              Operation)
          (properties HasBitmask
                      Operation
                      Compare
                      Move
                      Set
                      Memory
                      Logical)
          (defproperty-struct HasImmediateFlag
                              Operation)
          (properties HasImmediateFlag
                      Operation
                      Arithmetic
                      Shift
                      Compare
                      Logical
                      Branch)
          (defproperty-function usesDestination UsesDestination Operation)
          (defproperty-function usesSource UsesSource Operation)
          (defproperty-function hasBitmask HasBitmask Operation)
          (defproperty-function hasImmediateFlag HasImmediateFlag Operation)
          (defencoder/decoder Type Operation)
          (encoder/decoders Type
                            Operation
                            Arithmetic
                            Compare
                            Logical
                            Memory
                            Complex)
          (defencoder/decoder ComplexSubType ComplexSubTypes)
          (encoder/decoders ComplexSubType
                            ComplexSubTypes
                            Encoding
                            Extended
                            Parsing)
          (defencoder/decoder Bitmask Operation)
          (encoder/decoders Bitmask
                            Operation
                            Compare
                            Move
                            Set
                            Memory
                            Logical)
          (defencoder/decoder Destination Operation)
          (encoder/decoders Destination
                            Operation
                            Set
                            Arithmetic
                            Shift
                            Compare
                            Move
                            Swap
                            Memory
                            Branch
                            Logical)
          (defencoder/decoder Source Operation)
          (encoder/decoders Source
                            Operation
                            Arithmetic
                            Shift
                            Compare
                            Move
                            Swap
                            Logical)
          (defencoder/decoder FlagImmediate Operation)
          (encoder/decoders FlagImmediate
                            Operation
                            Arithmetic
                            Shift
                            Compare
                            Logical
                            Branch))


(defrule MAIN::multi-encode/decode-deocmpose
         (declare (salience ?*priority:first*))
         ?f <- (encoder/decoders ?title
                                 ?type
                                 $?values)
         =>
         (retract ?f)
         (progn$ (?v ?values)
                 (assert (encoder/decoder ?title
                                          ?type
                                          ?v))))

(defrule MAIN::convert-properties
         (declare (salience ?*priority:first*))
         ?f <- (properties ?title
                           ?type
                           $?values)
         =>
         (retract ?f)
         (progn$ (?v ?values)
                 (assert (property ?title
                                   ?type
                                   ?v))))

(defrule MAIN::convert-defencoder/decoder-facts
         (declare (salience ?*priority:first*))
         ?f <- (defencoder/decoder ?title
                                   ?type)
         =>
         (retract ?f)
         (assert (defencoder ?title
                             ?type)
                 (defdecoder ?title
                             ?type)))
(defrule MAIN::convert-encoder/decoder-facts
         (declare (salience ?*priority:first*))
         ?f <- (encoder/decoder ?title
                                ?type
                                ?value)
         =>
         (retract ?f)
         (assert (encoder ?title
                          ?type
                          ?value)
                 (decoder ?title
                          ?type
                          ?value)))
(defrule MAIN::generate-defproperty-function
         (made property-struct
               ?title
               ?input-type)
         (not (property ?title
                        ?input-type
                        ?))
         ?f <- (defproperty-function ?name
                                     ?title
                                     ?input-type)
         =>
         (retract ?f)
         (printout t
                   (template-decl (variable ?input-type
                                            val))
                   (constexpr (function-signature bool
                                                  ?name
                                                  (create$)
                                                  (noexcept)))
                   (scope-body (return-statement (explicit-enum (str-cat ?title
                                                                         (template-specialization val))
                                                                value))) crlf))


(defrule MAIN::generate-generic-struct-impl
         ?f <- (defproperty-struct ?title
                                   ?input-type)
         =>
         (retract ?f)
         (assert (made property-struct
                       ?title
                       ?input-type))
         (printout t
                   (generic-struct ?title
                                   (variable ?input-type
                                             i)) crlf))

(defrule MAIN::generate-property-specialization
         (declare (salience -1))
         (made property-struct
               ?title
               ?input-type)
         ?f <- (property ?title
                         ?input-type
                         ?value)
         =>
         (retract ?f)
         (printout t
                   (specialize-struct ?title
                                      (explicit-enum ?input-type
                                                     ?value))
                   crlf))

(defrule MAIN::generate-defencoder
         ?f <- (defencoder ?title
                           ?type)
         =>
         (retract ?f)
         (assert (made generic-encoder
                       ?title
                       ?type))
         (printout t
                   (generic-struct (str-cat Encode
                                            ?title)
                                   (variable ?type
                                             i)
                                   (standard-using-decl ReturnType
                                                        ?type)
                                   (standard-using-decl CastTo
                                                        ?type)
                                   "static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return in; }")
                   crlf))

(defrule MAIN::generate-specialized-encoder
         (declare (salience -3))
         ?f <- (encoder ?title
                        ?type
                        ?value)
         (made generic-encoder
               ?title
               ?type)
         (encoding-operation =(sym-cat ?value
                                       ?title)
                             ?operation
                             ?input
                             ?ret)
         =>
         (retract ?f)
         (printout t
                   (specialize-struct (str-cat Encode
                                               ?title)
                                      (explicit-enum ?type
                                                     ?value)
                                      (standard-using-decl ReturnType
                                                           ?ret)
                                      (standard-using-decl CastTo
                                                           ?input)
                                      "static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept "
                                      (scope-body (return-statement (str-cat ?operation "( in, val )"))))
                   crlf))

(defrule MAIN::generate-defdecoder
         ?f <- (defdecoder ?title
                           ?type)
         =>
         (retract ?f)
         (assert (made generic-decoder
                       ?title
                       ?type))
         (printout t
                   (generic-struct (str-cat Decode
                                            ?title)
                                   (variable ?type
                                             i)
                                   (standard-using-decl ReturnType
                                                        ?type)
                                   (standard-using-decl CastTo
                                                        ?type)
                                   "static constexpr ReturnType decode(CastTo in) noexcept { return static_cast<ReturnType>(in); }")
                   crlf))

(defrule MAIN::generate-specialized-decoder
         (declare (salience -3))
         ?f <- (decoder ?title
                        ?type
                        ?value)
         (made generic-decoder
               ?title
               ?type)
         (decoding-operation =(sym-cat ?value
                                       ?title)
                             ?operation
                             ?ret
                             ?input)
         =>
         (retract ?f)
         (printout t
                   (specialize-struct (str-cat Decode
                                               ?title)
                                      (explicit-enum ?type
                                                     ?value)
                                      (standard-using-decl ReturnType
                                                           ?ret)
                                      (standard-using-decl CastTo
                                                           ?input)
                                      (constexpr (function-signature "static ReturnType"
                                                                     decode
                                                                     (variable CastTo
                                                                               in)
                                                                     (noexcept)))
                                      (scope-body (return-statement
                                                    (function-call ?operation
                                                                   in))))
                   crlf))



(defrule MAIN::generate-encoder-wrapper
         (made generic-encoder
               ?title
               ?type)
         (not (encoder ?title
                       ?type
                       ?))
         =>
         (bind ?t2
               (str-cat Encode
                        ?title
                        (template-specialization v)))
         (bind ?ret-type
               (typename (explicit-enum ?t2
                                        ReturnType)))
         (bind ?cast-to
               (typename (explicit-enum ?t2
                                        CastTo)))
         (bind ?assert-message
               (format nil
                       "Provided control does not have support for concept %s!"
                       (str-cat ?title)))

         (printout t
                   (template-decl (variable ?type
                                            v)
                                  (assign (typename T)
                                          ?cast-to))
                   (constexpr (function-signature ?ret-type
                                                  (str-cat encode
                                                           ?title)
                                                  (create$ (variable ?ret-type
                                                                     in)
                                                           (variable T
                                                                     value))
                                                  (noexcept)))
                   (scope-body (static-assert (fulfills-condition ?t2)
                                              ?assert-message)
                               (return-statement (function-call (explicit-enum ?t2
                                                                               encode)
                                                                in
                                                                (static-cast ?cast-to
                                                                             value))))
                   crlf))

(defrule MAIN::generate-decoder-wrapper
         (made generic-decoder
               ?title
               ?type)
         (not (decoder ?title
                       ?type
                       ?))
         =>
         (bind ?t2
               (str-cat Decode
                        ?title
                        (template-specialization v)))
         (printout t
                   (template-decl (variable ?type
                                            v))
                   (constexpr (function-signature (typename (explicit-enum ?t2
                                                                           ReturnType))
                                                  (str-cat decode
                                                           ?title)
                                                  (variable (typename (explicit-enum ?t2
                                                                                     CastTo))
                                                            in)
                                                  (noexcept)))
                   (scope-body
                     (static-assert (templated-function-call "syn::fulfillsCondition"
                                                             ?t2)
                                    (str-cat "Provided control does not have support for concept " ?title "!"))
                     (return-statement (function-call (explicit-enum ?t2
                                                                     decode)
                                                      in))) crlf))
