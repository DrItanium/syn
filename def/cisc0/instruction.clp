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

(deffunction MAIN::wrap-entries
             (?prefix ?contents ?postfix)
             (str-cat ?prefix
                      ?contents
                      ?postfix))

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
          (enum (name LegalRegisterNames)
                (children Destination
                          Source)
                (cast-to byte)
                (max-size "2"))
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
                          Return
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
                          GreaterThanOrEqualTo
                          MoveFromCondition
                          MoveToCondition)
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
                          Extended
                          Parsing)
                (cast-to byte)
                (max-size "16"))
          (enum (name EncodingOperation)
                (children Encode
                          Decode
                          BitSet
                          BitUnset)
                (cast-to byte)
                (max-size "16"))
          (enum (name ParsingOperation)
                (children Hex8ToRegister
                          RegisterToHex8
                          MemCopy)
                (cast-to byte)
                (max-size "16"))
          (enum (name ExtendedOperation)
                (children PushValueAddr
                          PopValueAddr
                          IsOdd
                          IsEven
                          IncrementValueAddr
                          DecrementValueAddr
                          WordsBeforeFirstZero)
                (cast-to byte)
                (max-size "16")))

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
(deffunction MAIN::add-terminator
             (?str)
             (str-cat ?str ";"))
(deffunction MAIN::explicit-enum
             (?type ?value)
             (str-cat ?type " :: "  ?value))
(deffunction MAIN::variable
             (?type ?name)
             (str-cat ?type " " ?name))
(deffunction MAIN::standard-using-decl
             (?name ?value)
             (add-terminator (str-cat "using " ?name " = " ?value)))
(deffunction MAIN::scope-body
             ($?body)
             (str-cat " { " (expand$ ?body) " } "))

(deffunction MAIN::terminated-scope-body
             ($?body)
             (add-terminator (scope-body ?body)))


(deffunction MAIN::comma-list
             (?first $?rest)
             (bind ?output
                   ?first)
             (progn$ (?r ?rest)
                     (bind ?output
                           (str-cat ?output
                                    ", " ?r)))
             ?output)
(deffunction MAIN::template-specialization
             (?first $?rest)
             (wrap-entries "<"
                           (comma-list ?first
                                       ?rest)
                           ">"))
(deffunction MAIN::template-decl
             (?first $?rest)
             (str-cat "template"
                      (template-specialization ?first
                                               ?rest)))

(deffunction MAIN::string-if-true
             (?val ?prefix)
             (if ?val then
               (str-cat ?prefix " " ?val)
               else
               ""))

(deffunction MAIN::struct
             (?name ?name-postfix ?extends $?body)
             (str-cat "struct " ?name
                      (string-if-true ?name-postfix
                                      " ")
                      (string-if-true ?extends
                                      " : ")
                      (terminated-scope-body ?body)))
(deffunction MAIN::cond-fulfill
             (?result)
             (str-cat "syn::ConditionFulfillment< "
                      (if ?result then
                        true
                        else
                        false)
                      " >"))

(deffunction MAIN::generic-struct
             (?name ?template-parameters $?body)
             (str-cat (template-decl ?template-parameters) " "
                      (struct ?name
                              FALSE
                              (cond-fulfill FALSE)
                              ?body)))

(deffunction MAIN::specialize-struct
             (?name ?special-value $?body)
             (str-cat (template-decl "")
                      (struct ?name
                              (template-specialization ?special-value)
                              (cond-fulfill TRUE)
                              (expand$ ?body))))

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

(deffunction MAIN::return-statement
             (?statement)
             (add-terminator (str-cat "return "
                                      ?statement)))
(deffunction MAIN::typename
             (?statement)
             (str-cat "typename "
                      ?statement))
(deffunction MAIN::constexpr
             (?statement)
             (str-cat "constexpr "
                      ?statement))
(deffunction MAIN::function-decl
             (?prefix ?return-type ?name ?name-post ?args ?specifiers $?body)
             (str-cat (string-if-true ?prefix
                                      "")
                      " " ?return-type
                      " " ?name
                      (string-if-true ?name-post
                                      " ")
                      "( " ?args " )"
                      (string-if-true ?specifiers
                                      " ")
                      " "
                      (scope-body ?body)))

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
                   "template<" ?top " v>" crlf
                   "struct " ?q2 " : syn::ConditionFulfillment<false> {" crlf
                   "using ReturnType = " ?full-type ";" crlf
                   (standard-using-decl CastTo
                                        ?q) crlf
                   "static constexpr ReturnType encodeSubType(ReturnType input, " ?q " data) noexcept { return input; }" crlf
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
                   " constexpr bool " ?name "() noexcept"
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
                                      "static constexpr ReturnType decode(CastTo in) noexcept "
                                      (scope-body (return-statement (str-cat ?operation "( in )"))))
                   crlf))

(deffunction MAIN::parens
             (?first $?args)
             (wrap-entries "("
                           (comma-list ?first
                                       ?args)
                           ")"))

(deffunction MAIN::string-quote
             (?str)
             (format nil
                     "\"%s\""
                     ?str))
(deffunction MAIN::static-assert
             (?condition ?message)
             (add-terminator (str-cat static_assert
                                      (parens ?condition
                                              (string-quote ?message)))))
(deffunction MAIN::fulfills-condition
             (?type)
             (str-cat "syn::fulfillsCondition"
                      (template-specialization ?type)
                      "()"))
(deffunction MAIN::static-cast
             (?type ?value)
             (str-cat "static_cast"
                      (template-specialization ?type)
                      (parens ?value)))
(deffunction MAIN::assign
             (?a ?b)
             (str-cat ?a " = " ?b))

(deffunction MAIN::function-call
             (?function ?first-arg $?args)
             (str-cat ?function " " (parens ?first-arg ?args)))


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
                   " constexpr "
                   ?ret-type
                   (str-cat " encode"
                            ?title)
                   (parens (variable ?ret-type
                                     in)
                           (variable T
                                     value))
                   " noexcept "
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
                   " constexpr typename "
                   (explicit-enum ?t2
                                  ReturnType)
                   (str-cat " decode"
                            ?title)
                   (str-cat "( typename "
                            (explicit-enum ?t2
                                           CastTo)
                            " in) noexcept ")
                   (scope-body "static_assert( syn::fulfillsCondition<" ?t2 ">(), \"Provided control does not have support for concept " ?title "!\");"
                               (return-statement (str-cat (explicit-enum ?t2
                                                                         decode)
                                                          "(in)")))
                   crlf))
