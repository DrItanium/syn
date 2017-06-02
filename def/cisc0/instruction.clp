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
          (defbitfield CompareFlagImmediate 0b0010000000000000 13)
          (defsubtypefield CompareType      0b0000011100000000 8 CompareStyle)
          (deffield CompareDestination        0b0000000000001111 0 byte)
          (deffield CompareSource        0b0000000011110000 4 byte)
          (deffield CompareImmediate        0b1111111100000000 8 byte))

(deffacts cisc0-arithmetic-fields
          (defbitfield ArithmeticFlagImmediate 0b0000000000010000 4)
          (defsubtypefield ArithmeticFlagType  0b0000000011100000 5 ArithmeticOps)
          (deffield ArithmeticImmediate        0b1111000000000000 12 RegisterValue)
          (deffield ArithmeticDestination      0b0000111100000000 8 byte)
          (deffield ArithmeticSource           0b1111000000000000 12 byte))

(deffacts cisc0-logical-fields
          (defbitfield LogicalFlagImmediate      0b0000000000010000 4)
          (defbitmask LogicalFlagImmediateMask   0b0000111100000000 8)
          (deffield LogicalImmediateDestination  0b1111000000000000 12 byte)
          (defsubtypefield LogicalFlagType       0b0000000011100000 5 LogicalOps)
          (deffield LogicalDestination           0b0000111100000000 8 byte)
          (deffield LogicalSource                0b1111000000000000 12 byte))

(deffacts cisc0-shift-fields
          (defbitfield ShiftFlagLeft        0b0000000000010000 4)
          (defbitfield ShiftFlagImmediate   0b0000000000100000 5)
          (deffield ShiftImmediate          0b1111100000000000 11 RegisterValue)
          (deffield ShiftDestination          0b0000011110000000 7  byte)
          (deffield ShiftSource          0b0111100000000000 11 byte))

(deffacts cisc0-branch-fields
          (defbitfield BranchFlagIsConditional   0b0000000010000000 7)
          (defbitfield BranchFlagIsIfForm        0b0000000001000000 6)
          (defbitfield BranchFlagIsCallForm      0b0000000000100000 5)
          (defbitfield BranchFlagImmediate       0b0000000000010000 4)
          (deffield BranchIfOnTrue               0b0000111100000000 8  byte)
          (deffield BranchIfOnFalse              0b1111000000000000 12 byte)
          (deffield BranchIndirectDestination    0b1111000000000000 12 byte))

(deffacts cisc0-memory-fields
          (defsubtypefield MemoryFlagType  0b0000000000110000 4 MemoryOperation)
          (defbitmask MemoryFlagBitmask    0b0000111100000000 8)
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
          (defsubtypefield ComplexSubClass          0b0000000011110000 4 ComplexSubTypes)
          (deffield ComplexClassEncodingType        0b0000111100000000 8 EncodingOperation)
          (deffield ComplexClassExtendedType        0b0000111100000000 8 ExtendedOperation)
          (deffield ComplexClassExtendedDestination 0b1111000000000000 12 byte))



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
                (max-size "16"))
          (enum (name ExtendedOperation)
                (children PushValueAddr
                          PopValueAddr
                          PushRegisters
                          PopRegisters
                          ;SaveRegisters
                          ;LoadRegisters
                          IsOdd
                          IsEven)
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
             (str-cat "< " 
                      (comma-list ?first
                                  ?rest)
                      " >"))
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
                   (standard-using-decl SubTypeOf 
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
         (printout t
                   "template<" ?top " v>" crlf
                   "struct EncodeSubType : syn::ConditionFulfillment<false> {" crlf
                   "using ReturnType = " ?full-type ";" crlf
                   "using CastTo = SubTypeOf<v>;" crlf
                   "static constexpr ReturnType encodeSubType(ReturnType input, SubTypeOf<v> data) noexcept { return input; }" crlf
                   "};" crlf))

(defrule MAIN::generate-top-level-type-conversion-specialization-encoding-op
         (declare (salience -3))
         (made-top-level-to-sub-type-specialization ?top ?v -> ?sub-type)
         (sub-type-field ?name
                         ?sub-type)
         (encoding-operation ?name
                             ?str
                             ?sub-type
                             ?full-type)
         (made-top-level-to-sub-type-query ?top)
         (generic encoding of sub type generated ?top)
         =>
         (printout t 
                   (specialize-struct EncodeSubType
                                      (explicit-enum ?top
                                                     ?v)
                                      (standard-using-decl ReturnType
                                                           ?full-type)
                                      (standard-using-decl CastTo
                                                           (str-cat SubTypeOf
                                                                    (template-specialization (explicit-enum ?top 
                                                                                                            ?v))))
                                      "static constexpr ReturnType encodeSubType(ReturnType input, CastTo value) noexcept { return " ?str "(input, value); }") 
                   crlf))

(defrule MAIN::generate-basic-sub-type-encoder
         (declare (salience -4))
         (generic encoding of sub type generated ?top)
         (not (built encode sub type function ?top))
         =>
         (assert (built encode sub type function ?top))
         (printout t 
                   "template<" ?top " v, typename T = typename EncodeSubType<v>::CastTo> " crlf
                   "constexpr typename EncodeSubType<v>::ReturnType encodeSubType(typename EncodeSubType<v>::ReturnType input, T value) noexcept {" crlf
                   "static_assert(HasSubType<v>(), \"Provided operation does not have a subtype!\");" crlf
                   "return EncodeSubType<v>::encodeSubType(input, static_cast<typename EncodeSubType<v>::CastTo>(value));" crlf
                   "}" crlf))

(defrule MAIN::generate-top-level-type-conversion-specialization-decoding-op:generic-case
         (declare (salience -2))
         (made-top-level-to-sub-type-specialization ?top ?v -> ?sub-type)
         (decoding-operation ?name
                             ?str
                             ?sub-type
                             ?full-type)
         (made-top-level-to-sub-type-query ?top)
         (not (generic decoding of sub type generated ?top))
         =>
         (assert (generic decoding of sub type generated ?top))
         (printout t
                   "template<" ?top " v>" crlf
                   "struct DecodeSubType : syn::ConditionFulfillment<false> {" crlf
                   "using ReturnType = SubTypeOf<v>;" crlf 
                   "using InputType = " ?full-type ";" crlf
                   "static constexpr ReturnType decodeSubType(InputType input) noexcept { return input; }" crlf
                   "};" crlf))

(defrule MAIN::generate-top-level-type-conversion-specialization-decoding-op
         (declare (salience -3))
         (made-top-level-to-sub-type-specialization ?top ?v -> ?sub-type)
         (sub-type-field ?name
                         ?sub-type)
         (decoding-operation ?name
                             ?str
                             ?sub-type
                             ?full-type)
         (made-top-level-to-sub-type-query ?top)
         (generic decoding of sub type generated ?top)
         =>
         (printout t 
                   "template<>" crlf
                   "struct DecodeSubType <" ?top " :: " ?v "> : syn::ConditionFulfillment<true> {" crlf
                   "using InputType = " ?full-type ";" crlf
                   "using ReturnType = SubTypeOf<" ?top " :: " ?v">;" crlf
                   "static constexpr ReturnType decodeSubType(InputType input) noexcept {" crlf
                   "return " ?str " ( input );" crlf
                   "}" crlf
                   "};" crlf))

(defrule MAIN::generate-basic-sub-type-decoder
         (declare (salience -4))
         (generic decoding of sub type generated ?top)
         (not (built decode sub type function ?top))
         =>
         (assert (built decode sub type function ?top))
         (printout t 
                   "template<" ?top " v>" crlf
                   "constexpr typename DecodeSubType<v>::ReturnType decodeSubType(typename DecodeSubType<v>::InputType input) noexcept {" crlf
                   "static_assert(HasSubType<v>(), \"Provided operation does not have a subtype!\");" crlf
                   "return DecodeSubType<v>::decodeSubType(input);" crlf
                   "}" crlf))

(deffacts cisc0-destination-register-usage
          (defproperty-struct UsesDestination 
                              Operation)
          (property UsesDestination Operation Arithmetic)
          (property UsesDestination Operation Swap)
          (property UsesDestination Operation Move)
          (property UsesDestination Operation Shift)
          (property UsesDestination Operation Compare)
          (property UsesDestination Operation Set)
          (property UsesDestination Operation Memory)
          (property UsesDestination Operation Logical)
          (defproperty-struct UsesSource
                              Operation)
          (property UsesSource Operation Arithmetic)
          (property UsesSource Operation Shift)
          (property UsesSource Operation Compare)
          (property UsesSource Operation Move)
          (property UsesSource Operation Swap)
          (property UsesSource Operation Logical)
          (defproperty-struct HasBitmask
                              Operation)
          (property HasBitmask Operation Move)
          (property HasBitmask Operation Set)
          (property HasBitmask Operation Memory)
          (property HasBitmask Operation Logical)
          (defproperty-struct HasImmediateFlag
                              Operation)
          (property HasImmediateFlag Operation Arithmetic)
          (property HasImmediateFlag Operation Shift)
          (property HasImmediateFlag Operation Compare)
          (property HasImmediateFlag Operation Logical)
          (property HasImmediateFlag Operation Branch)
          (defproperty-function usesDestination
                                UsesDestination
                                Operation)
          (defproperty-function usesSource 
                                UsesSource 
                                Operation)
          (defproperty-function hasBitmask
                                HasBitmask
                                Operation)
          (defproperty-function hasImmediateFlag
                                HasImmediateFlag
                                Operation)
          )

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
