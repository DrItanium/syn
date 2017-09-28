[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::file
   (is-a thing
[MSGPSR1] A class must be defined before its message-handlers.

ERROR:
(defmessage-handler lisp-parse::file
   parent-is
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::node
   (is-a thing
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::composite-node "A node that is made up of other nodes. This is separate from a list!"
   (is-a node
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::scalar-node
   (is-a node
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::typed-scalar-node
   (is-a scalar-node
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::string "Strings need to be wrapped in their own nodes"
   (is-a scalar-node
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::variable
   (is-a scalar-node
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::global-variable
   (is-a variable
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::singlefield-global-variable
   (is-a global-variable
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::multifield-global-variable
   (is-a global-variable
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::local-variable
   (is-a variable
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::multifield-variable
   (is-a local-variable
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::singlefield-variable
   (is-a local-variable
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::constraint
   (is-a scalar-node
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::not-constraint
   (is-a constraint
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::and-constraint
   (is-a constraint
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::or-constraint
   (is-a constraint
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::wildcard
   (is-a scalar-node
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::multifield-wildcard
   (is-a wildcard
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::singlefield-wildcard
   (is-a wildcard
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::list
   (is-a node
[INHERPSR3] A class must be defined after all its superclasses.

ERROR:
(defclass lisp-parse::reference "An indirect reference to something else, useful for deffunctions and arguments"
   (is-a scalar-node
[PRNTUTIL1] Unable to find class file.

ERROR:
(defrule lisp-parse::open-file
   ?f <- (parse-request (path ?path))
   =>
   (retract ?f)
   (bind ?name (gensym*))
   (if (open ?path ?name "r")
      then
      (make-instance ?name of file
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::new-top-level
   (declare (salience 2))
   (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::new-list
   (declare (salience 3))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::end-list
   (declare (salience 3))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::end-list:top-level
   (declare (salience 3))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::parse-special-element
   (declare (salience 1))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::warn:parse-special-element-outside-list
   (declare (salience 1))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::parse-string
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::parse-string-outside-list
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::parse-normal-element
   (declare (salience 1))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::warn:parse-normal-element-outside-list
   (declare (salience 1))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::error:end-list-without-beginning
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::finished-completely
   (declare (salience 3))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::error:bottom-of-top-should-be-self-referential
   (declare (salience 10000))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::error:top-is-empty
   (declare (salience 10000))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance:or-constraint "convert a symbol of type OR_CONSTRAINT to class of type or-constraint"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance-outside-list:or-constraint "convert a symbol of type OR_CONSTRAINT to class of type or-constraint"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance:and-constraint "convert a symbol of type AND_CONSTRAINT to class of type and-constraint"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance-outside-list:and-constraint "convert a symbol of type AND_CONSTRAINT to class of type and-constraint"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance:not-constraint "convert a symbol of type NOT_CONSTRAINT to class of type not-constraint"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance-outside-list:not-constraint "convert a symbol of type NOT_CONSTRAINT to class of type not-constraint"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance:multifield-wildcard "convert a symbol of type MF_WILDCARD to class of type multifield-wildcard"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance-outside-list:multifield-wildcard "convert a symbol of type MF_WILDCARD to class of type multifield-wildcard"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance:singlefield-wildcard "convert a symbol of type SF_WILDCARD to class of type singlefield-wildcard"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance-outside-list:singlefield-wildcard "convert a symbol of type SF_WILDCARD to class of type singlefield-wildcard"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance:multifield-variable "convert a symbol of type MF_VARIABLE to class of type multifield-variable"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance-outside-list:multifield-variable "convert a symbol of type MF_VARIABLE to class of type multifield-variable"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance:singlefield-variable "convert a symbol of type SF_VARIABLE to class of type singlefield-variable"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance-outside-list:singlefield-variable "convert a symbol of type SF_VARIABLE to class of type singlefield-variable"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance:multifield-global-variable "convert a symbol of type MF_GBL_VARIABLE to class of type multifield-global-variable"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance-outside-list:multifield-global-variable "convert a symbol of type MF_GBL_VARIABLE to class of type multifield-global-variable"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance:singlefield-global-variable "convert a symbol of type GBL_VARIABLE to class of type singlefield-global-variable"
   (declare (salience 2))
   ?f <- (object (is-a file)
[OBJRTBLD5] Undefined class in object pattern.

ERROR:
(defrule lisp-parse::construct-special-instance-outside-list:singlefield-global-variable "convert a symbol of type GBL_VARIABLE to class of type singlefield-global-variable"
   (declare (salience 2))
   ?f <- (object (is-a file)
// NOTE: this file is auto generated, DO NOT MODIFY!
#ifndef _CISC0_DECL
#define _CISC0_DECL
#include "Base.h"
#include "Cisc0CoreConstants.h"
#include "ExecutionUnits.h"
namespace cisc0 { 
template<typename T, T op>
constexpr auto toExecutionUnitValue = syn::defaultErrorState<T>;
using CompareUnitOperation = syn::Comparator::StandardOperations;
using ALUOperation = syn::ALU::StandardOperations;
enum class Operation : byte { Memory, Arithmetic, Shift, Logical, Compare, Branch, Move, Set, Swap, Return, Complex, Count } ;
static_assert(static_cast<byte>(Operation :: Count) <= static_cast<byte>(ArchitectureConstants::MaxInstructionCount), "Too many Operation entries defined!");
template<Operation op>
constexpr auto translateOperation = toExecutionUnitValue<decltype(op), op>;
template<Operation value> struct OperationToSubType :  syn::ConditionFulfillment< false > { using type = Operation; } ;
enum class ArithmeticOps : byte { Add, Sub, Mul, Div, Rem, Min, Max, Count } ;
static_assert(static_cast<byte>(ArithmeticOps :: Count) <= static_cast<byte>(8), "Too many ArithmeticOps entries defined!");
template<ArithmeticOps op>
constexpr auto translateArithmeticOps = toExecutionUnitValue<decltype(op), op>;
template<>struct OperationToSubType  <Operation :: Arithmetic> :  syn::ConditionFulfillment< true > { using type = ArithmeticOps; } ;
enum class CompareStyle : byte { Equals, NotEquals, LessThan, GreaterThan, LessThanOrEqualTo, GreaterThanOrEqualTo, MoveFromCondition, MoveToCondition, Count } ;
static_assert(static_cast<byte>(CompareStyle :: Count) <= static_cast<byte>(8), "Too many CompareStyle entries defined!");
template<CompareStyle op>
constexpr auto translateCompareStyle = toExecutionUnitValue<decltype(op), op>;
template<>struct OperationToSubType  <Operation :: Compare> :  syn::ConditionFulfillment< true > { using type = CompareStyle; } ;
enum class LogicalOps : byte { And, Or, Xor, Nand, Not, Count } ;
static_assert(static_cast<byte>(LogicalOps :: Count) <= static_cast<byte>(8), "Too many LogicalOps entries defined!");
template<LogicalOps op>
constexpr auto translateLogicalOps = toExecutionUnitValue<decltype(op), op>;
template<>struct OperationToSubType  <Operation :: Logical> :  syn::ConditionFulfillment< true > { using type = LogicalOps; } ;
enum class MemoryOperation : byte { Load, Store, Push, Pop, Count } ;
static_assert(static_cast<byte>(MemoryOperation :: Count) <= static_cast<byte>(4), "Too many MemoryOperation entries defined!");
template<MemoryOperation op>
constexpr auto translateMemoryOperation = toExecutionUnitValue<decltype(op), op>;
template<>struct OperationToSubType  <Operation :: Memory> :  syn::ConditionFulfillment< true > { using type = MemoryOperation; } ;
enum class ComplexSubTypes : byte { Encoding, Extended, Parsing, FeatureCheck, Count } ;
static_assert(static_cast<byte>(ComplexSubTypes :: Count) <= static_cast<byte>(16), "Too many ComplexSubTypes entries defined!");
template<ComplexSubTypes op>
constexpr auto translateComplexSubTypes = toExecutionUnitValue<decltype(op), op>;
template<ComplexSubTypes value> struct ComplexSubTypesToSubType :  syn::ConditionFulfillment< false > { using type = ComplexSubTypes; } ;
template<>struct OperationToSubType  <Operation :: Complex> :  syn::ConditionFulfillment< true > { using type = ComplexSubTypes; } ;
template<Operation value>
using SubTypeOfOperation = typename OperationToSubType<value> :: type;
template<Operation value>
constexpr bool HasSubtype() noexcept { return OperationToSubType<value> :: value; } 
enum class EncodingOperation : byte { Encode, Decode, BitSet, BitUnset, Count } ;
static_assert(static_cast<byte>(EncodingOperation :: Count) <= static_cast<byte>(16), "Too many EncodingOperation entries defined!");
template<EncodingOperation op>
constexpr auto translateEncodingOperation = toExecutionUnitValue<decltype(op), op>;
template<>struct ComplexSubTypesToSubType  <ComplexSubTypes :: Encoding> :  syn::ConditionFulfillment< true > { using type = EncodingOperation; } ;
enum class ParsingOperation : byte { Hex8ToRegister, RegisterToHex8, MemCopy, Count } ;
static_assert(static_cast<byte>(ParsingOperation :: Count) <= static_cast<byte>(16), "Too many ParsingOperation entries defined!");
template<ParsingOperation op>
constexpr auto translateParsingOperation = toExecutionUnitValue<decltype(op), op>;
template<>struct ComplexSubTypesToSubType  <ComplexSubTypes :: Parsing> :  syn::ConditionFulfillment< true > { using type = ParsingOperation; } ;
enum class ExtendedOperation : byte { PushValueAddr, PopValueAddr, IsOdd, IsEven, IncrementValueAddr, DecrementValueAddr, WordsBeforeFirstZero, Count } ;
static_assert(static_cast<byte>(ExtendedOperation :: Count) <= static_cast<byte>(16), "Too many ExtendedOperation entries defined!");
template<ExtendedOperation op>
constexpr auto translateExtendedOperation = toExecutionUnitValue<decltype(op), op>;
template<>struct ComplexSubTypesToSubType  <ComplexSubTypes :: Extended> :  syn::ConditionFulfillment< true > { using type = ExtendedOperation; } ;
enum class FeatureCheckOperation : byte { GetModelNumber, GetTerminateAddress, SetTerminateAddress, Count } ;
static_assert(static_cast<byte>(FeatureCheckOperation :: Count) <= static_cast<byte>(16), "Too many FeatureCheckOperation entries defined!");
template<FeatureCheckOperation op>
constexpr auto translateFeatureCheckOperation = toExecutionUnitValue<decltype(op), op>;
template<>struct ComplexSubTypesToSubType  <ComplexSubTypes :: FeatureCheck> :  syn::ConditionFulfillment< true > { using type = FeatureCheckOperation; } ;
template<ComplexSubTypes value>
using SubTypeOfComplexSubTypes = typename ComplexSubTypesToSubType<value> :: type;
template<ComplexSubTypes value>
constexpr bool HasSubtype() noexcept { return ComplexSubTypesToSubType<value> :: value; } 
enum class LegalRegisterNames : byte { Destination, Source, Count } ;
static_assert(static_cast<byte>(LegalRegisterNames :: Count) <= static_cast<byte>(2), "Too many LegalRegisterNames entries defined!");
template<LegalRegisterNames op>
constexpr auto translateLegalRegisterNames = toExecutionUnitValue<decltype(op), op>;
template<>constexpr auto toExecutionUnitValue<LogicalOps, LogicalOps :: Nand> = ALUOperation :: BinaryNand;
template<>constexpr auto toExecutionUnitValue<LogicalOps, LogicalOps :: Xor> = ALUOperation :: BinaryXor;
template<>constexpr auto toExecutionUnitValue<LogicalOps, LogicalOps :: And> = ALUOperation :: BinaryAnd;
template<>constexpr auto toExecutionUnitValue<LogicalOps, LogicalOps :: Or> = ALUOperation :: BinaryOr;
template<>constexpr auto toExecutionUnitValue<LogicalOps, LogicalOps :: Not> = ALUOperation :: UnaryNot;
template<>constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle :: GreaterThanOrEqualTo> = CompareUnitOperation :: GreaterThanOrEqualTo;
template<>constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle :: GreaterThan> = CompareUnitOperation :: GreaterThan;
template<>constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle :: LessThanOrEqualTo> = CompareUnitOperation :: LessThanOrEqualTo;
template<>constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle :: LessThan> = CompareUnitOperation :: LessThan;
template<>constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle :: NotEquals> = CompareUnitOperation :: Neq;
template<>constexpr auto toExecutionUnitValue<CompareStyle, CompareStyle :: Equals> = CompareUnitOperation :: Eq;
template<>constexpr auto toExecutionUnitValue<ArithmeticOps, ArithmeticOps :: Rem> = ALUOperation :: Remainder;
template<>constexpr auto toExecutionUnitValue<ArithmeticOps, ArithmeticOps :: Div> = ALUOperation :: Divide;
template<>constexpr auto toExecutionUnitValue<ArithmeticOps, ArithmeticOps :: Mul> = ALUOperation :: Multiply;
template<>constexpr auto toExecutionUnitValue<ArithmeticOps, ArithmeticOps :: Sub> = ALUOperation :: Subtract;
template<>constexpr auto toExecutionUnitValue<ArithmeticOps, ArithmeticOps :: Add> = ALUOperation :: Add;
constexpr Operation decodeControl(Word value) noexcept { return syn::decodeBits<Word, Operation, 0b0000000000001111, 0> (value); } 
constexpr Word encodeControl(Word value, Operation field) noexcept { return syn::encodeBits<Word, Operation, 0b0000000000001111, 0> (value, field); } 
constexpr byte decodeUpper(Word value) noexcept { return syn::decodeBits<Word, byte, 0b1111111100000000, 8> (value); } 
constexpr Word encodeUpper(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b1111111100000000, 8> (value, field); } 
constexpr byte decodeLower(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000000011111111, 0> (value); } 
constexpr Word encodeLower(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000000011111111, 0> (value, field); } 
constexpr byte decodeGenericCommonSubTypeField(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000000011110000, 4> (value); } 
constexpr Word encodeGenericCommonSubTypeField(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000000011110000, 4> (value, field); } 
constexpr bool decodeGenericImmediateFlag(Word value) noexcept { return syn::decodeBits<Word, bool, 0b0000000000010000, 4> (value); } 
constexpr Word encodeGenericImmediateFlag(Word value, bool field) noexcept { return syn::encodeBits<Word, bool, 0b0000000000010000, 4> (value, field); } 
constexpr byte decodeGenericDestination(Word value) noexcept { return syn::decodeBits<Word, byte, 0b1111000000000000, 12> (value); } 
constexpr Word encodeGenericDestination(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b1111000000000000, 12> (value, field); } 
constexpr byte decodeGenericSource(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeGenericSource(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr byte decodeGenericBitmask(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeGenericBitmask(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr byte decodeGenericImmediate4(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeGenericImmediate4(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr byte decodeGenericImmediate5(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111110000000, 7> (value); } 
constexpr Word encodeGenericImmediate5(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111110000000, 7> (value, field); } 
constexpr CompareStyle decodeCompareType(Word value) noexcept { return syn::decodeBits<Word, CompareStyle, 0b0000000011100000, 5> (value); } 
constexpr Word encodeCompareType(Word value, CompareStyle field) noexcept { return syn::encodeBits<Word, CompareStyle, 0b0000000011100000, 5> (value, field); } 
constexpr byte decodeCompareSource(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeCompareSource(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr byte decodeCompareBitmask(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeCompareBitmask(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr ArithmeticOps decodeArithmeticType(Word value) noexcept { return syn::decodeBits<Word, ArithmeticOps, 0b0000000011100000, 5> (value); } 
constexpr Word encodeArithmeticType(Word value, ArithmeticOps field) noexcept { return syn::encodeBits<Word, ArithmeticOps, 0b0000000011100000, 5> (value, field); } 
constexpr byte decodeArithmeticSource(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeArithmeticSource(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr LogicalOps decodeLogicalType(Word value) noexcept { return syn::decodeBits<Word, LogicalOps, 0b0000000011100000, 5> (value); } 
constexpr Word encodeLogicalType(Word value, LogicalOps field) noexcept { return syn::encodeBits<Word, LogicalOps, 0b0000000011100000, 5> (value, field); } 
constexpr byte decodeLogicalBitmask(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeLogicalBitmask(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr byte decodeLogicalSource(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeLogicalSource(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr bool decodeShiftFlagLeft(Word value) noexcept { return syn::decodeBits<Word, bool, 0b0000000000100000, 5> (value); } 
constexpr Word encodeShiftFlagLeft(Word value, bool field) noexcept { return syn::encodeBits<Word, bool, 0b0000000000100000, 5> (value, field); } 
constexpr byte decodeShiftImmediate(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111110000000, 7> (value); } 
constexpr Word encodeShiftImmediate(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111110000000, 7> (value, field); } 
constexpr byte decodeShiftSource(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeShiftSource(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr bool decodeBranchFlagIsCallForm(Word value) noexcept { return syn::decodeBits<Word, bool, 0b0000000000100000, 5> (value); } 
constexpr Word encodeBranchFlagIsCallForm(Word value, bool field) noexcept { return syn::encodeBits<Word, bool, 0b0000000000100000, 5> (value, field); } 
constexpr bool decodeBranchFlagIsConditional(Word value) noexcept { return syn::decodeBits<Word, bool, 0b0000000001000000, 6> (value); } 
constexpr Word encodeBranchFlagIsConditional(Word value, bool field) noexcept { return syn::encodeBits<Word, bool, 0b0000000001000000, 6> (value, field); } 
constexpr MemoryOperation decodeMemoryType(Word value) noexcept { return syn::decodeBits<Word, MemoryOperation, 0b0000000000110000, 4> (value); } 
constexpr Word encodeMemoryType(Word value, MemoryOperation field) noexcept { return syn::encodeBits<Word, MemoryOperation, 0b0000000000110000, 4> (value, field); } 
constexpr byte decodeMemoryFlagIndirect(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000000001000000, 6> (value); } 
constexpr Word encodeMemoryFlagIndirect(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000000001000000, 6> (value, field); } 
constexpr byte decodeMemoryBitmask(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeMemoryBitmask(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr byte decodeMoveBitmask(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000000011110000, 4> (value); } 
constexpr Word encodeMoveBitmask(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000000011110000, 4> (value, field); } 
constexpr byte decodeMoveSource(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeMoveSource(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr byte decodeSetBitmask(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeSetBitmask(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr byte decodeSwapSource(Word value) noexcept { return syn::decodeBits<Word, byte, 0b0000111100000000, 8> (value); } 
constexpr Word encodeSwapSource(Word value, byte field) noexcept { return syn::encodeBits<Word, byte, 0b0000111100000000, 8> (value, field); } 
constexpr ComplexSubTypes decodeComplexType(Word value) noexcept { return syn::decodeBits<Word, ComplexSubTypes, 0b0000000011110000, 4> (value); } 
constexpr Word encodeComplexType(Word value, ComplexSubTypes field) noexcept { return syn::encodeBits<Word, ComplexSubTypes, 0b0000000011110000, 4> (value, field); } 
constexpr EncodingOperation decodeEncodingComplexSubType(Word value) noexcept { return syn::decodeBits<Word, EncodingOperation, 0b0000111100000000, 8> (value); } 
constexpr Word encodeEncodingComplexSubType(Word value, EncodingOperation field) noexcept { return syn::encodeBits<Word, EncodingOperation, 0b0000111100000000, 8> (value, field); } 
constexpr ExtendedOperation decodeExtendedComplexSubType(Word value) noexcept { return syn::decodeBits<Word, ExtendedOperation, 0b0000111100000000, 8> (value); } 
constexpr Word encodeExtendedComplexSubType(Word value, ExtendedOperation field) noexcept { return syn::encodeBits<Word, ExtendedOperation, 0b0000111100000000, 8> (value, field); } 
constexpr ParsingOperation decodeParsingComplexSubType(Word value) noexcept { return syn::decodeBits<Word, ParsingOperation, 0b0000111100000000, 8> (value); } 
constexpr Word encodeParsingComplexSubType(Word value, ParsingOperation field) noexcept { return syn::encodeBits<Word, ParsingOperation, 0b0000111100000000, 8> (value, field); } 
constexpr FeatureCheckOperation decodeFeatureCheckComplexSubType(Word value) noexcept { return syn::decodeBits<Word, FeatureCheckOperation, 0b0000111100000000, 8> (value); } 
constexpr Word encodeFeatureCheckComplexSubType(Word value, FeatureCheckOperation field) noexcept { return syn::encodeBits<Word, FeatureCheckOperation, 0b0000111100000000, 8> (value, field); } 
template<Operation i> struct DecodeType :  syn::ConditionFulfillment< false > { using ReturnType = Operation;using CastTo = Operation;static constexpr ReturnType decode(CastTo in) noexcept { return static_cast<ReturnType>(in); }  } ;
template<Operation i> struct EncodeType :  syn::ConditionFulfillment< false > { using ReturnType = Operation;using CastTo = Operation;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return in; }  } ;
template<ComplexSubTypes i> struct DecodeComplexSubType :  syn::ConditionFulfillment< false > { using ReturnType = ComplexSubTypes;using CastTo = ComplexSubTypes;static constexpr ReturnType decode(CastTo in) noexcept { return static_cast<ReturnType>(in); }  } ;
template<ComplexSubTypes i> struct EncodeComplexSubType :  syn::ConditionFulfillment< false > { using ReturnType = ComplexSubTypes;using CastTo = ComplexSubTypes;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return in; }  } ;
template<Operation i> struct DecodeBitmask :  syn::ConditionFulfillment< false > { using ReturnType = Operation;using CastTo = Operation;static constexpr ReturnType decode(CastTo in) noexcept { return static_cast<ReturnType>(in); }  } ;
template<Operation i> struct EncodeBitmask :  syn::ConditionFulfillment< false > { using ReturnType = Operation;using CastTo = Operation;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return in; }  } ;
template<Operation i> struct DecodeDestination :  syn::ConditionFulfillment< false > { using ReturnType = Operation;using CastTo = Operation;static constexpr ReturnType decode(CastTo in) noexcept { return static_cast<ReturnType>(in); }  } ;
template<Operation i> struct EncodeDestination :  syn::ConditionFulfillment< false > { using ReturnType = Operation;using CastTo = Operation;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return in; }  } ;
template<Operation i> struct DecodeSource :  syn::ConditionFulfillment< false > { using ReturnType = Operation;using CastTo = Operation;static constexpr ReturnType decode(CastTo in) noexcept { return static_cast<ReturnType>(in); }  } ;
template<Operation i> struct EncodeSource :  syn::ConditionFulfillment< false > { using ReturnType = Operation;using CastTo = Operation;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return in; }  } ;
template<Operation i> struct DecodeFlagImmediate :  syn::ConditionFulfillment< false > { using ReturnType = Operation;using CastTo = Operation;static constexpr ReturnType decode(CastTo in) noexcept { return static_cast<ReturnType>(in); }  } ;
template<Operation i> struct EncodeFlagImmediate :  syn::ConditionFulfillment< false > { using ReturnType = Operation;using CastTo = Operation;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return in; }  } ;
template<Operation i> struct HasImmediateFlag :  syn::ConditionFulfillment< false > {  } ;
template<Operation i> struct HasBitmask :  syn::ConditionFulfillment< false > {  } ;
template<Operation i> struct UsesSource :  syn::ConditionFulfillment< false > {  } ;
template<Operation i> struct UsesDestination :  syn::ConditionFulfillment< false > {  } ;
template<>struct UsesDestination  <Operation :: Branch> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesDestination  <Operation :: Complex> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesDestination  <Operation :: Logical> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesDestination  <Operation :: Memory> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesDestination  <Operation :: Set> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesDestination  <Operation :: Compare> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesDestination  <Operation :: Shift> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesDestination  <Operation :: Move> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesDestination  <Operation :: Swap> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesDestination  <Operation :: Arithmetic> :  syn::ConditionFulfillment< true > {  } ;
template<Operation val>constexpr bool usesDestination() noexcept { return UsesDestination<val> :: value; } 
template<>struct UsesSource  <Operation :: Logical> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesSource  <Operation :: Swap> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesSource  <Operation :: Move> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesSource  <Operation :: Compare> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesSource  <Operation :: Shift> :  syn::ConditionFulfillment< true > {  } ;
template<>struct UsesSource  <Operation :: Arithmetic> :  syn::ConditionFulfillment< true > {  } ;
template<Operation val>constexpr bool usesSource() noexcept { return UsesSource<val> :: value; } 
template<>struct HasBitmask  <Operation :: Logical> :  syn::ConditionFulfillment< true > {  } ;
template<>struct HasBitmask  <Operation :: Memory> :  syn::ConditionFulfillment< true > {  } ;
template<>struct HasBitmask  <Operation :: Set> :  syn::ConditionFulfillment< true > {  } ;
template<>struct HasBitmask  <Operation :: Move> :  syn::ConditionFulfillment< true > {  } ;
template<>struct HasBitmask  <Operation :: Compare> :  syn::ConditionFulfillment< true > {  } ;
template<Operation val>constexpr bool hasBitmask() noexcept { return HasBitmask<val> :: value; } 
template<>struct HasImmediateFlag  <Operation :: Branch> :  syn::ConditionFulfillment< true > {  } ;
template<>struct HasImmediateFlag  <Operation :: Logical> :  syn::ConditionFulfillment< true > {  } ;
template<>struct HasImmediateFlag  <Operation :: Compare> :  syn::ConditionFulfillment< true > {  } ;
template<>struct HasImmediateFlag  <Operation :: Shift> :  syn::ConditionFulfillment< true > {  } ;
template<>struct HasImmediateFlag  <Operation :: Arithmetic> :  syn::ConditionFulfillment< true > {  } ;
template<Operation val>constexpr bool hasImmediateFlag() noexcept { return HasImmediateFlag<val> :: value; } 
template<ComplexSubTypes v>struct EncodeSubTypeComplexSubTypes : syn::ConditionFulfillment<false> { using ReturnType = Word;using CastTo = SubTypeOfComplexSubTypes<v>;constexpr ReturnType encodeSubType(ReturnType input, SubTypeOfComplexSubTypes<v> data) noexcept { return input; }  } ;
template<Operation v>struct EncodeSubTypeOperation : syn::ConditionFulfillment<false> { using ReturnType = Word;using CastTo = SubTypeOfOperation<v>;constexpr ReturnType encodeSubType(ReturnType input, SubTypeOfOperation<v> data) noexcept { return input; }  } ;
constexpr ALUOperation translate(ArithmeticOps op) noexcept { switch(op) { case ArithmeticOps :: Rem:return translateArithmeticOps<ArithmeticOps :: Rem>;case ArithmeticOps :: Div:return translateArithmeticOps<ArithmeticOps :: Div>;case ArithmeticOps :: Mul:return translateArithmeticOps<ArithmeticOps :: Mul>;case ArithmeticOps :: Sub:return translateArithmeticOps<ArithmeticOps :: Sub>;case ArithmeticOps :: Add:return translateArithmeticOps<ArithmeticOps :: Add>;default: return syn::defaultErrorState<ALUOperation>; }  } 
constexpr CompareUnitOperation translate(CompareStyle op) noexcept { switch(op) { case CompareStyle :: GreaterThanOrEqualTo:return translateCompareStyle<CompareStyle :: GreaterThanOrEqualTo>;case CompareStyle :: GreaterThan:return translateCompareStyle<CompareStyle :: GreaterThan>;case CompareStyle :: LessThanOrEqualTo:return translateCompareStyle<CompareStyle :: LessThanOrEqualTo>;case CompareStyle :: LessThan:return translateCompareStyle<CompareStyle :: LessThan>;case CompareStyle :: NotEquals:return translateCompareStyle<CompareStyle :: NotEquals>;case CompareStyle :: Equals:return translateCompareStyle<CompareStyle :: Equals>;default: return syn::defaultErrorState<CompareUnitOperation>; }  } 
constexpr ALUOperation translate(LogicalOps op) noexcept { switch(op) { case LogicalOps :: Nand:return translateLogicalOps<LogicalOps :: Nand>;case LogicalOps :: Xor:return translateLogicalOps<LogicalOps :: Xor>;case LogicalOps :: And:return translateLogicalOps<LogicalOps :: And>;case LogicalOps :: Or:return translateLogicalOps<LogicalOps :: Or>;case LogicalOps :: Not:return translateLogicalOps<LogicalOps :: Not>;default: return syn::defaultErrorState<ALUOperation>; }  } 
template<>struct EncodeSource  <Operation :: Logical> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = byte;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeLogicalSource (in, val); }  } ;
template<>struct EncodeSource  <Operation :: Swap> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = byte;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeSwapSource (in, val); }  } ;
template<>struct EncodeSource  <Operation :: Move> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = byte;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeMoveSource (in, val); }  } ;
template<>struct EncodeSource  <Operation :: Compare> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = byte;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeCompareSource (in, val); }  } ;
template<>struct EncodeSource  <Operation :: Shift> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = byte;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeShiftSource (in, val); }  } ;
template<>struct EncodeSource  <Operation :: Arithmetic> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = byte;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeArithmeticSource (in, val); }  } ;
template<Operation v, typename T = typename EncodeSource<v> :: CastTo>constexpr typename EncodeSource<v> :: ReturnType encodeSource(typename EncodeSource<v> :: ReturnType in, T value) noexcept { static_assert(syn::fulfillsCondition<EncodeSource<v>> (), "Provided control does not have support for concept Source!");return EncodeSource<v> :: encode (in, static_cast<typename EncodeSource<v> :: CastTo>(value)); } 
template<>struct DecodeSource  <Operation :: Logical> :  syn::ConditionFulfillment< true > { using ReturnType = byte;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeLogicalSource (in); }  } ;
template<>struct DecodeSource  <Operation :: Swap> :  syn::ConditionFulfillment< true > { using ReturnType = byte;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeSwapSource (in); }  } ;
template<>struct DecodeSource  <Operation :: Move> :  syn::ConditionFulfillment< true > { using ReturnType = byte;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeMoveSource (in); }  } ;
template<>struct DecodeSource  <Operation :: Compare> :  syn::ConditionFulfillment< true > { using ReturnType = byte;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeCompareSource (in); }  } ;
template<>struct DecodeSource  <Operation :: Shift> :  syn::ConditionFulfillment< true > { using ReturnType = byte;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeShiftSource (in); }  } ;
template<>struct DecodeSource  <Operation :: Arithmetic> :  syn::ConditionFulfillment< true > { using ReturnType = byte;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeArithmeticSource (in); }  } ;
template<Operation v>constexpr typename DecodeSource<v> :: ReturnType decodeSource(typename DecodeSource<v> :: CastTo in) noexcept { static_assert(syn::fulfillsCondition<DecodeSource<v>> (), "Provided control does not have support for concept Source!");return DecodeSource<v> :: decode (in); } 
template<>struct EncodeBitmask  <Operation :: Logical> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = byte;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeLogicalBitmask (in, val); }  } ;
template<>struct EncodeBitmask  <Operation :: Memory> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = byte;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeMemoryBitmask (in, val); }  } ;
template<>struct EncodeBitmask  <Operation :: Set> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = byte;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeSetBitmask (in, val); }  } ;
template<>struct EncodeBitmask  <Operation :: Move> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = byte;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeMoveBitmask (in, val); }  } ;
template<>struct EncodeBitmask  <Operation :: Compare> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = byte;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeCompareBitmask (in, val); }  } ;
template<Operation v, typename T = typename EncodeBitmask<v> :: CastTo>constexpr typename EncodeBitmask<v> :: ReturnType encodeBitmask(typename EncodeBitmask<v> :: ReturnType in, T value) noexcept { static_assert(syn::fulfillsCondition<EncodeBitmask<v>> (), "Provided control does not have support for concept Bitmask!");return EncodeBitmask<v> :: encode (in, static_cast<typename EncodeBitmask<v> :: CastTo>(value)); } 
template<>struct DecodeBitmask  <Operation :: Logical> :  syn::ConditionFulfillment< true > { using ReturnType = byte;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeLogicalBitmask (in); }  } ;
template<>struct DecodeBitmask  <Operation :: Memory> :  syn::ConditionFulfillment< true > { using ReturnType = byte;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeMemoryBitmask (in); }  } ;
template<>struct DecodeBitmask  <Operation :: Set> :  syn::ConditionFulfillment< true > { using ReturnType = byte;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeSetBitmask (in); }  } ;
template<>struct DecodeBitmask  <Operation :: Move> :  syn::ConditionFulfillment< true > { using ReturnType = byte;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeMoveBitmask (in); }  } ;
template<>struct DecodeBitmask  <Operation :: Compare> :  syn::ConditionFulfillment< true > { using ReturnType = byte;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeCompareBitmask (in); }  } ;
template<Operation v>constexpr typename DecodeBitmask<v> :: ReturnType decodeBitmask(typename DecodeBitmask<v> :: CastTo in) noexcept { static_assert(syn::fulfillsCondition<DecodeBitmask<v>> (), "Provided control does not have support for concept Bitmask!");return DecodeBitmask<v> :: decode (in); } 
template<>struct EncodeComplexSubType  <ComplexSubTypes :: FeatureCheck> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = FeatureCheckOperation;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeFeatureCheckComplexSubType (in, val); }  } ;
template<>struct EncodeComplexSubType  <ComplexSubTypes :: Parsing> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = ParsingOperation;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeParsingComplexSubType (in, val); }  } ;
template<>struct EncodeComplexSubType  <ComplexSubTypes :: Extended> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = ExtendedOperation;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeExtendedComplexSubType (in, val); }  } ;
template<>struct EncodeComplexSubType  <ComplexSubTypes :: Encoding> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = EncodingOperation;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeEncodingComplexSubType (in, val); }  } ;
template<ComplexSubTypes v, typename T = typename EncodeComplexSubType<v> :: CastTo>constexpr typename EncodeComplexSubType<v> :: ReturnType encodeComplexSubType(typename EncodeComplexSubType<v> :: ReturnType in, T value) noexcept { static_assert(syn::fulfillsCondition<EncodeComplexSubType<v>> (), "Provided control does not have support for concept ComplexSubType!");return EncodeComplexSubType<v> :: encode (in, static_cast<typename EncodeComplexSubType<v> :: CastTo>(value)); } 
template<>struct DecodeComplexSubType  <ComplexSubTypes :: FeatureCheck> :  syn::ConditionFulfillment< true > { using ReturnType = FeatureCheckOperation;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeFeatureCheckComplexSubType (in); }  } ;
template<>struct DecodeComplexSubType  <ComplexSubTypes :: Parsing> :  syn::ConditionFulfillment< true > { using ReturnType = ParsingOperation;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeParsingComplexSubType (in); }  } ;
template<>struct DecodeComplexSubType  <ComplexSubTypes :: Extended> :  syn::ConditionFulfillment< true > { using ReturnType = ExtendedOperation;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeExtendedComplexSubType (in); }  } ;
template<>struct DecodeComplexSubType  <ComplexSubTypes :: Encoding> :  syn::ConditionFulfillment< true > { using ReturnType = EncodingOperation;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeEncodingComplexSubType (in); }  } ;
template<ComplexSubTypes v>constexpr typename DecodeComplexSubType<v> :: ReturnType decodeComplexSubType(typename DecodeComplexSubType<v> :: CastTo in) noexcept { static_assert(syn::fulfillsCondition<DecodeComplexSubType<v>> (), "Provided control does not have support for concept ComplexSubType!");return DecodeComplexSubType<v> :: decode (in); } 
template<>struct EncodeType  <Operation :: Complex> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = ComplexSubTypes;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeComplexType (in, val); }  } ;
template<>struct EncodeType  <Operation :: Memory> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = MemoryOperation;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeMemoryType (in, val); }  } ;
template<>struct EncodeType  <Operation :: Logical> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = LogicalOps;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeLogicalType (in, val); }  } ;
template<>struct EncodeType  <Operation :: Compare> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = CompareStyle;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeCompareType (in, val); }  } ;
template<>struct EncodeType  <Operation :: Arithmetic> :  syn::ConditionFulfillment< true > { using ReturnType = Word;using CastTo = ArithmeticOps;static constexpr ReturnType encode(ReturnType in, CastTo val) noexcept { return encodeArithmeticType (in, val); }  } ;
template<Operation v, typename T = typename EncodeType<v> :: CastTo>constexpr typename EncodeType<v> :: ReturnType encodeType(typename EncodeType<v> :: ReturnType in, T value) noexcept { static_assert(syn::fulfillsCondition<EncodeType<v>> (), "Provided control does not have support for concept Type!");return EncodeType<v> :: encode (in, static_cast<typename EncodeType<v> :: CastTo>(value)); } 
template<>struct DecodeType  <Operation :: Complex> :  syn::ConditionFulfillment< true > { using ReturnType = ComplexSubTypes;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeComplexType (in); }  } ;
template<>struct DecodeType  <Operation :: Memory> :  syn::ConditionFulfillment< true > { using ReturnType = MemoryOperation;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeMemoryType (in); }  } ;
template<>struct DecodeType  <Operation :: Logical> :  syn::ConditionFulfillment< true > { using ReturnType = LogicalOps;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeLogicalType (in); }  } ;
template<>struct DecodeType  <Operation :: Compare> :  syn::ConditionFulfillment< true > { using ReturnType = CompareStyle;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeCompareType (in); }  } ;
template<>struct DecodeType  <Operation :: Arithmetic> :  syn::ConditionFulfillment< true > { using ReturnType = ArithmeticOps;using CastTo = Word;static constexpr ReturnType decode(CastTo in) noexcept { return decodeArithmeticType (in); }  } ;
template<Operation v>constexpr typename DecodeType<v> :: ReturnType decodeType(typename DecodeType<v> :: CastTo in) noexcept { static_assert(syn::fulfillsCondition<DecodeType<v>> (), "Provided control does not have support for concept Type!");return DecodeType<v> :: decode (in); } 
} // end namespace cisc0
#endif // end _CISC0_DECL
