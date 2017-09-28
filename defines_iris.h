// NOTE: this file is auto generated, DO NOT MODIFY!
#ifndef _IRIS_DECL
#define _IRIS_DECL
#include "Base.h"
#include "ExecutionUnits.h"
namespace iris { 
template<typename T, T op>
constexpr auto toExecutionUnitValue = syn::defaultErrorState<T>;
using CRUnitOp = syn::Comparator::BooleanOperations;
using ComparatorOp = syn::Comparator::StandardOperations;
using ALUOperation = syn::ALU::StandardOperations;
enum class InstructionGroup : byte { Arithmetic, Move, Jump, Compare, ConditionalRegister, Unused0, CustomInstructionReserved, Count } ;
static_assert(static_cast<byte>(InstructionGroup :: Count) <= static_cast<byte>(ArchitectureConstants::MaxGroups), "Too many InstructionGroup entries defined!");
template<InstructionGroup op>
constexpr auto translateInstructionGroup = toExecutionUnitValue<decltype(op), op>;
enum class ArithmeticOp : byte { Add, Sub, Mul, Div, Rem, ShiftLeft, ShiftRight, BinaryAnd, BinaryOr, BinaryNot, BinaryXor, BinaryNand, BinaryNor, AddImmediate, SubImmediate, MulImmediate, DivImmediate, RemImmediate, ShiftLeftImmediate, ShiftRightImmediate, Min, Max, Count } ;
static_assert(static_cast<byte>(ArithmeticOp :: Count) <= static_cast<byte>(ArchitectureConstants::MaxOperations), "Too many ArithmeticOp entries defined!");
template<ArithmeticOp op>
constexpr auto translateArithmeticOp = toExecutionUnitValue<decltype(op), op>;
enum class JumpOp : byte { BranchUnconditionalImmediate, BranchUnconditionalImmediateLink, BranchUnconditional, BranchUnconditionalLink, BranchConditionalImmediate, BranchConditionalImmediateLink, BranchConditional, BranchConditionalLink, BranchUnconditionalLR, BranchUnconditionalLRAndLink, BranchConditionalLR, BranchConditionalLRAndLink, ReturnFromError, Count } ;
static_assert(static_cast<byte>(JumpOp :: Count) <= static_cast<byte>(ArchitectureConstants::MaxOperations), "Too many JumpOp entries defined!");
template<JumpOp op>
constexpr auto translateJumpOp = toExecutionUnitValue<decltype(op), op>;
enum class MoveOp : byte { Move, Set, Swap, Load, LoadImmediate, LoadWithOffset, Store, StoreImmediate, StoreWithOffset, Push, PushImmediate, Pop, LoadCode, StoreCode, LoadIO, StoreIO, LoadIOWithOffset, StoreIOWithOffset, MoveFromIP, MoveToIP, MoveFromLR, MoveToLR, SaveAllRegisters, RestoreAllRegisters, Count } ;
static_assert(static_cast<byte>(MoveOp :: Count) <= static_cast<byte>(ArchitectureConstants::MaxOperations), "Too many MoveOp entries defined!");
template<MoveOp op>
constexpr auto translateMoveOp = toExecutionUnitValue<decltype(op), op>;
enum class CompareOp : byte { Eq, EqImmediate, Neq, NeqImmediate, LessThan, LessThanImmediate, GreaterThan, GreaterThanImmediate, LessThanOrEqualTo, LessThanOrEqualToImmediate, GreaterThanOrEqualTo, GreaterThanOrEqualToImmediate, Count } ;
static_assert(static_cast<byte>(CompareOp :: Count) <= static_cast<byte>(ArchitectureConstants::MaxOperations), "Too many CompareOp entries defined!");
template<CompareOp op>
constexpr auto translateCompareOp = toExecutionUnitValue<decltype(op), op>;
enum class ConditionRegisterOp : byte { SaveCRs, RestoreCRs, CRXor, CRNot, CRAnd, CROr, CRNand, CRNor, CRSwap, CRMove, Count } ;
static_assert(static_cast<byte>(ConditionRegisterOp :: Count) <= static_cast<byte>(ArchitectureConstants::MaxOperations), "Too many ConditionRegisterOp entries defined!");
template<ConditionRegisterOp op>
constexpr auto translateConditionRegisterOp = toExecutionUnitValue<decltype(op), op>;
template<>constexpr auto toExecutionUnitValue<ConditionRegisterOp, ConditionRegisterOp :: CRNot> = CRUnitOp :: UnaryNot;
template<>constexpr auto toExecutionUnitValue<ConditionRegisterOp, ConditionRegisterOp :: CRXor> = CRUnitOp :: BinaryXor;
template<>constexpr auto toExecutionUnitValue<ConditionRegisterOp, ConditionRegisterOp :: CRNor> = CRUnitOp :: BinaryNor;
template<>constexpr auto toExecutionUnitValue<ConditionRegisterOp, ConditionRegisterOp :: CRNand> = CRUnitOp :: BinaryNand;
template<>constexpr auto toExecutionUnitValue<ConditionRegisterOp, ConditionRegisterOp :: CROr> = CRUnitOp :: BinaryOr;
template<>constexpr auto toExecutionUnitValue<ConditionRegisterOp, ConditionRegisterOp :: CRAnd> = CRUnitOp :: BinaryAnd;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: NeqImmediate> = ComparatorOp :: Neq;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: Neq> = ComparatorOp :: Neq;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: EqImmediate> = ComparatorOp :: Eq;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: Eq> = ComparatorOp :: Eq;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: GreaterThanOrEqualToImmediate> = ComparatorOp :: GreaterThanOrEqualTo;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: GreaterThanOrEqualTo> = ComparatorOp :: GreaterThanOrEqualTo;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: GreaterThanImmediate> = ComparatorOp :: GreaterThan;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: GreaterThan> = ComparatorOp :: GreaterThan;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: LessThanOrEqualToImmediate> = ComparatorOp :: LessThanOrEqualTo;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: LessThanOrEqualTo> = ComparatorOp :: LessThanOrEqualTo;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: LessThanImmediate> = ComparatorOp :: LessThan;
template<>constexpr auto toExecutionUnitValue<CompareOp, CompareOp :: LessThan> = ComparatorOp :: LessThan;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: BinaryXor> = ALUOperation :: BinaryXor;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: BinaryAnd> = ALUOperation :: BinaryAnd;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: BinaryOr> = ALUOperation :: BinaryOr;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: BinaryNot> = ALUOperation :: UnaryNot;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: ShiftRightImmediate> = ALUOperation :: ShiftRight;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: ShiftRight> = ALUOperation :: ShiftRight;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: ShiftLeftImmediate> = ALUOperation :: ShiftLeft;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: ShiftLeft> = ALUOperation :: ShiftLeft;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: RemImmediate> = ALUOperation :: Remainder;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: Rem> = ALUOperation :: Remainder;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: DivImmediate> = ALUOperation :: Divide;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: Div> = ALUOperation :: Divide;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: MulImmediate> = ALUOperation :: Multiply;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: Mul> = ALUOperation :: Multiply;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: SubImmediate> = ALUOperation :: Subtract;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: Sub> = ALUOperation :: Subtract;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: AddImmediate> = ALUOperation :: Add;
template<>constexpr auto toExecutionUnitValue<ArithmeticOp, ArithmeticOp :: Add> = ALUOperation :: Add;
constexpr byte decodeGroup(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, byte, 0x00000007, 0> (value); } 
constexpr raw_instruction encodeGroup(raw_instruction value, byte field) noexcept { return syn::encodeBits<raw_instruction, byte, 0x00000007, 0> (value, field); } 
constexpr byte decodeOperation(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, byte, 0x000000F8, 3> (value); } 
constexpr raw_instruction encodeOperation(raw_instruction value, byte field) noexcept { return syn::encodeBits<raw_instruction, byte, 0x000000F8, 3> (value, field); } 
constexpr byte decodeDestination(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, byte, 0x0000FF00, 8> (value); } 
constexpr raw_instruction encodeDestination(raw_instruction value, byte field) noexcept { return syn::encodeBits<raw_instruction, byte, 0x0000FF00, 8> (value, field); } 
constexpr byte decodeSource0(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, byte, 0x00FF0000, 16> (value); } 
constexpr raw_instruction encodeSource0(raw_instruction value, byte field) noexcept { return syn::encodeBits<raw_instruction, byte, 0x00FF0000, 16> (value, field); } 
constexpr byte decodeSource1(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, byte, 0xFF000000, 24> (value); } 
constexpr raw_instruction encodeSource1(raw_instruction value, byte field) noexcept { return syn::encodeBits<raw_instruction, byte, 0xFF000000, 24> (value, field); } 
constexpr word decodeHalfImmediate(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, word, 0xFF000000, 24> (value); } 
constexpr raw_instruction encodeHalfImmediate(raw_instruction value, word field) noexcept { return syn::encodeBits<raw_instruction, word, 0xFF000000, 24> (value, field); } 
constexpr word decodeImmediate(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, word, 0xFFFF0000, 16> (value); } 
constexpr raw_instruction encodeImmediate(raw_instruction value, word field) noexcept { return syn::encodeBits<raw_instruction, word, 0xFFFF0000, 16> (value, field); } 
constexpr byte decodePredicateResult(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, byte, 0x00000F00, 8> (value); } 
constexpr raw_instruction encodePredicateResult(raw_instruction value, byte field) noexcept { return syn::encodeBits<raw_instruction, byte, 0x00000F00, 8> (value, field); } 
constexpr byte decodePredicateInverseResult(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, byte, 0x0000F000, 12> (value); } 
constexpr raw_instruction encodePredicateInverseResult(raw_instruction value, byte field) noexcept { return syn::encodeBits<raw_instruction, byte, 0x0000F000, 12> (value, field); } 
constexpr byte decodePredicateSource0(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, byte, 0x000F0000, 16> (value); } 
constexpr raw_instruction encodePredicateSource0(raw_instruction value, byte field) noexcept { return syn::encodeBits<raw_instruction, byte, 0x000F0000, 16> (value, field); } 
constexpr byte decodePredicateSource1(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, byte, 0x00F00000, 20> (value); } 
constexpr raw_instruction encodePredicateSource1(raw_instruction value, byte field) noexcept { return syn::encodeBits<raw_instruction, byte, 0x00F00000, 20> (value, field); } 
constexpr byte decodeLower4Bits(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, byte, 0x0F, 0> (value); } 
constexpr raw_instruction encodeLower4Bits(raw_instruction value, byte field) noexcept { return syn::encodeBits<raw_instruction, byte, 0x0F, 0> (value, field); } 
constexpr byte decodeUpper4Bits(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, byte, 0xF0, 4> (value); } 
constexpr raw_instruction encodeUpper4Bits(raw_instruction value, byte field) noexcept { return syn::encodeBits<raw_instruction, byte, 0xF0, 4> (value, field); } 
constexpr bool decodeStatusInError(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, bool, 0b0000000000000001, 0> (value); } 
constexpr raw_instruction encodeStatusInError(raw_instruction value, bool field) noexcept { return syn::encodeBits<raw_instruction, bool, 0b0000000000000001, 0> (value, field); } 
constexpr bool decodeStatusDivideByZero(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, bool, 0b0000000000000010, 1> (value); } 
constexpr raw_instruction encodeStatusDivideByZero(raw_instruction value, bool field) noexcept { return syn::encodeBits<raw_instruction, bool, 0b0000000000000010, 1> (value, field); } 
constexpr bool decodeStatusIllegalInstruction(raw_instruction value) noexcept { return syn::decodeBits<raw_instruction, bool, 0b0000000000000100, 2> (value); } 
constexpr raw_instruction encodeStatusIllegalInstruction(raw_instruction value, bool field) noexcept { return syn::encodeBits<raw_instruction, bool, 0b0000000000000100, 2> (value, field); } 
constexpr ALUOperation translate(ArithmeticOp op) noexcept { switch(op) { case ArithmeticOp :: BinaryXor:return translateArithmeticOp<ArithmeticOp :: BinaryXor>;case ArithmeticOp :: BinaryAnd:return translateArithmeticOp<ArithmeticOp :: BinaryAnd>;case ArithmeticOp :: BinaryOr:return translateArithmeticOp<ArithmeticOp :: BinaryOr>;case ArithmeticOp :: BinaryNot:return translateArithmeticOp<ArithmeticOp :: BinaryNot>;case ArithmeticOp :: ShiftRightImmediate:return translateArithmeticOp<ArithmeticOp :: ShiftRightImmediate>;case ArithmeticOp :: ShiftRight:return translateArithmeticOp<ArithmeticOp :: ShiftRight>;case ArithmeticOp :: ShiftLeftImmediate:return translateArithmeticOp<ArithmeticOp :: ShiftLeftImmediate>;case ArithmeticOp :: ShiftLeft:return translateArithmeticOp<ArithmeticOp :: ShiftLeft>;case ArithmeticOp :: RemImmediate:return translateArithmeticOp<ArithmeticOp :: RemImmediate>;case ArithmeticOp :: Rem:return translateArithmeticOp<ArithmeticOp :: Rem>;case ArithmeticOp :: DivImmediate:return translateArithmeticOp<ArithmeticOp :: DivImmediate>;case ArithmeticOp :: Div:return translateArithmeticOp<ArithmeticOp :: Div>;case ArithmeticOp :: MulImmediate:return translateArithmeticOp<ArithmeticOp :: MulImmediate>;case ArithmeticOp :: Mul:return translateArithmeticOp<ArithmeticOp :: Mul>;case ArithmeticOp :: SubImmediate:return translateArithmeticOp<ArithmeticOp :: SubImmediate>;case ArithmeticOp :: Sub:return translateArithmeticOp<ArithmeticOp :: Sub>;case ArithmeticOp :: AddImmediate:return translateArithmeticOp<ArithmeticOp :: AddImmediate>;case ArithmeticOp :: Add:return translateArithmeticOp<ArithmeticOp :: Add>;default: return syn::defaultErrorState<ALUOperation>; }  } 
constexpr ComparatorOp translate(CompareOp op) noexcept { switch(op) { case CompareOp :: NeqImmediate:return translateCompareOp<CompareOp :: NeqImmediate>;case CompareOp :: Neq:return translateCompareOp<CompareOp :: Neq>;case CompareOp :: EqImmediate:return translateCompareOp<CompareOp :: EqImmediate>;case CompareOp :: Eq:return translateCompareOp<CompareOp :: Eq>;case CompareOp :: GreaterThanOrEqualToImmediate:return translateCompareOp<CompareOp :: GreaterThanOrEqualToImmediate>;case CompareOp :: GreaterThanOrEqualTo:return translateCompareOp<CompareOp :: GreaterThanOrEqualTo>;case CompareOp :: GreaterThanImmediate:return translateCompareOp<CompareOp :: GreaterThanImmediate>;case CompareOp :: GreaterThan:return translateCompareOp<CompareOp :: GreaterThan>;case CompareOp :: LessThanOrEqualToImmediate:return translateCompareOp<CompareOp :: LessThanOrEqualToImmediate>;case CompareOp :: LessThanOrEqualTo:return translateCompareOp<CompareOp :: LessThanOrEqualTo>;case CompareOp :: LessThanImmediate:return translateCompareOp<CompareOp :: LessThanImmediate>;case CompareOp :: LessThan:return translateCompareOp<CompareOp :: LessThan>;default: return syn::defaultErrorState<ComparatorOp>; }  } 
constexpr CRUnitOp translate(ConditionRegisterOp op) noexcept { switch(op) { case ConditionRegisterOp :: CRNot:return translateConditionRegisterOp<ConditionRegisterOp :: CRNot>;case ConditionRegisterOp :: CRXor:return translateConditionRegisterOp<ConditionRegisterOp :: CRXor>;case ConditionRegisterOp :: CRNor:return translateConditionRegisterOp<ConditionRegisterOp :: CRNor>;case ConditionRegisterOp :: CRNand:return translateConditionRegisterOp<ConditionRegisterOp :: CRNand>;case ConditionRegisterOp :: CROr:return translateConditionRegisterOp<ConditionRegisterOp :: CROr>;case ConditionRegisterOp :: CRAnd:return translateConditionRegisterOp<ConditionRegisterOp :: CRAnd>;default: return syn::defaultErrorState<CRUnitOp>; }  } 
} // end namespace iris
#endif // end _IRIS_DECL
