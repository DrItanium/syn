/**
 * @file
 * The assembler grammar and other related functions
 * @copyright
 * syn
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#ifndef CISC0_CORE_ASSEMBLER_H__
#define CISC0_CORE_ASSEMBLER_H__

#include <string>
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include <tao/pegtl.hpp>
#include <tao/pegtl/analyze.hpp>
#include <tao/pegtl/contrib/raw_string.hpp>
#include <tao/pegtl/contrib/abnf.hpp>
#include <tao/pegtl/parse.hpp>
#include <vector>

#include "Base.h"
#include "AssemblerBase.h"
#include "Problem.h"
#include "ClipsExtensions.h"
#include "Cisc0CoreInstructionEncoder.h"
#include "Cisc0ClipsExtensions.h"
#include "Cisc0CoreAssemblerKeywords.h"
#include "Cisc0CoreAssemblerStructures.h"

using namespace tao::TAOCPP_PEGTL_NAMESPACE;
namespace cisc0 {

    /**
     * The cisc0 text -> encoded number translator
     */
    namespace assembler {
	    using Separator = syn::AsmSeparator;
	    using SingleLineComment = syn::SingleLineComment<';'>;
	    template<typename R> struct Action : syn::Action<R> { };

	    template<typename First, typename Rest, typename Sep = Separator>
	    struct SeparatedBinaryThing : syn::TwoPartComponent<First, Rest, Sep> { };

	    template<typename First, typename Second, typename Third, typename Sep = Separator>
	    struct SeparatedTrinaryThing : syn::ThreePartComponent<First, Second, Third, Sep, Sep> { };

	    template<typename First, typename Second, typename Third, typename Fourth, typename Sep = Separator>
	    struct SeparatedQuadThing : seq<First, Sep, Second, Sep, Third, Sep, Fourth> { };



#define DefApplyInstruction DefApplyGeneric(cisc0::assembler::AssemblerInstruction)
#define DefApplyAsmState DefApplyGeneric(cisc0::assembler::AssemblerState)


        template<Operation op>
        struct SetOperationOnApply {
            static void apply0(AssemblerInstruction& state) {
                state.setType<op>();
            }
        };

        /**
         * Describes the instruction as an immediate type!
         */
	    struct UsesImmediate : KeywordImmediate { };

	    DefAction(UsesImmediate) {
	    	DefApplyInstruction {
	    		state.markImmediate();
	    	}
	    };

	    DefAction(syn::HexadecimalNumber) { DefApplyGeneric(NumberContainer) { syn::populateContainer<RegisterValue, syn::KnownNumberTypes::Hexadecimal>(in.string(), state); } };
	    DefAction(syn::BinaryNumber) { DefApplyGeneric(NumberContainer) { syn::populateContainer<RegisterValue, syn::KnownNumberTypes::Binary>(in.string(), state); } };
	    using DecimalNumber = syn::Base10Number;
	    DefAction(DecimalNumber) { DefApplyGeneric(NumberContainer) { syn::populateContainer<RegisterValue, syn::KnownNumberTypes::Decimal>(in.string(), state); } };
	    struct Number : syn::StatefulNumberAll<NumberContainer> { };
	    DefAction(Number) {
	    	DefApplyInstruction {
	    		state.markAsNotLabel();
	    	}
	    	DefApplyGenericEmpty(ChangeCurrentAddress)
	    	DefApplyGenericEmpty(WordCreator)
	    	DefApplyGenericEmpty(DwordCreator)
	    };

	    struct BitmaskNumber : syn::GenericNumeral<'m', abnf::BIT> { };

	    DefAction(BitmaskNumber) {
	    	DefApplyInstruction {
	    		state.setBitmask(syn::decodeBits<RegisterValue, byte, 0x000000FF, 0>(syn::getBinaryImmediate<RegisterValue>(in.string(), syn::reportError)));
	    	}
	    };
	    using Lexeme = syn::Lexeme;
	    DefAction(Lexeme) {
	    	DefApplyInstruction {
	    		state.setFullImmediate(0);
	    		state.setLabelName(in.string());
	    	}
	    	DefApplyGeneric(RegisterLabel) {
	    		state.setTitle(in.string());
	    	}
	    	template<typename Input, int width>
	    		static void applyToWordCreator(const Input& in, AssemblerWordCreator<width>& state) {
	    			state.setLabel(in.string());
	    		}

	    	DefApplyGeneric(WordCreator) {
	    		applyToWordCreator<Input, 1>(in, state);
	    	}
	    	DefApplyGeneric(DwordCreator) {
	    		applyToWordCreator<Input, 2>(in, state);
	    	}
	    };
	    struct LexemeOrNumber : syn::LexemeOr<Number> { };

        using NormalRegister = syn::GPR;
	    struct GeneralPurposeRegister : sor<
	    								NormalRegister,
	    								SymbolAddrRegister,
	    								SymbolStackPointer,
	    								SymbolInstructionPointer,
	    								SymbolCallStackPointer,
	    								SymbolValueRegister,
	    								SymbolMaskRegister,
	    								SymbolFieldRegister> { };

	    struct IndirectGPR : seq<GeneralPurposeRegister> { };
#define DefIndirectGPR(title) \
	    struct title : IndirectGPR { }

	    DefIndirectGPR(DestinationRegister);
	    DefAction(DestinationRegister) {
	    	DefApplyInstruction {
                state.setFirstArg(translateRegister(in.string()));
	    	}
	    };

	    DefIndirectGPR(SourceRegister);
	    DefAction(SourceRegister) {
	    	DefApplyInstruction {
	    		state.setSecondArg(translateRegister(in.string()));
	    	}
	    };

	    DefIndirectGPR(SourceRegister1);
	    DefAction(SourceRegister1) {
	    	DefApplyInstruction {
	    		state.setThirdArg(translateRegister(in.string()));
	    	}
	    };
	    template<typename S>
	    struct TwoArgumentOperation : SeparatedBinaryThing<
	    							  DestinationRegister,
	    							  S> { };

	    struct TwoGPRs : TwoArgumentOperation<SourceRegister> { };
	    DefAction(TwoGPRs) {
	    	DefApplyInstruction {
	    		state.markImmediate(false);
	    	}
	    };


	    struct ShiftLeftOrRight : sor<
	    						  SymbolLeft,
	    						  SymbolRight> { };

	    DefAction(ShiftLeftOrRight) {
	    	DefApplyInstruction {
	    		state.setShiftDirection(in.string() == "left");
	    	}
	    };

	    template<typename Source>
	    struct ImmediateOperationArgs : SeparatedBinaryThing<
	    								UsesImmediate,
	    								TwoArgumentOperation<Source>> { };
	    template<typename Source>
	    struct ImmediateOperationArgsWithBitmask : SeparatedTrinaryThing<
	    										   UsesImmediate,
	    										   BitmaskNumber,
	    										   TwoArgumentOperation<Source>> { };
	    struct SpecialImmediate : seq<Number> { };
	    struct ShiftImmediateValue : SpecialImmediate { };
	    DefAction(ShiftImmediateValue) {
	    	DefApplyInstruction {
                state.setSecondArg(static_cast<byte>(state.getFullImmediate()) & 0b11111);
	    	}
	    };
	    struct ShiftArgs : sor<
	    				   TwoGPRs,
	    				   ImmediateOperationArgs<ShiftImmediateValue>> { };

	    struct ShiftOperation : SeparatedTrinaryThing<
	    						SymbolShift,
	    						ShiftLeftOrRight,
	    						ShiftArgs> { };
        DefAction(ShiftOperation) : SetOperationOnApply<Operation::Shift> { };

	    struct ByteCastImmediate : SpecialImmediate { };
	    DefAction(ByteCastImmediate) {
	    	DefApplyInstruction {
                state.setSecondArg(static_cast<byte>(state.getFullImmediate()));
	    	}
	    };

        template<Operation op>
        struct ConvertOperationToSubType {
            static_assert(HasSubtype<op>(), "Provided operation does not have a subtype!");
            DefApplyGeneric(AssemblerInstruction) {
                switch(op) {
                    case Operation::Compare:
                        state.setSubType(stringToCompareStyle(in.string()));
                        break;
                    case Operation::Arithmetic:
                        state.setSubType(stringToArithmeticOps(in.string()));
                        break;
                    case Operation::Memory:
                        state.setSubType(stringToMemoryOperation(in.string()));
                        break;
                    case Operation::Logical:
                        state.setSubType(stringToLogicalOps(in.string()));
                        break;
                    case Operation::Complex:
                        state.setSubType(stringToComplexSubTypes(in.string()));
                        break;
                }
            }
        };

        using GetCompareSubType = ConvertOperationToSubType<Operation::Compare>;
	    struct CompareType : sor<
	    					 SymbolEquals,
	    					 SymbolNotEquals,
	    					 SymbolLessThan,
	    					 SymbolLessThanOrEqualTo,
	    					 SymbolGreaterThan,
	    					 SymbolGreaterThanOrEqualTo> { };
	    struct SpecialCompareType : sor<
	    							SymbolMoveFromCondition,
                                    SymbolMoveToCondition> { };
        DefAction(CompareType) : GetCompareSubType { };
        DefAction(SpecialCompareType) : GetCompareSubType { };
	    struct CompareArgs : sor<
	    					 TwoGPRs,
	    					 ImmediateOperationArgsWithBitmask<LexemeOrNumber>> { };
	    struct NormalCompareOperation : SeparatedBinaryThing<
	    								CompareType,
	    								CompareArgs> { };
	    struct SpecialCompareOperation : SeparatedBinaryThing<
	    								 SpecialCompareType,
	    								 DestinationRegister> { };
	    struct CompareOperation : SeparatedBinaryThing<
	    						  SymbolCompare,
	    						  sor<
	    									 NormalCompareOperation,
	    									 SpecialCompareOperation>> { };
        DefAction(CompareOperation) : SetOperationOnApply<Operation::Compare> { };
	    struct MoveOperation : SeparatedTrinaryThing<
	    					   SymbolMove,
	    					   BitmaskNumber,
	    					   TwoGPRs> { };
        DefAction(MoveOperation) : SetOperationOnApply<Operation::Move> { };
	    struct SetOperation : SeparatedQuadThing<
	    					  SymbolSet,
	    					  BitmaskNumber,
	    					  DestinationRegister,
	    					  LexemeOrNumber> { };
        DefAction(SetOperation) : SetOperationOnApply<Operation::Set> { };
	    struct SwapOperation : SeparatedBinaryThing<
	    					   SymbolSwap,
	    					   TwoGPRs> { };
        DefAction(SwapOperation) : SetOperationOnApply<Operation::Swap> { };

	    struct Arg0ImmediateValue : SpecialImmediate { };
	    DefAction(Arg0ImmediateValue) {
	    	DefApplyInstruction {
                state.setFirstArg(static_cast<byte>(state.getFullImmediate()) & 0b1111);
	    	}
	    };
	    struct ArithmeticType : sor<
                                SymbolAdd,
                                SymbolSub,
                                SymbolMul,
                                SymbolDiv,
                                SymbolRem,
                                SymbolMin,
                                SymbolMax> { };
        DefAction(ArithmeticType) : ConvertOperationToSubType<Operation::Arithmetic> { };

	    struct ArithmeticArgs : sor<
	    						TwoGPRs,
	    						ImmediateOperationArgs<ByteCastImmediate>> { };
	    struct ArithmeticOperation : SeparatedTrinaryThing<SymbolArithmetic, ArithmeticType, ArithmeticArgs> { };

        DefAction(ArithmeticOperation) : SetOperationOnApply<Operation::Arithmetic> { };


        using GetMemorySubType = ConvertOperationToSubType<Operation::Memory>;
	    struct LoadStoreType : sor<
	    					   SymbolLoad,
	    					   SymbolStore> { };

        DefAction(LoadStoreType) : GetMemorySubType { };
	    struct StackMemoryType : sor<
	    						 SymbolPush,
	    						 SymbolPop> { };
        DefAction(StackMemoryType) : GetMemorySubType { };
	    struct StackOperation : SeparatedTrinaryThing<StackMemoryType, BitmaskNumber, DestinationRegister> { };
	    struct FlagIndirect : syn::SingleEntrySequence<SymbolIndirect> { };
	    struct FlagDirect : syn::SingleEntrySequence<SymbolDirect> { };
	    struct FlagDirectOrIndirect : sor<
	    							  FlagDirect,
	    							  FlagIndirect> { };
	    DefAction(FlagDirectOrIndirect) {
	    	DefApplyInstruction {
	    		static std::string compare("indirect");
	    		state.markIndirect(in.string() == compare);
	    	}
	    };
	    struct LoadStoreOperation : SeparatedQuadThing<
	    							LoadStoreType,
	    							BitmaskNumber,
	    							FlagDirectOrIndirect,
	    							Arg0ImmediateValue> { };

	    struct MemoryTypes : sor<
	    					 StackOperation,
	    					 LoadStoreOperation> { };
	    struct MemoryInstruction : SeparatedBinaryThing<
	    						   SymbolMemory,
	    						   MemoryTypes> { };
        DefAction(MemoryInstruction) : SetOperationOnApply<Operation::Memory> { };



	    struct LogicalOpsType : sor<
	    						SymbolAnd,
	    						SymbolOr,
	    						SymbolNot,
	    						SymbolXor,
	    						SymbolNand> { };
        DefAction(LogicalOpsType) : ConvertOperationToSubType<Operation::Logical> { };
	    struct LogicalArgs : sor<
	    					 TwoGPRs,
	    					 ImmediateOperationArgsWithBitmask<LexemeOrNumber>> { };
	    struct LogicalOperation : SeparatedTrinaryThing<SymbolLogical, LogicalOpsType, LogicalArgs> { };
        DefAction(LogicalOperation) : SetOperationOnApply<Operation::Logical> { };

        template<ComplexSubTypes type>
        struct SetComplexSubSubType {
            static_assert(!syn::isErrorState(type), "Can't operate on the error type!");
            DefApplyInstruction {
                switch(type) {
                    case ComplexSubTypes::Encoding:
                        state.setBitmask(stringToEncodingOperation(in.string()));
                        break;
                    case ComplexSubTypes::Extended:
                        state.setBitmask(stringToExtendedOperation(in.string()));
                        break;
                    case ComplexSubTypes::Parsing:
                        state.setBitmask(stringToParsingOperation(in.string()));
                        break;
                }
            }
        };

	    struct ComplexEncodingSubOperation : sor<
	    									 SymbolDecode,
	    									 SymbolEncode,
	    									 SymbolBitSet,
	    									 SymbolBitUnset> { };
        DefAction(ComplexEncodingSubOperation) : SetComplexSubSubType<ComplexSubTypes::Encoding> { };

	    struct ComplexExtendedSubOperation_NoArgs : sor<
	    									 SymbolPopValueAddr,
	    									 SymbolPushValueAddr,
	    									 SymbolDecrementValueAddr,
	    									 SymbolIncrementValueAddr,
	    									 SymbolWordsBeforeFirstZero> { };
        DefAction(ComplexExtendedSubOperation_NoArgs) : SetComplexSubSubType<ComplexSubTypes::Extended> { };
	    struct ComplexExtendedOneArg_Operations : sor<
	    										  SymbolIsEven,
	    										  SymbolIsOdd> { };
        DefAction(ComplexExtendedOneArg_Operations) : SetComplexSubSubType<ComplexSubTypes::Extended> { };

	    struct ComplexExtendedSubOperation_OneArg : SeparatedBinaryThing<
	    											ComplexExtendedOneArg_Operations,
	    											DestinationRegister> { };
	    struct ComplexExtendedSubOperation : sor<
	    									 ComplexExtendedSubOperation_NoArgs,
	    									 ComplexExtendedSubOperation_OneArg> { };


	    struct ComplexParsingSubOperation_NoArgs : sor<
	    									 SymbolHex8ToRegister,
	    									 SymbolRegisterToHex8,
	    									 SymbolMemCopy> { };
        DefAction(ComplexParsingSubOperation_NoArgs) : SetComplexSubSubType<ComplexSubTypes::Parsing> { };
	    struct ComplexParsingSubOperation : sor<
	    									ComplexExtendedSubOperation_NoArgs> { };


        using ConvertComplexSubtype = ConvertOperationToSubType<Operation::Complex>;
	    struct ComplexEncodingOperation : SeparatedBinaryThing<
	    								  SymbolEncoding,
	    								  ComplexEncodingSubOperation> { };
        DefAction(ComplexEncodingOperation) : ConvertComplexSubtype { };
	    struct ComplexExtendedOperation : SeparatedBinaryThing<
	    								  SymbolExtended,
	    								  ComplexExtendedSubOperation> { };
        DefAction(ComplexExtendedOperation) : ConvertComplexSubtype { };
	    struct ComplexParsingOperation : SeparatedBinaryThing<
	    								 SymbolParsing,
	    								 ComplexParsingSubOperation> { };
        DefAction(ComplexParsingOperation) : ConvertComplexSubtype { };
	    struct ComplexSubOperations : sor<
	    							  ComplexEncodingOperation,
	    							  ComplexExtendedOperation,
	    							  ComplexParsingOperation> { };

	    struct ComplexOperation : SeparatedBinaryThing<
	    						  SymbolComplex,
	    						  ComplexSubOperations> { };
        DefAction(ComplexOperation) : SetOperationOnApply<Operation::Complex> { };

	    struct BranchFlagCall : SymbolCall { };
	    DefAction(BranchFlagCall) {
	    	DefApplyInstruction {
	    		state.markCall();
	    	}
	    };

	    struct BranchFlagConditional : SymbolConditional { };
	    DefAction(BranchFlagConditional) {
	    	DefApplyInstruction {
	    		state.markConditional();
	    	}
	    };

	    struct BranchFlagUnconditional : SymbolUnconditional { };
	    DefAction(BranchFlagUnconditional) {
	    	DefApplyInstruction {
	    		state.markUnconditional();
	    	}
	    };

	    struct ChooseBranchFlagUsePredicate : sor<BranchFlagConditional, BranchFlagUnconditional> { };

	    struct BranchNormalArgs : sor<
	    						  SeparatedBinaryThing<UsesImmediate, LexemeOrNumber>,
	    						  DestinationRegister> { };
        template<typename T>
        struct BranchWithNormalArgs : SeparatedBinaryThing<T, BranchNormalArgs> { };
        struct BranchCallOperation : BranchWithNormalArgs<BranchFlagCall> { };
        struct BranchJumpOperation : BranchWithNormalArgs<ChooseBranchFlagUsePredicate> { };

	    struct BranchTypes : sor<BranchCallOperation, BranchJumpOperation> { };
	    struct BranchOperation : SeparatedBinaryThing<SymbolBranch, BranchTypes> { };
        DefAction(BranchOperation) : SetOperationOnApply<Operation::Branch> { };

	    struct ReturnOperation : seq<SymbolReturn> { };
        DefAction(ReturnOperation) : SetOperationOnApply<Operation::Return> { };

	    struct Instructions : state<AssemblerInstruction,
	    sor<
	    		   BranchOperation,
	    		   ComplexOperation,
	    		   MemoryInstruction,
	    		   MoveOperation,
	    		   SetOperation,
	    		   SwapOperation,
	    		   ArithmeticOperation,
	    		   ShiftOperation,
	    		   CompareOperation,
	    		   LogicalOperation,
	    		   ReturnOperation>
	    							> { };
        struct WordDirective : syn::WordDirective<WordCreator, LexemeOrNumber> { };
	    struct DwordDirective : syn::DwordDirective<DwordCreator, LexemeOrNumber> { };

	    struct Directive : sor<
	    				   syn::StatefulOrgDirective<ChangeCurrentAddress, Number>,
	    				   syn::StatefulLabelDirective<RegisterLabel, Lexeme>,
	    				   WordDirective,
	    				   DwordDirective > { };

	    struct Statement : sor<
	    				   Instructions,
	    				   Directive> { };

	    struct Anything : sor<
	    				  Separator,
	    				  SingleLineComment,
	    				  Statement> { };

	    struct Main : syn::MainFileParser<Anything> { };
    } // end namespace assembler
} // end namespace cisc0
#endif  // end CISC0_CORE_ASSEMBLER_H__
