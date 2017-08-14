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

namespace cisc0 {
#define DefTranslators(type, str) \
    type stringTo ## type ( const std::string& title ) noexcept; \
    const std::string& str ## ToString ( type value ) noexcept
    DefTranslators(CompareStyle, compareStyle);
    DefTranslators(ArithmeticOps, arithmeticOps);
    DefTranslators(ComplexSubTypes, complexSubTypes);
    DefTranslators(MemoryOperation, memoryOperation);
    DefTranslators(EncodingOperation, encodingOperation);
    DefTranslators(ExtendedOperation, extendedOperation);
    DefTranslators(LogicalOps, logicalOps);
    DefTranslators(ParsingOperation, parsingOperation);
    DefTranslators(Operation, operation);
#undef DefTranslators

    /**
     * Given an integer index, convert it to a string representation
     * understandable by the assembler.
     * @param index the numerical index of a register, this will masked to
     * the appropriate range automatically.
     * @return the string representation of the provided register index
     */
    const std::string& registerIndexToString(Word index);

    /**
     * Given an integer index, convert it to a string representation
     * understandable by the assembler.
     * @param index the numerical index of a register, this will masked to
     * the appropriate range automatically.
     * @return the string representation of the provided register index
     */
    inline const std::string& translateRegister(Word index) { return registerIndexToString(index); }

    /**
     * Given a string representation of a register, see if it is possible to
     * translate it to an index value.
     * @param input The string input to attempt to convert
     * @return The index corresponding to the provided input
     */
	Word translateRegister(const std::string& input);

    /**
     * Given an encoded instruction, convert it back to a text representation
     * and save it to the provided stream.
     * @param out the stream to output the data to
     * @param first the first word that makes up the encoded instruction
     * @param second the second word that makes up the encoded instruction (not always applicable)
     * @param third the third word that makes up the encoded instruction (not always applicable)
     */
    void translateInstruction(std::ostream& out, Word first, Word second = 0, Word third = 0) noexcept;

    /**
     * Given an encoded instruction, convert it back to a text representation
     * and return that as a string
     * @param out the stream to output the data to
     * @param first the first word that makes up the encoded instruction
     * @param second the second word that makes up the encoded instruction (not always applicable)
     * @param third the third word that makes up the encoded instruction (not always applicable)
     * @return a string containing the text representation of the encoded * instruction.
     */
    std::string translateInstruction(Word first, Word second = 0, Word third = 0) noexcept;

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
	    struct SeparatedQuadThing : pegtl::seq<First, Sep, Second, Sep, Third, Sep, Fourth> { };

	    using AssemblerWord = syn::AssemblerWord<RegisterValue>;
        /**
         * Keeps track of the overall state throughout the lifetime of a given
         * parsing
         */
	    struct AssemblerState : public syn::LabelTracker<RegisterValue>, public syn::AddressTracker<RegisterValue>, public syn::FinishedDataTracker<InstructionEncoder> {
	    	std::vector<AssemblerWord> finalWords;
	    	std::vector<AssemblerWord> wordsToResolve;
            void output(void* env, CLIPSValue* ret) noexcept;
	    	void resolveInstructions();
	    	void resolveDeclarations();
	    };
	    template<int width>
	    	struct AssemblerWordCreator {
	    		static_assert(width > 0, "Can't have a width of zero or less!");
	    		template<typename Input>
	    			AssemblerWordCreator(const Input& in, AssemblerState& parent) { }
	    		virtual ~AssemblerWordCreator() { }
	    		template<typename Input>
	    			void success(const Input& in, AssemblerState& parent) {
                        auto address = parent.getCurrentAddress();
	    				if (_isLabel) {
	    					parent.wordsToResolve.emplace_back(address, _label, width);
	    				} else {
	    					parent.wordsToResolve.emplace_back(address, _value, width);
	    				}
	    				parent.incrementCurrentAddress(width);
	    			}
	    		void setLabel(const std::string& name) noexcept {
	    			_label = name;
	    			_isLabel = true;
	    		}
	    		void setValue(RegisterValue value) noexcept {
	    			_isLabel = false;
	    			_value = value;
	    		}
	    		bool _isLabel;
	    		std::string _label;
	    		RegisterValue _value;
	    	};
	    using WordCreator = AssemblerWordCreator<1>;
	    using DwordCreator = AssemblerWordCreator<2>;

	    struct ChangeCurrentAddress : public syn::NumberContainer<RegisterValue> {
	    	using syn::NumberContainer<RegisterValue>::NumberContainer;

	    	template<typename Input>
	    		void success(const Input& in, AssemblerState& parent) {
	    			parent.setCurrentAddress(getValue());
	    		}
	    };
	    struct RegisterLabel : public syn::NameToAddressMapping<Address> {
	    	using Parent = syn::NameToAddressMapping<Address>;
	    	template<typename Input>
	    		RegisterLabel(const Input& in, AssemblerState& parent) : Parent(in, parent) {
	    			setValue(parent.getCurrentAddress());
	    		}

	    	template<typename Input>
	    		void success(const Input& in, AssemblerState& parent) {
	    			parent.registerLabel(getTitle(), getValue());
	    		}
	    };
	    struct AssemblerInstruction : public InstructionEncoder {
	    	template<typename Input>
	    		AssemblerInstruction(const Input& in, AssemblerState& parent) {
	    			clear();
	    			setAddress(parent.getCurrentAddress());
	    		}

	    	template<typename Input>
	    		void success(const Input& in, AssemblerState& parent) {
	    			parent.incrementCurrentAddress(numWords());
	    			// for now, make a copy because I do not care!
	    		    parent.copyToFinishedData(*this);
	    		}
	    };

	    struct NumberContainer : public syn::NumberContainer<RegisterValue> {
	    	using syn::NumberContainer<RegisterValue>::NumberContainer;

	    	template<typename Input>
	    		void success(const Input& in, AssemblerInstruction& parent) {
	    			parent.setFullImmediate(getValue());
	    		}

	    	template<typename Input>
	    		void success(const Input& in, RegisterLabel& parent) {
	    			parent.setValue(getValue());
	    		}

	    	template<typename Input>
	    		void success(const Input& in, ChangeCurrentAddress& parent) {
                    parent.setValue(getValue());
	    		}

	    	template<typename Input>
	    		void success(const Input& in, AssemblerWord& parent) {
	    			parent.setValue(getValue());
	    		}
	    	template<typename Input, int width>
	    		void success(const Input& in, AssemblerWordCreator<width>& parent) {
	    			parent.setValue(getValue());
	    		}
	    	template<typename Input>
	    		void success(const Input& in, WordCreator& parent) {
	    			success<Input, 1>(in, parent);
	    		}

	    	template<typename Input>
	    		void success(const Input& in, DwordCreator& parent) {
	    			success<Input, 2>(in, parent);
	    		}
	    };


#define DefApplyInstruction DefApplyGeneric(cisc0::assembler::AssemblerInstruction)
#define DefApplyAsmState DefApplyGeneric(cisc0::assembler::AssemblerState)


        template<Operation op>
        struct SetOperationOnApply {
            DefApplyInstruction {
                state.setType<op>();
            }
        };

        /**
         * Describes the instruction as an immediate type!
         */
	    struct UsesImmediate : pegtl::seq<SymbolImmediate> { };

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

	    struct BitmaskNumber : syn::GenericNumeral<'m', pegtl::abnf::BIT> { };

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
	    struct GeneralPurposeRegister : pegtl::sor<
	    								NormalRegister,
	    								SymbolAddrRegister,
	    								SymbolStackPointer,
	    								SymbolInstructionPointer,
	    								SymbolCallStackPointer,
	    								SymbolValueRegister,
	    								SymbolMaskRegister,
	    								SymbolFieldRegister> { };

	    struct IndirectGPR : pegtl::seq<GeneralPurposeRegister> { };
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


	    struct ShiftLeftOrRight : pegtl::sor<
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
	    struct SpecialImmediate : pegtl::seq<Number> { };
	    struct ShiftImmediateValue : SpecialImmediate { };
	    DefAction(ShiftImmediateValue) {
	    	DefApplyInstruction {
                state.setSecondArg(static_cast<byte>(state.getFullImmediate()) & 0b11111);
	    	}
	    };
	    struct ShiftArgs : pegtl::sor<
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
	    struct CompareType : pegtl::sor<
	    					 SymbolEquals,
	    					 SymbolNotEquals,
	    					 SymbolLessThan,
	    					 SymbolLessThanOrEqualTo,
	    					 SymbolGreaterThan,
	    					 SymbolGreaterThanOrEqualTo> { };
	    struct SpecialCompareType : pegtl::sor<
	    							SymbolMoveFromCondition,
                                    SymbolMoveToCondition> { };
        DefAction(CompareType) : GetCompareSubType { };
        DefAction(SpecialCompareType) : GetCompareSubType { };
	    struct CompareArgs : pegtl::sor<
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
	    						  pegtl::sor<
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
	    struct ArithmeticType : pegtl::sor<
                                SymbolAdd,
                                SymbolSub,
                                SymbolMul,
                                SymbolDiv,
                                SymbolRem,
                                SymbolMin,
                                SymbolMax> { };
        DefAction(ArithmeticType) : ConvertOperationToSubType<Operation::Arithmetic> { };

	    struct ArithmeticArgs : pegtl::sor<
	    						TwoGPRs,
	    						ImmediateOperationArgs<ByteCastImmediate>> { };
	    struct ArithmeticOperation : SeparatedTrinaryThing<SymbolArithmetic, ArithmeticType, ArithmeticArgs> { };

        DefAction(ArithmeticOperation) : SetOperationOnApply<Operation::Arithmetic> { };


        using GetMemorySubType = ConvertOperationToSubType<Operation::Memory>;
	    struct LoadStoreType : pegtl::sor<
	    					   SymbolLoad,
	    					   SymbolStore> { };

        DefAction(LoadStoreType) : GetMemorySubType { };
	    struct StackMemoryType : pegtl::sor<
	    						 SymbolPush,
	    						 SymbolPop> { };
        DefAction(StackMemoryType) : GetMemorySubType { };
	    struct StackOperation : SeparatedTrinaryThing<StackMemoryType, BitmaskNumber, DestinationRegister> { };
	    struct FlagIndirect : syn::SingleEntrySequence<SymbolIndirect> { };
	    struct FlagDirect : syn::SingleEntrySequence<SymbolDirect> { };
	    struct FlagDirectOrIndirect : pegtl::sor<
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

	    struct MemoryTypes : pegtl::sor<
	    					 StackOperation,
	    					 LoadStoreOperation> { };
	    struct MemoryInstruction : SeparatedBinaryThing<
	    						   SymbolMemory,
	    						   MemoryTypes> { };
        DefAction(MemoryInstruction) : SetOperationOnApply<Operation::Memory> { };



	    struct LogicalOpsType : pegtl::sor<
	    						SymbolAnd,
	    						SymbolOr,
	    						SymbolNot,
	    						SymbolXor,
	    						SymbolNand> { };
        DefAction(LogicalOpsType) : ConvertOperationToSubType<Operation::Logical> { };
	    struct LogicalArgs : pegtl::sor<
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

	    struct ComplexEncodingSubOperation : pegtl::sor<
	    									 SymbolDecode,
	    									 SymbolEncode,
	    									 SymbolBitSet,
	    									 SymbolBitUnset> { };
        DefAction(ComplexEncodingSubOperation) : SetComplexSubSubType<ComplexSubTypes::Encoding> { };

	    struct ComplexExtendedSubOperation_NoArgs : pegtl::sor<
	    									 SymbolPopValueAddr,
	    									 SymbolPushValueAddr,
	    									 SymbolDecrementValueAddr,
	    									 SymbolIncrementValueAddr,
	    									 SymbolWordsBeforeFirstZero> { };
        DefAction(ComplexExtendedSubOperation_NoArgs) : SetComplexSubSubType<ComplexSubTypes::Extended> { };
	    struct ComplexExtendedOneArg_Operations : pegtl::sor<
	    										  SymbolIsEven,
	    										  SymbolIsOdd> { };
        DefAction(ComplexExtendedOneArg_Operations) : SetComplexSubSubType<ComplexSubTypes::Extended> { };

	    struct ComplexExtendedSubOperation_OneArg : SeparatedBinaryThing<
	    											ComplexExtendedOneArg_Operations,
	    											DestinationRegister> { };
	    struct ComplexExtendedSubOperation : pegtl::sor<
	    									 ComplexExtendedSubOperation_NoArgs,
	    									 ComplexExtendedSubOperation_OneArg> { };


	    struct ComplexParsingSubOperation_NoArgs : pegtl::sor<
	    									 SymbolHex8ToRegister,
	    									 SymbolRegisterToHex8,
	    									 SymbolMemCopy> { };
        DefAction(ComplexParsingSubOperation_NoArgs) : SetComplexSubSubType<ComplexSubTypes::Parsing> { };
	    struct ComplexParsingSubOperation : pegtl::sor<
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
	    struct ComplexSubOperations : pegtl::sor<
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

	    struct ChooseBranchFlagUsePredicate : pegtl::sor<BranchFlagConditional, BranchFlagUnconditional> { };

	    struct BranchNormalArgs : pegtl::sor<
	    						  SeparatedBinaryThing<UsesImmediate, LexemeOrNumber>,
	    						  DestinationRegister> { };
        template<typename T>
        struct BranchWithNormalArgs : SeparatedBinaryThing<T, BranchNormalArgs> { };
        struct BranchCallOperation : BranchWithNormalArgs<BranchFlagCall> { };
        struct BranchJumpOperation : BranchWithNormalArgs<ChooseBranchFlagUsePredicate> { };

	    struct BranchTypes : pegtl::sor<BranchCallOperation, BranchJumpOperation> { };
	    struct BranchOperation : SeparatedBinaryThing<SymbolBranch, BranchTypes> { };
        DefAction(BranchOperation) : SetOperationOnApply<Operation::Branch> { };

	    struct ReturnOperation : pegtl::seq<SymbolReturn> { };
        DefAction(ReturnOperation) : SetOperationOnApply<Operation::Return> { };

	    struct Instructions : pegtl::state<AssemblerInstruction,
	    pegtl::sor<
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

	    struct Directive : pegtl::sor<
	    				   syn::StatefulOrgDirective<ChangeCurrentAddress, Number>,
	    				   syn::StatefulLabelDirective<RegisterLabel, Lexeme>,
	    				   WordDirective,
	    				   DwordDirective > { };

	    struct Statement : pegtl::sor<
	    				   Instructions,
	    				   Directive> { };

	    struct Anything : pegtl::sor<
	    				  Separator,
	    				  SingleLineComment,
	    				  Statement> { };

	    struct Main : syn::MainFileParser<Anything> { };
    } // end namespace assembler
} // end namespace cisc0
#endif  // end CISC0_CORE_ASSEMBLER_H__
