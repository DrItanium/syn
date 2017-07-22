/*
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
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>
#include <pegtl/parse.hh>
#include <vector>

#include "Base.h"
#include "AssemblerBase.h"
#include "Problem.h"
#include "ClipsExtensions.h"
#include "Cisc0CoreInstructionEncoder.h"
#include "Cisc0ClipsExtensions.h"

namespace cisc0 {
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

#define DefApplyInstruction DefApplyGeneric(cisc0::AssemblerInstruction)
#define DefApplyAsmState DefApplyGeneric(cisc0::AssemblerState)

#define DefGroup(title, str) \
	DefSymbol(title, str); \
	struct Group ## title : syn::SingleEntrySequence<Symbol ## title> { }; \
	DefAction(Group ## title) { \
		DefApplyInstruction { \
			state.setType < Operation:: title > (); \
		} \
	}
	// done
	DefGroup(Shift, shift);
	DefGroup(Compare, compare);
	DefGroup(Move, move);
	DefGroup(Set, set);
	DefGroup(Swap, swap);
	DefGroup(Arithmetic, arithmetic);
	DefGroup(Memory, memory);
	DefGroup(Logical, logical);
	DefGroup(Complex, complex);
	DefGroup(Branch, branch);
	DefGroup(Return, return);

	DefSymbol(Immediate, immediate);

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
	DefSymbol(AddrRegister, addr);
	DefSymbol(StackPointer, sp);
	DefSymbol(InstructionPointer, ip);
	DefSymbol(ValueRegister, value);
	DefSymbol(MaskRegister, mask);
	DefSymbol(FieldRegister, field);
	DefSymbol(CallStackPointer, csp);
	struct GeneralPurposeRegister : pegtl::sor<
									NormalRegister,
									SymbolAddrRegister,
									SymbolStackPointer,
									SymbolInstructionPointer,
									SymbolCallStackPointer,
									SymbolValueRegister,
									SymbolMaskRegister,
									SymbolFieldRegister> { };
	Word translateRegister(const std::string& input);

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

	DefSymbol(Left, left);
	DefSymbol(Right, right);

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
							GroupShift,
							ShiftLeftOrRight,
							ShiftArgs> { };

	struct ByteCastImmediate : SpecialImmediate { };
	DefAction(ByteCastImmediate) {
		DefApplyInstruction {
            state.setSecondArg(static_cast<byte>(state.getFullImmediate()));
		}
	};
#define DefSubType(title, str, subgroup) \
	struct SubGroup ## subgroup ## title : syn::SingleEntrySequence<Symbol ## title> { }; \
	DefAction(SubGroup ## subgroup ## title) { \
		DefApplyInstruction { \
			state.setSubType(cisc0 :: subgroup :: title ); \
		} \
	}

#define DefSubTypeWithSymbol(title, str, subgroup) \
	DefSymbol(title, str); \
	DefSubType(title, str, subgroup)

#define DefCompareStyle(title, str) DefSubType(title, str, CompareStyle)

#define DefCompareStyleWithSymbol(title, str) DefSubTypeWithSymbol(title, str, CompareStyle)

    CompareStyle stringToCompareStyle(const std::string& str) noexcept;

	DefSymbol(Equals, ==);
	DefSymbol(NotEquals, !=);
	DefSymbol(LessThan, <);
	DefSymbol(LessThanOrEqualTo, <=);
	DefSymbol(GreaterThan, >);
	DefSymbol(GreaterThanOrEqualTo, >=);
	struct CompareType : pegtl::sor<
						 SymbolEquals,
						 SymbolNotEquals,
						 SymbolLessThan,
						 SymbolLessThanOrEqualTo,
						 SymbolGreaterThan,
						 SymbolGreaterThanOrEqualTo> { };
    DefAction(CompareType) {
        DefApplyInstruction {
            state.setSubType(stringToCompareStyle(in.string()));
        }
    };
    DefSymbol(MoveToCondition, MoveToCondition);
    DefSymbol(MoveFromCondition, MoveFromCondition);
	struct SpecialCompareType : pegtl::sor<
								SymbolMoveFromCondition,
                                SymbolMoveToCondition> { };
    DefAction(SpecialCompareType) {
        DefApplyInstruction {
            state.setSubType(stringToCompareStyle(in.string()));
        }
    };
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
							  GroupCompare,
							  pegtl::sor<
										 NormalCompareOperation,
										 SpecialCompareOperation>> { };
	struct MoveOperation : SeparatedTrinaryThing<
						   GroupMove,
						   BitmaskNumber,
						   TwoGPRs> { };
	struct SetOperation : SeparatedQuadThing<
						  GroupSet,
						  BitmaskNumber,
						  DestinationRegister,
						  LexemeOrNumber> { };
	struct SwapOperation : SeparatedBinaryThing<
						   GroupSwap,
						   TwoGPRs> { };

	struct Arg0ImmediateValue : SpecialImmediate { };
	DefAction(Arg0ImmediateValue) {
		DefApplyInstruction {
            state.setFirstArg(static_cast<byte>(state.getFullImmediate()) & 0b1111);
		}
	};
#define DefArithmeticOperation(title, str) \
    using Symbol ## title = syn:: Symbol ## title ## Keyword; \
    DefSubType(title, str, ArithmeticOps)
    ArithmeticOps stringToArithmeticOps(const std::string& str) noexcept;
    using SymbolAdd = syn::SymbolAddKeyword;
    using SymbolSub = syn::SymbolSubKeyword;
    using SymbolMul = syn::SymbolMulKeyword;
    using SymbolDiv = syn::SymbolDivKeyword;
    using SymbolRem = syn::SymbolRemKeyword;
    DefSymbol(Min, min);
    DefSymbol(Max, max);
	struct ArithmeticType : pegtl::sor<
                            SymbolAdd,
                            SymbolSub,
                            SymbolMul,
                            SymbolDiv,
                            SymbolRem,
                            SymbolMin,
                            SymbolMax> { };
    DefAction(ArithmeticType) {
        DefApplyInstruction {
            state.setSubType(stringToArithmeticOps(in.string()));
        }
    };

	struct ArithmeticArgs : pegtl::sor<
							TwoGPRs,
							ImmediateOperationArgs<ByteCastImmediate>> { };
	struct ArithmeticOperation : SeparatedTrinaryThing<GroupArithmetic, ArithmeticType, ArithmeticArgs> { };

	DefSymbol(Load, load);
	DefSymbol(Store, store);
	DefSymbol(Push, push);
	DefSymbol(Pop, pop);

    MemoryOperation stringToMemoryOperation(const std::string& str) noexcept;
	struct LoadStoreType : pegtl::sor<
						   SymbolLoad,
						   SymbolStore> { };
    DefAction(LoadStoreType) {
        DefApplyInstruction {
            state.setSubType(stringToMemoryOperation(in.string()));
        }
    };
	struct StackMemoryType : pegtl::sor<
							 SymbolPush,
							 SymbolPop> { };
    DefAction(StackMemoryType) {
        DefApplyInstruction {
            state.setSubType(stringToMemoryOperation(in.string()));
        }
    };
	struct StackOperation : SeparatedTrinaryThing<StackMemoryType, BitmaskNumber, DestinationRegister> { };
	DefSymbol(Indirect, indirect);
	struct FlagIndirect : syn::SingleEntrySequence<SymbolIndirect> { };
	DefAction(FlagIndirect) {
		DefApplyInstruction {
			state.markIndirect();
		}
	};
	DefSymbol(Direct, direct);
	struct FlagDirect : syn::SingleEntrySequence<SymbolDirect> { };
	DefAction(FlagDirect) {
		DefApplyInstruction {
			state.markIndirect(false);
		}
	};
	struct FlagDirectOrIndirect : pegtl::sor<
								  FlagDirect,
								  FlagIndirect> { };
	struct LoadStoreOperation : SeparatedQuadThing<
								LoadStoreType,
								BitmaskNumber,
								FlagDirectOrIndirect,
								Arg0ImmediateValue> { };

	struct MemoryTypes : pegtl::sor<
						 StackOperation,
						 LoadStoreOperation> { };
	struct MemoryInstruction : SeparatedBinaryThing<
							   GroupMemory,
							   MemoryTypes> { };

    LogicalOps stringToLogicalOps(const std::string& str) noexcept;
    using SymbolAnd = syn::SymbolAndKeyword;
    using SymbolOr = syn::SymbolOrKeyword;
    using SymbolNot = syn::SymbolNotKeyword;
	DefSymbol(Xor, xor);
	DefSymbol(Nand, nand);


	struct LogicalOpsType : pegtl::sor<
							SymbolAnd,
							SymbolOr,
							SymbolNot,
							SymbolXor,
							SymbolNand> { };
    DefAction(LogicalOpsType) {
        DefApplyInstruction {
            state.setSubType(stringToLogicalOps(in.string()));
        }
    };
	struct LogicalArgs : pegtl::sor<
						 TwoGPRs,
						 ImmediateOperationArgsWithBitmask<LexemeOrNumber>> { };
	struct LogicalOperation : SeparatedTrinaryThing<GroupLogical, LogicalOpsType, LogicalArgs> { };

    EncodingOperation stringToEncodingOperation(const std::string& str) noexcept;
	DefSymbol(BitSet, bitset);
	DefSymbol(BitUnset, bitunset);
	DefSymbol(Encode, encode);
	DefSymbol(Decode, decode);
	struct ComplexEncodingSubOperation : pegtl::sor<
										 SymbolDecode,
										 SymbolEncode,
										 SymbolBitSet,
										 SymbolBitUnset> { };
    DefAction(ComplexEncodingSubOperation) {
        DefApplyInstruction {
            state.setBitmask(stringToEncodingOperation(in.string()));
        }
    };

    ExtendedOperation stringToExtendedOperation(const std::string& str) noexcept;
	DefSymbol(PushValueAddr, PushValueAddr);
	DefSymbol(PopValueAddr,  PopValueAddr);
	DefSymbol(IncrementValueAddr, IncrementValueAddr);
	DefSymbol(DecrementValueAddr, DecrementValueAddr);
	DefSymbol(WordsBeforeFirstZero, CountWordsBeforeFirstZero);
	struct ComplexExtendedSubOperation_NoArgs : pegtl::sor<
										 SymbolPopValueAddr,
										 SymbolPushValueAddr,
										 SymbolDecrementValueAddr,
										 SymbolIncrementValueAddr,
										 SymbolWordsBeforeFirstZero> { };
    DefAction(ComplexExtendedSubOperation_NoArgs) {
        DefApplyInstruction {
            state.setBitmask(stringToExtendedOperation(in.string()));
        }
    };
	DefSymbol(IsEven, evenp);
	DefSymbol(IsOdd, oddp);
	struct ComplexExtendedOneArg_Operations : pegtl::sor<
											  SymbolIsEven,
											  SymbolIsOdd> { };
    DefAction(ComplexExtendedOneArg_Operations) {
        DefApplyInstruction {
            state.setBitmask(stringToExtendedOperation(in.string()));
        }
    };

	struct ComplexExtendedSubOperation_OneArg : SeparatedBinaryThing<
												ComplexExtendedOneArg_Operations,
												DestinationRegister> { };
	struct ComplexExtendedSubOperation : pegtl::sor<
										 ComplexExtendedSubOperation_NoArgs,
										 ComplexExtendedSubOperation_OneArg> { };


	DefSymbol(Hex8ToRegister, Hex8ToRegister);
	DefSymbol(RegisterToHex8, RegisterToHex8);
	DefSymbol(MemCopy, MemCopy);
    ParsingOperation stringToParsingOperation(const std::string& str) noexcept;
	struct ComplexParsingSubOperation_NoArgs : pegtl::sor<
										 SymbolHex8ToRegister,
										 SymbolRegisterToHex8,
										 SymbolMemCopy> { };
    DefAction(ComplexParsingSubOperation_NoArgs) {
        DefApplyInstruction {
            state.setBitmask(stringToParsingOperation(in.string()));
        }
    };
	struct ComplexParsingSubOperation : pegtl::sor<
										ComplexExtendedSubOperation_NoArgs> { };


    ComplexSubTypes stringToComplexSubTypes(const std::string& str) noexcept;
#define DefComplexOperation(title, str) \
	DefSubTypeWithSymbol(title, str, ComplexSubTypes)
	DefComplexOperation(Encoding, encoding);
	DefComplexOperation(Extended, extended);
	DefComplexOperation(Parsing, parsing);

	struct ComplexEncodingOperation : SeparatedBinaryThing<
									  SubGroupComplexSubTypesEncoding,
									  ComplexEncodingSubOperation> { };
	struct ComplexExtendedOperation : SeparatedBinaryThing<
									  SubGroupComplexSubTypesExtended,
									  ComplexExtendedSubOperation> { };
	struct ComplexParsingOperation : SeparatedBinaryThing<
									 SubGroupComplexSubTypesParsing,
									 ComplexParsingSubOperation> { };
	struct ComplexSubOperations : pegtl::sor<
								  ComplexEncodingOperation,
								  ComplexExtendedOperation,
								  ComplexParsingOperation> { };

	struct ComplexOperation : SeparatedBinaryThing<
							  GroupComplex,
							  ComplexSubOperations> { };

	DefSymbol(Call, call);
	DefSymbol(NoCall, nocall);
	DefSymbol(Conditional, conditional);
	DefSymbol(Unconditional, unconditional);

	template<typename T, typename F>
		struct ChoiceFlag : pegtl::sor<T, F> { };

	struct BranchFlagCall : pegtl::seq<
							SymbolCall> { };
	DefAction(BranchFlagCall) {
		DefApplyInstruction {
			state.markCall();
		}
	};

	struct BranchFlagNoCall : pegtl::seq<
							  SymbolNoCall> { };
	DefAction(BranchFlagNoCall) {
		DefApplyInstruction {
			state.markCall(false);
		}
	};

	struct ChooseBranchFlagCall : ChoiceFlag<
								  BranchFlagCall,
								  BranchFlagNoCall> { };

	struct BranchFlagConditional : pegtl::seq<
								   SymbolConditional> { };
	DefAction(BranchFlagConditional) {
		DefApplyInstruction {
			state.markConditional();
		}
	};

	struct BranchFlagUnconditional : pegtl::seq<
									 SymbolUnconditional> { };
	DefAction(BranchFlagUnconditional) {
		DefApplyInstruction {
			state.markUnconditional();
		}
	};

	struct ChooseBranchFlagUsePredicate : ChoiceFlag<BranchFlagConditional, BranchFlagUnconditional> { };

	struct BranchNormalArgs : pegtl::sor<
							  SeparatedBinaryThing<UsesImmediate, LexemeOrNumber>,
							  DestinationRegister> { };
    template<typename T>
    struct BranchWithNormalArgs : SeparatedBinaryThing<T, BranchNormalArgs> { };
    struct BranchCallOperation : BranchWithNormalArgs<BranchFlagCall> { };
    struct BranchJumpOperation : BranchWithNormalArgs<ChooseBranchFlagUsePredicate> { };

	struct BranchTypes : pegtl::sor<BranchCallOperation, BranchJumpOperation> { };
	struct BranchOperation : SeparatedBinaryThing<GroupBranch, BranchTypes> { };

	struct ReturnOperation : pegtl::seq<GroupReturn> { };

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
} // end namespace cisc0
#endif  // end CISC0_CORE_ASSEMBLER_H__
