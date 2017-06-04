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
	struct AssemblerState {
		cisc0::Address currentAddress = 0;
		std::vector<InstructionEncoder> finishedInstructions;
		std::map<std::string, RegisterValue> labels;
		std::vector<AssemblerWord> finalWords;
		std::vector<AssemblerWord> wordsToResolve;
		void setCurrentAddress(Address addr) noexcept;
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
					if (_isLabel) {
						parent.wordsToResolve.emplace_back(parent.currentAddress, _label, width);
					} else {
						parent.wordsToResolve.emplace_back(parent.currentAddress, _value, width);
					}
					parent.currentAddress += width;
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
				parent.currentAddress = getValue();
			}
	};
	struct RegisterLabel : public syn::NameToAddressMapping<Address> {
		using Parent = syn::NameToAddressMapping<Address>;
		template<typename Input>
			RegisterLabel(const Input& in, AssemblerState& parent) : Parent(in, parent) {
				setValue(parent.currentAddress);
			}

		template<typename Input>
			void success(const Input& in, AssemblerState& parent) {
				parent.labels.emplace(getTitle(), getValue());
			}
	};
	struct AssemblerInstruction : public InstructionEncoder {
		template<typename Input>
			AssemblerInstruction(const Input& in, AssemblerState& parent) {
				clear();
				setAddress(parent.currentAddress);
			}

		template<typename Input>
			void success(const Input& in, AssemblerState& parent) {
				parent.currentAddress += numWords();
				// for now, make a copy because I do not care!
				parent.finishedInstructions.push_back(*this);
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

#define DefAction(rule) template<> struct Action < rule >
#define DefApplyGeneric(type) template<typename Input> static void apply(const Input& in, type& state)
#define DefApplyInstruction DefApplyGeneric(cisc0::AssemblerInstruction)
#define DefApplyAsmState DefApplyGeneric(cisc0::AssemblerState)

#define DefGroup(title, str) \
	DefSymbol(title, str); \
	struct Group ## title : syn::Indirection<Symbol ## title> { }; \
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

	//DefSymbol(Nop, nop);
	//DefSymbol(Return, return);
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
	struct Number : public pegtl::state<NumberContainer, pegtl::sor<syn::HexadecimalNumber, DecimalNumber, syn::BinaryNumber > > { };
	DefAction(Number) {
		DefApplyInstruction {
			state.markAsNotLabel();
		}
		DefApplyGeneric(ChangeCurrentAddress) { }
		DefApplyGeneric(WordCreator) { }
		DefApplyGeneric(DwordCreator) { }
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
	struct LexemeOrNumber : public syn::LexemeOr<Number> { };

    using NormalRegister = syn::GPR;
	DefSymbol(AddrRegister, addr);
	DefSymbol(StackPointer, sp);
	DefSymbol(InstructionPointer, ip);
	DefSymbol(ConditionRegister, cr);
	DefSymbol(ValueRegister, value);
	DefSymbol(MaskRegister, mask);
	DefSymbol(FieldRegister, field);
	struct GeneralPurposeRegister : pegtl::sor<
									NormalRegister,
									SymbolAddrRegister,
									SymbolStackPointer,
									SymbolInstructionPointer,
									SymbolConditionRegister,
									SymbolValueRegister,
									SymbolMaskRegister,
									SymbolFieldRegister> { };
	Word translateRegister(const std::string& input);

	using IndirectGPR = syn::Indirection<GeneralPurposeRegister>;
#define DefIndirectGPR(title) \
	struct title : public IndirectGPR { }

	DefIndirectGPR(DestinationRegister);
	DefAction(DestinationRegister) {
		DefApplyInstruction {
			state.setArg<0>(translateRegister(in.string()));
		}
	};

	DefIndirectGPR(SourceRegister);
	DefAction(SourceRegister) {
		DefApplyInstruction {
			state.setArg<1>(translateRegister(in.string()));
		}
	};

	DefIndirectGPR(SourceRegister1);
	DefAction(SourceRegister1) {
		DefApplyInstruction {
			state.setArg<2>(translateRegister(in.string()));
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
			state.setArg<1>(static_cast<byte>(state.getFullImmediate()) & 0b11111);
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
			state.setArg<1>(static_cast<byte>(state.getFullImmediate()));
		}
	};
#define DefSubType(title, str, subgroup) \
	struct SubGroup ## subgroup ## title : syn::Indirection<Symbol ## title> { }; \
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

	DefCompareStyleWithSymbol(Equals, ==);
	DefCompareStyleWithSymbol(NotEquals, !=);
	DefCompareStyleWithSymbol(LessThan, <);
	DefCompareStyleWithSymbol(LessThanOrEqualTo, <=);
	DefCompareStyleWithSymbol(GreaterThan, >);
	DefCompareStyleWithSymbol(GreaterThanOrEqualTo, >=);
	struct CompareType : pegtl::sor<
						 SubGroupCompareStyleEquals,
						 SubGroupCompareStyleNotEquals,
						 SubGroupCompareStyleLessThan,
						 SubGroupCompareStyleLessThanOrEqualTo,
						 SubGroupCompareStyleGreaterThan,
						 SubGroupCompareStyleGreaterThanOrEqualTo> { };
	DefCompareStyleWithSymbol(MoveFromCondition, MoveFromCondition);
	DefCompareStyleWithSymbol(MoveToCondition, MoveToCondition);
	struct SpecialCompareType : pegtl::sor<
								SubGroupCompareStyleMoveFromCondition, 
								SubGroupCompareStyleMoveToCondition> { };
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
			state.setArg<0>(static_cast<byte>(state.getFullImmediate()) & 0b1111);
		}
	};
#define DefArithmeticOperation(title, str) \
    using Symbol ## title = syn:: Symbol ## title ## Keyword; \
    DefSubType(title, str, ArithmeticOps)

	DefArithmeticOperation(Add, add);
	DefArithmeticOperation(Sub, sub);
	DefArithmeticOperation(Mul, mul);
	DefArithmeticOperation(Div, div);
	DefArithmeticOperation(Rem, rem);
    DefSubTypeWithSymbol(Min, min, ArithmeticOps);
    DefSubTypeWithSymbol(Max, max, ArithmeticOps);
	struct ArithmeticType : pegtl::sor<
							SubGroupArithmeticOpsAdd,
							SubGroupArithmeticOpsSub,
							SubGroupArithmeticOpsMul,
							SubGroupArithmeticOpsDiv,
							SubGroupArithmeticOpsRem,
                            SubGroupArithmeticOpsMin,
                            SubGroupArithmeticOpsMax> { };

	struct ArithmeticArgs : pegtl::sor<
							TwoGPRs,
							ImmediateOperationArgs<ByteCastImmediate>> { };
	struct ArithmeticOperation : SeparatedTrinaryThing<GroupArithmetic, ArithmeticType, ArithmeticArgs> { };

#define DefMemoryOperation(title, str) \
	DefSubTypeWithSymbol(title, str, MemoryOperation)
	DefMemoryOperation(Load, load);
	DefMemoryOperation(Store, store);
	DefMemoryOperation(Push, push);
	DefMemoryOperation(Pop, pop);

	struct LoadStoreType : pegtl::sor<
						   SubGroupMemoryOperationLoad,
						   SubGroupMemoryOperationStore> { };
	struct StackMemoryType : pegtl::sor<
							 SubGroupMemoryOperationPush,
							 SubGroupMemoryOperationPop> { };
	struct StackOperation : SeparatedTrinaryThing<StackMemoryType, BitmaskNumber, DestinationRegister> { };
	DefSymbol(Indirect, indirect);
	struct FlagIndirect : public syn::Indirection<SymbolIndirect> { };
	DefAction(FlagIndirect) {
		DefApplyInstruction {
			state.markIndirect();
		}
	};
	DefSymbol(Direct, direct);
	struct FlagDirect : public syn::Indirection<SymbolDirect> { };
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

#define DefLogicalOperation(title, str) \
	DefSubTypeWithSymbol(title, str, LogicalOps)
	DefLogicalOperation(And, and);
	DefLogicalOperation(Or, or);
	DefLogicalOperation(Not, not);
	DefLogicalOperation(Xor, xor);
	DefLogicalOperation(Nand, nand);


	struct LogicalOpsType : pegtl::sor<
							SubGroupLogicalOpsAnd,
							SubGroupLogicalOpsOr,
							SubGroupLogicalOpsNot,
							SubGroupLogicalOpsXor,
							SubGroupLogicalOpsNand> { };
	struct LogicalArgs : pegtl::sor<
						 TwoGPRs,
						 ImmediateOperationArgsWithBitmask<LexemeOrNumber>> { };
	struct LogicalOperation : SeparatedTrinaryThing<GroupLogical, LogicalOpsType, LogicalArgs> { };



#define DefEncodingSubType(title, str) \
	DefSymbol(title, str); \
	struct SubGroupEncodingOperation ## title : syn::Indirection<Symbol ## title> { }; \
	DefAction(SubGroupEncodingOperation ## title) { \
		DefApplyInstruction { \
			state.setBitmask( cisc0:: EncodingOperation :: title ); \
		} \
	}
	DefEncodingSubType(BitSet, bitset);
	DefEncodingSubType(BitUnset, bitunset);
	DefEncodingSubType(Encode, encode);
	DefEncodingSubType(Decode, decode);
	struct ComplexEncodingSubOperation : pegtl::sor<
										 SubGroupEncodingOperationDecode,
										 SubGroupEncodingOperationEncode,
										 SubGroupEncodingOperationBitSet,
										 SubGroupEncodingOperationBitUnset> { };
#define DefExtendedSubType(title, str) \
	DefSymbol(title, str); \
	struct SubGroupExtendedOperation ## title : syn::Indirection<Symbol ## title> { }; \
	DefAction(SubGroupExtendedOperation ## title) { \
		DefApplyInstruction { \
			state.setBitmask( cisc0:: ExtendedOperation :: title ); \
		} \
	}
	DefExtendedSubType(PushValueAddr, PushValueAddr);
	DefExtendedSubType(PopValueAddr,  PopValueAddr);
	DefExtendedSubType(IncrementValueAddr, IncrementValueAddr);
	DefExtendedSubType(DecrementValueAddr, DecrementValueAddr);
	DefExtendedSubType(WordsBeforeFirstZero, CountWordsBeforeFirstZero);
	struct ComplexExtendedSubOperation_NoArgs : pegtl::sor<
										 SubGroupExtendedOperationPopValueAddr,
										 SubGroupExtendedOperationPushValueAddr,
										 SubGroupExtendedOperationDecrementValueAddr,
										 SubGroupExtendedOperationIncrementValueAddr,
										 SubGroupExtendedOperationWordsBeforeFirstZero> { };
	DefExtendedSubType(IsEven, evenp);
	DefExtendedSubType(IsOdd, oddp);
	struct ComplexExtendedOneArg_Operations : pegtl::sor<
											  SubGroupExtendedOperationIsEven, 
											  SubGroupExtendedOperationIsOdd> { };

	struct ComplexExtendedSubOperation_OneArg : SeparatedBinaryThing<
												ComplexExtendedOneArg_Operations, 
												DestinationRegister> { };
	struct ComplexExtendedSubOperation : pegtl::sor<
										 ComplexExtendedSubOperation_NoArgs, 
										 ComplexExtendedSubOperation_OneArg> { };


#define DefParsingSubType(title, str) \
	DefSymbol(title, str); \
	struct SubGroupParsingOperation ## title : syn::Indirection<Symbol ## title> { }; \
	DefAction(SubGroupParsingOperation ## title) { \
		DefApplyInstruction { \
			state.setBitmask( cisc0:: ParsingOperation :: title ); \
		} \
	}
	DefParsingSubType(Hex8ToRegister, Hex8ToRegister);
	DefParsingSubType(RegisterToHex8, RegisterToHex8);
	DefParsingSubType(MemCopy, MemCopy);
	struct ComplexParsingSubOperation_NoArgs : pegtl::sor<
										 SubGroupParsingOperationHex8ToRegister,
										 SubGroupParsingOperationRegisterToHex8,
										 SubGroupParsingOperationMemCopy> { };
	struct ComplexParsingSubOperation : pegtl::sor<
										ComplexExtendedSubOperation_NoArgs> { };


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
	struct BranchCallOperation : SeparatedBinaryThing<BranchFlagCall, BranchNormalArgs> { };
	struct BranchJumpOperation : SeparatedBinaryThing<ChooseBranchFlagUsePredicate, BranchNormalArgs> { };

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
    using OrgDirective = syn::StatefulOrgDirective<ChangeCurrentAddress, Number>;
    using LabelDirective = syn::StatefulLabelDirective<RegisterLabel, Lexeme>;
    struct WordDirective : syn::StatefulOneArgumentDirective<WordCreator, syn::SymbolWordDirective, LexemeOrNumber> { };
    struct DwordDirective : syn::StatefulOneArgumentDirective<DwordCreator, syn::SymbolDwordDirective, LexemeOrNumber> { };

	struct Directive : pegtl::sor<
					   OrgDirective,
					   LabelDirective,
					   WordDirective,
					   DwordDirective
								   > { };

	struct Statement : pegtl::sor<
					   Instructions,
					   Directive> { };
	struct Anything : pegtl::sor<
					  Separator,
					  SingleLineComment,
					  Statement> { };

	struct Main : public syn::MainFileParser<Anything> { };
} // end namespace cisc0
#endif  // end CISC0_CORE_ASSEMBLER_H__
