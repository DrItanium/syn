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


// IrisCoreAssembler rewritten to use pegtl
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include "Base.h"
#include "AssemblerBase.h"
#include "Problem.h"
#include "IrisCore.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>
#include <pegtl/parse.hh>
#include <vector>
#include "IrisClipsExtensions.h"
#include "ClipsExtensions.h"
#include "IrisCoreAssemblerStructures.h"

namespace iris {
    AssemblerData::AssemblerData() noexcept : instruction(false), address(0), dataValue(0), group(0), operation(0), destination(0), source0(0), source1(0), hasLexeme(false), fullImmediate(false) { }

	void AssemblerData::setImmediate(word value) noexcept {
		source0 = syn::getLowerHalf<word>(value);
		source1 = syn::getUpperHalf<word>(value);
	}
    raw_instruction AssemblerData::encode() const noexcept {
        if (instruction) {
            return iris::encodeInstruction(group, operation, destination, source0, source1);
        } else {
            return dataValue;
        }
    }

	void AssemblerState::setCurrentAddress(word value) noexcept {
		if (inDataSection()) {
			data.setCurrentAddress(value);
		} else {
			code.setCurrentAddress(value);
		}
	}
	void AssemblerState::registerLabel(const std::string& value) noexcept {
		LabelTracker::registerLabel(value, getCurrentAddress());
	}
	word AssemblerState::getCurrentAddress() const noexcept {
		return inDataSection() ? data.getCurrentAddress() : code.getCurrentAddress();
	}
	void AssemblerState::incrementCurrentAddress() noexcept {
		if (inDataSection()) {
			data.incrementCurrentAddress();
		} else {
			code.incrementCurrentAddress();
		}
	}
	void AssemblerInstruction::setField(RegisterPositionType type, byte value) {
		using Type = RegisterPositionType;
		switch(type) {
			case Type::DestinationGPR:
				destination = value;
				break;
			case Type::Source0GPR:
				source0 = value;
				break;
			case Type::Source1GPR:
				source1 = value;
				break;
			case Type::PredicateDestination:
				destination = iris::encode4Bits<false>(destination, value);
				break;
			case Type::PredicateInverseDestination:
				destination = iris::encode4Bits<true>(destination, value);
				break;
			case Type::PredicateSource0:
				source0 = iris::encode4Bits<false>(source0, value);
				break;
			case Type::PredicateSource1:
				source0 = iris::encode4Bits<true>(source0, value);
				break;
			default:
				syn::reportError("Illegal index provided!");
		}
	}
	ConditionRegisterOp stringToConditionRegisterOp(const std::string& title) noexcept {
		static std::map<std::string, ConditionRegisterOp> lookup = {
			{ "psave", ConditionRegisterOp::SaveCRs },
			{ "prestore", ConditionRegisterOp::RestoreCRs },
			{ "pxor", ConditionRegisterOp::CRXor },
			{ "pnot", ConditionRegisterOp::CRNot },
			{ "pand", ConditionRegisterOp::CRAnd },
			{ "por", ConditionRegisterOp::CROr },
			{ "pnand", ConditionRegisterOp::CRNand },
			{ "pnor", ConditionRegisterOp::CRNor },
			{ "pswap", ConditionRegisterOp::CRSwap },
			{ "pmove", ConditionRegisterOp::CRMove },
		};
		auto find = lookup.find(title);
		if (find == lookup.end()) {
			return syn::defaultErrorState<ConditionRegisterOp>;
		} else {
			return find->second;
		}
	}
	
	const std::string& conditionRegisterOpToString(ConditionRegisterOp op) noexcept {
		static std::string errorState;
		static std::map<ConditionRegisterOp, std::string> lookup = {
			{ ConditionRegisterOp::SaveCRs, "psave" },
			{ ConditionRegisterOp::RestoreCRs, "prestore"},
			{ ConditionRegisterOp::CRXor, "pxor" },
			{ ConditionRegisterOp::CRNot, "pnot" },
			{ ConditionRegisterOp::CRAnd, "pand" },
			{ ConditionRegisterOp::CROr, "por" },
			{ ConditionRegisterOp::CRNand, "pnand" },
			{ ConditionRegisterOp::CRNor, "pnor" },
			{ ConditionRegisterOp::CRSwap , "pswap" },
			{ ConditionRegisterOp::CRMove, "pmove" },
		};
		auto find = lookup.find(op);
		if (find == lookup.end()) {
			return errorState;
		} else {
			return find->second;
		}

	}
	ArithmeticOp stringToArithmeticOp(const std::string& title) noexcept {
		static std::map<std::string, ArithmeticOp> lookup = {
			{ "add", ArithmeticOp::Add},
			{ "sub", ArithmeticOp::Sub},
			{ "mul", ArithmeticOp::Mul},
			{ "div", ArithmeticOp::Div},
			{ "rem", ArithmeticOp::Rem},
			{ "shl", ArithmeticOp::ShiftLeft},
			{ "shr", ArithmeticOp::ShiftRight },
			{ "and", ArithmeticOp::BinaryAnd},
			{ "or", ArithmeticOp::BinaryOr},
			{ "not", ArithmeticOp::BinaryNot},
			{ "xor", ArithmeticOp::BinaryXor },
			{ "nand", ArithmeticOp::BinaryNand},
			{ "nor", ArithmeticOp::BinaryNor},
			{ "min", ArithmeticOp::Min},
			{ "max", ArithmeticOp::Max},
			{ "addi", ArithmeticOp::AddImmediate},
			{ "subi", ArithmeticOp::SubImmediate},
			{ "muli", ArithmeticOp::MulImmediate},
			{ "divi", ArithmeticOp::DivImmediate},
			{ "remi", ArithmeticOp::RemImmediate},
			{ "shli", ArithmeticOp::ShiftLeftImmediate },
			{ "shri", ArithmeticOp::ShiftRightImmediate },
		};
		auto find = lookup.find(title);
		if (find == lookup.end()) {
			return syn::defaultErrorState<ArithmeticOp>;
		} else {
			return find->second;
		}
	}
	MoveOp stringToMoveOp(const std::string& title) noexcept {
		static std::map<std::string, MoveOp> lookup = {
			{ "mtip", MoveOp::MoveToIP},
			{ "mfip", MoveOp::MoveFromIP},
			{ "mtlr", MoveOp::MoveToLR},
			{ "mflr", MoveOp::MoveFromLR},
			{ "rregs", MoveOp::RestoreAllRegisters},
			{ "sregs", MoveOp::SaveAllRegisters},
			{ "move", MoveOp::Move},
			{ "swap", MoveOp::Swap},
			{ "load", MoveOp::Load},
			{ "store", MoveOp::Store},
			{ "io-load", MoveOp::IORead},
			{ "io-store", MoveOp::IOWrite },
			{ "push", MoveOp::Push },
			{ "pop", MoveOp::Pop },
			{ "load-offset", MoveOp::LoadWithOffset },
			{ "store-offset", MoveOp::StoreWithOffset },
			{ "ioldof", MoveOp::IOReadWithOffset },
			{ "iostof", MoveOp::IOWriteWithOffset },
			{ "code-load", MoveOp::LoadCode },
			{ "code-store", MoveOp::StoreCode },
			{ "pushi", MoveOp::PushImmediate},
			{ "set", MoveOp::Set },
			{ "loadi", MoveOp::LoadImmediate },
			{ "storei", MoveOp::StoreImmediate },
		};
		auto find = lookup.find(title);
		if (find == lookup.end()) {
			return syn::defaultErrorState<MoveOp>;
		} else {
			return find->second;
		}
	}
	JumpOp stringToJumpOp(const std::string& title) noexcept {
		static std::map<std::string, JumpOp> lookup = {
			{ "branch", JumpOp::BranchUnconditional},
			{ "branch_l", JumpOp::BranchUnconditionalLink},
			{ "branch_i", JumpOp::BranchUnconditionalImmediate},
			{ "branch_il", JumpOp::BranchUnconditionalImmediateLink},
			{ "branch_c", JumpOp::BranchConditional },
			{ "branch_cl", JumpOp::BranchConditionalLink},
			{ "branch_ci", JumpOp::BranchConditionalImmediate},
			{ "branch_cil", JumpOp::BranchConditionalImmediateLink},
			{ "branch_clr", JumpOp::BranchConditionalLR},
			{ "branch_clrl", JumpOp::BranchConditionalLRAndLink},
			{ "branch_lr", JumpOp::BranchUnconditionalLR},
			{ "branch_lrl", JumpOp::BranchUnconditionalLRAndLink},
			{ "rfe", JumpOp::ReturnFromError},
		};
		auto find = lookup.find(title);
		if (find == lookup.end()) {
			return syn::defaultErrorState<JumpOp>;
		} else {
			return find->second;
		}
	}
	CompareOp stringToCompareOp(const std::string& title) noexcept {
		static std::map<std::string, CompareOp> lookup = {
			{ "eq",   CompareOp::Eq},
			{ "neq",  CompareOp::Neq},
			{ "lt",   CompareOp::LessThan},
			{ "gt",   CompareOp::GreaterThan},
			{ "le",   CompareOp::LessThanOrEqualTo},
			{ "ge",   CompareOp::GreaterThanOrEqualTo},
			{ "eqi",  CompareOp::EqImmediate },
			{ "neqi", CompareOp::NeqImmediate },
			{ "lti",  CompareOp::LessThanImmediate },
			{ "gti",  CompareOp::GreaterThanImmediate },
			{ "lei",  CompareOp::LessThanOrEqualToImmediate },
			{ "gei",  CompareOp::GreaterThanOrEqualToImmediate },
		};
		auto find = lookup.find(title);
		if (find == lookup.end()) {
			return syn::defaultErrorState<CompareOp>;
		} else {
			return find->second;
		}
	}
		bool AssemblerDirective::shouldChangeSectionToCode() const noexcept {
            return (action == AssemblerDirectiveAction::ChangeSection) && (section == SectionType::Code);
        }
		bool AssemblerDirective::shouldChangeSectionToData() const noexcept {
            return (action == AssemblerDirectiveAction::ChangeSection) && (section == SectionType::Data);
        }
		bool AssemblerDirective::shouldChangeCurrentAddress() const noexcept {
            return (action == AssemblerDirectiveAction::ChangeCurrentAddress);
        }
		bool AssemblerDirective::shouldDefineLabel() const noexcept {
            return (action == AssemblerDirectiveAction::DefineLabel);
        }
        bool AssemblerDirective::shouldStoreWord() const noexcept {
            return (action == AssemblerDirectiveAction::StoreWord);
        }
        bool AssemblerData::shouldResolveLabel() const noexcept {
            return fullImmediate && hasLexeme;
        }

        void AssemblerState::nowInCodeSection() noexcept {
            _section = SectionType::Code;
        }
        void AssemblerState::nowInDataSection() noexcept {
            _section = SectionType::Data;
        }
		bool AssemblerState::inCodeSection() const noexcept { return _section == SectionType::Code; }
		bool AssemblerState::inDataSection() const noexcept { return _section == SectionType::Data; }

		const std::string& registerIndexToString(byte index) noexcept {
			static bool init = true;
			static std::string container[ArchitectureConstants::RegisterCount];
			if (init) {
				init = false;
				std::stringstream builder;
				for (int i = 0; i < ArchitectureConstants::RegisterCount; ++i) {
					builder << "r" << i;
					auto tmp = builder.str();
					container[i] = tmp;
					builder.str();
				}
			}
			return container[index];
		}
		const std::string& predicateIndexToString(byte index) noexcept {
			static bool init = true;
			static std::string container[ArchitectureConstants::ConditionRegisterCount];
			if (init) {
				init = false;
				std::stringstream builder;
				for (int i = 0; i < ArchitectureConstants::ConditionRegisterCount; ++i) {
					builder << "p" << i;
					auto tmp = builder.str();
					container[i] = tmp;
					builder.str();
				}
			}
			return container[index & ArchitectureConstants::ConditionRegisterMask];
		}

		const std::string& instructionGroupToString(InstructionGroup group) noexcept {
			static std::string errorState;
			static std::map<InstructionGroup, std::string> lookup = {
				{ InstructionGroup::Move, "move" },
				{ InstructionGroup::Jump, "jump" },
				{ InstructionGroup::Compare, "compare" },
				{ InstructionGroup::Arithmetic, "arithmetic" },
				{ InstructionGroup::Unused0, "unused0" },
				{ InstructionGroup::CustomInstructionReserved, "custom-instruction" },
				{ InstructionGroup::ConditionalRegister, "conditional-register" },
			};
			auto find = lookup.find(group);
			if (find == lookup.end()) {
				return errorState;
			} else {
				return find->second;
			}
		}

} // end namespace iris