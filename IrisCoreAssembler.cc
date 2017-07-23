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
#include "IrisCoreAssembler.h"

namespace iris {
    AssemblerData::AssemblerData() noexcept : instruction(false), address(0), dataValue(0), group(0), operation(0), destination(0), source0(0), source1(0), hasLexeme(false), fullImmediate(false) { }
	void AssemblerData::reset() noexcept {
		instruction = false;
		address = 0;
		dataValue = 0;
		group = 0;
		operation = 0;
		destination = 0;
		source0 = 0;
		source1 = 0;
		hasLexeme = false;
		currentLexeme.clear();
		fullImmediate = false;
	}

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
		if (inData) {
			data.setCurrentAddress(value);
		} else {
			code.setCurrentAddress(value);
		}
	}
	void AssemblerState::setImmediate(word value) noexcept {
		current.setImmediate(value);
	}
	void AssemblerState::setGroup(InstructionGroup value) noexcept {
		current.group = static_cast<byte>(value);
	}
	void AssemblerState::setHalfImmediate(byte value) noexcept {
		current.source1 = value;
	}

	void AssemblerState::resetCurrentData() noexcept {
		current.reset();
	}

	void AssemblerState::registerLabel(const std::string& value) noexcept {
		LabelTracker::registerLabel(value, getCurrentAddress());
	}
	word AssemblerState::getCurrentAddress() const noexcept {
		return inData ? data.getCurrentAddress() : code.getCurrentAddress();
	}
	void AssemblerState::incrementCurrentAddress() noexcept {
		if (inData) {
			data.incrementCurrentAddress();
		} else {
			code.incrementCurrentAddress();
		}
	}
	void AssemblerState::saveToFinished() noexcept {
		current.address = getCurrentAddress();
		auto copy = current;
		addToFinishedData(copy);
		resetCurrentData();
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
			{ "ld", MoveOp::Load},
			{ "st", MoveOp::Store}, 
			{ "iold", MoveOp::IORead},
			{ "iost", MoveOp::IOWrite },
			{ "push", MoveOp::Push },
			{ "pop", MoveOp::Pop },
			{ "ldof", MoveOp::LoadWithOffset },
			{ "stof", MoveOp::StoreWithOffset },
			{ "ioldof", MoveOp::IOReadWithOffset },
			{ "iostof", MoveOp::IOWriteWithOffset },
			{ "cld", MoveOp::LoadCode },
			{ "cst", MoveOp::StoreCode },
			{ "pushi", MoveOp::PushImmediate}, 
			{ "set", MoveOp::Set },
			{ "ldi", MoveOp::LoadImmediate },
			{ "sti", MoveOp::StoreImmediate },
		};
		auto find = lookup.find(title);
		if (find == lookup.end()) {
			return syn::defaultErrorState<MoveOp>;
		} else {
			return find->second;
		}
	}
} // end namespace iris
