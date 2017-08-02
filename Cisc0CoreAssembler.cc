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


// Cisc0CoreAssembler rewritten to use pegtl
#include "Cisc0ClipsExtensions.h"
#include "Cisc0CoreAssembler.h"
#include "Cisc0CoreDecodedInstruction.h"
#include "Cisc0Core.h"

namespace cisc0 {
    namespace assembler {
	    void AssemblerState::resolveInstructions() {
	    	applyToFinishedData([this](InstructionEncoder& op) {
	    		if (op.isLabel()) {
	    			auto label = op.getLabelValue();
	    			auto f = findLabel(label);
	    			if (f == labelsEnd()) {
	    				std::stringstream stream;
	    				stream << "label " << label << " does not exist!\n";
	    				throw syn::Problem(stream.str());
	    			}
	    			op.setFullImmediate(f->second);
	    		}
	    		// now that it has been resolved, we need to go through and setup
	    		// the encoding correctly!
	    		auto address = op.getAddress();
                auto encoding = op.encode();
	    		switch(encoding.getNumWords()) {
	    			case 3:
	    				finalWords.emplace_back(address + 2, encoding.getWord2());
	    			case 2:
	    				finalWords.emplace_back(address + 1, encoding.getWord1());
	    			case 1:
	    				finalWords.emplace_back(address, encoding.getWord0());
	    				break;
	    			default:
	    				throw syn::Problem("Number of words described is not possible!");
	    		}
	    	});
	    }
	    void AssemblerState::resolveDeclarations() {
	    	for (auto & op: wordsToResolve) {
	    		if (op.isLabel()) {
	    			auto label = op.getLabel();
	    			auto f = findLabel(label);
	    			if (f == labelsEnd()) {
	    				std::stringstream stream;
	    				stream << "label " << label << " does not exist!\n";
	    				throw syn::Problem(stream.str());
	    			}
	    			op.setValue(f->second);
	    		}
	    		switch(op.getWidth()) {
	    			case 2:
	    				finalWords.emplace_back(op.getAddress() + 1, syn::getUpperHalf(op.getValue()));
	    			case 1:
	    				finalWords.emplace_back(op.getAddress(), syn::getLowerHalf(op.getValue()));
	    				break;
	    			default:
	    				throw syn::Problem("Got a declaration of with a width that was not 1 or 2");
	    		}
	    	}
	    }
    } // end namespace assembler

    void outputBitmask(std::ostream& out, byte value) noexcept {
        static std::string output[16] = {
            "0m0000",
            "0m0001",
            "0m0010",
            "0m0011",
            "0m0100",
            "0m0101",
            "0m0110",
            "0m0111",
            "0m1000",
            "0m1001",
            "0m1010",
            "0m1011",
            "0m1100",
            "0m1101",
            "0m1110",
            "0m1111",
        };
        out << output[value & 0xF] << " ";
    }
    template<Operation op>
    void outputBitmask(std::ostream& out, const DecodedInstruction& inst) noexcept {
        outputBitmask(out, inst.getBitmask<op>());
    }

    void outputRegister(std::ostream& out, byte index) {
        out << translateRegister(index) << " ";
    }
    template<Operation op>
    void outputDestinationRegister(std::ostream& out, const DecodedInstruction& inst) noexcept {
        outputRegister(out, inst.getDestinationRegister<op>());
    }

    template<ComplexSubTypes op>
    void outputDestinationRegister(std::ostream& out, const DecodedInstruction& inst) noexcept {
        outputRegister(out, inst.getDestinationRegister<op>());
    }

    template<Operation op>
    void outputSourceRegister(std::ostream& out, const DecodedInstruction& inst) noexcept {
        outputRegister(out, inst.getSourceRegister<op>());
    }

	void outputByteImmediate(std::ostream& out, byte value) noexcept {
		out << "0x" << std::hex << int(value) << " ";
	}
    void outputFullImmediate(std::ostream& out, Word lower, Word upper) noexcept {
        out << "0x" << std::hex << cisc0::encodeRegisterValue(upper, lower) << " ";
    }

    std::string translateInstruction(Word a, Word b, Word c) noexcept {
        std::stringstream out;
        translateInstruction(out, a, b, c);
        auto tmp = out.str();
        return tmp;
    }

	inline void outputShiftDirection(std::ostream& out, const DecodedInstruction& inst) noexcept {
		if (inst.shouldShiftLeft()) {
			out << "left ";
		} else {
			out << "right ";
		}
	}

	template<Operation op>
	bool outputImmediateFlag(std::ostream& out, const DecodedInstruction& inst) noexcept {
		auto immediateForm = inst.getImmediateFlag<op>();
		if (immediateForm) {
			out << "immediate ";
		}
		return immediateForm;
	}

	void translateShiftInstruction(std::ostream& out, const DecodedInstruction& inst) noexcept {
		constexpr auto op = Operation::Shift;
		outputShiftDirection(out, inst);
		auto immediateForm = outputImmediateFlag<op>(out, inst);
		outputDestinationRegister<op>(out, inst);
		if (immediateForm) {
			outputByteImmediate(out, inst.getImmediate<op>());
		} else {
			outputSourceRegister<op>(out, inst);
		}
	}

	void translateArithmetic(std::ostream& out, const DecodedInstruction& inst) noexcept {
		constexpr auto op = Operation::Arithmetic;
		out << arithmeticOpsToString(inst.getSubtype<op>()) << " ";
		auto immediateForm = outputImmediateFlag<op>(out, inst);
		outputDestinationRegister<op>(out, inst);
		if (immediateForm) {
			outputByteImmediate(out, inst.getImmediate<op>());
		} else {
			outputSourceRegister<op>(out, inst);
		}
	}

	void translateLogical(std::ostream& out, const DecodedInstruction& inst, Word second, Word third) noexcept {
		constexpr auto op = Operation::Logical;
		out << logicalOpsToString(inst.getSubtype<op>()) << " ";
		auto immediateForm = outputImmediateFlag<op>(out, inst);
		if (immediateForm) {
			outputBitmask<op>(out, inst);
		}
		outputDestinationRegister<op>(out, inst);
		if (immediateForm) {
			outputFullImmediate(out, second, third);
		} else {
			outputSourceRegister<op>(out, inst);
		}
	}

	void translateCompare(std::ostream& out, const DecodedInstruction& inst, Word second, Word third) noexcept {
		constexpr auto op = Operation::Compare;
		auto subType = inst.getSubtype<op>();
		out << compareStyleToString(subType) << " ";
		if (subType == CompareStyle::MoveFromCondition) {
			outputDestinationRegister<op>(out, inst);
		} else if (subType == CompareStyle::MoveToCondition) {
			outputDestinationRegister<op>(out, inst);
		} else {
			auto immediateForm = outputImmediateFlag<op>(out, inst);
			if (immediateForm) {
				outputBitmask<op>(out, inst);
			}
			outputDestinationRegister<op>(out, inst);
			if (immediateForm) {
				outputFullImmediate(out, second, third);
			} else {
				outputSourceRegister<op>(out, inst);
			}
		}
	}

	void translateBranch(std::ostream& out, const DecodedInstruction& inst, Word second, Word third) noexcept {
		constexpr auto op = Operation::Branch;
		if (inst.isCallBranch()) {
			out << "call";
		} else {
			if (inst.isConditionalBranch()) {
				out << "conditional";
			} else {
				out << "unconditional";
			}
		}
		out << " ";
		if (outputImmediateFlag<op>(out, inst)) {
			outputFullImmediate(out, second, third);
		} else {
			outputDestinationRegister<op>(out, inst);
		}
	}
	constexpr bool isStackOperation(MemoryOperation op) noexcept {
		switch(op) {
			case MemoryOperation::Pop:
			case MemoryOperation::Push:
				return true;
			default:
				return false;
		}
	}
	void translateMemory(std::ostream& out, const DecodedInstruction& inst) noexcept {
		constexpr auto op = Operation::Memory;
		auto subType = inst.getSubtype<op>();
		out << memoryOperationToString(subType) << " ";
		outputBitmask<op>(out, inst);
		if (isStackOperation(subType)) {
			outputDestinationRegister<op>(out, inst);
		} else {
			if (inst.isIndirectOperation()) {
				out << "indirect";
			} else {
				out << "direct";
			}
			out << " ";
			outputByteImmediate(out, inst.getMemoryOffset());
		}
	}

	void translateComplex(std::ostream& out, const DecodedInstruction& inst) noexcept {
		constexpr auto op = Operation::Complex;
		auto subType = inst.getSubtype<op>();
		out << complexSubTypesToString(subType) << " ";
		auto extendedOp = [&inst, &out]() {
			constexpr auto op = ComplexSubTypes::Extended;
			auto subType = inst.getComplexSubType<op>();
			using T = decltype(subType);
			out << extendedOperationToString(subType) << " ";
			switch(subType) {
				case T::IsEven:
				case T::IsOdd:
					outputDestinationRegister<op>(out, inst);
					break;
				default:
					break;
			}
		};
		switch(subType) {
			case ComplexSubTypes::Encoding:
				out << encodingOperationToString(inst.getComplexSubType<ComplexSubTypes::Encoding>()) << " ";
				break;
			case ComplexSubTypes::Extended:
				extendedOp();
				break;
			case ComplexSubTypes::Parsing:
				out << parsingOperationToString(inst.getComplexSubType<ComplexSubTypes::Parsing>()) << " ";
				break;
			case ComplexSubTypes::FeatureCheck:
				out << " ; NOTE: feature check is unimplemented right now so this will fail to parse!";
				break;
			default:
				break;
		}

	}

    void translateInstruction(std::ostream& out, Word a, Word b, Word c) noexcept {
        DecodedInstruction first(a);
        out << operationToString(first.getControl()) << " ";
        switch(first.getControl()) {
            case Operation::Return:
                break;
            case Operation::Set:
                outputBitmask<Operation::Set>(out, first);
                outputDestinationRegister<Operation::Set>(out, first);
                outputFullImmediate(out, b, c);
                break;
            case Operation::Swap:
                outputDestinationRegister<Operation::Swap>(out, first);
                outputSourceRegister<Operation::Swap>(out, first);
                break;
			case Operation::Move:
				outputBitmask<Operation::Move>(out, first);
				outputDestinationRegister<Operation::Move>(out, first);
				outputSourceRegister<Operation::Move>(out, first);
				break;
			case Operation::Shift:
				translateShiftInstruction(out, first);
				break;
			case Operation::Arithmetic:
				translateArithmetic(out, first);
				break;
			case Operation::Logical:
				translateLogical(out, first, b, c);
				break;
			case Operation::Compare:
				translateCompare(out, first, b, c);
				break;
			case Operation::Branch:
				translateBranch(out, first, b, c);
				break;
			case Operation::Memory:
				translateMemory(out, first);
				break;
			case Operation::Complex:
				translateComplex(out, first);
				break;
			default:
				out << " ; type is unimplemented!";
				break;
        }
    }

#define StringToEnumEntry(str, type) { str , type },
#define EnumToStringEntry(str, type) { type , str },
#define DefBeginStringToEnumFn(type) \
    type stringTo ## type (const std::string& str) noexcept { \
        static std::map<std::string, type > translation = {
#define DefEndStringToEnumFn(type) \
        }; \
        auto x = translation.find(str); \
        if (x == translation.end()) { \
            return syn::defaultErrorState< type > ; \
        } else { \
            return x->second; \
        } \
    }
#define DefBeginEnumToStringFn(type, title) \
    const std::string& title ## ToString ( type value ) noexcept { \
        static std::string errorState; \
        static std::map < type , std::string > translation = {

#define DefEndEnumToStringFn(type) \
        }; \
        auto x = translation.find(value); \
        if (x == translation.end()) { \
            return errorState; \
        } else { \
            return x->second; \
        } \
    }


	Word translateRegister(const std::string& input) {
		static std::map<std::string, Word> builtinAliases = {
#define X(str, type, _) \
            { str , static_cast<Word>(type) } ,
#include "desc/cisc0/RegisterNames.desc"
#undef X
		};
		auto result = builtinAliases.find(input);
		if (result == builtinAliases.end()) {
			return syn::getRegister<Word, ArchitectureConstants::RegisterCount>(input, syn::reportError);
		} else {
			return result->second;
		}
	}

    const std::string& registerIndexToString(Word input) {
        static std::map<Word, std::string> lookup = {
#define X(str, type, _) \
            { static_cast<Word>(type), str },
#include "desc/cisc0/RegisterNames.desc"
#undef X
        };
        static std::string constants[ArchitectureConstants::RegisterCount];
        static bool init = true;
        if (init) {
            init = false;
            for (int i = 0; i < static_cast<int>(ArchitectureConstants::RegisterCount); ++i) {
                std::stringstream tmp;
                tmp << "r" << i;
                auto str = tmp.str();
                constants[i] = str;
            }
        }
        auto result = lookup.find(input);
        if (result == lookup.end()) {
            auto index = input & 0xF;
            return constants[index];
        } else {
            return result->second;
        }
    }

DefBeginStringToEnumFn(CompareStyle)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/CompareStyle.desc"
#undef X
DefEndStringToEnumFn(CompareStyle)

DefBeginStringToEnumFn(ArithmeticOps)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/ArithmeticOps.desc"
#undef X
DefEndStringToEnumFn(ArithmeticOps)

DefBeginStringToEnumFn(MemoryOperation)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/MemoryOperation.desc"
#undef X
DefEndStringToEnumFn(MemoryOperation)

DefBeginStringToEnumFn(LogicalOps)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/LogicalOps.desc"
#undef X
DefEndStringToEnumFn(LogicalOps)

DefBeginStringToEnumFn(EncodingOperation)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/EncodingOperation.desc"
#undef X
DefEndStringToEnumFn(EncodingOperation)

DefBeginStringToEnumFn(ExtendedOperation)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/ExtendedOperation.desc"
#undef X
DefEndStringToEnumFn(ExtendedOperation)

DefBeginStringToEnumFn(ComplexSubTypes)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/ComplexSubTypes.desc"
#undef X
DefEndStringToEnumFn(ComplexSubTypes)

DefBeginStringToEnumFn(ParsingOperation)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/ParsingOperation.desc"
#undef X
DefEndStringToEnumFn(ParsingOperation)

DefBeginEnumToStringFn(CompareStyle, compareStyle)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/CompareStyle.desc"
#undef X
DefEndEnumToStringFn(CompareStyle)

DefBeginEnumToStringFn(ArithmeticOps, arithmeticOps)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/ArithmeticOps.desc"
#undef X
DefEndEnumToStringFn(ArithmeticOps)

DefBeginEnumToStringFn(MemoryOperation, memoryOperation)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/MemoryOperation.desc"
#undef X
DefEndEnumToStringFn(MemoryOperation)

DefBeginEnumToStringFn(LogicalOps, logicalOps)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/LogicalOps.desc"
#undef X
DefEndEnumToStringFn(LogicalOps)

DefBeginEnumToStringFn(EncodingOperation, encodingOperation)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/EncodingOperation.desc"
#undef X
DefEndEnumToStringFn(EncodingOperation)

DefBeginEnumToStringFn(ExtendedOperation, extendedOperation)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/ExtendedOperation.desc"
#undef X
DefEndEnumToStringFn(ExtendedOperation)

DefBeginEnumToStringFn(ComplexSubTypes, complexSubTypes)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/ComplexSubTypes.desc"
#undef X
DefEndEnumToStringFn(ComplexSubTypes)

DefBeginEnumToStringFn(ParsingOperation, parsingOperation)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/ParsingOperation.desc"
#undef X
DefEndEnumToStringFn(ParsingOperation)

DefBeginEnumToStringFn(Operation, operation)
#define X(str, type, _) EnumToStringEntry(str, type)
#include "desc/cisc0/Operation.desc"
#undef X
DefEndEnumToStringFn(Operation)

DefBeginStringToEnumFn(Operation)
#define X(str, type, _) StringToEnumEntry(str, type)
#include "desc/cisc0/Operation.desc"
#undef X
DefEndStringToEnumFn(Operation)
}
