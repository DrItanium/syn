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


#include "ClipsExtensions.h"
#include "Base.h"
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <cstdint>
#include <climits>
#include <sstream>
#include <memory>
#include <map>
#include <iostream>

extern "C" {
#include "clips.h"
}

namespace syn {
	void CLIPS_errorMessageGeneric(UDFContext* context, CLIPSValue* ret, const char* msg) noexcept {
		UDFInvalidArgumentMessage(context, msg);
		CVSetBoolean(ret, false);
	}
	void CLIPS_errorMessageGeneric(UDFContext* context, CLIPSValue* ret, const std::string& msg) noexcept {
		CLIPS_errorMessageGeneric(context, ret, msg.c_str());
	}
	void CLIPS_errorOverflowedNumber(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_errorMessageGeneric(context, ret, "number is too large and overflowed");
	}
	void CLIPS_translateBitmask(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPSValue value;
		if (!UDFFirstArgument(context, LEXEME_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else {
			std::string str(CVToString(&value));
			if (boost::starts_with(str, "0m")) {
				str.at(1) = '0';
				auto tmp = strtoul(str.c_str(), NULL, 2);
				if (tmp == ULONG_MAX && errno == ERANGE) {
					CLIPS_errorOverflowedNumber(context, ret);
				} else {
					if (tmp > 0xFF) {
						CLIPS_errorMessageGeneric(context, ret, "provided number is larger than 8-bits!");
					} else {
						CVSetInteger(ret, static_cast<CLIPSInteger>(static_cast<byte>(tmp)));
					}
				}
			} else {
				CLIPS_errorMessageGeneric(context, ret, "Bitmask must start with 0m");
			}
		}
	}
	constexpr unsigned long long maximumIntegerValue = 0xFFFFFFFFFFFFFFFF;
	void CLIPS_errorNumberLargerThan64Bits(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_errorMessageGeneric(context, ret, "provided number is larger than 64-bits!");
	}

	void CLIPS_translateNumberBase(UDFContext* context, CLIPSValue* ret, const std::string& prefix, int base, const std::string& badPrefix) noexcept {
		CLIPSValue value;
		if (!UDFFirstArgument(context, LEXEME_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else {
			std::string str(CVToString(&value));
			if (boost::starts_with(str, prefix)) {
				auto tmp = strtoull(str.c_str(), nullptr, base);
				if (tmp == ULLONG_MAX && errno == ERANGE) {
					CLIPS_errorOverflowedNumber(context, ret);
				} else {
					if (tmp > maximumIntegerValue) {
						CLIPS_errorNumberLargerThan64Bits(context, ret);
					} else {
						CVSetInteger(ret, static_cast<CLIPSInteger>(tmp));
					}
				}
			} else {
				CLIPS_errorMessageGeneric(context, ret, badPrefix);
			}
		}
	}
	void CLIPS_translateBinary(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_translateNumberBase(context, ret, "0b", 2, "Binary must start with 0b");
	}

	void CLIPS_translateHex(UDFContext* context, CLIPSValue* ret) {
		CLIPS_translateNumberBase(context, ret, "0x", 16, "Hex must start with 0x");
	}


	enum CLIPS_UnaryOperations {
		Not,
		ExpandBits,
	};
	template<CLIPS_UnaryOperations op>
	void CLIPS_genericUnaryIntegerOperation(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPSValue number;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &number)) {
			CVSetBoolean(ret, false);
		} else {
			switch(op) {
				case CLIPS_UnaryOperations::Not:
					CVSetInteger(ret, binaryNot<CLIPSInteger>(CVToInteger(&number)));
					break;
				case CLIPS_UnaryOperations::ExpandBits:
					CVSetInteger(ret, static_cast<CLIPSInteger>(expandBit(CVIsTrueSymbol(&number))));
					break;
				default:
					CVSetBoolean(ret, false);
					break;
			}
		}

	}
	enum CLIPS_BinaryOperations {
		And,
		Or,
		Xor,
		Nand,
		ShiftLeft,
		ShiftRight,
	};

	template<CLIPS_BinaryOperations op>
	void CLIPS_genericBinaryIntegerOperation(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPSValue a, b;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &a)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &b)) {
			CVSetBoolean(ret, false);
		} else {
			CLIPSInteger result = 0;
			auto first = CVToInteger(&a);
			auto second = CVToInteger(&b);
			switch(op) {
				case CLIPS_BinaryOperations::And:
					result = binaryAnd<CLIPSInteger>(first, second);
					break;
				case CLIPS_BinaryOperations::Or:
					result = binaryOr<CLIPSInteger>(first, second);
					break;
				case CLIPS_BinaryOperations::Xor:
					result = binaryXor<CLIPSInteger>(first, second);
					break;
				case CLIPS_BinaryOperations::Nand:
					result = binaryNand<CLIPSInteger>(first, second);
					break;
				case CLIPS_BinaryOperations::ShiftLeft:
					result = shiftLeft<CLIPSInteger>(first, second);
					break;
				case CLIPS_BinaryOperations::ShiftRight:
					result = shiftRight<CLIPSInteger>(first, second);
					break;
				default:
					CVSetBoolean(ret, false);
					return;
			}
			CVSetInteger(ret, result);
		}
	}
	void CLIPS_binaryAnd(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<CLIPS_BinaryOperations::And>(context, ret);
	}
	void CLIPS_binaryOr(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<CLIPS_BinaryOperations::Or>(context, ret);
	}
	void CLIPS_binaryXor(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<CLIPS_BinaryOperations::Xor>(context, ret);
	}
	void CLIPS_binaryNand(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<CLIPS_BinaryOperations::Nand>(context, ret);
	}

	void CLIPS_shiftLeft(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<CLIPS_BinaryOperations::ShiftLeft>(context, ret);
	}

	void CLIPS_shiftRight(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericBinaryIntegerOperation<CLIPS_BinaryOperations::ShiftRight>(context, ret);
	}

	void CLIPS_binaryNot(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericUnaryIntegerOperation<CLIPS_UnaryOperations::Not>(context, ret);
	}

	void CLIPS_expandBit(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPS_genericUnaryIntegerOperation<CLIPS_UnaryOperations::ExpandBits>(context, ret);
	}

	void CLIPS_basePrintAddress(void* env, const char* logicalName, void* theValue, const char* func, const char* majorType) {
		std::stringstream ss;
		void* ptr = EnvValueToExternalAddress(env, theValue);
		ss << "<" << majorType << "-" << func << "-" << std::hex << ((ptr) ? ptr : theValue) << ">";
		auto str = ss.str();
		EnvPrintRouter(env, logicalName, str.c_str());
	}
	void CLIPS_basePrintAddress_Pointer(void* env, const char* logicalName, void* theValue, const char* func) noexcept {
		CLIPS_basePrintAddress(env, logicalName, theValue, func, "Pointer");
	}
	void CLIPS_decodeBits(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue value, mask, shift;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &mask)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &shift)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, decodeBits<CLIPSInteger, CLIPSInteger>(CVToInteger(&value), CVToInteger(&mask), CVToInteger(&shift)));
		}
	}
	void CLIPS_encodeBits(UDFContext* context, CLIPSValue* ret) noexcept {
		CLIPSValue input, value, mask, shift;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &input)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &value)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &mask)) {
			CVSetBoolean(ret, false);
		} else if (!UDFNextArgument(context, NUMBER_TYPES, &shift)) {
			CVSetBoolean(ret, false);
		} else {
			CVSetInteger(ret, encodeBits<CLIPSInteger, CLIPSInteger>(CVToInteger(&input), CVToInteger(&value), CVToInteger(&mask), CVToInteger(&shift)));
		}
	}

	bool errorMessage(void* env, const std::string& idClass, int idIndex, const std::string& msgPrefix, const std::string& msg) noexcept {
		PrintErrorID(env, idClass.c_str(), idIndex, false);
		EnvPrintRouter(env, WERROR, msgPrefix.c_str());
		EnvPrintRouter(env, WERROR, msg.c_str());
		EnvPrintRouter(env, WERROR, "\n");
		EnvSetEvaluationError(env, true);
		return false;
	}

	DefWrapperSymbolicName(CLIPSInteger[], "memory-block");
	class ManagedMemoryBlock : public ExternalAddressWrapper<CLIPSInteger[]> {
		public:
			using Word = CLIPSInteger;
			static ManagedMemoryBlock* make(CLIPSInteger capacity) noexcept;
			static void newFunction(void* env, DataObjectPtr ret);
			static bool callFunction(void* env, DataObjectPtr value, DataObjectPtr ret);
			static void registerWithEnvironment(void* env, const char* title) { ExternalAddressWrapper<Word[]>::registerWithEnvironment(env, title, newFunction, callFunction); }
			static void registerWithEnvironment(void* env) { registerWithEnvironment(env, "memory-block"); }
		public:
			ManagedMemoryBlock(CLIPSInteger capacity) :
				ExternalAddressWrapper<Word[]>(std::move(std::make_unique<Word[]>(capacity))), _capacity(capacity) { }
			inline CLIPSInteger size() const noexcept                                    { return _capacity; }
			inline bool legalAddress(CLIPSInteger idx) const noexcept                    { return inRange<CLIPSInteger>(_capacity, idx); }
			inline Word getMemoryCellValue(CLIPSInteger addr) noexcept                   { return this->_value.get()[addr]; }
			inline void setMemoryCell(CLIPSInteger addr0, Word value) noexcept           { this->_value.get()[addr0] = value; }
			inline void swapMemoryCells(CLIPSInteger addr0, CLIPSInteger addr1) noexcept { swap<Word>(this->_value.get()[addr0], this->_value.get()[addr1]); }
			inline void decrementMemoryCell(CLIPSInteger address) noexcept               { --this->_value.get()[address]; }
			inline void incrementMemoryCell(CLIPSInteger address) noexcept               { ++this->_value.get()[address]; }

			inline void copyMemoryCell(CLIPSInteger from, CLIPSInteger to) noexcept {
				auto ptr = this->_value.get();
				ptr[to] = ptr[from];
			}
			inline void setMemoryToSingleValue(Word value) noexcept {
				auto ptr = this->_value.get();
				for (CLIPSInteger i = 0; i < _capacity; ++i) {
					ptr[i] = value;
				}
			}
		private:
			CLIPSInteger _capacity;
			static std::string _type;
	};

	std::string ManagedMemoryBlock::_type = ManagedMemoryBlock::getType();

	using ManagedMemoryBlock_Ptr = ManagedMemoryBlock*;

	ManagedMemoryBlock* ManagedMemoryBlock::make(CLIPSInteger capacity) noexcept {
		return new ManagedMemoryBlock(capacity);
	}
#define argCheck(storage, position, type) EnvArgTypeCheck(env, funcStr.c_str(), position, type, storage)
	void ManagedMemoryBlock::newFunction(void* env, DataObjectPtr ret) {
		static bool init = true;
		static std::string funcStr;
		static std::string funcErrorPrefix;
		if (init) {
			init = false;
			std::stringstream ss, ss2;
			ss << "new (" << _type << ")";
			funcStr = ss.str();
			ss2 << "Function " << funcStr;
			funcErrorPrefix = ss2.str();
		}

		try {
			if (EnvRtnArgCount(env) == 2) {
				CLIPSValue capacity;
				if (!argCheck(&capacity, 2, INTEGER)) {
					CVSetBoolean(ret, false);
					errorMessage(env, "NEW", 1, funcErrorPrefix, " expected an integer for capacity!");
				} else {
					auto size = EnvDOToLong(env, capacity);
					auto idIndex = ManagedMemoryBlock::getAssociatedEnvironmentId(env);
					ret->bitType = EXTERNAL_ADDRESS_TYPE;
					SetpType(ret, EXTERNAL_ADDRESS);
					SetpValue(ret, EnvAddExternalAddress(env, ManagedMemoryBlock::make(size), idIndex));
				}
			} else {
				errorMessage(env, "NEW", 1, funcErrorPrefix, " function new expected no arguments besides type!");
				CVSetBoolean(ret, false);
			}
		} catch(syn::Problem p) {
			CVSetBoolean(ret, false);
			std::stringstream s;
			s << "an exception was thrown: " << p.what();
			auto str = s.str();
			errorMessage(env, "NEW", 2, funcErrorPrefix, str);
		}
	}
	bool ManagedMemoryBlock::callFunction(void* env, DataObjectPtr value, DataObjectPtr ret) {
		static bool init = true;
		static std::string funcStr;
		static std::string funcErrorPrefix;
#include "syn_memory_block_defines.h"
		auto isArithmeticOp = [](MemoryBlockOp op) {
			switch(op) {
				case MemoryBlockOp::Combine:
				case MemoryBlockOp::Difference:
				case MemoryBlockOp::Product:
				case MemoryBlockOp::Divide:
				case MemoryBlockOp::Remainder:
					return true;
				default:
					return false;
			}
		};
		if (init) {
			init = false;
			std::stringstream ss, ss2;
			ss << "call (" << _type << ")";
			funcStr = ss.str();
			ss2 << "Function " << funcStr;
			funcErrorPrefix = ss2.str();
		}
		if (GetpType(value) == EXTERNAL_ADDRESS) {
			auto ptr = static_cast<ManagedMemoryBlock_Ptr>(DOPToExternalAddress(value));
			auto callErrorMessage = [env, ret](const std::string& subOp, const std::string& rest) {
				CVSetBoolean(ret, false);
				std::stringstream stm;
				stm << " " << subOp << ": " << rest << std::endl;
				auto msg = stm.str();
				return errorMessage(env, "CALL", 3, funcErrorPrefix, msg);
			};
			auto errOutOfRange = [callErrorMessage, env, ret](const std::string& subOp, CLIPSInteger capacity, CLIPSInteger address) {
				std::stringstream ss;
				ss << funcErrorPrefix << ": Provided address " << std::hex << address << " is either less than zero or greater than " << std::hex << capacity << std::endl;
				return callErrorMessage(subOp, ss.str());
			};
			CLIPSValue operation;
			if (!argCheck(&operation, 2, SYMBOL)) {
				return errorMessage(env, "CALL", 2, funcErrorPrefix, "expected a function name to call!");
			} else {
				std::string str(EnvDOToString(env, operation));
				// translate the op to an enumeration
				auto result = opTranslation.find(str);
				if (result == opTranslation.end()) {
					CVSetBoolean(ret, false);
					return callErrorMessage(str, "<- unknown operation requested!");
				} else {
					auto rangeViolation = [errOutOfRange, ptr, &str](CLIPSInteger addr) { errOutOfRange(str, ptr->size(), addr); };
					CLIPSValue arg0, arg1;
					auto checkArg = [callErrorMessage, &str, env](unsigned int index, unsigned int type, const std::string& msg, CLIPSValue* dat) {
						if (!argCheck(dat, index, type)) {
							return callErrorMessage(str, msg);
						} else {
							return true;
						}
					};
					auto checkArg0 = [checkArg, &arg0, env, &str](unsigned int type, const std::string& msg) { return checkArg(3, type, msg, &arg0); };
					auto checkArg1 = [checkArg, &arg1, env, &str](unsigned int type, const std::string& msg) { return checkArg(4, type, msg, &arg1); };
					auto oneCheck = [checkArg0](unsigned int type, const std::string& msg) { return checkArg0(type, msg); };
					auto twoCheck = [checkArg0, checkArg1](unsigned int type0, const std::string& msg0, unsigned int type1, const std::string& msg1) {
						return checkArg0(type0, msg0) && checkArg1(type1, msg1);
					};
					auto op = result->second;
					// TODO: clean this up when we migrate to c++17 and use the
					// inline if variable declarations
					auto findOpCount = opArgCounts.find(op);
					if (findOpCount == opArgCounts.end()) {
						CVSetBoolean(ret, false);
						return callErrorMessage(str, "<- unknown argument count, not registered!!!!!");
					} else {
						// if it is registered then check the length
						auto argCount = 2 /* always have two arguments */  + findOpCount->second;
						if (argCount != EnvRtnArgCount(env)) {
							std::stringstream ss;
							ss << " expected " << std::dec << argCount << " arguments";
							CVSetBoolean(ret, false);
							auto tmp = ss.str();
							return callErrorMessage(str, tmp);
						}
					}
					CVSetBoolean(ret, true);
					// now check and see if we are looking at a legal
					// instruction count
					if (op == MemoryBlockOp::Type) {
						CVSetSymbol(ret, _type.c_str());
					} else if (op == MemoryBlockOp::Size) {
						CVSetInteger(ret, ptr->size());
					} else if (op == MemoryBlockOp::Clear) {
						ptr->setMemoryToSingleValue(0);
					} else if (op == MemoryBlockOp::Initialize) {
						ptr->setMemoryToSingleValue(0);
					} else if (op == MemoryBlockOp::Shutdown) {
						// do nothing right now
					} else if (op == MemoryBlockOp::Get) {
						auto check = oneCheck(INTEGER, "Argument 0 must be an integer address!");
						if (check) {
							auto addr = EnvDOToLong(env, arg0);
							if (!ptr->legalAddress(addr)) {
								rangeViolation(addr);
								check = false;
							} else {
								CVSetInteger(ret, ptr->getMemoryCellValue(addr));
							}
						}
						return check;
					} else if (op == MemoryBlockOp::Populate) {
						auto check = oneCheck(INTEGER, "First argument must be an INTEGER value to populate all of the memory cells with!");
						if (check) {
							ptr->setMemoryToSingleValue(EnvDOToLong(env, arg0));
						}
						return check;
					} else if (op == MemoryBlockOp::Increment || op == MemoryBlockOp::Decrement) {
						auto check = oneCheck(INTEGER, "First argument must be an address");
						if (check) {
							auto addr = EnvDOToLong(env, arg0);
							if (!ptr->legalAddress(addr)) {
								check = false;
								rangeViolation(addr);
							} else {
								if (op == MemoryBlockOp::Increment) {
									ptr->incrementMemoryCell(addr);
								} else {
									ptr->decrementMemoryCell(addr);
								}
							}
						}
						return check;
					} else if (op == MemoryBlockOp::Set || op == MemoryBlockOp::Swap || op == MemoryBlockOp::Move) {
						auto check = twoCheck(INTEGER, "First argument must be an address", INTEGER, "Second argument must be an address");
						if (check) {
							auto addr0 = EnvDOToLong(env, arg0);
							auto addr1 = EnvDOToLong(env, arg1);
							if (!ptr->legalAddress(addr0)) {
								check = false;
								rangeViolation(addr0);
							} else {
								if (op == MemoryBlockOp::Set) {
									ptr->setMemoryCell(addr0, addr1);
								} else {
									if (!ptr->legalAddress(addr1)) {
										check = false;
										rangeViolation(addr1);
									} else {
										if (op == MemoryBlockOp::Swap) {
											ptr->swapMemoryCells(addr0, addr1);
										} else {
											ptr->copyMemoryCell(addr0, addr1);
										}
									}
								}
							}
						}
						return check;
					} else if (isArithmeticOp(op)) {
						auto check = twoCheck(INTEGER, "First argument must be an address", INTEGER, "Second argument must be an address!");
						if (check) {
							auto addr0 = EnvDOToLong(env, arg0);
							auto addr1 = EnvDOToLong(env, arg1);
							if (!ptr->legalAddress(addr0)) {
								check = false;
								rangeViolation(addr0);
							} else if (!ptr->legalAddress(addr1)) {
								check = false;
								rangeViolation(addr1);
							} else {
								using ArithmeticOperation = std::function<CLIPSInteger(CLIPSInteger, CLIPSInteger)>;
								ArithmeticOperation fn;
								switch(op) {
									case MemoryBlockOp::Combine:
										fn = syn::add<CLIPSInteger>;
										break;
									case MemoryBlockOp::Difference:
										fn = syn::sub<CLIPSInteger>;
										break;
									case MemoryBlockOp::Product:
										fn = syn::mul<CLIPSInteger>;
										break;
									case MemoryBlockOp::Divide:
										fn = syn::div<CLIPSInteger>;
										break;
									case MemoryBlockOp::Remainder:
										fn = syn::rem<CLIPSInteger>;
										break;
									default:
										return callErrorMessage(str, "<- legal but unimplemented arithmetic operation!");
								}
								try {
									auto val0 = ptr->getMemoryCellValue(addr0);
									auto val1 = ptr->getMemoryCellValue(addr1);
									CVSetInteger(ret, fn(val0, val1));
								} catch (syn::Problem p) {
									check = false;
									CVSetBoolean(ret, false);
									std::stringstream s;
									s << "an exception was thrown: " << p.what();
									auto str = s.str();
									errorMessage(env, "CALL", 2, funcErrorPrefix, str);
								}
							}
						} else {
							CVSetBoolean(ret, false);
						}
						return check;
					} else {
						return callErrorMessage(str, "<- legal but unimplemented operation!");
					}
					return true;
				}
			}
		} else {
			return errorMessage(env, "CALL", 1, funcErrorPrefix, "Function call expected an external address as the first argument!");
		}
#undef argCheck
	}
	void CLIPS_breakApartNumber(UDFContext* context, CLIPSValue* ret) {
		CLIPSValue number;
		if (!UDFFirstArgument(context, NUMBER_TYPES, &number)) {
			CVSetBoolean(ret, false);
		} else {
			auto env = UDFContextEnvironment(context);
			auto integer = CVToInteger(&number);
            static constexpr auto iTypeSize = sizeof(decltype(integer));
			byte container[iTypeSize] = { 0 };
			syn::decodeInt64LE(integer, container);
            MultifieldBuilder mf(env, iTypeSize);
			for (int i = 0, j = 1; i < static_cast<int>(iTypeSize); ++i, ++j) {
                mf.setField(j, INTEGER, EnvAddLong(env, container[i]));
			}
            mf.assign(ret);
		}
	}
	void installExtensions(void* theEnv) {
		Environment* env = static_cast<Environment*>(theEnv);

		EnvAddUDF(env, "bitmask->int", "l", CLIPS_translateBitmask, "CLIPS_translateBitmask", 1, 1, "sy", nullptr);
		EnvAddUDF(env, "binary->int", "l", CLIPS_translateBinary, "CLIPS_translateBinary", 1, 1, "sy", nullptr);
		EnvAddUDF(env, "hex->int", "l", CLIPS_translateHex, "CLIPS_translateHex", 1, 1, "sy", nullptr);
		EnvAddUDF(env, "binary-not", "l", CLIPS_binaryNot, "CLIPS_binaryNot", 1, 1, "l", nullptr);
		EnvAddUDF(env, "binary-and", "l", CLIPS_binaryAnd, "CLIPS_binaryAnd", 2, 2, "l;l", nullptr);
		EnvAddUDF(env, "binary-or", "l", CLIPS_binaryOr, "CLIPS_binaryOr", 2, 2, "l;l", nullptr);
		EnvAddUDF(env, "binary-xor", "l", CLIPS_binaryXor, "CLIPS_binaryXor", 2, 2, "l;l", nullptr);
		EnvAddUDF(env, "binary-nand", "l", CLIPS_binaryNand, "CLIPS_binaryNand", 2, 2, "l;l", nullptr);
		EnvAddUDF(env, "expand-bit", "l", CLIPS_expandBit, "CLIPS_expandBit", 1, 1,  nullptr, nullptr);
		EnvAddUDF(env, "decode-bits", "l", CLIPS_decodeBits, "CLIPS_decodeBits", 3, 3, "l;l;l", nullptr);
		EnvAddUDF(env, "encode-bits", "l", CLIPS_encodeBits, "CLIPS_encodeBits", 4, 4, "l;l;l;l", nullptr);
		EnvAddUDF(env, "left-shift", "l", CLIPS_shiftLeft, "CLIPS_shiftLeft", 2, 2, "l;l", nullptr);
		EnvAddUDF(env, "right-shift", "l", CLIPS_shiftRight, "CLIPS_shiftRight", 2, 2, "l;l", nullptr);
		EnvAddUDF(env, "break-apart-number", "m", CLIPS_breakApartNumber, "CLIPS_breakApartNumber", 1, 1, "l", nullptr);

		ManagedMemoryBlock::registerWithEnvironment(theEnv, "memory-space");
	}

    MultifieldBuilder::MultifieldBuilder(void* env, long capacity) : _size(capacity), _rawMultifield(EnvCreateMultifield(env, capacity)) { }

    void MultifieldBuilder::setField(int index, int type, void* value) {
        if (index <= 0) {
            throw syn::Problem("Can't set a value to a field with a negative index!");
        } else if (index > _size) {
            throw syn::Problem("Attempted to set a field which was out of range of the multifield!");
        } else {
            SetMFType(_rawMultifield, index, type);
            SetMFValue(_rawMultifield, index, value);
        }
    }
        void MultifieldBuilder::assign(DataObjectPtr ptr) noexcept {
            ptr->type = MULTIFIELD;
            ptr->begin = 0;
            ptr->end = _size - 1;
            ptr->value = _rawMultifield;
        }

}
