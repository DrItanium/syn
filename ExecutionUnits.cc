/**
 * @file
 * CLIPS wrapper impls of different execution units
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
#include "ExecutionUnits.h"
#include "CommonExternalAddressWrapper.h"
namespace syn {
	namespace ALU {
	template<typename Word, typename Return = Word, typename Operation = StandardOperations>
	class Unit {
		public:
			using Self = Unit<Word, Return, Operation>;
			using HandleDivideByZero = syn::OnDivideByZero<Return>;
			static constexpr Return defaultHandler() noexcept {
				return static_cast<Return>(0);
			}
		public:
			Unit(HandleDivideByZero handler = defaultHandler) : _handler(handler) { }
			~Unit() { }
			Return performOperation(Operation op, Word a, Word b) {
				return performOperation(op, a, b, _handler);
			}
			Return performOperation(Operation op, Word a, Word b, HandleDivideByZero customOp) {
				return syn::ALU::performOperation(op, a, b, customOp);
			}
			HandleDivideByZero getDivideByZeroHandler() const noexcept { return _handler; }
		private:
			HandleDivideByZero _handler;
	};

	template<typename Word, typename Return = Word, typename Operation = StandardOperations>
	class ALUWrapper : public syn::CommonExternalAddressWrapper<Unit<Word, Return, Operation>> {
		public:
			static_assert(std::is_integral<Word>::value, "Expected the word type to be an integral type!");
			static_assert(std::is_integral<Return>::value, "Expected the return type to an integral type!");
			using WrappedType = Unit<Word, Return, Operation>;
			using Parent = syn::CommonExternalAddressWrapper<WrappedType>;
			using CheckerFunction = syn::ArgCountChecker<int>;
			using OperationToCheckerFunction = syn::OperationToArgCountChecker<Operation>;
		public:
			using Parent::Parent;
			virtual ~ALUWrapper() { }
        	virtual bool handleCallOperation(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation) override {
				static std::map<std::string, OperationToCheckerFunction> ops = {
					{ "add", syn::binaryOperation(Operation::Add) },
					{ "sub", syn::binaryOperation(Operation::Subtract) },
					{ "mul", syn::binaryOperation(Operation::Multiply) },
					{ "shift-left", syn::binaryOperation(Operation::ShiftLeft) },
					{ "shift-right", syn::binaryOperation(Operation::ShiftRight) },
					{ "binary-and", syn::binaryOperation(Operation::BinaryAnd) },
					{ "binary-or", syn::binaryOperation(Operation::BinaryOr) },
					{ "binary-xor", syn::binaryOperation(Operation::BinaryXor) },
					{ "binary-nand", syn::binaryOperation(Operation::BinaryNand) },
					{ "circular-shift-right", syn::binaryOperation(Operation::CircularShiftRight) },
					{ "circular-shift-left", syn::binaryOperation(Operation::CircularShiftLeft) },
					{ "unary-not", syn::unaryOperation(Operation::UnaryNot) },
					{ "div", syn::expectRangeInclusive(Operation::Divide, 2, 3) },
					{ "rem", syn::expectRangeInclusive(Operation::Remainder, 2, 3) },
				};
				auto result = ops.find(operation);
				__RETURN_FALSE_ON_FALSE__(Parent::isLegalOperation(env, ret, operation, result, ops.end()));
				Operation op;
				CheckerFunction fn;
				std::tie(op, fn) = result->second;
				__RETURN_FALSE_ON_FALSE__(Parent::checkArgumentCount(env, ret, operation, fn));
				CLIPSValue arg0, arg1;
				__RETURN_FALSE_ON_FALSE__(Parent::tryExtractArgument1(env, ret, &arg0, syn::MayaType::Integer, "Must provide an integer for the first argument!"));
				__RETURN_FALSE_ON_FALSE__(Parent::tryExtractArgument2(env, ret, &arg1, syn::MayaType::Integer, "Must provide an integer for the second argument!"));
				auto a = syn::extractLong<Word>(env, arg0);
				auto b = syn::extractLong<Word>(env, arg1);
				auto standardAluOperation = [this, env, ret, op, fn, opStr = operation](Word a, Word b) {
					try {
						CVSetInteger(ret, this->get()->performOperation(op, a, b));
					} catch (const syn::Problem& p) {
						return Parent::callErrorMessageCode3((void*)env, ret, opStr, p);
					}
					return true;
				};
				auto handleDivideRemOperation = [this, env, ret, op, fn, opStr = operation, standardAluOperation](Word a, Word b) -> bool {
					auto argCount = Parent::getCorrectArgCount(env);
					if (argCount == 3) {
						// we have to extract the name of the function to call
						// on failure from within clips!
						CLIPSValue arg2;
						__RETURN_FALSE_ON_FALSE__(Parent::tryExtractArgument3(env, ret, &arg2, syn::MayaType::Symbol, "Must provide the name of a clips function to call as the third argument!"));
						std::string fnName(syn::extractLexeme(env, arg2));
						// we now need to setup a new lambda to call this
						// function!
						auto fnToCallOnFailure = [this, env, fnName]() noexcept {
							CLIPSValue tmp;
							if (EnvFunctionCall(env, fnName.c_str(), "", &tmp)) {
								return static_cast<Return>(0);
							} else {
								return syn::extractLong<Return>(env, tmp);
							}
						};
						try {
							CVSetInteger(ret, this->get()->performOperation(op, a, b, fnToCallOnFailure));
							return true;
						} catch (const syn::Problem& p) {
							return Parent::callErrorMessageCode3(env, ret, opStr, p);
						}
					} else {
						return standardAluOperation(a, b);
					}
				};
				switch(op) {
					case Operation::Divide:
					case Operation::Remainder:
						return handleDivideRemOperation(a, b);
					default:
						return standardAluOperation(a, b);
				}
			}
	};
	using ClipsALUWrapper = ALUWrapper<CLIPSInteger, CLIPSInteger>;
	using ClipsALU = Unit<CLIPSInteger, CLIPSInteger>;
	} // end namespace ALU
DefWrapperSymbolicName(ALU::ClipsALUWrapper::WrappedType,  "alu");
DefExternalAddressWrapperType(ALU::ClipsALUWrapper::WrappedType, ALU::ClipsALUWrapper);

void InstallExecutionUnits(void* theEnv) noexcept {
	ALU::ClipsALUWrapper::registerWithEnvironment(theEnv);
}

} // end namespace syn
