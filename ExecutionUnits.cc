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
	template<typename W, typename R, typename O>
	class BasicCLIPSExecutionUnit {
		public:
			using Word = W;
			using Return = R;
			using Operation = O;
			using Self = BasicCLIPSExecutionUnit<W, R, O>;
			using OperationToArgCountChecker = syn::OperationToArgCountChecker<Operation, int>;
			using ArgCountCheckMap = std::map<std::string, OperationToArgCountChecker>;
			using UndefinedOperationHandler = std::function<Return()>;
			static constexpr Return defaultUndefinedHandlerOperation() noexcept {
				return static_cast<Return>(0);
			}
		public:
			BasicCLIPSExecutionUnit() : _undefinedOperationHandler(defaultUndefinedHandlerOperation), _undefinedOperationHandlerName("C++defaultUndefinedHandlerOperation") { }
			virtual ~BasicCLIPSExecutionUnit() { }
			virtual const ArgCountCheckMap& getMap() const = 0;
			virtual bool isBinaryOperation(Operation op) const = 0;
			virtual typename ArgCountCheckMap::const_iterator findOperation(const std::string& operation) const { return getMap().find(operation); }
			virtual typename ArgCountCheckMap::const_iterator end() const { return getMap().end(); }
			void setUndefinedOperationHandlerName(const std::string& function) noexcept { _undefinedOperationHandlerName = function; }
			void setUndefinedOperationHandler(UndefinedOperationHandler handler) noexcept { _undefinedOperationHandler = handler; }
			UndefinedOperationHandler getUndefinedOperationHandler() const noexcept { return _undefinedOperationHandler; }
			const std::string& getUndefinedOperationHandlerName() const noexcept { return _undefinedOperationHandlerName; }
		protected:
			UndefinedOperationHandler _undefinedOperationHandler;
			std::string _undefinedOperationHandlerName;

	};
	template<typename T>
	class BasicCLIPSExecutionUnitWrapper : public syn::CommonExternalAddressWrapper<T> {
		public:
			enum class UnitOperations {
				SetUndefinedOperationHandler,
				GetUndefinedOperationHandler,
				Count,
			};
		public:
			using WrappedType = T;
			using Operation = typename WrappedType::Operation;
			using Word = typename WrappedType::Word;
			using Return = typename WrappedType::Return;
			using Self = BasicCLIPSExecutionUnitWrapper<WrappedType>;
			using Parent = syn::CommonExternalAddressWrapper<T>;
			using CheckerFunction = syn::ArgCountChecker<int>;
			using OperationToArgCountChecker = syn::OperationToArgCountChecker<UnitOperations, int>;
			using ArgCountCheckMap = std::map<std::string, OperationToArgCountChecker>;
		public:
			using Parent::Parent;
			virtual ~BasicCLIPSExecutionUnitWrapper() { }
			virtual bool handleCallOperation(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation) override {
				static ArgCountCheckMap ops = {
					{ "get-undefined-operation-handler", syn::expectExactly(UnitOperations::GetUndefinedOperationHandler, 0) },
					{ "set-undefined-operation-handler", syn::unaryOperation(UnitOperations::SetUndefinedOperationHandler) },
				};
				auto basicOps = ops.find(operation);
				if (basicOps != ops.end()) {
					UnitOperations op;
					CheckerFunction fn;
					std::tie(op, fn) = basicOps->second;
					__RETURN_FALSE_ON_FALSE__(Parent::checkArgumentCount(env, ret, operation, fn));
					return handleUnitOperations(env, value, ret, operation, op);
				}
				auto* target = this->get();
				auto result = target->findOperation(operation);
				__RETURN_FALSE_ON_FALSE__(Parent::isLegalOperation(env, ret, operation, result, target->end()));
				Operation op;
				CheckerFunction fn;
				std::tie(op, fn) = result->second;
				__RETURN_FALSE_ON_FALSE__(Parent::checkArgumentCount(env, ret, operation, fn));
				return handleArgumentsAndExecute(env, value, ret, operation, op);
			}
			bool handleUnitOperations(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation, UnitOperations op) {
				auto setUndefinedOperationHandler = [this, env, value, ret, operation, op]() {
					CLIPSValue arg0;
					__RETURN_FALSE_ON_FALSE__(Parent::tryExtractArgument1(env, ret, &arg0, syn::MayaType::Symbol, "Must provide a function name for the first argument!"));
					std::string fnName(syn::extractLexeme(env, arg0));
					// we now need to setup a new lambda to call this
					// function!
					auto fnToCallOnFailure = [env, fnName]() noexcept {
						CLIPSValue tmp;
						if (EnvFunctionCall(env, fnName.c_str(), "", &tmp)) {
							return static_cast<Return>(0);
						} else {
							return syn::extractLong<Return>(env, tmp);
						}
					};
					this->get()->setUndefinedOperationHandlerName(fnName);
					this->get()->setUndefinedOperationHandler(fnToCallOnFailure);
					CVSetBoolean(ret, true);
					return true;
				};
				switch(op) {
					case UnitOperations::SetUndefinedOperationHandler:
						return setUndefinedOperationHandler();
					case UnitOperations::GetUndefinedOperationHandler:
						CVSetSymbol(ret, this->get()->getUndefinedOperationHandlerName().c_str());
						return true;
					default:
						throw syn::Problem("Found an unimplemented operation!");
				}
			}
			virtual bool handleArgumentsAndExecute(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation, Operation op) = 0;
	};
	template<typename T>
	class BasicCLIPSBinaryExecutionUnitWrapper : public BasicCLIPSExecutionUnitWrapper<T> {
		public:
			using WrappedType = T;
			using Parent = BasicCLIPSExecutionUnitWrapper<WrappedType>;
			using Self = BasicCLIPSBinaryExecutionUnitWrapper<WrappedType>;
			using Operation = typename Parent::Operation;
			using CheckerFunction = typename Parent::CheckerFunction;
			using Word = typename Parent::Word;
		public:
			using Parent::Parent;
			virtual ~BasicCLIPSBinaryExecutionUnitWrapper() { }
			virtual bool extractArg1(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept = 0;
			virtual bool extractArg2(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept = 0;
			virtual Word unpackArg1(void* env, CLIPSValuePtr storage) noexcept = 0;
			virtual Word unpackArg2(void* env, CLIPSValuePtr storage) noexcept = 0;
			virtual bool execute(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation, Operation op, Word a, Word b) = 0;
			virtual bool handleArgumentsAndExecute(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation, Operation op) override {
				CLIPSValue arg0;
				__RETURN_FALSE_ON_FALSE__(extractArg1(env, ret, &arg0));
				Word a = unpackArg1(env, &arg0);
				Word b = 0;
				if (this->get()->isBinaryOperation(op)) {
					CLIPSValue arg1;
					__RETURN_FALSE_ON_FALSE__(extractArg2(env, ret, &arg1));
					b = unpackArg2(env, &arg1);
				}
				return execute(env, value, ret, operation, op, a, b);
			}
	};
namespace FPU {
		class CLIPSUnit : public BasicCLIPSExecutionUnit<CLIPSFloat, CLIPSFloat, StandardOperations> {
			public:
				using Word = CLIPSFloat;
				using Return = CLIPSFloat;
				using Operation = StandardOperations;
				using Self = CLIPSUnit;
				static constexpr bool operationIsBinary(Operation op) noexcept {
					return op != Operation::SquareRoot;
				}
			public:
				CLIPSUnit() { }
				virtual ~CLIPSUnit() { }
				Return performOperation(Operation op, Word a, Word b);
				virtual const ArgCountCheckMap& getMap() const override;
				virtual bool isBinaryOperation(Operation op) const override { return operationIsBinary(op); }
		};
		class CLIPSUnitWrapper : public BasicCLIPSBinaryExecutionUnitWrapper<CLIPSUnit> {
			public:
				using WrappedType = CLIPSUnit;
				using Self = CLIPSUnitWrapper;
				using Parent = BasicCLIPSBinaryExecutionUnitWrapper<WrappedType>;
				using Word = typename Parent::Word;
				using Return = typename Parent::Return;
				using Operation = typename Parent::Operation;
			public:
				using Parent::Parent;
				virtual ~CLIPSUnitWrapper() { }
				virtual bool extractArg1(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept override;
				virtual bool extractArg2(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept override;
				virtual Word unpackArg1(void* env, CLIPSValuePtr storage) noexcept override;
				virtual Word unpackArg2(void* env, CLIPSValuePtr storage) noexcept override;
				virtual bool execute(void* env, CLIPSValuePtr storage, CLIPSValuePtr ret, const std::string& operation, Operation op, Word a, Word b) override;
		};
	const CLIPSUnit::ArgCountCheckMap& CLIPSUnit::getMap() const {
		static CLIPSUnit::ArgCountCheckMap ops = {
			{ "add", syn::binaryOperation(Operation::Add) },
			{ "sub", syn::binaryOperation(Operation::Subtract) },
			{ "mul", syn::binaryOperation(Operation::Multiply) },
			{ "div", syn::binaryOperation(Operation::Divide) },
			{ "sqrt", syn::unaryOperation(Operation::SquareRoot) },
		};
		return ops;
	}
	CLIPSUnit::Return CLIPSUnit::performOperation(CLIPSUnit::Operation op, CLIPSUnit::Word a, CLIPSUnit::Word b) {
		return FPU::performOperation<CLIPSUnit::Word, CLIPSUnit::Return, CLIPSUnit::Operation>(op, a, b);
	}
	bool CLIPSUnitWrapper::extractArg1(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept { 
		return Parent::tryExtractArgument1(env, ret, storage, syn::MayaType::Float, "Must provide a floating point number for the first argument!");
	}
	bool CLIPSUnitWrapper::extractArg2(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept { 
		return Parent::tryExtractArgument2(env, ret, storage, syn::MayaType::Float, "Must provide a floating point number for the second argument!");
	}
	bool CLIPSUnitWrapper::execute(void* env, CLIPSValuePtr storage, CLIPSValuePtr ret, const std::string& operation, Operation op, Word a, Word b) { 
		try {
			auto result = this->get()->performOperation(op, a, b);
			CVSetFloat(ret, result);
			return true;
		} catch (const syn::UndefinedOperationProblem& p) {
			auto result = this->get()->getUndefinedOperationHandler()();
			CVSetFloat(ret, result);
			return true;
		} catch(const syn::Problem& p) {
			return Parent::callErrorMessageCode3(env, ret, operation, p);
		}
	}
	CLIPSUnitWrapper::Word unpackArg(void* env, CLIPSValuePtr storage) noexcept {
		return syn::extractFloat<CLIPSUnitWrapper::Word>(env, storage);
	}
	CLIPSUnitWrapper::Word CLIPSUnitWrapper::unpackArg1(void* env, CLIPSValuePtr storage) noexcept {
		return unpackArg(env, storage);
	}

	CLIPSUnitWrapper::Word CLIPSUnitWrapper::unpackArg2(void* env, CLIPSValuePtr storage) noexcept {
		return unpackArg(env, storage);
	}
} // end namespace FPU
namespace ALU {
	class CLIPSUnit : public BasicCLIPSExecutionUnit<CLIPSInteger, CLIPSInteger, StandardOperations> {
		public:
			using Word = CLIPSInteger;
			using Return = CLIPSInteger;
			using Operation = StandardOperations;
			using Self = CLIPSUnit;
			using HandleDivideByZero = syn::OnDivideByZero<Return>;
			static constexpr Return defaultDivideByZeroHandler() noexcept {
				return static_cast<Return>(0);
			}
			static constexpr bool operationIsBinary(Operation op) noexcept {
				return op != Operation::UnaryNot;
			}
		public:
			CLIPSUnit(HandleDivideByZero handler = defaultDivideByZeroHandler) : _handler(handler) { }
			~CLIPSUnit() { }
			virtual const ArgCountCheckMap& getMap() const override;
			virtual bool isBinaryOperation(Operation op) const override { return operationIsBinary(op); }
			Return performOperation(Operation op, Word a, Word b);
			Return performOperation(Operation op, Word a, Word b, HandleDivideByZero customOp);
			void setDivideByZeroHandler(HandleDivideByZero handler) noexcept { _handler = handler; }
			HandleDivideByZero getDivideByZeroHandler() const noexcept { return _handler; }
		private:
			HandleDivideByZero _handler;
	};
	class CLIPSUnitWrapper : public syn::BasicCLIPSBinaryExecutionUnitWrapper<CLIPSUnit> {
		public:
			using WrappedType = CLIPSUnit;
			using Operation = WrappedType::Operation;
			using Word = WrappedType::Word;
			using Return = WrappedType::Return;
			using Parent = syn::BasicCLIPSBinaryExecutionUnitWrapper<WrappedType>;
		public:
			using Parent::Parent;
			virtual ~CLIPSUnitWrapper() { }
			virtual bool extractArg1(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept override;
			virtual bool extractArg2(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept override;
			virtual Word unpackArg1(void* env, CLIPSValuePtr storage) noexcept override;
			virtual Word unpackArg2(void* env, CLIPSValuePtr storage) noexcept override;
			virtual bool execute(void* env, CLIPSValuePtr storage, CLIPSValuePtr ret, const std::string& operation, Operation op, Word a, Word b) override;
	};
	const CLIPSUnit::ArgCountCheckMap& CLIPSUnit::getMap() const {
		static ArgCountCheckMap ops = {
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
		return ops;
	}
	CLIPSUnit::Return CLIPSUnit::performOperation(CLIPSUnit::Operation op, CLIPSUnit::Word a, CLIPSUnit::Word b) {
		return performOperation(op, a, b, _handler);
	}
	CLIPSUnit::Return CLIPSUnit::performOperation(CLIPSUnit::Operation op, CLIPSUnit::Word a, CLIPSUnit::Word b, CLIPSUnit::HandleDivideByZero customOp) {
		return syn::ALU::performOperation(op, a, b, customOp);
	}
	bool CLIPSUnitWrapper::extractArg1(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept { 
		return Parent::tryExtractArgument1(env, ret, storage, syn::MayaType::Integer, "Must provide an integer for the first argument!");
	}
	bool CLIPSUnitWrapper::extractArg2(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept { 
		return Parent::tryExtractArgument2(env, ret, storage, syn::MayaType::Integer, "Must provide an integer for the second argument!");
	}
	bool CLIPSUnitWrapper::execute(void* env, CLIPSValuePtr storage, CLIPSValuePtr ret, const std::string& operation, Operation op, Word a, Word b) {
		auto standardAluOperation = [this, env, ret, op, opStr = operation](Word a, Word b) {
			try {
				CVSetInteger(ret, this->get()->performOperation(op, a, b));
				return true;
			} catch (const syn::UndefinedOperationProblem& p) {
				CVSetInteger(ret, this->get()->getUndefinedOperationHandler()());
				return true;
			} catch (const syn::Problem& p) {
				return Parent::callErrorMessageCode3((void*)env, ret, opStr, p);
			}
		};
		auto handleDivideRemOperation = [this, env, ret, op, opStr = operation, standardAluOperation](Word a, Word b) -> bool {
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
	CLIPSUnitWrapper::Word unpackArg(void* env, CLIPSValuePtr storage) noexcept {
		return syn::extractLong<CLIPSUnitWrapper::Word>(env, storage);
	}
	CLIPSUnitWrapper::Word CLIPSUnitWrapper::unpackArg1(void* env, CLIPSValuePtr storage) noexcept {
		return unpackArg(env, storage);
	}

	CLIPSUnitWrapper::Word CLIPSUnitWrapper::unpackArg2(void* env, CLIPSValuePtr storage) noexcept {
		return unpackArg(env, storage);
	}
} // end namespace ALU
namespace Comparator {
	class CLIPSUnit : public BasicCLIPSExecutionUnit<CLIPSInteger, CLIPSInteger, StandardOperations> {
		public:
			using Parent = BasicCLIPSExecutionUnit<CLIPSInteger, CLIPSInteger, StandardOperations>;
			using Word = CLIPSInteger;
			using Return = CLIPSInteger;
			using Operation = StandardOperations;
			using Self = CLIPSUnit;
			static constexpr bool operationIsBinary(Operation op) noexcept {
				return op != Operation::UnaryNot;
			}
		public:
			CLIPSUnit() { }
			virtual ~CLIPSUnit() { }
			virtual const ArgCountCheckMap& getMap() const override;
			virtual bool isBinaryOperation(Operation op) const override { return operationIsBinary(op); }
			Return performOperation(Operation op, Word a, Word b);


	};
	CLIPSUnit::Return CLIPSUnit::performOperation(Operation op, Word a, Word b) {
		return Comparator::performOperation<Word, Return, Operation>(op, a, b);
	}
	const CLIPSUnit::ArgCountCheckMap& CLIPSUnit::getMap() const {
		static ArgCountCheckMap ops = {
			{ "eq", syn::binaryOperation(Operation::Eq) },
			{ "neq", syn::binaryOperation(Operation::Neq) },
			{ "less-than", syn::binaryOperation(Operation::LessThan) },
			{ "greater-than", syn::binaryOperation(Operation::GreaterThan) },
			{ "less-than-or-equal-to", syn::binaryOperation(Operation::LessThanOrEqualTo) },
			{ "greater-than-or-equal-to", syn::binaryOperation(Operation::GreaterThanOrEqualTo) },
			{ "binary-and", syn::binaryOperation(Operation::BinaryAnd) },
			{ "binary-or", syn::binaryOperation(Operation::BinaryOr) },
			{ "binary-nand", syn::binaryOperation(Operation::BinaryNand) },
			{ "binary-nor", syn::binaryOperation(Operation::BinaryNor) },
			{ "unary-not", syn::unaryOperation(Operation::UnaryNot) },
			{ "shift-left", syn::binaryOperation(Operation::ShiftLeft) },
			{ "circular-shift-left", syn::binaryOperation(Operation::CircularShiftLeft) },
			{ "shift-right", syn::binaryOperation(Operation::ShiftRight) },
			{ "circular-shift-right", syn::binaryOperation(Operation::CircularShiftRight) },
		};
		return ops;
	}

	class BooleanCLIPSUnit : public BasicCLIPSExecutionUnit<bool, bool, BooleanOperations> {
		public:
			using Self = BooleanCLIPSUnit;
			using Parent = BasicCLIPSExecutionUnit<bool, bool, BooleanOperations>;
			using Word = bool;
			using Return = bool;
			using Operation = BooleanOperations;
			static constexpr bool operationIsBinary(Operation op) noexcept {
				return op != Operation::UnaryNot;
			}
		public:
			BooleanCLIPSUnit() { }
			virtual ~BooleanCLIPSUnit() { }
			virtual const ArgCountCheckMap& getMap() const override;
			virtual bool isBinaryOperation(Operation op) const override { return operationIsBinary(op); }
			Return performOperation(Operation op, Word a, Word b);
	};

	BooleanCLIPSUnit::Return BooleanCLIPSUnit::performOperation(Operation op, Word a, Word b) {
		return Comparator::performOperation<Word, Return, Operation>(op, a, b);
	}

	const BooleanCLIPSUnit::ArgCountCheckMap& BooleanCLIPSUnit::getMap() const {
		static ArgCountCheckMap ops = {
			{ "eq", syn::binaryOperation(Operation::Eq) },
			{ "neq", syn::binaryOperation(Operation::Neq) },
			{ "binary-and", syn::binaryOperation(Operation::BinaryAnd) },
			{ "binary-or", syn::binaryOperation(Operation::BinaryOr) },
			{ "binary-nand", syn::binaryOperation(Operation::BinaryNand) },
			{ "binary-nor", syn::binaryOperation(Operation::BinaryNor) },
			{ "unary-not", syn::unaryOperation(Operation::UnaryNot) },
		};
		return ops;
	}

	class CLIPSUnitWrapper : public syn::BasicCLIPSBinaryExecutionUnitWrapper<CLIPSUnit> {
		public:
			using WrappedType = CLIPSUnit;
			using Operation = WrappedType::Operation;
			using Word = WrappedType::Word;
			using Return = WrappedType::Return;
			using Parent = syn::BasicCLIPSBinaryExecutionUnitWrapper<WrappedType>;
		public:
			using Parent::Parent;
			virtual ~CLIPSUnitWrapper() { }
			virtual bool extractArg1(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept override;
			virtual bool extractArg2(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept override;
			virtual Word unpackArg1(void* env, CLIPSValuePtr storage) noexcept override;
			virtual Word unpackArg2(void* env, CLIPSValuePtr storage) noexcept override;
			virtual bool execute(void* env, CLIPSValuePtr storage, CLIPSValuePtr ret, const std::string& operation, Operation op, Word a, Word b) override;
	};

	bool CLIPSUnitWrapper::extractArg1(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept { 
		return Parent::tryExtractArgument1(env, ret, storage, syn::MayaType::Integer, "Must provide an integer for the first argument!");
	}
	bool CLIPSUnitWrapper::extractArg2(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept { 
		return Parent::tryExtractArgument2(env, ret, storage, syn::MayaType::Integer, "Must provide an integer number for the second argument!");
	}
	bool CLIPSUnitWrapper::execute(void* env, CLIPSValuePtr storage, CLIPSValuePtr ret, const std::string& operation, Operation op, Word a, Word b) { 
		try {
			auto result = this->get()->performOperation(op, a, b);
			CVSetInteger(ret, result);
			return true;
		} catch (const syn::UndefinedOperationProblem& p) {
			auto result = this->get()->getUndefinedOperationHandler()();
			CVSetInteger(ret, result);
			return true;
		} catch(const syn::Problem& p) {
			return Parent::callErrorMessageCode3(env, ret, operation, p);
		}
	}
	CLIPSUnitWrapper::Word unpackArg(void* env, CLIPSValuePtr storage) noexcept {
		return syn::extractLong<CLIPSUnitWrapper::Word>(env, storage);
	}
	CLIPSUnitWrapper::Word CLIPSUnitWrapper::unpackArg1(void* env, CLIPSValuePtr storage) noexcept {
		return unpackArg(env, storage);
	}

	CLIPSUnitWrapper::Word CLIPSUnitWrapper::unpackArg2(void* env, CLIPSValuePtr storage) noexcept {
		return unpackArg(env, storage);
	}
	class BooleanCLIPSUnitWrapper : public syn::BasicCLIPSBinaryExecutionUnitWrapper<BooleanCLIPSUnit> {
		public:
			using WrappedType = BooleanCLIPSUnit;
			using Operation = WrappedType::Operation;
			using Word = WrappedType::Word;
			using Return = WrappedType::Return;
			using Parent = syn::BasicCLIPSBinaryExecutionUnitWrapper<WrappedType>;
		public:
			using Parent::Parent;
			virtual ~BooleanCLIPSUnitWrapper() { }
			virtual bool extractArg1(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept override;
			virtual bool extractArg2(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept override;
			virtual Word unpackArg1(void* env, CLIPSValuePtr storage) noexcept override;
			virtual Word unpackArg2(void* env, CLIPSValuePtr storage) noexcept override;
			virtual bool execute(void* env, CLIPSValuePtr storage, CLIPSValuePtr ret, const std::string& operation, Operation op, Word a, Word b) override;
	};

	bool BooleanCLIPSUnitWrapper::extractArg1(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept { 
		return Parent::tryExtractArgument1(env, ret, storage, syn::MayaType::Symbol, "Must provide a true or false symbol for the first argument!");
	}

	bool BooleanCLIPSUnitWrapper::extractArg2(void* env, CLIPSValuePtr ret, CLIPSValuePtr storage) noexcept { 
		return Parent::tryExtractArgument2(env, ret, storage, syn::MayaType::Symbol, "Must provide a true or false symbol for the second argument!");
	}
	bool BooleanCLIPSUnitWrapper::execute(void* env, CLIPSValuePtr storage, CLIPSValuePtr ret, const std::string& operation, Operation op, Word a, Word b) { 
		try {
			auto result = this->get()->performOperation(op, a, b);
			CVSetBoolean(ret, result);
			return true;
		} catch (const syn::UndefinedOperationProblem& p) {
			auto result = this->get()->getUndefinedOperationHandler()();
			CVSetBoolean(ret, result);
			return true;
		} catch(const syn::Problem& p) {
			return Parent::callErrorMessageCode3(env, ret, operation, p);
		}
	}
	BooleanCLIPSUnitWrapper::Word unpackBoolean(void* env, CLIPSValuePtr storage) noexcept {
		return !CVIsFalseSymbol(storage);
	}
	BooleanCLIPSUnitWrapper::Word BooleanCLIPSUnitWrapper::unpackArg1(void* env, CLIPSValuePtr storage) noexcept {
		return unpackBoolean(env, storage);
	}

	BooleanCLIPSUnitWrapper::Word BooleanCLIPSUnitWrapper::unpackArg2(void* env, CLIPSValuePtr storage) noexcept {
		return unpackBoolean(env, storage);
	}

} // end namespace Comparator


DefWrapperSymbolicName(Comparator::CLIPSUnitWrapper::WrappedType, "comparator");
DefWrapperSymbolicName(Comparator::BooleanCLIPSUnitWrapper::WrappedType, "boolean-comparator");
DefWrapperSymbolicName(ALU::CLIPSUnitWrapper::WrappedType,  "alu");
DefWrapperSymbolicName(FPU::CLIPSUnit, "fpu");
DefExternalAddressWrapperType(ALU::CLIPSUnitWrapper::WrappedType, ALU::CLIPSUnitWrapper);
DefExternalAddressWrapperType(FPU::CLIPSUnitWrapper::WrappedType, FPU::CLIPSUnitWrapper);
DefExternalAddressWrapperType(Comparator::CLIPSUnitWrapper::WrappedType, Comparator::CLIPSUnitWrapper);
DefExternalAddressWrapperType(Comparator::BooleanCLIPSUnitWrapper::WrappedType, Comparator::BooleanCLIPSUnitWrapper);

void installExecutionUnits(void* theEnv) noexcept {
	ALU::CLIPSUnitWrapper::registerWithEnvironment(theEnv);
	FPU::CLIPSUnitWrapper::registerWithEnvironment(theEnv);
	Comparator::CLIPSUnitWrapper::registerWithEnvironment(theEnv);
	Comparator::BooleanCLIPSUnitWrapper::registerWithEnvironment(theEnv);
}


} // end namespace syn
