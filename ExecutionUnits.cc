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
	template<typename O>
	class RequiresArgCountChecking {
		public:
			using Operation = O;
			using Self = RequiresArgCountChecking<Operation>;
			using OperationToArgCountChecker = syn::OperationToArgCountChecker<Operation, int>;
			using ArgCountCheckMap = std::map<std::string, OperationToArgCountChecker>;
		public:
			virtual ~RequiresArgCountChecking() { }
			virtual const ArgCountCheckMap& getMap() const = 0;
			virtual typename ArgCountCheckMap::const_iterator findOperation(const std::string& operation) const { return getMap().find(operation); }
			virtual typename ArgCountCheckMap::const_iterator end() const { return getMap().end(); }


	};
	template<typename W, typename R, typename O>
	class BasicCLIPSExecutionUnit : public RequiresArgCountChecking<O> {
		public:
			using Parent = RequiresArgCountChecking<O>;
			using Word = W;
			using Return = R;
			using Operation = typename Parent::Operation;
			using Self = BasicCLIPSExecutionUnit<W, R, O>;
			using UndefinedOperationHandler = std::function<Return()>;
			static constexpr Return defaultUndefinedHandlerOperation() noexcept {
				return static_cast<Return>(0);
			}
		public:
			BasicCLIPSExecutionUnit() : _undefinedOperationHandler(defaultUndefinedHandlerOperation), _undefinedOperationHandlerName("C++defaultUndefinedHandlerOperation") { }
			virtual ~BasicCLIPSExecutionUnit() { }
			virtual bool isBinaryOperation(Operation op) const = 0;
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
							return syn::extractCLIPSInteger<Return>(env, tmp);
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
		return syn::extractCLIPSInteger<CLIPSUnitWrapper::Word>(env, storage);
	}
	CLIPSUnitWrapper::Word CLIPSUnitWrapper::unpackArg1(void* env, CLIPSValuePtr storage) noexcept {
		return unpackArg(env, storage);
	}

	CLIPSUnitWrapper::Word CLIPSUnitWrapper::unpackArg2(void* env, CLIPSValuePtr storage) noexcept {
		return unpackArg(env, storage);
	}
} // end namespace Comparator

enum class RegisterOperations {
	Get,
	Set,
	Increment,
	Decrement,
	Decode,
	Encode,
	SetMask,
	GetMask,
	Count,
};



DefWrapperSymbolicName(Comparator::CLIPSUnitWrapper::WrappedType, "comparator");
DefExternalAddressWrapperType(Comparator::CLIPSUnitWrapper::WrappedType, Comparator::CLIPSUnitWrapper);

void installExecutionUnits(void* theEnv) noexcept {
	Comparator::CLIPSUnitWrapper::registerWithEnvironment(theEnv);
}


} // end namespace syn
