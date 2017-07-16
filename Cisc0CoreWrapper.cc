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

#include "Cisc0CoreWrapper.h"
#include "Cisc0Core.h"
#include "Cisc0CoreModel0.h"
#include "Cisc0CoreModel1.h"
#include "ClipsExtensions.h"
#include "CoreWrapper.h"


namespace cisc0 {
    template<typename T = Core>
    class CoreWrapper : public syn::CoreWrapper<T> {
        public:
            using Parent = syn::CoreWrapper<T>;
			enum class Operations {
                __DEFAULT_CORE_OPERATIONS__,
				WriteMemory,
				ReadMemory,
				GetRegister,
				SetRegister,
                __DEFAULT_ERROR_STATE__,
			};
        public:
            using Parent::Parent;
    };
    using DefaultCoreWrapper = CoreWrapper<Core>;

	void installCoreWrapper(void* env) {
        CoreWrapper<CoreModel0>::registerWithEnvironment(env);
        CoreWrapper<CoreModel1>::registerWithEnvironment(env);
	}
    template<typename T>
    constexpr bool isIllegalRegisterIndex(T value) noexcept {
        return value >= ArchitectureConstants::RegisterCount || value < 0;
    }
    template<typename T>
    bool failOnIllegalRegisterIndex(void* env, CLIPSValue* ret, T value) noexcept {
        if (isIllegalRegisterIndex(value)) {
            return DefaultCoreWrapper::callErrorCode3(env, ret, "Illegal register index!");
        }
        return true;
    }
	bool Core::handleOperation(void* env, CLIPSValue* ret) {
		using OpToArgCount = std::tuple<DefaultCoreWrapper::Operations, int>;
		using WrappedOp = DefaultCoreWrapper::Operations;
		static std::map<std::string, OpToArgCount> ops = {
			{ "initialize", std::make_tuple( WrappedOp::Initialize, 0 )},
			{ "shutdown", std::make_tuple(WrappedOp::Shutdown, 0 )},
			{ "run", std::make_tuple(WrappedOp::Run, 0 )},
			{ "cycle", std::make_tuple(WrappedOp::Cycle, 0) },
			{ "write-memory", std::make_tuple(WrappedOp::WriteMemory, 2 ) },
			{ "read-memory", std::make_tuple(WrappedOp::ReadMemory, 1 ) },
			{ "get-register", std::make_tuple(WrappedOp::GetRegister, 1 ) },
			{ "set-register", std::make_tuple(WrappedOp::SetRegister, 2 ) },
		};
		CLIPSValue operation;
        __RETURN_FALSE_ON_FALSE__(DefaultCoreWrapper::tryExtractFunctionName(env, ret, &operation));
		std::string opStr(syn::extractLexeme(env, operation));
		auto result = ops.find(opStr);
        __RETURN_FALSE_ON_FALSE__(DefaultCoreWrapper::isLegalOperation(env, ret, opStr, result, ops.end()));
		WrappedOp fop;
		int argCount;
		std::tie(fop, argCount) = result->second;
        __RETURN_FALSE_ON_FALSE__(DefaultCoreWrapper::checkArgumentCount(env, ret, opStr, argCount));
		auto getRegister = [this, env, ret]() {
			CLIPSValue index;
            __RETURN_FALSE_ON_FALSE__(DefaultCoreWrapper::tryExtractArgument1(env, ret, &index, syn::MayaType::Integer, "Must provide an integer index to retrieve a register value!"));
			auto i = syn::extractLong(env, index);
            __RETURN_FALSE_ON_FALSE__(failOnIllegalRegisterIndex(env, ret, i));
			CVSetInteger(ret, this->registerValue(static_cast<byte>(i)));
			return true;
		};
		auto setRegister = [this, env, ret]() {
			CLIPSValue index;
            __RETURN_FALSE_ON_FALSE__(DefaultCoreWrapper::tryExtractArgument1(env, ret, &index, syn::MayaType::Integer, "Must provide an integer index to assign a register value!"));
			auto ind = syn::extractLong(env, index);
            __RETURN_FALSE_ON_FALSE__(failOnIllegalRegisterIndex(env, ret, ind));
            CLIPSValue value;
            __RETURN_FALSE_ON_FALSE__(DefaultCoreWrapper::tryExtractArgument2(env, ret, &value, syn::MayaType::Integer, "Must provide an integer value to assign to the given register!"));
			registerValue(static_cast<byte>(ind)) = syn::extractLong<RegisterValue>(env, value);
            return syn::setClipsBoolean(ret);
		};
		auto readMemory = [this, env, ret]() {
			CLIPSValue index;
            __RETURN_FALSE_ON_FALSE__(DefaultCoreWrapper::tryExtractArgument1(env, ret, &index, syn::MayaType::Integer, "Must provide an integer index to retrieve a memory value!"));
			CVSetInteger(ret, loadWord(syn::extractLong<RegisterValue>(env, index)));
			return true;
		};
		auto writeMemory = [this, env, ret]() {
			CLIPSValue index;
            __RETURN_FALSE_ON_FALSE__(DefaultCoreWrapper::tryExtractArgument1(env, ret, &index, syn::MayaType::Integer, "Must provide an integer index to assign a register value!"));
			auto ind = syn::extractLong<Address>(env, index);
            CLIPSValue value;
            __RETURN_FALSE_ON_FALSE__(DefaultCoreWrapper::tryExtractArgument2(env, ret, &value, syn::MayaType::Integer, "Must provide an integer value to assign to the given register!"));
			storeWord(ind, syn::extractLong<Word>(env, value));
            return syn::setClipsBoolean(ret);
		};
		CVSetBoolean(ret, true);
		try {
			switch(fop) {
                __DEFAULT_CORE_OPERATIONS_EXEC__(WrappedOp);
				case WrappedOp::GetRegister:
					return getRegister();
				case WrappedOp::SetRegister:
					return setRegister();
				case WrappedOp::ReadMemory:
					return readMemory();
				case WrappedOp::WriteMemory:
					return writeMemory();
				default:
                    return DefaultCoreWrapper::callErrorMessageCode3(env, ret, opStr, " <- legal but unimplemented operation!");
			}
			return true;
		} catch(const syn::Problem& p) {
            return DefaultCoreWrapper::callErrorCode2(env, ret, p.what());
		}
	}
} // end namespace cisc0

namespace syn {
	DefWrapperSymbolicName(cisc0::CoreModel0,  "cisc0-core-model0");
	DefWrapperSymbolicName(cisc0::CoreModel1,  "cisc0-core-model1");
    // this is here for compilation purposes only!
    DefWrapperSymbolicName(cisc0::Core, "cisc0-abstract-core");
    DefExternalAddressWrapperType(cisc0::CoreModel0, cisc0::CoreWrapper<cisc0::CoreModel0>);
    DefExternalAddressWrapperType(cisc0::CoreModel1, cisc0::CoreWrapper<cisc0::CoreModel1>);
} // end namespace syn

