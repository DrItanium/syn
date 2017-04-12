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

#include "WrappedIODevice.h"
namespace syn {
	using CLIPSRandomNumberGeneratorDevice = RandomDevice<CLIPSInteger, CLIPSInteger>;
    using RandomNumberGenerator16bitDevice = RandomDevice<uint16_t, CLIPSInteger>;

	DefWrapperSymbolicName(CLIPSRandomNumberGeneratorDevice, "random-number-generator");
    DefWrapperSymbolicName(RandomNumberGenerator16bitDevice, "random-number-generator:uint16");

	using WrappedCLIPSRandomNumberGeneratorDevice = WrappedGenericRandomDevice<CLIPSInteger>;
    using WrappedRandomNumberGenerator16bitDevice = WrappedGenericRandomDevice<uint16_t>;

    void CLIPS_installDefaultIODevices(void* theEnv) {
		WrappedCLIPSRandomNumberGeneratorDevice::registerWithEnvironment(theEnv);
        WrappedRandomNumberGenerator16bitDevice::registerWithEnvironment(theEnv);
    }
    constexpr int WrappedIODeviceConstants::getArgCount(WrappedIODeviceConstants::Operations op) noexcept {
        using Op = WrappedIODeviceConstants::Operations;
        switch(op) {
            case Op::Type:
            case Op::Initialize:
            case Op::Shutdown:
            case Op::ListCommands:
                return 0;
            case Op::Read:
                return 1;
            case Op::Write:
                return 2;
            default:
                return -1;
        }
    }
    WrappedIODeviceConstants::Operations WrappedIODeviceConstants::nameToOperation(const std::string& title) noexcept {
        static std::map<std::string, Operations> opTranslation = {
            { WrappedIODeviceConstants::operationsName(Operations::Read), Operations::Read },
            { WrappedIODeviceConstants::operationsName(Operations::Write), Operations::Write },
            { WrappedIODeviceConstants::operationsName(Operations::Type),  Operations::Type },
            { WrappedIODeviceConstants::operationsName(Operations::Initialize), Operations::Initialize },
            { WrappedIODeviceConstants::operationsName(Operations::Shutdown), Operations::Shutdown },
            { WrappedIODeviceConstants::operationsName(Operations::ListCommands), Operations::ListCommands },
        };
        auto result = opTranslation.find(title);
        if (result == opTranslation.end()) {
            return defaultErrorState<Operations>;
        } else {
            return result->second;
        }
    }
    std::string WrappedIODeviceConstants::operationsName(WrappedIODeviceConstants::Operations op) noexcept {

        // update this list of names only! everything else is dependent on it!
        static std::map<Operations, std::string> reverseNameLookup = {
            { Operations::Read, "read" },
            { Operations::Write, "write" },
            { Operations::Type, "type" },
            { Operations::Initialize, "initialize" },
            { Operations::Shutdown, "shutdown" },
            { Operations::ListCommands, "list-commands" },
        };
        auto result = reverseNameLookup.find(op);
        if (result == reverseNameLookup.end()) {
            return "";
        } else {
            return result->second;
        }

    }
    bool WrappedIODeviceConstants::getCommandList(void* env, CLIPSValuePtr ret) noexcept {
        static std::map<Operations, std::string> reverseNameLookup = {
            { Operations::Read, WrappedIODeviceConstants::operationsName(Operations::Read) },
            { Operations::Write, WrappedIODeviceConstants::operationsName(Operations::Write) },
            { Operations::Type, WrappedIODeviceConstants::operationsName(Operations::Type) },
            { Operations::Initialize, WrappedIODeviceConstants::operationsName(Operations::Initialize) },
            { Operations::Shutdown, WrappedIODeviceConstants::operationsName(Operations::Shutdown) },
            { Operations::ListCommands, WrappedIODeviceConstants::operationsName(Operations::ListCommands) },
        };
        FixedSizeMultifieldBuilder<static_cast<long>(Operations::Count)> mb(env);
        auto setField = [&mb, env](int index, Operations op) {
            mb.setField(index, SYMBOL, EnvAddSymbol(env, reverseNameLookup[op].c_str()));
        };
        setField(1, Operations::Read);
        setField(2, Operations::Write);
        setField(3, Operations::Type);
        setField(4, Operations::Initialize);
        setField(5, Operations::Shutdown);
        setField(6, Operations::ListCommands);
        mb.assign(ret);
        return true;
    }

    void handleProblem(void* env, syn::Problem& p, CLIPSValue* ret, const std::string& funcErrorPrefix, const char* type, int code) noexcept {
        CVSetBoolean(ret, false);
        std::stringstream s;
        s << "an exception was thrown: " << p.what();
        auto str = s.str();
        errorMessage(env, type, code, funcErrorPrefix, str);
    }

} // end namespace syn
