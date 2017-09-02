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
	DefWrapperSymbolicName(CLIPSRandomNumberGeneratorDevice, "random-number-generator");
	using WrappedCLIPSRandomNumberGeneratorDevice = WrappedGenericRandomDevice<CLIPSInteger>;
    DefExternalAddressWrapperType(CLIPSRandomNumberGeneratorDevice, WrappedCLIPSRandomNumberGeneratorDevice);

    void CLIPS_installDefaultIODevices(void* theEnv) {
		WrappedCLIPSRandomNumberGeneratorDevice::registerWithEnvironment(theEnv);
    }
    namespace WrappedIODeviceConstants {
        constexpr int getArgCount(Operations op) noexcept {
            using Op = Operations;
            switch(op) {
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

        Operations nameToOperation(const std::string& title) noexcept {
            static std::map<std::string, Operations> opTranslation = {
                { operationsName(Operations::Read), Operations::Read },
                { operationsName(Operations::Write), Operations::Write },
                { operationsName(Operations::Initialize), Operations::Initialize },
                { operationsName(Operations::Shutdown), Operations::Shutdown },
                { operationsName(Operations::ListCommands), Operations::ListCommands },
            };
            auto result = opTranslation.find(title);
            if (result == opTranslation.end()) {
                return defaultErrorState<Operations>;
            }
            return result->second;
        }
        const std::string& operationsName(Operations op) noexcept {

            // update this list of names only! everything else is dependent on it!
            static std::map<Operations, std::string> reverseNameLookup = {
                { Operations::Read, "read" },
                { Operations::Write, "write" },
                { Operations::Initialize, "initialize" },
                { Operations::Shutdown, "shutdown" },
                { Operations::ListCommands, "list-commands" },
            };
			static std::string empty;
            auto result = reverseNameLookup.find(op);
            if (result == reverseNameLookup.end()) {
                return empty;
            } else {
                return result->second;
            }
        }

        bool getCommandList(void* env, CLIPSValuePtr ret) noexcept {
			createMultifield(env, ret, 
					symbol(env, operationsName(Operations::Read)),
					symbol(env, operationsName(Operations::Write)),
					symbol(env, operationsName(Operations::Initialize)),
					symbol(env, operationsName(Operations::Shutdown)),
					symbol(env, operationsName(Operations::ListCommands)));
            return true;
        }
    } // end namespace WrappedIODeviceConstants

    void handleProblem(void* env, const syn::Problem& p, CLIPSValue* ret, const std::string& funcErrorPrefix, const char* type, int code) noexcept {
        CVSetBoolean(ret, false);
        std::stringstream s;
        s << "an exception was thrown: " << p.what();
        auto str = s.str();
        errorMessage(env, type, code, funcErrorPrefix, str);
    }

} // end namespace syn
