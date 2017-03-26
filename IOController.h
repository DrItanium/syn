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


/**
 * The concept of an IO devices which is mapped into memory
 */
#ifndef IRIS_IO_CONTROLLER_H_
#define IRIS_IO_CONTROLLER_H_
#include <tuple>
#include <functional>
#include <memory>
#include <vector>
#include "Problem.h"
#include "Device.h"
#include "IODevice.h"
#include "WrappedIODevice.h"

namespace syn {

#define IO_CONTROLLER_REFERENCE USER_ENVIRONMENT_DATA
struct IOControllerWrapper {
    CLIPSInteger baseAddress;
    CLIPSInteger endAddress;
    CLIPSInteger size;
};

void getCLIPSIOControllerBaseAddress(UDFContext* context, CLIPSValue* ret);
void getCLIPSIOControllerEndAddress(UDFContext* context, CLIPSValue* ret);
void getCLIPSIOControllerSize(UDFContext* context, CLIPSValue* ret);

template<typename D, typename A = D>
class CLIPSIOController : public AddressableIODevice<D, A> {
	public:
		using Parent = AddressableIODevice<D, A>;
		using Self = CLIPSIOController<D, A>;
		using SharedSelf = std::shared_ptr<Self>;
	public:
		CLIPSIOController(A base, A length, const std::string& bootstrapFileLocation) : Parent(base, length), _bootstrapLocation(bootstrapFileLocation) {
            _env = CreateEnvironment();
        }
		virtual ~CLIPSIOController() {
            if (_env) {
			    DestroyEnvironment(_env);
            }
		}
		virtual void initialize() override {
			auto theEnv = static_cast<Environment*>(_env);
            installExtensions(_env);
			CLIPS_installDefaultIODevices(_env);
			// install custom functions into the environment
			EnvAddUDF(theEnv, "io-controller:get-base-address", "l", getCLIPSIOControllerBaseAddress, "getCLIPSIOControllerBaseAddress", 0, 0, "", nullptr);
			EnvAddUDF(theEnv, "io-controller:get-end-address", "l", getCLIPSIOControllerEndAddress, "getCLIPSIOControllerEndAddress", 0, 0, "", nullptr);
			EnvAddUDF(theEnv, "io-controller:get-address-size", "l", getCLIPSIOControllerSize, "getCLIPSIOControllerSize", 0, 0, "", nullptr);
            // save self into the environment as a form of callback!
            AllocateEnvironmentData(_env, IO_CONTROLLER_REFERENCE, sizeof(IOControllerWrapper), NULL);
            auto wrap = static_cast<IOControllerWrapper*>(GetEnvironmentData(_env, IO_CONTROLLER_REFERENCE));
            wrap->baseAddress = this->baseAddress();
            wrap->endAddress = this->endAddress();
            wrap->size = this->size();
			if (!EnvBatchStar(theEnv, _bootstrapLocation.c_str())) {
				std::stringstream msg;
				msg << "Could not load the bootstrap microcode file " << _bootstrapLocation << "! Make sure the file exists and is accessible!";
				auto str = msg.str();
				throw syn::Problem(str);
			}
		}
		virtual D read(A addr) override {
			std::stringstream args;
			args << addr;
			auto str = args.str();
			CLIPSValue result;
			if (EnvFunctionCall(_env, "read-from-io-address", str.c_str(), &result)) {
				throw syn::Problem("Calling read-from-io-address failed!");
			} else {
				if (CVIsType(&result, INTEGER)) {
					return static_cast<D>(CVToInteger(&result));
				} else {
					throw syn::Problem("Calling read-from-io-address failed!");
				}
			}
		}
		virtual void write(A addr, D value) override {
			std::stringstream args;
			args << addr << " " << value;
			auto str = args.str();
			CLIPSValue result;
			if (EnvFunctionCall(_env, "write-to-io-address", str.c_str(), &result)) {
				throw syn::Problem("Calling write-to-io-address failed!");
			} else if (!CVIsType(&result, INTEGER)) {
				throw syn::Problem("Calling write-to-io-address failed!");
			}
		}
	private:
		std::string _bootstrapLocation;
		void* _env;
};




} // end namespace syn
#endif // end IRIS_IO_CONTROLLER_H_
