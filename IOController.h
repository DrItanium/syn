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

namespace syn {

template<typename D, typename A = D>
class IOController : public IODevice<D, A> {
	public:
		using IONode = IODevice<D, A>;
		using SharedIONodePtr = std::shared_ptr<IONode>;
	public:
		IOController(A base, A length) : IODevice<D, A>(base, length) { }
		virtual ~IOController() {
			for (auto & dev : _devices) {
				dev->shutdown();
			}

		}
		virtual D read(A addr) override {
			if (this->respondsTo(addr)) {
				auto caddr = addr - this->baseAddress(); // make sure that we adjust it to be a "flat" model
				auto dev = findResponsiveChild(caddr);
				if (dev) {
					return dev->read(caddr);
				} else {
					throw syn::Problem("Provided address is not mapped to anything");
				}
			} else {
				throw syn::Problem("Provided base address is out of range!");
			}
		}
		virtual void write(A addr, D value) override {
			if (this->respondsTo(addr)) {
				auto caddr = addr - this->baseAddress(); // make sure that we adjust it to be a "flat" model
				auto dev = findResponsiveChild(caddr);
				if (dev) {
					dev->write(caddr, value);
				} else {
					throw syn::Problem("Provided address is not mapped to anything");
				}
			} else {
				throw syn::Problem("Provided base address is out of range!");
			}
		}
		void install(SharedIONodePtr ptr) {
			if (ptr->baseAddress() < this->baseAddress()) {
				throw syn::Problem("Base address of provided device starts out of range of the IO space!");
			} else if (ptr->baseAddress() > this->endAddress()) {
				throw syn::Problem("Base address of provided device starts out after the range of the IO space!");
			} else if (ptr->endAddress() > this->endAddress()) {
				throw syn::Problem("End address of provided device ends beyond the IO space!");
			} else if (ptr->endAddress() < this->baseAddress()) {
				throw syn::Problem("End address of provided device ends before the beginning of IO space!");
			} else {
				if (childrenRespondTo(ptr)) {
					throw syn::Problem("Provided device installation will interfere with already installed device!");
				}
				_devices.emplace_back(ptr);
				ptr->initialize();
			}
		}
	private:
		bool childrenRespondTo(const SharedIONodePtr& ptr) {
			return childrenRespondTo(ptr->baseAddress(), ptr->size());
		}
		bool childrenRespondTo(A addr, A length = 1) {
			return static_cast<bool>(findResponsiveChild(addr, length));
		}
		SharedIONodePtr findResponsiveChild(A addr, A length = 1) {
			for (auto & dev : _devices) {
				if (dev->respondsTo(addr, length)) {
					return dev;
				}
			}
			return std::shared_ptr<IODevice<D, A>>();
		}
	private:
		std::vector<SharedIONodePtr> _devices;
};

template<typename D, typename A = D>
using MemoryController = IOController<D, A>;

template<typename D, typename A = D>
class CLIPSIOController : public IODevice<D, A> {
	public:
		using Parent = IODevice<D, A>;
		using Self = CLIPSIOController<D, A>;
		using SharedSelf = std::shared_ptr<Self>;
	public:
		CLIPSIOController(const std::string& bootstrapFileLocation, A base, A length) : IODevice<D, A>(base, length), _bootstrapLocation(bootstrapFileLocation) {
			_env = CreateEnvironment();
		}
		virtual ~CLIPSIOController() {
			DestroyEnvironment(_env);
		}
		virtual void initialize() override {
			auto theEnv = static_cast<Environment*>(_env);
			// install custom functions into the environment
			EnvAddUDF(theEnv, "io-controller:get-base-address", "l", [this](UDFContext* context, CLIPSValue* ret) { CVSetInteger(ret, this->baseAddress()); }, "CustomLambdaFunction", 0, 0, "", nullptr);
			EnvAddUDF(theEnv, "io-controller:get-end-address", "l", [this](UDFContext* context, CLIPSValue* ret) { CVSetInteger(ret, this->endAddress()); }, "CustomLambdaFunction", 0, 0, "", nullptr);
			EnvAddUDF(theEnv, "io-controller:get-address-size", "l", [this](UDFContext* context, CLIPSValue* ret) { CVSetInteger(ret, this->size()); }, "CustomLambdaFunction", 0, 0, "", nullptr);
			CLIPS_installDefaultIODevices(_env);
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
			if (!EnvFunctionCall(_env, "read-from-io-address", str.c_str(), &result)) {
				throw syn::Problem("Calling read-from-io-address failed!");
			} else {
				return static_cast<D>(CVToInteger(&result));
			}
		}
		virtual void write(A addr, D value) override {
			std::stringstream args;
			args << addr << " " << value;
			auto str = args.str();
			CLIPSValue result;
			if (!EnvFunctionCall(_env, "write-to-io-address", str.c_str(), &result)) {
				throw syn::Problem("Calling write-to-io-address failed!");
			}
		}
	private:
		std::string _bootstrapLocation;
		void* _env;
};


} // end namespace syn
#endif // end IRIS_IO_CONTROLLER_H_
