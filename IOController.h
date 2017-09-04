/**
 * @file
 * The concept of an IO device which is mapped into memory
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

#ifndef IRIS_IO_CONTROLLER_H_
#define IRIS_IO_CONTROLLER_H_
#include <map>
#include <tuple>
#include <functional>
#include <memory>
#include <vector>
#include "Problem.h"
#include "Device.h"
#include "IODevice.h"
#include "WrappedIODevice.h"
#include "MemoryBlock.h"

namespace syn {

class CLIPSIOController : public AddressableIODevice<CLIPSInteger, CLIPSInteger> {
	public:
		using Parent = AddressableIODevice<CLIPSInteger, CLIPSInteger>;
		using Self = CLIPSIOController;
		using SharedSelf = std::shared_ptr<Self>;
	public:
		CLIPSIOController(CLIPSInteger base = 0, CLIPSInteger length = INT64_MAX);
		virtual ~CLIPSIOController();
		virtual void initialize() override;
		virtual CLIPSInteger read(CLIPSInteger addr) override;
		virtual void write(CLIPSInteger addr, CLIPSInteger value) override;
        void* getRawEnvironment() const noexcept { return _env; }
	private:
		/**
		 * Keep track of IOControllers as they are built
		 */
		class Registrar 
		{
			public:
				Registrar() = default;
				~Registrar() = default;
				Registrar(const Registrar&) = delete;
				Registrar(Registrar&&) = delete;
				Registrar& operator=(const Registrar&) = delete;
				Registrar& operator=(Registrar&&) = delete;
				void add(CLIPSIOController* controller);
				void remove(CLIPSIOController* controller);
				CLIPSIOController& get(void* env);
			private:
				std::map<void*, CLIPSIOController*> _backing;
		};
	private:
		void* _env;
	private:
		static Registrar _registrar;
		static void addIOController(CLIPSIOController* c) { _registrar.add(c); }
		static void removeIOController(CLIPSIOController* c) { _registrar.remove(c); }
	public:
		static CLIPSIOController& fromRaw(void* env) { return _registrar.get(env); }
};

} // end namespace syn
#endif // end IRIS_IO_CONTROLLER_H_
