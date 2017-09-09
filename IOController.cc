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
#include "IOController.h"
#include "MemoryBlock.h"
namespace syn {
	CLIPSIOController::CLIPSIOController() : _env(CreateEnvironment()) {
		addIOController(this);
	}
	CLIPSIOController::~CLIPSIOController() {
		removeIOController(this);
		if (_env) {
			DestroyEnvironment(_env);
		}
	}
	void CLIPSIOController::initialize() {
		installExtensions(_env);
		installMemoryBlockTypes(_env);
		// install custom functions into the environment
	}
	void CLIPSIOController::shutdown() {

	}
	CLIPSInteger CLIPSIOController::read(CLIPSInteger addr) {
		std::stringstream args;
		args << addr;
		auto str = args.str();
		CLIPSValue result;
		if (EnvFunctionCall(_env, "read-from-io-address", str.c_str(), &result)) {
			throw syn::Problem("Calling read-from-io-address failed!");
		}
		if (result.type != INTEGER) {
			throw syn::Problem("Resultant type from read call is not an integer!");
		}
		return (EnvDOToLong(_env, result));
	}
	void CLIPSIOController::write(CLIPSInteger addr, CLIPSInteger value) {
		std::stringstream args;
		args << addr << " " << value;
		auto str = args.str();
		CLIPSValue result;
		if (EnvFunctionCall(_env, "write-to-io-address", str.c_str(), &result)) {
			throw syn::Problem("Calling write-to-io-address failed!");
		} else if (result.type != INTEGER) {
			throw syn::Problem("Calling write-to-io-address failed!");
		}
	}

	void CLIPSIOController::Registrar::add(CLIPSIOController* c) {
		if (_backing.count(c->getRawEnvironment()) > 0) {
			throw syn::Problem("Can't register an environment twice!");
		} else {
			_backing.emplace(c->getRawEnvironment(), c);
		}
	}
	void CLIPSIOController::Registrar::remove(CLIPSIOController* c) {
		auto result = _backing.find(c->getRawEnvironment());
		if (result == _backing.end()) {
			throw syn::Problem("Given CLIPSIOController is not registered!");
		} else {
			_backing.erase(result);
		}
	}

	CLIPSIOController& CLIPSIOController::Registrar::get(void* env) {
		auto result = _backing.find(env);
		if (result == _backing.end()) {
			throw syn::Problem("Given environment is not linked to a CLIPSIOController!");
		} else {
			return *(result->second);
		}
	}

	CLIPSIOController::Registrar CLIPSIOController::_registrar;

}
