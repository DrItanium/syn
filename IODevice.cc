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

#include "IODevice.h"
namespace syn {
	using RandomNumberGenerator64bitDevice = RandomDevice<uint64_t, CLIPSInteger>;
	using RandomNumberGeneratorSigned64bitDevice = RandomDevice<int64_t, CLIPSInteger>;
	using RandomNumberGenerator32bitDevice = RandomDevice<uint32_t, CLIPSInteger>;
	using RandomNumberGeneratorSigned32bitDevice = RandomDevice<int32_t, CLIPSInteger>;
	using RandomNumberGenerator16bitDevice = RandomDevice<uint16_t, CLIPSInteger>;
	using RandomNumberGeneratorSigned16bitDevice = RandomDevice<int16_t, CLIPSInteger>;
	//using RandomNumberGenerator8bitDevice = RandomDevice<uint8_t, CLIPSInteger>;
	//using RandomNumberGeneratorSigned8bitDevice = RandomDevice<int8_t, CLIPSInteger>;

	DefWrapperSymbolicName(RandomNumberGenerator64bitDevice, "random-number-generator:uint64");
	DefWrapperSymbolicName(RandomNumberGeneratorSigned64bitDevice, "random-number-generator:int64");
	DefWrapperSymbolicName(RandomNumberGenerator32bitDevice, "random-number-generator:uint32");
	DefWrapperSymbolicName(RandomNumberGeneratorSigned32bitDevice, "random-number-generator:int32");
	DefWrapperSymbolicName(RandomNumberGenerator16bitDevice, "random-number-generator:uint16");
	DefWrapperSymbolicName(RandomNumberGeneratorSigned16bitDevice, "random-number-generator:int16");
	//DefWrapperSymbolicName(RandomNumberGenerator8bitDevice, "random-number-generator:uint8");
	//DefWrapperSymbolicName(RandomNumberGeneratorSigned8bitDevice, "random-number-generator:int8");

	using WrappedRandomNumberGenerator64bitDevice = WrappedGenericRandomDevice<uint64_t>;
	using WrappedRandomNumberGeneratorSigned64bitDevice = WrappedGenericRandomDevice<int64_t>;
	using WrappedRandomNumberGenerator32bitDevice = WrappedGenericRandomDevice<uint32_t>;
	using WrappedRandomNumberGeneratorSigned32bitDevice = WrappedGenericRandomDevice<int32_t>;
	using WrappedRandomNumberGenerator16bitDevice = WrappedGenericRandomDevice<uint16_t>;
	using WrappedRandomNumberGeneratorSigned16bitDevice = WrappedGenericRandomDevice<int16_t>;
	//using WrappedRandomNumberGenerator8bitDevice = WrappedGenericRandomDevice<uint8_t>;
	//using WrappedRandomNumberGeneratorSigned8bitDevice = WrappedGenericRandomDevice<int8_t>;
	void CLIPS_installDefaultIODevices(void* theEnv) {
		WrappedRandomNumberGenerator64bitDevice::registerWithEnvironment(theEnv);
		WrappedRandomNumberGeneratorSigned64bitDevice::registerWithEnvironment(theEnv);
		WrappedRandomNumberGenerator32bitDevice::registerWithEnvironment(theEnv);
		WrappedRandomNumberGeneratorSigned32bitDevice::registerWithEnvironment(theEnv);
		WrappedRandomNumberGenerator16bitDevice::registerWithEnvironment(theEnv);
		WrappedRandomNumberGeneratorSigned16bitDevice::registerWithEnvironment(theEnv);
		//WrappedRandomNumberGenerator8bitDevice::registerWithEnvironment(theEnv);
		//WrappedRandomNumberGeneratorSigned8bitDevice::registerWithEnvironment(theEnv);
	}
} // end namespace syn
