/**
 * @file
 * Basic wrapper for all objects which inherit from device
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


#ifndef __SYN_DEVICE_WRAPPER_H
#define __SYN_DEVICE_WRAPPER_H
#include "CommonExternalAddressWrapper.h"
#include "Device.h"

namespace syn {

template<typename T>
class DeviceWrapper : public CommonExternalAddressWrapper<T> {
	public:
		using Self = DeviceWrapper<T>;
		using Parent = CommonExternalAddressWrapper<T>;
		static_assert(std::is_base_of<Device, T>::value, "Given type must extend Device");
		enum class CommonDeviceOperations {
			Initialize,
			Shutdown,
			ToggleDebug,
			DebugEnabled,
			Count,
		};
	public:
		using Parent::Parent;
		virtual ~DeviceWrapper() { }
		virtual bool handleCallOperation(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& op) override final {
			static std::map<std::string, CommonDeviceOperations> lookup = {
				{ "initialize", CommonDeviceOperations::Initialize },
				{ "shutdown", CommonDeviceOperations::Shutdown },
				{ "toggle-debug", CommonDeviceOperations::ToggleDebug },
				{ "debugp", CommonDeviceOperations::DebugEnabled },
			};

			auto result = lookup.find(op);
			if (result != lookup.end()) {
				CVSetBoolean(ret, true);
				switch(result->second) {
					case CommonDeviceOperations::Initialize:
						this->get()->initialize();
						break;
					case CommonDeviceOperations::Shutdown:
						this->get()->shutdown();
						break;
					case CommonDeviceOperations::ToggleDebug:
						CVSetBoolean(ret, this->get()->debugEnabled());
						this->get()->toggleDebug();
						break;
					case CommonDeviceOperations::DebugEnabled:
						CVSetBoolean(ret, this->get()->debugEnabled());
						break;
					default:
            			return Parent::callErrorMessageCode3(env, ret, op, "<- unknown but registered operation!!!!");
				}
				return true;
			} else {
				return handleExtendedCallOperation(env, value, ret, op);
			}
		}
		virtual bool handleExtendedCallOperation(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& op) = 0; 
		virtual bool isDevice() noexcept override final { return true; }
};

} // end namespace syn

#endif // end __SYN_DEVICE_WRAPPER_H
