/**
 * @file
 * The device class concept
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


#ifndef IRIS_DEVICE_H_
#define IRIS_DEVICE_H_
namespace syn {
/**
 * The concept of a devices which is initialized and shutdown. This is the base
 * class of all other invokable things in syn.
 */
class Device {
	public:
        Device() { }
        virtual ~Device() { }
        /**
         * Called manually when this device wants to be brought up. It is _NOT_
         * automatically called on instantiation.
         */
		virtual void initialize() = 0;
        /**
         * Called manually when this device wants to be brought down. It is
         * _NOT_ automatically called by the destructor.
         */
		virtual void shutdown() = 0;
        /// Is debugging active?
		bool debugEnabled() const noexcept { return _debug; }
		void toggleDebug() noexcept { _debug = !_debug; }
	private:
		bool _debug = false;
};

} // end namespace syn
#endif // end IRIS_DEVICE_H_
