/**
 * @file
 * An abstract class which simplifies the process of adding cores to CLIPS as
 * external address types.
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

#ifndef SYN_CLIPS_CORE_H_
#define SYN_CLIPS_CORE_H_
#include "Core.h"
extern "C" {
    #include "clips.h"
}
#include "IOController.h"
namespace syn {
    /**
     * A Core which can interface with CLIPS
     */
	template<typename Word, typename AddressWord = Word>
    class ClipsCore : public Core {
		public:
			using Parent = Core;
			using Self = ClipsCore<Word, AddressWord>;
        public:
			ClipsCore(CLIPSIOController& bus) : _bus(bus) { }
			virtual ~ClipsCore() { }
            /**
             * given an environment and return value, figure out what action is
             * requested and carry it out if it makes sense.
             * @param env the clips environment
             * @param ret the return address
             * @return a boolean value signifying if the operation was executed
             * successfully.
             */
            virtual bool handleOperation(void* env, CLIPSValue* ret) = 0;
			virtual Word readFromBus(AddressWord addr) { return Word(_bus.read(addr)); }
			virtual void writeToBus(AddressWord addr, Word value) { _bus.write(addr, value); }
		protected:
			CLIPSIOController& _bus;
			

    };
} // end namespace syn

#endif // end SYN_CLIPS_CORE_H_
