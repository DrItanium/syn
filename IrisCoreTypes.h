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


#ifndef IRIS_CORE_TYPES_H__
#define IRIS_CORE_TYPES_H__
#include <cstdint>
namespace iris {
    using word = uint16_t;
    using dword = uint32_t;
    using raw_instruction = dword;
    using immediate = word;
    using QuadWord = uint64_t;
	enum ArchitectureConstants  {
		RegisterCount = 256,
		AddressMax = 0xFFFF,
        AddressCount = AddressMax + 1,
		RegisterMax = 0xFF,
		ConditionRegisterCount = 16,
		StackPointerIndex = RegisterCount - 1,
		MaxGroups = 8,
		MaxOperations = 32,
		ErrorDispatchVectorBase = 0x00FF,
		RegistersToSaveOnError = 18,
		ErrorRegisterStart = 255,
        TerminateIOAddress = 0xFFFF,
	};

} // end namespace iris


#endif // end IRIS_CORE_TYPES_H__
