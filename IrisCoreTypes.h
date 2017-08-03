/**
 * @file
 * basic types for the iris architecture.
 *
 * iris is a 16-bit RISCy CPU with 256 GPRs and a harvard architecture. Words
 * are 16-bits Little Endian and instructions are 32-bits wide.
 * It has four memory pools:
 * - code (32-bits per word)
 * - data (16-bits per word)
 * - stack (16-bits per word)
 * - io (16-bits per word)
 * Each memory pool contains 64k words
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


#ifndef IRIS_CORE_TYPES_H__
#define IRIS_CORE_TYPES_H__
#include "BaseTypes.h"

/// iris is a 16-bit RISCy harvard architecture
namespace iris {
    /// the standard unit of iris, 16-bits wide
    using word = uint16;
    /// a 32-bit word
    using dword = uint32;
    /// what an instruction looks like when it is accessed from code memory
    using raw_instruction = dword;
    /// the size of a full immediate
    using immediate = word;
    /// a 64-bit word
    using QuadWord = uint64;

    /// Known aspects of this architecture whill will NOT change
	enum ArchitectureConstants  {
        /// number of registers
		RegisterCount = 256,
        /// largest accepted address
		AddressMax = 0xFFFF,
        /// number of address positions
        AddressCount = AddressMax + 1,
        /// largest accepted register index
		RegisterMax = 0xFF,
        /// number of condition registers
		ConditionRegisterCount = 16,
        /// maximum number of overarching groups in an encoded instruction
		MaxGroups = 8,
        /// maximum number of sub-operations per group in an encoded instruction
		MaxOperations = 32,
        /// The address to dispatch to in code memory when an error occurs
		ErrorDispatchVectorBase = 0x00FF,
        /// The number of registers to save into internal chip memory when an error happens
		RegistersToSaveOnError = 18,
        /**
         * The starting register address (walking downward) to save when
         * dispatching to the error handler
         */
		ErrorRegisterStart = 255,
        /**
         * The address in IO space to write to when wanting to shutdown the
         * core
         */
        TerminateIOAddress = 0xFFFF,
        /**
         * The index mask used when determining condition register index.
         */
		ConditionRegisterMask = 0xF,
	};

} // end namespace iris


#endif // end IRIS_CORE_TYPES_H__
