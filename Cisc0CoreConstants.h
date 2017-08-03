/**
 * @file
 * Values and types that are constant to the entire cisc0 architecture
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


#ifndef _TARGET_CISC0_CONSTANTS_H
#define _TARGET_CISC0_CONSTANTS_H
#include "BaseTypes.h"

namespace cisc0 {
    /// the unit that memory is divided into
	using Word = uint16;
    /// the unit that can be described as two words
	using DWord = uint32;
    /// the basic unit of an instruction since the architecture is variable-length
	using RawInstruction = Word; // this is more of a packet!
    /// The type of registers
	using RegisterValue = DWord;
    /// the type of addresses
    using Address = DWord;

    /**
     * Constants that are always relevant to cisc0
     */
	enum ArchitectureConstants  {
		RegisterCount = 16,
		MaxInstructionCount = 16,
		MaxRegisterBanks = 32,
		RegistersPerBank = 8,
        /// Writing to this address will cause the cpu to stop
		TerminateAddress = 0xFFFFFFFF,
        /// Where ip starts on cpu boot
		StartingIPAddress = 0xFE000000,
		// unlike iris16 and iris32, there is a limited set of registers with
		// a majority of them marked for explicit usage, instructions
		// themselves are still 16 bits wide but 32bits are extracted per
		// packet.
		R15 = RegisterCount - 1,
		R14 = RegisterCount - 2,
		R13 = RegisterCount - 3,
		R12 = RegisterCount - 4,
		R11 = RegisterCount - 5,
		R10 = RegisterCount - 6,
		R9  = RegisterCount - 7,
		R8  = RegisterCount - 8,
		R7  = RegisterCount - 9,
		R6  = RegisterCount - 10,
		R5  = RegisterCount - 11,
		R4  = RegisterCount - 12,
		R3  = RegisterCount - 13,
		R2  = RegisterCount - 14,
		R1  = RegisterCount - 15,
		R0  = RegisterCount - 16,
		InstructionPointer = R15,
		StackPointer = R14,
		CallStackPointer = R13, // second stack
        /// used by load/store routines to describe the memory address
		AddressRegister = R12,
        /// used by load/store routines to describe the source or destination of the operation
		ValueRegister = R11,
		MaskRegister = R10,
		ShiftRegister = R9,
		FieldRegister = R9,
	};
} // end namespace cisc0
#endif // end _TARGET_CISC0_CONSTANTS_H
