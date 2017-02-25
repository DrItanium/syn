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
#include "IrisCoreTypes.h"
#include "IrisCoreSecondaryStorageController.h"

namespace iris {
	SecondaryStorageController::SecondaryStorageController(word address) : IODevice<word>(address, 3), _sectorAddress(0), _innerOffset(0) { }
	virtual word SecondaryStorageController::read(word targetAddress) override {
		auto address = computeInternalAddress(targetAddress);
		switch(static_cast<Mapping>(address)) {
			case Mapping::SectorAddress:
				return _sectorAddress;
			case Mapping::InnerOffset:
				return _innerOffset;
			case Mapping::AccessNode:
				return _media[_sectorAddress][_innerOffset];
			default:
				throw syn::Problem("Undefined action!");
		}
	}
	virtual void SecondaryStorageController::write(word address, word value) override {
		auto modAddr = computeInternalAddress(address);
		switch(static_cast<Mapping>(modAddr)) {
			case Mapping::SectorAddress:
				_sectorAddress = value;
				break;
			case Mapping::InnerOffset:
				_innerOffset = syn::decodeBits<word, byte, 0x00FF, 0>(value);
				break;
			case Mapping::AccessNode:
				_media[_sectorAddress][_innerOffset] = value;
				break;
			default:
				throw syn::Problem("Undefined action!");
		}

	}
	virtual void SecondaryStorageController::initialize() override {
		_media.initialize();
	}
	virtual void SecondaryStorageController::shutdown() override {
		_media.shutdown();
	}
} // end namespace iris
