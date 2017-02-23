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

#ifdef IRIS_SECONDARY_STORAGE_CONTROLLER__
#define IRIS_SECONDARY_STORAGE_CONTROLLER__
#include "Base.h"
#include "ExecutionUnits.h"
#include <memory>
#include "IODevice.h"
namespace iris {

/**
 * Simple wrapper interface over secondary storage media which the cpu
 * interacts with
 */
class SecondaryStorageController : public IODevice<word> {
    public:
        using Sector = FixedSizeLoadStoreUnit<word, word, 256>;
        using Media = FixedSizeLoadStoreUnit<Sector, word, 0xFFFF>;
        enum class Mapping {
            SectorAddress = 0x0000,
            InnerOffset = 0x0001,
            AccessNode = 0x0002,
        };
        SecondaryStorageController(word address) : IODevice<word>(address, 3), _sectorAddress(0), _innerOffset(0) { }
        virtual word read(word targetAddress) override {
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
        virtual void write(word address, word value) override {
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
        virtual void initialize() override {
            _media.initialize();
        }
        virtual void shutdown() override {
            _media.shutdown();
        }
    private:
        word _sectorAddress;
        word _innerOffset;
        Media _media;
};


} // end namespace iris
#endif // end IRIS_SECONDARY_STORAGE_CONTROLLER__
