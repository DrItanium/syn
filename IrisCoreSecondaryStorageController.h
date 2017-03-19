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

#ifndef IRIS_SECONDARY_STORAGE_CONTROLLER_H__
#define IRIS_SECONDARY_STORAGE_CONTROLLER_H__
#include "Base.h"
#include "IrisCoreTypes.h"
#include "ExecutionUnits.h"
#include <memory>
#include "IODevice.h"
namespace iris {

	/**
	 * Simple wrapper interface over secondary storage media which the cpu
	 * interacts with
	 */
	class SecondaryStorageController : public syn::AddressableIODevice<word> {
		public:
			using Sector = syn::FixedSizeLoadStoreUnit<word, word, 0xFF>;
			using Parent = syn::AddressableIODevice<word>;
			static constexpr uint32_t sectorCount = 0x10000;
			enum class Mapping {
				SectorAddress = 0x0000,
				InnerOffset = 0x0001,
				AccessNode = 0x0002,
			};
			SecondaryStorageController(word address);
			virtual word read(word targetAddress) override;
			virtual void write(word targetAddress, word value) override;
			virtual void initialize() override;
			virtual void shutdown() override;
		private:
			word _sectorAddress;
			word _innerOffset;
			std::unique_ptr<Sector[]> _media;
	};


} // end namespace iris
#endif // end IRIS_SECONDARY_STORAGE_CONTROLLER_H__
