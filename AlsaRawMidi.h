/**
 * @file
 * extensions for ALSA related operations
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


#ifndef ALSA_RAW_MIDI_H__
#define ALSA_RAW_MIDI_H__

#include <alsa/asoundlib.h>
#include <string>
namespace alsa {
    using Config = snd_config_t;
    using StatusCode = int;
    using CardId = int;
    using Controller = snd_ctl_t;
    std::string decodeStatusCode(StatusCode status) noexcept;
    StatusCode nextCard(CardId* card) noexcept { return snd_card_next(card); }
    StatusCode close(Controller* ctl) noexcept { return snd_ctl_close(ctl);  }
    // TODO: fix the open mode with an enum
    inline StatusCode open(Controller** ctl, const std::string& name, int mode) noexcept { return snd_ctl_open(ctl, name.c_str(), static_cast<int>(mode)); }
    inline StatusCode open(Controller** ctl, const std::string& name, int mode, Config* lconf) noexcept { return snd_ctl_open_lconf(ctl, name.c_str(), static_cast<int>(mode), lconf); }
	inline StatusCode getCardName(CardId card, char** storage) noexcept { return snd_card_get_name(card, storage); }
	inline StatusCode getCardLongName(CardId card, char** storage) noexcept { return snd_card_get_longname(card, storage); }
namespace rawmidi {
    using Device = snd_rawmidi_t;
	using RawInfo = snd_rawmidi_info_t;
    using DeviceId = int;
    enum class OpenMode : int {
        Append = SND_RAWMIDI_APPEND,
        NonBlocking = SND_RAWMIDI_NONBLOCK,
        Sync = SND_RAWMIDI_SYNC,
    };
    enum class StreamDirection {
        Input = SND_RAWMIDI_STREAM_INPUT,
        Output = SND_RAWMIDI_STREAM_OUTPUT,
    };
    alsa::StatusCode open(Device** inputp, Device** outputp, const std::string& name, OpenMode mode) noexcept { return snd_rawmidi_open(inputp, outputp, name.c_str(), static_cast<int>(mode)); }
    alsa::StatusCode open(Device** inRmidi, Device** outRMidi, const std::string& name, OpenMode mode, alsa::Config* lconf) noexcept { return snd_rawmidi_open_lconf(inRmidi, outRMidi, name.c_str(), static_cast<int>(mode), lconf); }
    alsa::StatusCode close(Device* rmidi) noexcept { return snd_rawmidi_close(rmidi); }
    ssize_t write(Device* dev, const void* buffer, size_t size) noexcept { return snd_rawmidi_write(dev, buffer, size); }
    ssize_t read(Device* dev, void* buffer, size_t size) noexcept { return snd_rawmidi_read(dev, buffer, size); }
    alsa::StatusCode nextDevice(alsa::Controller* ctl, int* device) noexcept { return snd_ctl_rawmidi_next_device(ctl, device); }

	void setDevice(RawInfo* _info, unsigned int val) noexcept { snd_rawmidi_info_set_device(_info, val); }
	void setSubdevice(RawInfo* _info, unsigned int val) noexcept { snd_rawmidi_info_set_subdevice(_info, val); }
	void setStream(RawInfo* _info, StreamDirection direction) noexcept { snd_rawmidi_info_set_stream(_info, snd_rawmidi_stream_t(direction)); }
	alsa::StatusCode populate(RawInfo* _info, alsa::Controller* ctl) noexcept { return snd_ctl_rawmidi_info(ctl, _info); }
	int getSubdeviceCount(RawInfo* _info) noexcept { return snd_rawmidi_info_get_subdevices_count(_info); }
	const char* getSubdeviceName(RawInfo* _info) noexcept { return snd_rawmidi_info_get_subdevice_name(_info); }
	const char* getName(RawInfo* _info) noexcept { return snd_rawmidi_info_get_name(_info); }

}
} // end namespace alsa

#endif // end ALSA_RAW_MIDI_H__

