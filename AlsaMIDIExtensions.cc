/**
 * @file
 * implementation of methods described in ClipsExtensions.h
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

#include "AlsaMIDIExtensions.h"
extern "C" {
#include "clips.h"
}
#include <alsa/asoundlib.h>

#include "MultifieldBuilder.h"
#include "ClipsExtensions.h"

namespace syn {
	void listSoundCards(UDFContext* context, CLIPSValue* ret);
	void listMidiPorts(UDFContext* context, CLIPSValue* ret);
	void writeToMidiPort(UDFContext* context, CLIPSValue* ret);
	void installAlsaMIDIExtensions(void* theEnv) {
		Environment* env = (Environment*)theEnv;
		EnvAddUDF(env, "list-sound-cards", "v", listSoundCards, "listSoundCards", 0, 0, nullptr, nullptr);
		EnvAddUDF(env, "list-midi-ports", "v", listMidiPorts, "listMidiPorts", 0, 0, nullptr, nullptr);
		EnvAddUDF(env, "write-to-midi-port", "b", writeToMidiPort, "writeToMidiPort", 4, 4, "syl;sy;l;l;l", nullptr);
	}
	using SoundCardId = int;
	using SoundCardStatusCode = int;
	std::string decodeSoundCardStatusCode(SoundCardStatusCode status) {
		return std::string(snd_strerror(status));
	}
	void soundCardError(UDFContext* context, CLIPSValue* ret, int code, const std::string& desc, SoundCardStatusCode status) {
		CVSetBoolean(ret, false);
		std::string cardId (decodeSoundCardStatusCode(status));
		std::stringstream msg;
		msg << desc << ": " << cardId;
		auto str2 = msg.str();
		errorMessage(context, "SYSTEM", code, "sound card error: ", str2);
	}

	void soundCardError(UDFContext* context, CLIPSValue* ret, int code, const std::string& desc) {
		CVSetBoolean(ret, false);
		errorMessage(context, "SYSTEM", code, "sound card error: ", desc);
	}
	void noSoundCardsFoundError(UDFContext* context, CLIPSValue* ret) {
		soundCardError(context, ret, 1, "no sound cards found!");
	}
	void unableToDetermineCardNumberError(UDFContext* context, CLIPSValue* ret, SoundCardStatusCode status) {
		soundCardError(context, ret, 1, "cannot determine card number", status);
	}
	template<typename First>
	void collectArguments(std::ostream& storage, First&& f) {
		storage << f;
	}
	template<typename First, typename ... Args>
	void collectArguments(std::ostream& storage, First&& f, Args&& ...rest) {
		collectArguments<First>(storage, f);
		collectArguments(storage, rest...);
	}
	template<typename ... Args>
	void soundCardError(UDFContext* context, CLIPSValue* ret, int code, Args&& ...args) {
		std::ostringstream str;
		collectArguments(str, args...);
		auto string = str.str();
		soundCardError(context, ret, code, string);
	}
	void listSoundCards(UDFContext* context, CLIPSValue* ret) {
		// an adaption of the code found at https://ccrma.stanford.edu/~craig/articles/linuxmidi/alsa-1.0/alsarawportlist.c
		SoundCardId card = -1;
		SoundCardStatusCode status;
		status = snd_card_next(&card);
		if (status < 0) {
			unableToDetermineCardNumberError(context, ret, status);
			return;
		}
		if (card < 0) {
			noSoundCardsFoundError(context, ret);
			return;
		}
		auto theEnv = UDFContextEnvironment(context);
		while (card >= 0) {
			std::stringstream msg;
			msg << "Card " << card << ":" << std::endl;
			auto cardId = msg.str();
			EnvPrintRouter(theEnv, WDISPLAY, cardId.c_str());
			msg.str("");
			char* longName = nullptr;
			char* shortName = nullptr;
			status = snd_card_get_name(card, &shortName);
			if (status < 0) {
				soundCardError(context, ret, 2, "cannot determine card shortname", status);
				return;
			}
			status = snd_card_get_longname(card, &longName);
			if (status < 0) {
				soundCardError(context, ret, 2, "cannot determine card longname", status);
				return;
			}
			msg << "\tLong name: " << longName << std::endl;
			msg << "\tShort name: " << shortName << std::endl;
			auto names = msg.str();
			EnvPrintRouter(theEnv, WDISPLAY, names.c_str());
			status = snd_card_next(&card);
			if (status < 0) {
				unableToDetermineCardNumberError(context, ret, status);
				return;
			}
		}
		CVSetBoolean(ret, true);
	}
	using SoundController = snd_ctl_t;
	using RawMidiDeviceId = int;
	using RawMidiInfo = snd_rawmidi_info_t;
	template<decltype(SND_RAWMIDI_STREAM_INPUT) direction>
	SoundCardStatusCode supportsDirection(SoundController* ctl, SoundCardId card, RawMidiDeviceId device, int sub) {
		RawMidiInfo* info = nullptr;
		SoundCardStatusCode status = 0;

		snd_rawmidi_info_alloca(&info);
		snd_rawmidi_info_set_device(info, device);
		snd_rawmidi_info_set_subdevice(info, sub);
		snd_rawmidi_info_set_stream(info, direction);

		status = snd_ctl_rawmidi_info(ctl, info);
		if (status < 0 && status != -ENXIO) {
			return status;
		} else if(status == 0) {
			return 1;
		} else {
			return 0;
		}
	}
	inline SoundCardStatusCode isOutput(SoundController* ctl, SoundCardId card, RawMidiDeviceId device, int sub) { return supportsDirection<SND_RAWMIDI_STREAM_OUTPUT>(ctl, card, device, sub); }
	inline SoundCardStatusCode isInput(SoundController* ctl, SoundCardId card, RawMidiDeviceId device, int sub) { return supportsDirection<SND_RAWMIDI_STREAM_INPUT>(ctl, card, device, sub); }

	void listSubdeviceInfo(UDFContext* context, CLIPSValue* ret, SoundController* ctl, SoundCardId card, RawMidiDeviceId device, const char* logicalName) {
		RawMidiInfo* info = nullptr;
		SoundCardStatusCode status = 0;
		snd_rawmidi_info_alloca(&info);
		snd_rawmidi_info_set_device(info, device);

		snd_rawmidi_info_set_stream(info, SND_RAWMIDI_STREAM_INPUT);
		snd_ctl_rawmidi_info(ctl, info);
		auto subsIn = snd_rawmidi_info_get_subdevices_count(info);
		snd_rawmidi_info_set_stream(info, SND_RAWMIDI_STREAM_OUTPUT);
		snd_ctl_rawmidi_info(ctl, info);
		auto subsOut = snd_rawmidi_info_get_subdevices_count(info);
		int subs = subsIn > subsOut ? subsIn : subsOut;

		auto sub = 0;
		auto in = 0;
		auto out = 0;
		status = isOutput(ctl, card, device, sub);
		if (status < 0) {
			soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ": ", decodeSoundCardStatusCode(status));
			return;
		} else if(status) {
			out = true;
		}
		if (status == 0) {
			status = isInput(ctl, card, device, sub);
			if (status < 0) {
				soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ": ", decodeSoundCardStatusCode(status));
				return;
			} else if (status) {
				out = true;
			}
		}
		if (status == 0) {
			return;
		}

		std::string name(snd_rawmidi_info_get_name(info));
		std::string subName(snd_rawmidi_info_get_subdevice_name(info));
		auto theEnv = UDFContextEnvironment(context);
		if (subName.empty()) {
			std::stringstream str;
			str << (in ? "I" : " ");
			str << (out ? "O" : " ");
			str << "  hw:" << card << "," << device <<  "    " << name;
			if (subs == 1) {
				str << std::endl;
			} else {
				str << " (" << subs << " subdevices)" << std::endl;

			}
			EnvPrintRouter(theEnv, logicalName, str.str().c_str());
		} else {
			sub = 0;
			for (;;) {
				std::stringstream str;
				str << (in ? "I" : " ");
				str << (out ? "O" : " ");
				str << "   hw:" << card << "," << device << "," << sub << "  " << subName << std::endl;
				EnvPrintRouter(theEnv, logicalName, str.str().c_str());
				++sub;
				if (sub >= subs) {
					break;
				}
				in = isInput(ctl, card, device, sub);
				out = isOutput(ctl, card, device, sub);
				snd_rawmidi_info_set_subdevice(info, sub);
				if (out) {
					snd_rawmidi_info_set_stream(info, SND_RAWMIDI_STREAM_OUTPUT);
					status = snd_ctl_rawmidi_info(ctl, info);
					if (status < 0) {
						soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ":", sub, ": ", decodeSoundCardStatusCode(status));
						break;
					}
				} else {
					snd_rawmidi_info_set_stream(info, SND_RAWMIDI_STREAM_INPUT);
					status = snd_ctl_rawmidi_info(ctl, info);
					if (status < 0) {
						soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ":", sub, ": ", decodeSoundCardStatusCode(status));
						break;
					}
				}
				subName = snd_rawmidi_info_get_subdevice_name(info);
			}
		}


	}
	void listMidiDevicesOnCard(UDFContext* context, CLIPSValue* ret, SoundCardId card, const char* logicalName) {
		SoundController* ctl;
		RawMidiDeviceId device = -1;
		SoundCardStatusCode status;
		std::stringstream msg;
		msg << "hw:" << card;
		auto name = msg.str();
		status = snd_ctl_open(&ctl, name.c_str(), 0);
		if (status < 0) {
			msg.str("");
			msg << "cannot open control for card " << card;
			auto e = msg.str();
			soundCardError(context, ret, 1, e, status);
			return;
		}
		do {
			status = snd_ctl_rawmidi_next_device(ctl, &device);
			if (status < 0) {
				soundCardError(context, ret, 2, "cannot determine device number", status);
				return;
			}
			if (device >= 0) {
				listSubdeviceInfo(context, ret, ctl, card, device, logicalName);
			}
		} while (device >= 0);
		snd_ctl_close(ctl);
	}

	void listMidiPorts(UDFContext* context, CLIPSValue* ret) {
		SoundCardStatusCode status;
		SoundCardId card = -1;  // use -1 to prime the pump of iterating through card list

		status = snd_card_next(&card);
		if (status < 0) {
			unableToDetermineCardNumberError(context, ret, status);
			return;
		}
		if (card < 0) {
			noSoundCardsFoundError(context, ret);
			return;
		}
		auto theEnv = UDFContextEnvironment(context);
		EnvPrintRouter(theEnv, WDISPLAY, "\nType Device    Name\n");
		EnvPrintRouter(theEnv, WDISPLAY, "====================================\n");
		while (card >= 0) {
			listMidiDevicesOnCard(context, ret, card, WDISPLAY);
			if ((status = snd_card_next(&card)) < 0) {
				unableToDetermineCardNumberError(context, ret, status);
				return;
			}
		} 
		EnvPrintRouter(theEnv, WDISPLAY, "\n");
	}

	using RawMidi = snd_rawmidi_t;
	void writeToMidiPort(UDFContext* context, CLIPSValue* ret) {
		// taken from https://ccrma.stanford.edu/~craig/articles/linuxmidi/alsa-1.0/alsarawmidiout.c
		CLIPSValue portname, noteP0, noteP1, noteP2;
		if (!UDFFirstArgument(context, LEXEME_TYPES, &portname)) {
			return;
		} else if (!UDFNextArgument(context, INTEGER_TYPE, &noteP0)) {
			return;
		} else if (!UDFNextArgument(context, INTEGER_TYPE, &noteP1)) {
			return;
		} else if (!UDFNextArgument(context, INTEGER_TYPE, &noteP2)) {
			return;
		}

		SoundCardStatusCode status;
		RawMidi* midiout = nullptr;
		std::string port(CVToString(&portname));
		status = snd_rawmidi_open(nullptr, &midiout, port.c_str(), SND_RAWMIDI_SYNC);
		if (status < 0) {
			soundCardError(context, ret, 1, "Problem opening midi output: ", decodeSoundCardStatusCode(status));
			return;
		}
		char note[3] = { 0 };
		note[0] = static_cast<char>(CVToInteger(&noteP0));
		note[1] = static_cast<char>(CVToInteger(&noteP1));
		note[2] = static_cast<char>(CVToInteger(&noteP2));
		status = snd_rawmidi_write(midiout, note, 3);
		if (status < 0) {
			soundCardError(context, ret, 2, "Problem writing to MIDI output: ", decodeSoundCardStatusCode(status));
			return;
		}

		snd_rawmidi_close(midiout);
		midiout = nullptr;
		CVSetBoolean(ret, true);
	}

} // end namespace syn
