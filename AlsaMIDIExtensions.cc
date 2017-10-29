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
	void installAlsaMIDIExtensions(void* theEnv) {
		Environment* env = (Environment*)theEnv;
		EnvAddUDF(env, "list-sound-cards", "v", listSoundCards, "listSoundCards", 0, 0, nullptr, nullptr);
		EnvAddUDF(env, "list-midi-ports", "v", listMidiPorts, "listMidiPorts", 0, 0, nullptr, nullptr);
	}
	using SoundCardId = int;
	using SoundCardStatusCode = int;
	void soundCardError(UDFContext* context, CLIPSValue* ret, int code, const std::string& desc, SoundCardStatusCode status) {
		CVSetBoolean(ret, false);
		std::string cardId (snd_strerror(status));
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
			// TODO: continue this
		} while (device >= 0);
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
			list_midi_devices_on_card(card);
			if ((status = snd_card_next(&card)) < 0) {
				unableToDetermineCardNumberError(context, ret, status);
				return;
			}
		} 
		EnvPrintRouter
		printf("\n");
	}

} // end namespace syn
