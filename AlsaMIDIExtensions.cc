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
#include "AlsaRawMidi.h"
extern "C" {
#include "clips.h"
}
#include <alsa/asoundlib.h>

#include "MultifieldBuilder.h"
#include "ClipsExtensions.h"
#include "ExternalAddressWrapper.h"
#include "CommonExternalAddressWrapper.h"
namespace alsa {
    std::string decodeStatusCode(StatusCode status) noexcept {
        return std::string(snd_strerror(status));
    }
}
namespace syn {
    class OutputMidiConnection {
        public:
            OutputMidiConnection(const std::string& id);
            ~OutputMidiConnection();
            const std::string& getId() const noexcept { return _id; }
            bool isOpen() const noexcept { return _open; }
            alsa::StatusCode open(alsa::rawmidi::OpenMode mode = alsa::rawmidi::OpenMode::Sync);
            alsa::StatusCode close();
            ssize_t write(const void* buffer, size_t count) noexcept;
            ssize_t read(void* buffer, size_t count) noexcept;
        private:
            std::string _id;
            bool _open;
            alsa::rawmidi::Device* _output;
    };
    OutputMidiConnection::OutputMidiConnection(const std::string& id) : _id(id), _open(false), _output(nullptr) { }
    OutputMidiConnection::~OutputMidiConnection() {
        if (_open) {
            close();
            _output = nullptr;
        }
    }

    alsa::StatusCode OutputMidiConnection::open(alsa::rawmidi::OpenMode mode) {
        if (_open) {
            return 0;
        }
        _open = true;
        return alsa::rawmidi::open(nullptr, &_output, _id, mode);
    }

    alsa::StatusCode OutputMidiConnection::close() {
        _open = false;
        auto status = alsa::rawmidi::close(_output);
        _output = nullptr;
        return status;
    }

    ssize_t OutputMidiConnection::write(const void* buffer, size_t size) noexcept {
        return alsa::rawmidi::write(_output, buffer, size);
    }

    ssize_t OutputMidiConnection::read(void* buffer, size_t size) noexcept {
        return alsa::rawmidi::read(_output, buffer, size);
    }




    class OutputMidiConnectionWrapper : public CommonExternalAddressWrapper<OutputMidiConnection> {
        public:
            using Parent = CommonExternalAddressWrapper<OutputMidiConnection>;
        public:
            OutputMidiConnectionWrapper(std::unique_ptr<OutputMidiConnection>&& ptr) : Parent(std::move(ptr)) { }
            OutputMidiConnectionWrapper(OutputMidiConnection* connection) : Parent(connection) { }
            OutputMidiConnectionWrapper(const std::string& id) : Parent(id) { }
            virtual ~OutputMidiConnectionWrapper() { }
            virtual bool handleCallOperation(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation) override;
    };

    //bool OutputMidiConnectionWrapper::handleCa
	void listSoundCards(UDFContext* context, CLIPSValue* ret);
	void listMidiPorts(UDFContext* context, CLIPSValue* ret);
	void writeToMidiPort(UDFContext* context, CLIPSValue* ret);
    void openMidiPort(UDFContext* context, CLIPSValue* ret);
	void installAlsaMIDIExtensions(void* theEnv) {
		Environment* env = (Environment*)theEnv;
		EnvAddUDF(env, "list-sound-cards", "v", listSoundCards, "listSoundCards", 0, 0, nullptr, nullptr);
		EnvAddUDF(env, "list-midi-ports", "v", listMidiPorts, "listMidiPorts", 0, 0, nullptr, nullptr);
        EnvAddUDF(env, "open-midi-port", "b", openMidiPort, "openMidiPort", 1, 1, "sy", nullptr);
		EnvAddUDF(env, "write-to-midi-port", "b", writeToMidiPort, "writeToMidiPort", 2, 2, "*;sy;m", nullptr);
	}
	void soundCardError(UDFContext* context, CLIPSValue* ret, int code, const std::string& desc, alsa::StatusCode status) {
		CVSetBoolean(ret, false);
		std::string cardId (alsa::decodeStatusCode(status));
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
	void unableToDetermineCardNumberError(UDFContext* context, CLIPSValue* ret, alsa::StatusCode status) {
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
		alsa::CardId card = -1;
        alsa::StatusCode status;
        status = alsa::nextCard(&card);
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
            status = alsa::nextCard(&card);
			if (status < 0) {
				unableToDetermineCardNumberError(context, ret, status);
				return;
			}
		}
		CVSetBoolean(ret, true);
	}
	template<alsa::rawmidi::StreamDirection direction>
	alsa::StatusCode supportsDirection(alsa::Controller* ctl, alsa::CardId card, alsa::rawmidi::DeviceId device, int sub) {
        alsa::rawmidi::Info* info = nullptr;
        alsa::StatusCode status = 0;

        alsa::rawmidi::allocateInfo(&info);
        alsa::rawmidi::setDevice(info, device);
        alsa::rawmidi::setSubdevice(info, sub);
        alsa::rawmidi::setStream(info, direction);

		status = snd_ctl_rawmidi_info(ctl, info);
		if (status < 0 && status != -ENXIO) {
			return status;
		} else if(status == 0) {
			return 1;
		} else {
			return 0;
		}
	}
	inline alsa::StatusCode isOutput(alsa::Controller* ctl, alsa::CardId card, alsa::rawmidi::DeviceId device, int sub) { return supportsDirection<alsa::rawmidi::StreamDirection::Output>(ctl, card, device, sub); }
	inline alsa::StatusCode isInput(alsa::Controller* ctl, alsa::CardId card, alsa::rawmidi::DeviceId device, int sub) { return supportsDirection<alsa::rawmidi::StreamDirection::Input>(ctl, card, device, sub); }

	void listSubdeviceInfo(UDFContext* context, CLIPSValue* ret, alsa::Controller* ctl, alsa::CardId card, alsa::rawmidi::DeviceId device, const char* logicalName) {
        alsa::rawmidi::Info* info = nullptr;
        alsa::StatusCode status = 0;
        alsa::rawmidi::allocateInfo(&info);
        alsa::rawmidi::setDevice(info, device);

        alsa::rawmidi::setStream(info, alsa::rawmidi::StreamDirection::Input);
		snd_ctl_rawmidi_info(ctl, info);
		auto subsIn = snd_rawmidi_info_get_subdevices_count(info);
        alsa::rawmidi::setStream(info, alsa::rawmidi::StreamDirection::Output);
		snd_ctl_rawmidi_info(ctl, info);
		auto subsOut = snd_rawmidi_info_get_subdevices_count(info);
		int subs = subsIn > subsOut ? subsIn : subsOut;

		auto sub = 0;
		auto in = 0;
		auto out = 0;
		status = isOutput(ctl, card, device, sub);
		if (status < 0) {
			soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ": ", alsa::decodeStatusCode(status));
			return;
		} else if(status) {
			out = true;
		}
		if (status == 0) {
			status = isInput(ctl, card, device, sub);
			if (status < 0) {
				soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ": ", alsa::decodeStatusCode(status));
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
                alsa::rawmidi::setSubdevice(info, sub);
				if (out) {
                    alsa::rawmidi::setStream(info, alsa::rawmidi::StreamDirection::Output);
					status = snd_ctl_rawmidi_info(ctl, info);
					if (status < 0) {
						soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ":", sub, ": ", alsa::decodeStatusCode(status));
						break;
					}
				} else {
                    alsa::rawmidi::setStream(info, alsa::rawmidi::StreamDirection::Input);
					status = snd_ctl_rawmidi_info(ctl, info);
					if (status < 0) {
						soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ":", sub, ": ", alsa::decodeStatusCode(status));
						break;
					}
				}
				subName = snd_rawmidi_info_get_subdevice_name(info);
			}
		}


	}
	void listMidiDevicesOnCard(UDFContext* context, CLIPSValue* ret, alsa::CardId card, const char* logicalName) {
        alsa::Controller* ctl = nullptr;
		alsa::rawmidi::DeviceId device = -1;
        alsa::StatusCode status;
		std::stringstream msg;
		msg << "hw:" << card;
		auto name = msg.str();
        status = alsa::open(&ctl, name, 0);
		if (status < 0) {
			msg.str("");
			msg << "cannot open control for card " << card;
			auto e = msg.str();
			soundCardError(context, ret, 1, e, status);
			return;
		}
		do {
            status = alsa::rawmidi::nextDevice(ctl, &device);
			if (status < 0) {
				soundCardError(context, ret, 2, "cannot determine device number", status);
				return;
			}
			if (device >= 0) {
				listSubdeviceInfo(context, ret, ctl, card, device, logicalName);
			}
		} while (device >= 0);
        alsa::close(ctl);
	}

	void listMidiPorts(UDFContext* context, CLIPSValue* ret) {
        alsa::StatusCode status;
		alsa::CardId card = -1;  // use -1 to prime the pump of iterating through card list

        status = alsa::nextCard(&card);
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
            status = alsa::nextCard(&card);
            if (status < 0) {
				unableToDetermineCardNumberError(context, ret, status);
				return;
			}
		}
		EnvPrintRouter(theEnv, WDISPLAY, "\n");
	}
	void writeToMidiPort(UDFContext* context, CLIPSValue* ret) {
		// taken from https://ccrma.stanford.edu/~craig/articles/linuxmidi/alsa-1.0/alsarawmidiout.c
		// TODO: update this function to take in a multifield of bytes instead
		// of a fixed count!
		CLIPSValue portname, noteP0;
        CVSetBoolean(ret, false);
		if (!UDFFirstArgument(context, LEXEME_TYPES, &portname)) {
			return;
		} else if (!UDFNextArgument(context, MULTIFIELD_TYPE, &noteP0)) {
			return;
		}

		alsa::StatusCode status;
        alsa::rawmidi::Device* midiout = nullptr;
		std::string port(CVToString(&portname));
        status = alsa::rawmidi::open(nullptr, &midiout, port, alsa::rawmidi::OpenMode::Sync);
		if (status < 0) {
			soundCardError(context, ret, 1, "Problem opening midi output: ", alsa::decodeStatusCode(status));
			return;
		}
		char note[3] = { 0 };
		note[0] = static_cast<char>(CVToInteger(&noteP0));
		note[1] = static_cast<char>(CVToInteger(&noteP1));
		note[2] = static_cast<char>(CVToInteger(&noteP2));
        status = alsa::rawmidi::write(midiout, note, 3);
		if (status < 0) {
			soundCardError(context, ret, 2, "Problem writing to MIDI output: ", alsa::decodeStatusCode(status));
			return;
		}
        alsa::rawmidi::close(midiout);
		midiout = nullptr;
		CVSetBoolean(ret, true);
	}
	//TODO: make the midi device an external address

} // end namespace syn
