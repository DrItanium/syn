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

#include "Base.h"
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
	void soundCardError_Base(UDFContext* context, CLIPSValue* ret, int code, const std::string& desc, alsa::StatusCode status) {
		CVSetBoolean(ret, false);
		std::string cardId (alsa::decodeStatusCode(status));
		std::stringstream msg;
		msg << desc << ": " << cardId;
		auto str2 = msg.str();
		errorMessage(context, "SYSTEM", code, "sound card error: ", str2);
	}
	void soundCardError_Base(UDFContext* context, CLIPSValue* ret, int code, const std::string& desc) {
		CVSetBoolean(ret, false);
		errorMessage(context, "SYSTEM", code, "sound card error: ", desc);
	}
	void soundCardError(UDFContext* context, CLIPSValue* ret, int code, const std::string& desc, alsa::StatusCode status) {
		soundCardError_Base(context, ret, code, desc, status); 
	}


	void soundCardError(UDFContext* context, CLIPSValue* ret, int code, const std::string& desc) {
		soundCardError_Base(context, ret, code, desc);
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
		soundCardError_Base(context, ret, code, string);
	}
    template<typename ... Args>
    void soundCardError(void* env, DataObjectPtr ret, int code, Args&& ... args) {
        std::ostringstream str;
        collectArguments(str, args...);
        auto string = str.str();
        CVSetBoolean(ret, false);
        errorMessage(env, "SYSTEM", code, "sound card error: ", str.str());
    }

    class MidiConnection {
        public:
            using MidiDirectionResults = std::tuple<alsa::StatusCode, alsa::StatusCode>;
        public:
            MidiConnection(const std::string& id);
            ~MidiConnection();
            const std::string& getId() const noexcept { return _id; }
            bool isOpen() const noexcept { return _open; }
            alsa::StatusCode open(alsa::rawmidi::OpenMode mode = alsa::rawmidi::OpenMode::Sync);
            MidiDirectionResults close();
            ssize_t write(const void* buffer, size_t count) noexcept;
            ssize_t read(void* buffer, size_t count) noexcept;
        private:
            std::string _id;
            bool _open;
            alsa::rawmidi::Device* _output;
            alsa::rawmidi::Device* _input;
    };
    MidiConnection::MidiConnection(const std::string& id) : _id(id), _open(false), _output(nullptr), _input(nullptr) { }
    MidiConnection::~MidiConnection() {
        if (_open) {
            close();
            _output = nullptr;
        }
    }

    alsa::StatusCode MidiConnection::open(alsa::rawmidi::OpenMode mode) {
        if (_open) {
            return 0;
        }
        auto status = alsa::rawmidi::open(&_input, &_output, _id, mode);
        if (status == 0) {
            _open = true;
        }
        return status;
    }

    std::tuple<alsa::StatusCode, alsa::StatusCode> MidiConnection::close() {
        _open = false;
        auto statusOut = alsa::rawmidi::close(_output);
        auto statusIn = alsa::rawmidi::close(_input);
        _output = nullptr;
        _input = nullptr;
        return std::make_tuple(statusIn, statusOut);
    }

    ssize_t MidiConnection::write(const void* buffer, size_t size) noexcept {
        if (_open) {
            return alsa::rawmidi::write(_output, buffer, size);
        } else {
            return 0;
        }
    }

    ssize_t MidiConnection::read(void* buffer, size_t size) noexcept {
        if (_open) {
            return alsa::rawmidi::read(_input, buffer, size);
        } else {
            return 0;
        }
    }

    namespace WrappedNewCallBuilder {
        template<>
        MidiConnection* invokeNewFunction(void* env, CLIPSValuePtr ret, const std::string& funcErrorPrefix, const std::string& function) noexcept {
            try {
                if (getArgCount(env) != 2) {
                    errorMessage(env, "NEW", 2, funcErrorPrefix, " need the hardware id of the midi device!");
                    return nullptr;
                }
                CLIPSValue hwid;
                if (!EnvArgTypeCheck(env, function.c_str(), 2, SYMBOL_OR_STRING, &hwid)) {
                    errorMessage(env, "NEW", 3, funcErrorPrefix, " provided input value must be a lexeme!");
                    return nullptr;
                }
                std::string id(DOToString(hwid));
                return new MidiConnection(id);
            } catch (const syn::Problem& p) {
                CVSetBoolean(ret, false);
                std::stringstream s;
                s << "an exception was thrown: " << p.what();
                auto str = s.str();
                errorMessage(env, "NEW", 2, funcErrorPrefix, str);
            }
            return nullptr;
        }
    } // end namespace WrappedNewCallBuilder



    class MidiConnectionWrapper : public CommonExternalAddressWrapper<MidiConnection> {
        public:
            using Parent = CommonExternalAddressWrapper<MidiConnection>;
            enum class Operations {
                Read,
                Write,
                Open,
                Close,
                IsOpen,
                GetID,
                Count,
            };
        public:
            MidiConnectionWrapper(std::unique_ptr<MidiConnection>&& ptr) : Parent(std::move(ptr)) { }
            MidiConnectionWrapper(MidiConnection* connection) : Parent(connection) { }
            MidiConnectionWrapper(const std::string& id) : Parent(id) { }
            virtual ~MidiConnectionWrapper() { }
            virtual bool handleCallOperation(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation) override;
            bool performRead(void* env, DataObjectPtr ret);
            bool performWrite(void* env, DataObjectPtr ret);
            bool openDevice(void* env, DataObjectPtr ret);
            bool closeDevice(void* env, DataObjectPtr ret);
            bool isOpen(void* env, DataObjectPtr ret);
            bool getId(void* env, DataObjectPtr ret);
    };
    bool MidiConnectionWrapper::getId(void* env, DataObjectPtr ret) {

        CVSetSymbol(ret, get()->getId().c_str());
        return true;
    }

    bool MidiConnectionWrapper::isOpen(void* env, DataObjectPtr ret) {
        CVSetBoolean(ret, get()->isOpen());
        return true;
    }
    bool MidiConnectionWrapper::closeDevice(void* env, DataObjectPtr ret) {
        if (!get()->isOpen()) {
            CVSetBoolean(ret, false);
        } else {
            alsa::StatusCode input, output;
            std::tie(input, output) = get()->close();
            if (input < 0) {
                soundCardError(env, ret, 2, "could not close input stream: ", alsa::decodeStatusCode(input));
                return false;
            }
            if (output < 0) {
                soundCardError(env, ret, 2, "could not close output stream: ", alsa::decodeStatusCode(input));
                return false;
            }
            CVSetBoolean(ret, true);
        }
        return true;
    }

    bool MidiConnectionWrapper::openDevice(void* env, DataObjectPtr ret) {
        if (get()->isOpen()) {
            CVSetBoolean(ret, false);
        } else {
            auto status = get()->open();
            if (status < 0) {
                soundCardError(env, ret, 2, "could not open midi device: ", alsa::decodeStatusCode(status));
                return false;
            }
            CVSetBoolean(ret, true);
        }
        return true;
    }

    bool MidiConnectionWrapper::performWrite(void* env, DataObjectPtr ret) {
        if (!get()->isOpen()) {
            soundCardError(env, ret, 2, "midi device not open");
            return false;
        } else {
            // this should always be a multifield
            CLIPSValue firstArgument;
            __RETURN_FALSE_ON_FALSE__(Parent::tryExtractArgument1(env, ret, &firstArgument, syn::MayaType::Multifield, "This operation only accepts a multifield"));
            auto length = GetDOLength(firstArgument);
            std::unique_ptr<char[]> code = std::make_unique<char[]>(length);
            auto* ptr = code.get();
            for(auto index = GetDOBegin(firstArgument); index <= GetDOEnd(firstArgument); ++index, ++ptr) {
                if (GetMFType(GetValue(firstArgument), index) != INTEGER) {
                    return Parent::callErrorCode4(env, ret, "Expected all arguments to be of type INTEGER!");
                }
                *ptr = static_cast<char>(EnvValueToInteger(env, GetMFValue(GetValue(firstArgument), index)));
            }
            auto result = get()->write(code.get(), length);
            if (result < 0) {
                soundCardError(env, ret, 2, "error occurred during write to midi device: ", alsa::decodeStatusCode(result));
                return false;
            }
            CVSetInteger(ret, result);
            return true;
        }
    }

    bool MidiConnectionWrapper::performRead(void* env, DataObjectPtr ret) {
        if (!get()->isOpen()) {
            soundCardError(env, ret, 2, "midi device not open!");
            return false;
        } else {
            CLIPSValue cap;
            __RETURN_FALSE_ON_FALSE__(Parent::tryExtractArgument1(env, ret, &cap, syn::MayaType::Integer, "This operation only accepts an integer which is a maximum"));
            auto capacity = CVToInteger(&cap);
            if (capacity <= 0) {
                CVSetBoolean(ret, false);
                errorMessage(env, "SYSTEM", 2, "Read from midi device failure: ", "max size is zero or negative!");
                return false;
            }
            std::unique_ptr<char[]> extraction = std::make_unique<char[]>(capacity);
            // okay now perform the read
            get()->read(extraction.get(), capacity);
            MultifieldBuilder mv(env, capacity);
            for (auto i = 0, j = 1; i <= capacity; ++i, ++j) {
                mv.setField(j, static_cast<CLIPSInteger>(extraction[i]));
            }
            mv.assign(ret);
            return true;
        }
    }

    bool MidiConnectionWrapper::handleCallOperation(void* env, DataObjectPtr value, DataObjectPtr ret, const std::string& operation) {
        static std::map<std::string, MidiConnectionWrapper::Operations> ops = {
            { "read", Operations::Read },
            { "write", Operations::Write },
            { "open", Operations::Open },
            { "close", Operations::Close },
            { "is-open", Operations::IsOpen },
            { "openp", Operations::IsOpen },
            { "get-id", Operations::GetID },
        };

        auto result = ops.find(operation);
        if (result == ops.end()) {
            return Parent::callErrorMessageCode3(env, ret, operation, "<- unknown operation!");
        } else {
            switch(result->second) {
                case Operations::Read:
                    return performRead(env, ret);
                case Operations::Write:
                    return performWrite(env, ret);
                case Operations::Open:
                    return openDevice(env, ret);
                case Operations::Close:
                    return closeDevice(env, ret);
                case Operations::IsOpen:
                    return isOpen(env, ret);
                case Operations::GetID:
                    return getId(env, ret);
                default:
                    return Parent::callErrorMessageCode3(env, ret, operation, "<- defined but unimplemented operation!");
            }
        }
    }
    DefWrapperSymbolicName(MidiConnection, "midi-connection");
    DefExternalAddressWrapperType(MidiConnection, MidiConnectionWrapper);
	void listSoundCards(UDFContext* context, CLIPSValue* ret);
	void listMidiPorts(UDFContext* context, CLIPSValue* ret);
	void installAlsaMIDIExtensions(void* theEnv) {
		Environment* env = (Environment*)theEnv;
		EnvAddUDF(env, "list-sound-cards", "v", listSoundCards, "listSoundCards", 0, 0, nullptr, nullptr);
		EnvAddUDF(env, "list-midi-ports", "v", listMidiPorts, "listMidiPorts", 0, 0, nullptr, nullptr);
        MidiConnectionWrapper::registerWithEnvironment(env);
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
            clips::printRouter(theEnv, WDISPLAY, cardId);
			msg.str("");
			char* longName = nullptr;
			char* shortName = nullptr;
			status = alsa::getCardName(card, &shortName);
			if (status < 0) {
				soundCardError(context, ret, 2, "cannot determine card shortname", status);
				return;
			}
			status = alsa::getCardLongName(card, &longName);
			if (status < 0) {
				soundCardError(context, ret, 2, "cannot determine card longname", status);
				return;
			}
			msg << "\tLong name: " << longName << std::endl;
			msg << "\tShort name: " << shortName << std::endl;
			auto names = msg.str();
            clips::printRouter(theEnv, WDISPLAY, names);
            status = alsa::nextCard(&card);
			if (status < 0) {
				unableToDetermineCardNumberError(context, ret, status);
				return;
			}
			free(longName);
			free(shortName);
		}
		CVSetBoolean(ret, true);
	}
	template<alsa::rawmidi::StreamDirection direction>
	alsa::StatusCode supportsDirection(alsa::Controller* ctl, alsa::CardId card, alsa::rawmidi::DeviceId device, int sub) {
		alsa::rawmidi::RawInfo* info = nullptr;
        alsa::StatusCode status = 0;
		snd_rawmidi_info_alloca(&info);
		alsa::rawmidi::setDevice(info, device);
		alsa::rawmidi::setSubdevice(info, sub);
		alsa::rawmidi::setStream(info, direction);

		status = alsa::rawmidi::populate(info, ctl);
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
		alsa::rawmidi::RawInfo* info;

		int subs, subsIn, subsOut;
		snd_rawmidi_info_alloca(&info);
		alsa::rawmidi::setDevice(info, device);
		alsa::rawmidi::setStream(info, alsa::rawmidi::StreamDirection::Input);
		alsa::rawmidi::populate(info, ctl);
		subsIn = alsa::rawmidi::getSubdeviceCount(info);
		alsa::rawmidi::setStream(info, alsa::rawmidi::StreamDirection::Output);
		alsa::rawmidi::populate(info, ctl);
		subsOut = alsa::rawmidi::getSubdeviceCount(info);
		subs = subsIn > subsOut ? subsIn : subsOut;

		int sub = 0;
		auto in = 0;
		auto out = 0;
		auto status = isOutput(ctl, card, device, sub);
		if (status < 0) {
			soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ": ", alsa::decodeStatusCode(status));
			return;
		} else if(status) {
			out = 1;
		}
		if (status == 0) {
			status = isInput(ctl, card, device, sub);
			if (status < 0) {
				soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ": ", alsa::decodeStatusCode(status));
				return;
			} 
		} else if (status) {
			in = 1;
		}
		if (status == 0) {
			return;
		}
		auto theEnv = UDFContextEnvironment(context);
		auto name = alsa::rawmidi::getName(info);
		auto subName = alsa::rawmidi::getSubdeviceName(info);
		std::cout << "name: " << name << ", subName: " << subName << std::endl;
		if (subName[0] == '\0') {
			std::stringstream str;
			str << (in ? "I" : " ");
			str << (out ? "O" : " ");
			if (subs == 1) {
				str << "   hw:" << card << "," << device << "    " << name;
			} else {
				str << "   hw:" << card << "," << device << "    " << name << " (" << subs << " subdevices)";
			}
			str << std::endl;
            auto statement = str.str();
            clips::printRouter(theEnv, logicalName, statement);
		} else {
			sub = 0;
			for (;;) {
				std::stringstream str;
				str << (in ? "I" : " ");
				str << (out ? "O" : " ");
				str << "   hw:" << card << "," << device << "," << sub << "  " << subName << std::endl;
                auto statement = str.str();
                clips::printRouter(theEnv, logicalName, statement);
				++sub;
				if (sub >= subs) {
					break;
				}
				in = isInput(ctl, card, device, sub);
				out = isOutput(ctl, card, device, sub);
				alsa::rawmidi::setSubdevice(info, sub);
				if (out) {
					alsa::rawmidi::setStream(info, alsa::rawmidi::StreamDirection::Output);
					status = alsa::rawmidi::populate(info, ctl);
					if (status < 0) {
						soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ":", sub, ": ", alsa::decodeStatusCode(status));
						break;
					}
				} else {
					alsa::rawmidi::setStream(info, alsa::rawmidi::StreamDirection::Input);
					status = alsa::rawmidi::populate(info, ctl);
					if (status < 0) {
						soundCardError(context, ret, 2, "cannot get rawmidi information ", card, ":", device, ":", sub, ": ", alsa::decodeStatusCode(status));
						break;
					}
				}
				subName = alsa::rawmidi::getSubdeviceName(info);
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
        clips::printRouter(theEnv, WDISPLAY, "\nType Device    Name\n");
        clips::printRouter(theEnv, WDISPLAY, "====================================\n");
		while (card >= 0) {
			listMidiDevicesOnCard(context, ret, card, WDISPLAY);
            status = alsa::nextCard(&card);
            if (status < 0) {
				unableToDetermineCardNumberError(context, ret, status);
				return;
			}
		}
        clips::printLine(theEnv, WDISPLAY);
	}

} // end namespace syn
