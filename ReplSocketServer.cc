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


#include "ClipsExtensions.h"
#include "MemoryBlock.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

extern "C" {
	#include "clips.h"
}
#ifdef PLATFORM_LINUX
    #include "AlsaMIDIExtensions.h"
#endif // end PLATFORM_LINUX
#include <string>

bool socketNameSet = false;
std::string socketName;

bool serverSetup = false;
sockaddr_un server;
int socketId;

void setServerSocket(Environment* env, UDFContext* context, UDFValue* ret) noexcept;
void getServerSocket(Environment* env, UDFContext* context, UDFValue* ret) noexcept;
void setupConnection(Environment* env, UDFContext* context, UDFValue* ret) noexcept;
void readCommand(Environment* env, UDFContext* context, UDFValue* ret) noexcept;
void writeCommand(Environment* env, UDFContext* context, UDFValue* ret) noexcept;
void setupServerFunctions(Environment* env) noexcept {
	socketNameSet = false;
	socketName = "undefined";
	serverSetup = false;
	socketId = 0;
	AddUDF(env, "set-socket-name", "b", 1, 1, "sy", setServerSocket, "setServerSocket", nullptr);
	AddUDF(env, "get-socket-name", "sy", 0, 0, nullptr, getServerSocket, "getServerSocket", nullptr);
	AddUDF(env, "setup-connection", "b", 0, 0, nullptr, setupConnection, "setupConnection", nullptr);
	AddUDF(env, "read-command", "syb", 0, 0, nullptr, readCommand, "readCommand", nullptr);
	//AddUDF(env, "write-command", "syb", 1, UNBOUNDED, "sy;sy;*", readCommand, "readCommand", nullptr);
	//TODO: add shutdown connection
}
int main(int argc, char* argv[]) {
	// make sure this is a common io bus
	auto* mainEnv = CreateEnvironment();
	// install features here
	syn::installExtensions(mainEnv);
	syn::installMemoryBlockTypes(mainEnv);
#ifdef PLATFORM_LINUX
    syn::installAlsaMIDIExtensions(mainEnv);
#endif // end PLATFORM_LINUX
	setupServerFunctions(mainEnv);
	RerouteStdin(mainEnv, argc, argv);
	CommandLoop(mainEnv);
	DestroyEnvironment(mainEnv);
	return -1;
}

void setServerSocket(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
	if (!socketNameSet) {
		UDFValue name;
		if (!UDFFirstArgument(context, LEXEME_BITS, &name)) {
			syn::setBoolean(env, ret, false);
			return;
		}
		socketName = syn::getLexeme(name);
		socketNameSet = true;
		syn::setBoolean(env, ret, true);
	} else {
		syn::setBoolean(env, ret, false);
	}
}

void getServerSocket(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
	if (socketNameSet) {
		syn::setString(env, ret, socketName);
	} else {
		syn::setString(env, ret, "undefined!");
	}
}

void setupConnection(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
	if (socketNameSet && !serverSetup) {
		socketId = socket(AF_UNIX, SOCK_STREAM, 0);
		if (socketId < 0) {
			clips::printRouter(env, STDERR, "Could not open stream socket!\n");
			syn::setBoolean(env, ret, false);
			return;
		}
		server.sun_family = AF_UNIX;
		strcpy(server.sun_path, socketName.c_str());
		if (bind(socketId, (sockaddr*)&server, sizeof(decltype(server)))) {
			clips::printRouter(env, STDERR, "Could not bind stream socket!\n");
			syn::setBoolean(env, ret, false);
			return;
		}
		//TODO: add depth setting tracking
		listen(socketId, 5);
		serverSetup = true;
		syn::setBoolean(env, ret, true);
	} else {
		syn::setBoolean(env, ret, false);
	}
}

void readCommand(Environment* env, UDFContext* context, UDFValue* ret) noexcept {
	if (!serverSetup) {
		syn::setBoolean(env, ret, false);
		return;
	}
	std::stringstream collector;
	auto msgsock = accept(socketId, 0, 0);
	if (msgsock == -1) {
		clips::printRouter(env, STDERR, "error during accept!\n");
		syn::setBoolean(env, ret, false);
		return;
	}
	char buf[1024];
	int rval = 0;
	bool failed = false;
	do {
		bzero(buf, sizeof(buf));
		rval = read(msgsock, buf, 1024);
		if (rval < 0) {
			clips::printRouter(env, STDERR, "error reading stream message");
			syn::setBoolean(env, ret, false);
			failed = true;
			break;
		} else if (rval == 0) {
			break;
		} else {
			std::string tmp(buf);
			collector << tmp;
		}
	} while (rval > 0);
	close(msgsock);
	if (failed) {
		syn::setBoolean(env, ret, false);
	} else {
		auto str = collector.str();
		syn::setString(env, ret, str);
	}
}
