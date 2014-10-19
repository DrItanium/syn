/*
electron
Copyright (c) 2013, Joshua Scoggins 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#ifndef _CORE_USER_SETUP_H
#define _CORE_USER_SETUP_H


/* Operating System Specific Defines */
#ifdef _WIN32
#define OS_NAME "Nt"
#ifdef _WIN64 
   #define OS_WIN64 1
   #define OS_WIN32 0
#else
   #define OS_WIN32 1
   #define OS_WIN64 0
#endif
#else
#define OS_WIN32 0
#define OS_WIN64 0
#endif

#if defined(__CYGWIN__)
#define OS_CYGWIN 1
#define OS_NAME "Cygwin"
#else
#define OS_CYGWIN 0
#endif

#ifdef __linux__
#if !(defined(__ANDROID__) || defined(__android__))
#define OS_LINUX 1
#define OS_ANDROID 0
#define OS_NAME "Linux"
#else
#define OS_ANDROID 1
#define OS_LINUX 0
#define OS_NAME "Android"
#endif
#endif

#ifdef __FreeBSD__
#define OS_FREEBSD 1
#define OS_NAME "FreeBSD"
#else
#define OS_FREEBSD 0
#endif

#ifdef __OpenBSD__
#define OS_OPENBSD 1
#define OS_NAME "OpenBSD"
#else
#define OS_OPENBSD 0
#endif

#ifdef __NetBSD__
#define OS_NETBSD 1
#define OS_NAME "NetBSD"
#else
#define OS_NETBSD 0
#endif

#ifdef __DragonFlyBSD__
#define OS_DRAGONFLYBSD 1
#define OS_NAME "DragonFlyBSD"
#else
#define OS_DRAGONFLYBSD 0
#endif

#ifdef __APPLE__
#include "TargetConditionals.h"
   #if TARGET_OS_MAC
       #define OS_OSX 1
       #define OS_IOS 0
       #define OS_APPLE_UNKNOWN 0
       #define OS_NAME "OSX"
   #elif TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR
       #define OS_IOS 1
       #define OS_OSX 0
       #define OS_APPLE_UNKNOWN 0
       #define OS_NAME "iOS"
   #else
       #define OS_IOS 0
       #define OS_OSX 0
       #define OS_APPLE_UNKNOWN 1
       #define OS_NAME "AppleUnknown"
   #endif
#else
       #define OS_IOS 0
       #define OS_OSX 0
       #define OS_APPLE_UNKNOWN 0
#endif

   

#if (! OS_WIN32) && (! OS_WIN64) && (! OS_LINUX) && (! OS_FREEBSD) && \
    (! OS_OSX) && (! OS_IOS) && (! OS_APPLE_UNKNOWN) && (! OS_OPENBSD) && \
    (! OS_NETBSD) && (! OS_DRAGONFLYBSD) && (! OS_CYGWIN) && (! OS_ANDROID) 
#define OS_UNKNOWN 1
#define OS_NAME "Unknown"
#else
#define OS_UNKNOWN 0
#endif

#endif
