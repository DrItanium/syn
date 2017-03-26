; syn
; Copyright (c) 2013-2017, Joshua Scoggins and Contributors
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defclass MAIN::memory
          "Concept of memory used to store data in it."
          (is-a native-io-device)
          (slot native-type
                (source composite)
                (default memory-space))
          (slot length
                (source composite)
                (default ?NONE))
          (message-handler get-native-construction-args after)
          (message-handler read primary)
          (message-handler write primary))

(defmessage-handler MAIN::memory get-native-construction-args primary
                    ()
                    (create$ (dynamic-get length)))

(defmessage-handler MAIN::memory read primary
                    (?address)
                    (call ?self:native-reference
                          get
                          ?address))

(defmessage-handler MAIN::memory write primary
                    (?address ?value)
                    (call ?self:native-reference
                          set
                          ?address
                          ?value))

(defclass MAIN::unconnected-memory
  "Memory which will not respond to io requests directly!"
  (is-a memory)
  (slot index
        (source composite)
        (default 0))
  (message-handler responds-to primary))

(defmessage-handler MAIN::unconnected-memory responds-to primary
                    (?address)
                    FALSE)
