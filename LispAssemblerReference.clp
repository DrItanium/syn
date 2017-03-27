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

(defclass MAIN::assembler
          (is-a USER)
          (slot assembler-type
                (type SYMBOL)
                (storage local)
                (visibility public)
                (default ?NONE))
          (slot native-reference
                (type EXTERNAL-ADDRESS)
                (storage local)
                (visibility public))
          (message-handler init after)
          (message-handler parse primary)
          (message-handler resolve primary)
          (message-handler get primary))

(defmessage-handler MAIN::assembler init after
                    ()
                    (bind ?self:native-reference
                          (new (dynamic-get assembler-type))))

(defmessage-handler MAIN::assembler parse primary
                    (?line)
                    (call ?self:native-reference
                          parse
                          ?line))

(defmessage-handler MAIN::assembler resolve primary
                    ()
                    (call ?self:native-reference
                          resolve))

(defmessage-handler MAIN::assembler get primary
                    ()
                    (call ?self:native-reference
                          get))
