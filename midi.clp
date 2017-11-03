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
;
; midi.clp - front end to the exposed midi functionality
; Doc strings taken from the specs on midi.org
(defmodule midi
           (import cortex
                   ?ALL)
           (export ?ALL))
; these functions form the midi messages only, you still have to send them off
(defgeneric midi::note-off)
(defgeneric midi::note-on)
(defgeneric midi::polyphonic-key-press)
(defgeneric midi::control-change)
; channel mode messages
(defgeneric midi::channel-mode
            "Same as the control change but implements mode control and special
            message by using reserved controller numbers 120-127. This is the raw
            interface and not meant to be visible")

(defmethod midi::all-sound-off
  "All oscillators will turn off, and their volume envelopes are set ot zero as soon as possible"
  ()
  (channel-mode 120
                0))
(defmethod midi::reset-all-controllers
  "All controller values are reset to their default values"
  ((?x INTEGER))
  (channel-mode 121
                ?x))
(defmethod midi::local-control-off
  "All devices on a given channel will respond only to data recieved over MIDI. Played data, etc. will be ignored"
  ()
  (channel-mode 122
                0))
(defmethod midi::local-control-on
  "restore the functionality of the normal controllers"
  ()
  (channel-mode 122
                127))
; NOTE: there are more under this segment but I'm not sure what they do nor am I comfortable adding them right now
; system common messages
(defgeneric midi::system-exclusive
            "Allows manufacturers to create their own messages (e.g. bulk
             dumps, patch parameters, and other non-spec data) and provides
            a mechanism for creating additional MIDI specification messages")
(defgeneric midi::time-code-quarter-frame)
(defgeneric midi::song-position-pointer
            "This is an internal 14-bit register that holds the number of midi
            beats (1 beat = six midi clocks) since the start of the song")
(defgeneric midi::song-select
            "Specifies which sequence or song is to be played")
(defgeneric midi::tune-request
            "upon receiving a tune request, all analog synthesizers should tune
            their oscillators")
(defgeneric midi::end-of-exclusive
            "Used to terminate a system exclusive dump")

; system real messages
(defgeneric midi::timing-clock
            "sent 24-times per quarter note when synchronization is required")

(defgeneric midi::start-sequence
            "Start the current sequence playing. Follow this message with timing clocks")
(defgeneric midi::continue-sequence
            "Continue at the point the sequence was stopped")
(defgeneric midi::stop-sequence
            "Stop the current sequence")

(defgeneric midi::reset-device
            "Reset all recievers in the system to power-up status.
            This should be used sparingly, preferable under manual control.
            In particular, it should not be sent on power-up")
