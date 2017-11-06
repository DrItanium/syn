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
           (export ?ALL))
(defgeneric midi::make-message)
(defgeneric midi::make-control-byte)
(defgeneric midi::seven-bit-value)
(defgeneric midi::key->note-number
            "Converts a lookup table of keys that a human understands to the midi note number")
(defgeneric midi::note-number->key
            "Converts a note-number to the corresponding human readable key")
(defgeneric midi::gm-patch-name->program-number)
(defgeneric midi::program-number->gm-patch-name)
; these functions form the midi messages only, you still have to send them off
(defgeneric midi::note-off)
(defgeneric midi::note-on)
(defgeneric midi::polyphonic-key-press
            "also known as aftertouch")
(defgeneric midi::control-change
            "sent when a controller value changes. Controller include devices such as pedals and levers")
(defgeneric midi::program-change
            "Changes the patch number")
(defgeneric midi::pitch-wheel
            "changes a notes pitch up or down")
(defgeneric midi::channel-pressure
            "Controls how hard a given key is being pressed, keyboards constantly send this information while one is playing")
(defgeneric midi::change-patch)
; channel mode messages
(defgeneric midi::channel-mode
            "Same as the control change but implements mode control and special
            message by using reserved controller numbers 120-127. This is the raw
            interface and not meant to be visible")

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

(defmethod midi::make-control-byte
  ((?cmd INTEGER)
   (?channel INTEGER))
  (binary-or (left-shift ?cmd
                         4)
             (binary-and ?channel
                         (hex->int 0x0f))))

(defmethod midi::seven-bit-value
  ((?value INTEGER))
  (decode-bits ?value
               (hex->int 0x7f)
               0))

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
(defmethod midi::control-change
  ((?channel INTEGER)
   (?c INTEGER)
   (?v INTEGER))
  (make-message (hex->int 0xb)
                ?channel
                (map seven-bit-value 
                     ?c ?v)))

(defmethod midi::bank-select-msb
  ((?channel INTEGER)
   (?bank INTEGER))
  (control-change ?channel
                  0
                  ?bank))
(defmethod midi::bank-select-lsb
  ((?channel INTEGER)
   (?bank INTEGER))
  (control-change ?channel
                  32
                  ?bank))

(defmethod midi::bank-select
  ((?channel INTEGER)
   (?msb INTEGER)
   (?lsb INTEGER
         (= ?current-argument 0)))
  (bank-select-msb ?channel
                   ?msb))
(defmethod midi::bank-select
  ((?channel INTEGER)
   (?msb INTEGER)
   (?lsb INTEGER
         (<> ?current-argument 0)))
  (create$ (bank-select-msb ?channel
                            ?msb)
           (bank-select-lsb ?channel
                            ?lsb)))


(defmethod midi::program-change
  ((?channel INTEGER)
   (?program INTEGER))
  (make-message (hex->int 0xc)
                ?channel
                (seven-bit-value ?program)))


(defmethod midi::change-patch
  ((?channel INTEGER)
   (?msb INTEGER)
   (?lsb INTEGER)
   (?patch INTEGER))
  (create$ (bank-select ?channel
                        ?msb
                        ?lsb)
           (program-change ?channel
                           ?patch)))


(defmethod midi::note-off
  ((?channel INTEGER)
   (?note-number INTEGER)
   (?velocity INTEGER))
  (make-message (hex->int 0x8)
                ?channel
                (map seven-bit-value 
                     ?note-number
                     ?velocity)))
(defmethod midi::note-on
  ((?channel INTEGER)
   (?note-number INTEGER)
   (?velocity INTEGER))
  (make-message (hex->int 0x9)
                ?channel
                (map seven-bit-value
                     ?note-number
                     ?velocity)))
(defmethod midi::polyphonic-key-pressure
  ((?channel INTEGER)
   (?note INTEGER)
   (?pressure INTEGER))
  (make-message (hex->int 0xa)
   ?channel
   (map seven-bit-value
    ?note
    ?pressure)))


(defmethod midi::pitch-wheel
  ((?channel INTEGER)
   (?position INTEGER))
  ; this is a little odd, since this is an emulation of a pitch wheel we have to actually carve this value up
  ; into two seven bit chunks
  (make-message (hex->int 0xe)
                ?channel
                (map seven-bit-value
                     ?position ; lower seven bits of the number
                     (right-shift ?position ; upper seven bits of the number
                                  7))))

(defmethod midi::center-pitch-wheel
  ((?channel INTEGER))
  (pitch-wheel ?channel
               (hex->int 0x2000)))

(defmethod midi::channel-pressure
  ((?channel INTEGER)
   (?pressure INTEGER))
  (make-message (hex->int 0xd)
                ?channel
                (seven-bit-value ?pressure)))


(deffunction midi::construct-octave
             (?octave-number)
             (bind ?output
                   (create$))
             (progn$ (?key (create$ c c#
                                    d d# e
                                    f f#
                                    g g#
                                    a a# b))
                     (bind ?output
                           ?output
                           (sym-cat ?key
                                    ?octave-number)))
             ?output)
(defglobal midi
           ?*piano-gm-patches* = (create$ "acoustic grand"
                                          "bright acoustic"
                                          "electric grand"
                                          honky-tonk
                                          "electric piano 1"
                                          "electric piano 2"
                                          harpsichord
                                          clavinet)
           ?*chromatic-percussion-gm-patches* = (create$ celesta
                                                         glockenspiel
                                                         "music box"
                                                         vibraphone
                                                         marimba
                                                         xylophone
                                                         "tubular bells"
                                                         dulcimer)
           ?*organ-gm-patches* = (create$ "drawbar organ"
                                          "percussive organ"
                                          "rock organ"
                                          "church organ"
                                          "reed organ"
                                          accordian
                                          harmonica
                                          "tango accordian")
           ?*guitar-gm-patches* = (create$ "nylon string guitar"
                                           "steel string guitar"
                                           "electric jazz guitar"
                                           "electric clean guitar"
                                           "electric muted guitar"
                                           "overdriven guitar"
                                           "distortion guitar"
                                           "guitar harmonics")
           ?*bass-gm-patches* = (create$ "acoustic bass"
                                         "electric bass(finger)"
                                         "electric bass(pick)"
                                         "fretless bass"
                                         "slap bass 1"
                                         "slap bass 2"
                                         "synth bass 1"
                                         "synth bass 2")
           ?*solo-strings-gm-patches* = (create$ violin
                                                 viola
                                                 cello
                                                 contrabass
                                                 "tremolo strings"
                                                 "pizzicato strings"
                                                 "orchestral strings"
                                                 timpani)
           ?*ensemble-gm-patches* = (create$ "string ensemble 1"
                                             "string ensemble 2"
                                             "synthstrings 1"
                                             "synthstrings 2"
                                             "choir aahs"
                                             "voice oohs"
                                             "synth voice"
                                             "orchestra hit")
           ?*brass-gm-patches* = (create$ trumpet
                                          trombone
                                          tuba
                                          "muted trumpet"
                                          "french horn"
                                          "brass section"
                                          "synthbrass 1"
                                          "synthbrass 2")
           ?*reed-gm-patches* = (create$ "soprano sax"
                                         "alto sax"
                                         "tenor sax"
                                         "baritone sax"
                                         oboe
                                         "english horn"
                                         bassoon
                                         clairinet)
           ?*pipe-gm-patches* = (create$ piccolo
                                         flute
                                         recorder
                                         "pan flute"
                                         "blown bottle"
                                         shakuhachi
                                         whistle
                                         ocarina)
           ?*synth-lead-gm-patches* = (create$ square
                                               sawtooth
                                               calliope
                                               chiff
                                               charang
                                               voice
                                               fifths
                                               bass+lead)
           ?*synth-pad-gm-patches* = (create$ "new age"
                                              warm
                                              polysynth
                                              choir
                                              bowed
                                              metallic
                                              halo
                                              sweep)
           ?*synth-effects-gm-patches* = (create$ rain
                                                  soundtrack
                                                  crystal
                                                  atmosphere
                                                  brightness
                                                  goblins
                                                  echoes
                                                  sci-fi)
           ?*ethnic-gm-patches* = (create$ sitar
                                           banjo
                                           shamisen
                                           koto
                                           kalimba
                                           bagpipe
                                           fiddle
                                           shanai)
           ?*percussive-gm-patches* = (create$ "tinkle bell"
                                               agogo
                                               "steel drums"
                                               woodblock
                                               "taiko drum"
                                               "melodic tom"
                                               "synth drum"
                                               "reverse cymbal")
           ?*sound-effects-gm-patches* = (create$ "guitar fret noise"
                                                  "breath noise"
                                                  seashore
                                                  "bird tweet"
                                                  "telephone ring"
                                                  helicopter
                                                  applause
                                                  gunshot)
           ; DON'T REORDER THIS, IT IS TO THE GM SPEC, thanks midi.teragonaudio.com/tutr/gm.htm
           ?*gm-patches* = (create$ ?*piano-gm-patches*
                                    ?*chromatic-percussion-gm-patches*
                                    ?*organ-gm-patches*
                                    ?*guitar-gm-patches*
                                    ?*bass-gm-patches*
                                    ?*solo-strings-gm-patches*
                                    ?*ensemble-gm-patches*
                                    ?*brass-gm-patches*
                                    ?*reed-gm-patches*
                                    ?*pipe-gm-patches*
                                    ?*synth-lead-gm-patches*
                                    ?*synth-pad-gm-patches*
                                    ?*synth-effects-gm-patches*
                                    ?*ethnic-gm-patches*
                                    ?*percussive-gm-patches*
                                    ?*sound-effects-gm-patches* ; last patch group
                                    )
           ?*key-to-note-table* = (create$ (construct-octave 0)
                                           (construct-octave 1)
                                           (construct-octave 2)
                                           (construct-octave 3)
                                           (construct-octave 4)
                                           (construct-octave 5)
                                           (construct-octave 6)
                                           (construct-octave 7)
                                           (construct-octave 8)
                                           (construct-octave 9)
                                           c10 c#10
                                           d10 d#10 e10
                                           f10 f#10 g10))
(defmethod midi::key->note-number
  ((?key LEXEME))
  (bind ?result
        (member$ (lowcase ?key)
                 ?*key-to-note-table*))
  (if ?result then
    (- ?result 1)))
(defmethod midi::note-number->key
  ((?note INTEGER))
  (nth$ (+ (seven-bit-value ?note)
           1)
        ?*key-to-note-table*))


(defmethod midi::gm-patch-name->program-number
  ((?name LEXEME))
  (bind ?result
        (member$ (lowcase ?name)
                 ?*gm-patches*))
  (if ?result then
    (- ?result 1)))
(defmethod midi::program-number->gm-patch-name
  ((?program-number INTEGER))
  (nth$ (+ (seven-bit-value ?program-number)
           1)
        ?*gm-patches*))

(defmethod midi::note-on
  ((?channel INTEGER)
   (?note LEXEME)
   (?velocity INTEGER))
  (note-on ?channel
           (key->note-number ?note)
           ?velocity))

(defmethod midi::note-off
  ((?channel INTEGER)
   (?note LEXEME)
   (?velocity INTEGER))
  (note-off ?channel
            (key->note-number ?note)
            ?velocity))

(defmethod midi::polyphonic-key-pressure
  ((?channel INTEGER)
   (?note LEXEME)
   (?pressure INTEGER))
  (polyphonic-key-pressure ?channel
                           (key->note-number ?note)
                           ?pressure))

(defmethod midi::program-change
  ((?channel INTEGER)
   (?program LEXEME))
  (program-change ?channel
                  (gm-patch-name->program-number ?program)))


(defmethod midi::change-patch
  ((?channel INTEGER)
   (?msb INTEGER)
   (?lsb INTEGER)
   (?patch LEXEME))
  (change-patch ?channel
                ?msb
                ?lsb
                (gm-patch-name->program-number ?patch)))
(defmethod midi::make-message
  ((?operation INTEGER)
   (?channel INTEGER)
   (?bytes MULTIFIELD))
  (create$ (make-control-byte ?operation
                              ?channel)
           ?bytes))

(defmethod midi::make-message
  ((?operation INTEGER)
   (?channel INTEGER)
   $?bytes)
  (make-message ?operation
                ?channel
                ?bytes))
