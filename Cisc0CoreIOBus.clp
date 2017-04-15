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
(batch* IODevice.clp)
(batch* keyboard-device.clp)
(batch* random-number-generator-device.clp)
(batch* memory-device.clp)


(defglobal MAIN
           ?*main-memory-size* = (hex->int 0x01000000)
           ?*io-bus-start* = (hex->int 0xFF000000))
(deffunction MAIN::in-io-space
             (?index)
             (+ ?*io-bus-start*
                ?index))
;------------------------------------------------------------------------------
; IO Controller Mapping Layout
;------------------------------------------------------------------------------
; 0x00000000 - 0x00FFFFFF : Main Memory
; 0xFE000000 - 0xFEFFFFFF : BOOT ROM
; 0xFF000000 - 0xFFFFFFFF : IO SPACE
;------------------------------------------------------------------------------
; currently, the memory space is comprised of 64-bit signed words!
; This means we are wasting a ton of space per word for this architecture.
; This can lead to some cool ideas in the future though
(definstances MAIN::io-map
              ([/dev/ram0] of memory
                             (index 0)
                             (length ?*main-memory-size*))
              ([/dev/ram1] of memory
                            (index ?*main-memory-size*)
                            (length ?*main-memory-size*))
              ([/dev/ram2] of memory
                            (index (* 2 ?*main-memory-size*))
                            (length ?*main-memory-size*))
              ([/dev/ram2] of memory
                            (index (* 3 ?*main-memory-size*))
                            (length ?*main-memory-size*))
              ([/dev/rom0] of memory
                           (index (hex->int 0xFE000000))
                           (length ?*main-memory-size*))
              ([/dev/stdin-out] of stdin/out-device
                          (index (in-io-space 0)))
              ([/dev/rng0] of random-number-generator
                           (index (in-io-space 1))))

(batch* io-bus-execution-loop.clp)

(reset)

