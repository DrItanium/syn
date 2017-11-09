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
; rng.clp - simple random number generator interface
(defclass MAIN::random-number-generator
  (is-a memory-map-entry)
  (slot parent
        (source composite)
        (default-dynamic FALSE))
  (slot seed
        (type INTEGER)
        (visibility public)
        (storage local)
        (default-dynamic (integer (time))))
  (slot current-value
        (type INTEGER)
        (visibility public)
        (create-accessor read)
        (storage local))
  (message-handler init after)
  (message-handler put-seed after)
  (message-handler compute-last-address primary)
  (message-handler read primary)
  (message-handler write primary))

(defmessage-handler MAIN::random-number-generator init after
                    ()
                    (seed (dynamic-get seed))
                    (dynamic-put current-value
                                 (random)))

(defmessage-handler MAIN::random-number-generator put-seed after
                    (?seed)
                    (seed ?seed))

(defmessage-handler MAIN::random-number-generator compute-last-address primary
                    (?addr)
                    (+ ?addr 1))
(defmessage-handler MAIN::random-number-generator read primary
                    (?address)
                    (switch (- ?address
                               (dynamic-get base-address))
                            ; random number port
                            (case 0 then
                              (bind ?result
                                    (dynamic-get current-value))
                              (dynamic-put current-value
                                           (random))
                              ?result)
                            ; seed port
                            (case 1 then
                              (dynamic-get seed))
                            (default 0)))
(defmessage-handler MAIN::random-number-generator write primary
                    (?address ?value)
                    (switch (- ?address
                               (dynamic-get base-address))
                            ; random number port, skip the current entry
                            (case 0 then
                              (dynamic-put current-value
                                           (random)))
                            (case 1 then
                              (dynamic-put seed
                                           ?value)
                              (seed ?value))
                            (default 0)))
