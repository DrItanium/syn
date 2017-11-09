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
; RunLevelController.clp - provides basic functionality for system runlevels etc

(defclass MAIN::runlevel-controller
  "An abstraction layer over basic system runlevels and such things!"
  (is-a memory-map-entry)
  (slot parent
        (source composite)
        (default-dynamic FALSE))
  (message-handler compute-last-address primary)
  (message-handler read primary)
  (message-handler write primary))
(defmessage-handler MAIN::runlevel-controller compute-last-address primary
                    (?addr)
                    (+ ?addr 1))
(defmessage-handler MAIN::runlevel-controller write primary
                    (?address ?value)
                    (switch (- ?address
                               (dynamic-get base-address))
                            (case 0 then
                              (halt)
                              0)
                            (case 1 then
                              (assert (should shutdown))
                              0)
                            (default
                              0)))
(defmessage-handler MAIN::runlevel-controller read primary
                    (?addr)
                    0)
