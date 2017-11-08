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
; mmu.clp - declares a simple memory management unit
(defclass MAIN::memory-map-entry
  (is-a thing
        device)
  (role concrete)
  (pattern-match reactive)
  (slot base-address
        (type INTEGER)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot last-address
        (type INTEGER)
        (storage local)
        (visibility public))
  (message-handler put-base-address after)
  (message-handler init after)
  (message-handler read primary)
  (message-handler write primary))
(defmessage-handler MAIN::memory-map-entry put-base-address after
                    (?addr)
                    (bind ?self:last-address
                          (send ?self
                                compute-last-address
                                ?addr)))
(defmessage-handler MAIN::memory-map-entry compute-last-address primary
                    (?addr)
                    (+ ?addr
                       (send (dynamic-get parent)
                             get-last-address)))
(defmessage-handler MAIN::memory-map-entry init after
                    ()
                    (bind ?self:last-address
                          (send ?self
                                compute-last-address
                                ?self:base-address)))

(defmessage-handler MAIN::memory-map-entry read primary
                    (?addr)
                    (send (dynamic-get parent)
                          read
                          (- ?addr
                             ?self:base-address)))
(defmessage-handler MAIN::memory-map-entry write primary
                    (?address ?value)
                    (send (dynamic-get parent)
                          write
                          (- ?address
                             ?self:base-address)
                          ?value))
