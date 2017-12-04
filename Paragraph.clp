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
; A paragraph is a 21-bits of 64-bit words
; Internally, it consists of 32-sentences of 256 words each

(defglobal MAIN
           ?*encyclopedia-paragraph-max-address* = (hex->int 0x1FFFFF)
           ?*encyclopedia-paragraph-size* = (+ ?*encyclopedia-paragraph-max-address*
                                               1))

(deffunction MAIN::address->paragraph-address
             (?address)
             (decode-bits ?address
                          ?*encyclopedia-paragraph-max-address*
                          0))

(defclass MAIN::encyclopedia-paragraph
          "A paragraph is a 21-bits of 64-bit words, it makes up 16-megabytes of space"
          (is-a memory-block)
          (slot capacity
                (source composite)
                (access read-only)
                (storage shared)
                (create-accessor read)
                (default ?*encyclopedia-paragraph-size*)))

(defglobal MAIN
           ?*encyclopedia-page-max-address* = (hex->int 0xFF)
           ?*encyclopedia-page-size* = (+ ?*encyclopedia-page-max-size*
                                          1)
           ?*encyclopedia-page-mask* = (hex->int 0x1FE00000))

(defclass MAIN::encyclopedia-page
          "A page consists of 256 paragaraphs, this class acts as a wrapper around
           the actual memory-blocks. Thus a page takes in a 29 bit address and
           does the correct dispatch"
          (is-a USER)
          (multislot paragraphs
                     (type INSTANCE)
                     (allowed-classes encyclopedia-paragraph)
                     (cardinality 1 256)
                     (default ?NONE))
          (message-handler select-paragraph primary)
          (message-handler read primary)
          (message-handler write primary))

(deffunction MAIN::address->page-address
             (?address)
             (decode-bits ?address
                          ?*encyclopedia-page-mask*
                          21))
                          
(defmessage-handler encyclopedia-page read primary
                    (?address)
                    (bind ?paragraph
                          (send ?self
                                select-paragraph
                                ?address))
                    (if ?paragraph then
                        (send ?paragraph
                              read
                              (address->paragraph-address ?address))))

(defmessage-handler encyclopedia-page write primary
                    (?address ?value)
                    (bind ?paragraph
                          (send ?self
                                select-paragraph
                                ?address))
                    (if ?paragraph then
                        (send ?paragraph
                              write
                              (address->paragraph-address ?address)
                              ?value)))
                        
(defmessage-handler encyclopedia-page select-paragraph primary
                    "Common code for extracting the target paragraph"
                    (?address)
                    ; since clips is one indexed we have to modify it slightly
                    (bind ?length
                          (length$ (dynamic-get paragraphs)))
                    (bind ?page-address
                          (+ (address->page-address ?address)
                             1))
                    (if (<= ?page-address
                            ?length) then
                        (nth$ ?page-address
                              (dynamic-get paragraphs))))

(defglobal MAIN
           ?*encyclopedia-section-max-address* = (hex->int 0xFF)
           ?*encyclopedia-section-size* = (+ ?*encyclopedia-section-max-size*
                                          1)
           ?*encyclopedia-section-mask* = (hex->int 0x1FE0000000))

(deffunction MAIN::address->section-address
             (?address)
             (decode-bits ?address
                          ?*encyclopedia-section-mask*
                          29))
(defclass MAIN::encyclopedia-section
          "A section consists of 256 pages"
          (is-a USER)
          (multislot pages
                     (type INSTANCE)
                     (allowed-classes encyclopedia-page)
                     (cardinality 1 256)
                     (default ?NONE))
          (message-handler select-page primary)
          (message-handler read primary)
          (message-handler write primary))

(defmessage-handler encyclopedia-section read primary
                    (?address)
                    (bind ?page
                          (send ?self
                                select-page
                                ?address))
                    (if ?page then
                        (send ?page
                              read
                              ?address)))

(defmessage-handler encyclopedia-section write primary
                    (?address ?value)
                    (bind ?page
                          (send ?self
                                select-page
                                ?address))
                    (if ?page then
                        (send ?page
                              write
                              ?address
                              ?value)))
                        
(defmessage-handler encyclopedia-section select-page primary
                    "Common code for extracting the target page"
                    (?address)
                    ; since clips is one indexed we have to modify it slightly
                    (bind ?length
                          (length$ (dynamic-get pages)))
                    (bind ?section-address
                          (+ (address->section-address ?address)
                             1))
                    (if (<= ?section-address
                            ?length) then
                        (nth$ ?section-address
                              (dynamic-get pages))))
