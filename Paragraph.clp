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
; the encyclopedia scheme is a 72 bit byte encoding / 69 bit word addressing scheme
; It defines several levels of delination from the sentence up through the book. While
; higher memory concepts are possible (for instance 56 more bits to get to 128-bits wide
; which could be called a set). There is no way I will ever see this in my lifetime nor
; I think ever as that is a lot of ram!!!

; See doc/iris64/Memory Layout.txt

(defglobal MAIN
           ?*encyclopedia-sentence-max-address* = (hex->int 0xFF)
           ?*encyclopedia-sentence-size* = (+ ?*encyclopedia-sentence-max-address* 
                                              1)
           ?*encyclopedia-sentence-mask* = ?*encyclopedia-sentence-max-address*)
(defclass MAIN::encyclopedia-sentence
  "A set of 256 words which is the smallest unit, this would be known as a block in
  other architectures! This translates to 2k per sentence"
  (is-a memory-block)
  (slot capacity
        (source composite)
        (access read-only)
        (storage shared)
        (create-accessor read)
        (default ?*encyclopedia-sentence-size*))
  (message-handler read primary)
  (message-handler write primary))
(deffunction MAIN::address->sentence-address
             "extract the lowest eight bits"
             (?address)
             (decode-bits ?address
                          ?*encyclopedia-sentence-mask*
                          0))

    
(defmessage-handler encyclopedia-sentence read primary
                    (?address)
                    (override-next-handler (address->sentence-address ?address)))
(defmessage-handler encyclopedia-sentence  write primary
                    (?address ?value)
                    (override-next-handler (address->sentence-address ?address)
                                           ?value))




(defclass MAIN::encyclopedia-container
  "The higher level containers for the encyclopedia concept which do not 
  directly interface with native memory blocks"
  (is-a device)
  (role concrete)
  (pattern-match reactive)
  (multislot children
             (type INSTANCE)
             (visibility public)
             (storage local)
             (default ?NONE))
  (message-handler select-child primary)
  (message-handler compute-child-address primary)
  (message-handler read primary)
  (message-handler write primary))
(defmessage-handler encyclopedia-container read primary
                    (?address)
                    (bind ?child
                          (send ?self
                                select-child
                                ?address))
                    (if ?child then
                      (send ?child
                            read
                            ?address)))
(defmessage-handler encyclopedia-container write primary
                    (?address ?value)
                    (bind ?child
                          (send ?self
                                select-child
                                ?address))
                    (if ?child then
                      (send ?child
                            write
                            ?address
                            ?value)))

(defmessage-handler encyclopedia-container select-child primary
                    (?address)
                    ; perform the address one-index conversion here
                    (bind ?conv
                          (+ (send ?self
                                   compute-child-address
                                   ?address)
                             1))

                    (if (<= ?conv 
                            (length$ (dynamic-get children))) then
                      (nth$ ?conv
                            (dynamic-get children))))

; A paragraph has 32 sentences in it for a total of 64k per paragraph
(defglobal MAIN
           ?*encyclopedia-paragraph-max-address* = (hex->int 0x1F)
           ?*encyclopedia-paragraph-size* = (+ ?*encyclopedia-paragraph-max-address*
                                               1)
           ?*encyclopedia-paragraph-mask* = (hex->int 0x1f00))



(deffunction MAIN::address->paragraph-address
             (?address)
             (decode-bits ?address
                          ?*encyclopedia-paragraph-mask*
                          8))

(defclass MAIN::encyclopedia-paragraph
  "A paragraph is 5 bits worth of sentences which make up 64k of space"
  (is-a encyclopedia-container)
  (multislot children
             (source composite)
             (allowed-classes encyclopedia-sentence)
             (cardinality 1 32)
             (default ?NONE))
  (message-handler compute-child-address primary))

(defmessage-handler encyclopedia-paragraph compute-child-address primary
                    (?address)
                    (decode-bits ?address
                                 ?*encyclopedia-paragraph-mask*
                                 8))

(defglobal MAIN
           ?*encyclopedia-page-max-address* = (hex->int 0xFF)
           ?*encyclopedia-page-size* = (+ ?*encyclopedia-page-max-address*
                                          1)
           ?*encyclopedia-page-mask* = (hex->int 0x1FE000))

(defclass MAIN::encyclopedia-page
  "A page consists of 256 pages which is 16 megabytes"
  (is-a encyclopedia-container)
  (multislot children 
             (source composite)
             (allowed-classes encyclopedia-paragraph)
             (cardinality 1 256)
             (default ?NONE))
  (message-handler compute-child-address primary))

(defmessage-handler encyclopedia-page compute-child-address primary
                    (?address)
                    (decode-bits ?address
                                 ?*encyclopedia-page-mask*
                                 13))

(defglobal MAIN
           ?*encyclopedia-section-max-address* = (hex->int 0xFF)
           ?*encyclopedia-section-size* = (+ ?*encyclopedia-section-max-address*
                                             1)
           ?*encyclopedia-section-mask* = (hex->int 0x1FE00000))

(deffunction MAIN::address->section-address
             (?address)
             (decode-bits ?address
                          ?*encyclopedia-section-mask*
                          21))

(defclass MAIN::encyclopedia-section
  "A section consists of 256 pages which is 4 gigabytes!"
  (is-a encyclopedia-container)
  (multislot children
             (source composite)
             (cardinality 1 256)
             (allowed-classes encyclopedia-page)
             (default ?NONE))
  (message-handler compute-child-address primary))

(defmessage-handler encyclopedia-section compute-child-address primary
                    (?address)
                    (address->section-address ?address))

(defglobal MAIN
           ?*encyclopedia-chapter-max-address* = (hex->int 0xFFFFF)
           ?*encyclopedia-chapter-size* = (+ ?*encyclopedia-chapter-max-address*
                                             1)
           ?*encyclopedia-chapter-mask* = (hex->int 0x1ffffe0000000))

(defclass MAIN::encyclopedia-chapter
  "An encyclopedia chapter consists of 2^20 sections which is 4 petabytes!!!!
  To put this in perspective, one chapter has the same storage capacity as the
  theoretical 52-bit maximum on x86_64 machines!!!!"
  (is-a encyclopedia-container)
  (multislot children
             (source composite)
             (cardinality 1 1048576)
             (allowed-classes encyclopedia-section)
             (default ?NONE))
  (message-handler compute-child-address primary))
(defmessage-handler MAIN::encyclopedia-chapter compute-child-address primary
                    (?address)
                    (decode-bits ?address
                                 ?*encyclopedia-chapter-mask*
                                 29))
; The highest level of the current addressing scheme is the book, there are 256 possible 
; books in the current addressing scheme and that totals 4096 exabytes!!! I do not see the point
; of actually implementing support for this as I'll never see it! I may eat my words but holy fuck,
; that is a lot of memory. I'll have to get creative anyway with the current clips implementation anyway
; to get values beyond 64-bit signed. For my emulation purposes, even having a full chapter is nuts!!!

(defclass MAIN::iris64-encyclopedia
  "A child of the encyclopedia-chapter, it is the mmu interface that an iris64 cpu would use, it is 
  hard masked to at most 4 full sections or 16 gigabytes!"
  (is-a encyclopedia-chapter)
  (multislot children
             (source composite)
             (cardinality 1 4))
  (message-handler compute-child-address primary))

(defmessage-handler MAIN::iris64-encyclopedia compute-child-address primary
                    (?address)
                    ; TODO: add support for handling the IO section and its magic :D
                    ; mask down to 4 sections
                    (decode-bits ?address
                                 (hex->int 0x60000000)
                                 29))
