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
; Comparator.clp - CLIPS wrapper code around the C++ classes
(defclass MAIN::basic-comparator
 (is-a USER)
 (message-handler eq primary)
 (message-handler neq primary))
(defmessage-handler MAIN::basic-comparator eq primary
                    (?a ?b)
                    (eq ?a 
                        ?b))
(defmessage-handler MAIN::basic-comparator neq primary
                    (?a ?b)
                    (neq ?a
                         ?b))

(defclass MAIN::integer-comparator
  (is-a basic-comparator
        external-address-wrapper)
  (slot backing-type
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default comparator))
  (message-handler binary-and primary)
  (message-handler binary-or primary)
  (message-handler binary-nand primary)
  (message-handler binary-nor primary)
  (message-handler unary-not primary)
  (message-handler shift-left primary)
  (message-handler circular-shift-left primary)
  (message-handler shift-right primary)
  (message-handler circular-shift-right primary))

(defmessage-handler MAIN::integer-comparator binary-and primary
                    (?a ?b)
                    (send ?self
                          call
                          binary-and
                          ?a 
                          ?b))
(defmessage-handler MAIN::integer-comparator binary-or primary
                    (?a ?b)
                    (send ?self
                          call
                          binary-or
                          ?a 
                          ?b))
(defmessage-handler MAIN::integer-comparator binary-nand primary
                    (?a ?b)
                    (send ?self
                          call
                          binary-nand
                          ?a 
                          ?b))
(defmessage-handler MAIN::integer-comparator binary-nor primary
                    (?a ?b)
                    (send ?self
                          call
                          binary-nor
                          ?a 
                          ?b))
(defmessage-handler MAIN::integer-comparator unary-not primary
                    (?a) 
                    (send ?self
                          call
                          unary-not
                          ?a))
(defmessage-handler MAIN::integer-comparator shift-left primary
                    (?a ?b)
                    (send ?self
                          call
                          shift-left
                          ?a 
                          ?b))
(defmessage-handler MAIN::integer-comparator circular-shift-left primary
                    (?a ?b)
                    (send ?self
                          call
                          circular-shift-left
                          ?a 
                          ?b))
(defmessage-handler MAIN::integer-comparator shift-right primary
                    (?a ?b)
                    (send ?self
                          call
                          shift-right
                          ?a 
                          ?b))
(defmessage-handler MAIN::integer-comparator circular-shift-right primary
                    (?a ?b)
                    (send ?self
                          call
                          circular-shift-right
                          ?a 
                          ?b))
