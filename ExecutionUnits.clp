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
; ExecutionUnits.clp - CLIPS wrapper code around the FPU and ALU types
(deffunction MAIN::default-undefined-operation-handler
             ()
             0)
(defclass MAIN::basic-execution-unit
  (is-a external-address-wrapper)
  (slot undefined-operation-handler
        (type SYMBOL)
        (storage local)
        (visbility public)
        (default-dynamic default-undefined-operation-handler))
  (message-handler put-undefined-operation-handler after))

(defmessage-handler MAIN::basic-execution-unit put-undefined-operation-handler after
                    (?sym)
                    (call (dynamic-get backing-store)
                          set-undefined-operation-handler
                          (if (not ?sym) then
                            default-undefined-operation-handler
                            else
                            ?sym)))


(defclass MAIN::basic-combinatorial-logic-unit
  (is-a basic-execution-unit)
  (message-handler add primary)
  (message-handler sum primary)
  (message-handler mul primary)
  (message-handler div primary))

(defmessage-handler MAIN::basic-combinatorial-logic-unit add primary 
                    (?a ?b) 
                    (call (dynamic-get backing-store)
                          add
                          ?a
                          ?b))
(defmessage-handler MAIN::basic-combinatorial-logic-unit sub primary 
                    (?a ?b) 
                    (call (dynamic-get backing-store)
                          sub
                          ?a
                          ?b))
(defmessage-handler MAIN::basic-combinatorial-logic-unit mul primary 
                    (?a ?b) 
                    (call (dynamic-get backing-store)
                          mul
                          ?a
                          ?b))
(defmessage-handler MAIN::basic-combinatorial-logic-unit div primary 
                    (?a ?b) 
                    (call (dynamic-get backing-store)
                          div
                          ?a
                          ?b))

(defclass MAIN::fpu
  (is-a basic-combinatorial-logic-unit)
  (slot backing-type
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default fpu))
  (message-handler sqrt primary))

(defmessage-handler MAIN::fpu sqrt primary
                    (?a)
                    (call (dynamic-get backing-store)
                          sqrt
                          ?a))

(deffunction MAIN::default-divide-by-zero-handler
             ()
             0)
(defclass MAIN::alu
  (is-a basic-combinatorial-logic-unit)
  (slot backing-type
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default alu))
  (slot divide-by-zero-handler
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic default-divide-by-zero-handler))
  (message-handler init around)
  (message-handler div primary)
  (message-handler rem primary)
  (message-handler binary-and primary)
  (message-handler binary-or primary)
  (message-handler binary-xor primary)
  (message-handler binary-nand primary)
  (message-handler unary-not primary)
  (message-handler circular-shift-left primary)
  (message-handler circular-shift-right primary)
  (message-handler shift-left primary)
  (message-handler shift-right primary))


(defmessage-handler MAIN::alu shift-left primary (?a ?b) (call (dynamic-get backing-store) shift-left ?a ?b))
(defmessage-handler MAIN::alu circular-shift-left primary (?a ?b) (call (dynamic-get backing-store) circular-shift-left ?a ?b))
(defmessage-handler MAIN::alu shift-right primary (?a ?b) (call (dynamic-get backing-store) shift-right ?a ?b))
(defmessage-handler MAIN::alu circular-shift-right primary (?a ?b) (call (dynamic-get backing-store) circular-shift-right ?a ?b))
(defmessage-handler MAIN::alu binary-and primary (?a ?b) (call (dynamic-get backing-store) binary-and ?a ?b))
(defmessage-handler MAIN::alu binary-or primary (?a ?b) (call (dynamic-get backing-store) binary-or ?a ?b))
(defmessage-handler MAIN::alu binary-nand primary (?a ?b) (call (dynamic-get backing-store) binary-nand ?a ?b))
(defmessage-handler MAIN::alu unary-not primary (?a) (call (dynamic-get backing-store) unary-not ?a))

(defmessage-handler MAIN::alu div primary
                    (?a ?b $?handler)
                    (call (dynamic-get backing-store)
                          div
                          ?a
                          ?b
                          (if (= (length$ ?handler) 0) then
                           ?self:divide-by-zero-handler
                           else
                           (nth$ 1 
                            ?handler))))

(defmessage-handler MAIN::alu rem primary
                    (?a ?b $?handler)
                    (call (dynamic-get backing-store)
                          rem
                          ?a
                          ?b
                          (if (= (length$ ?handler) 0) then
                           ?self:remide-by-zero-handler
                           else
                           (nth$ 1 
                            ?handler))))
