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
(batch* lib/cortex.clp)
(defmodule MAIN
           (import cortex
                   ?ALL)
           (export ?ALL))
(set-current-module MAIN)
(batch* SynCores.clp)
(batch* SynAssemblers.clp)

(definstances MAIN::core-init
              (c0 of cisc0-core
                  (init-arguments)))


(printout t 
          "Welcome to the cisc0 repl!" crlf
          "At this point the interactive prompt has loaded the minimum set of things necessary for execution" crlf
          "Please enter (reset) to construct a cisc0-core to start playing with!" crlf)


(deffunction MAIN::step
             (?target)
             (send ?target cycle))

(deffunction MAIN::inspect-register
             (?target ?index)
             (send ?target 
                   get-register 
                   ?index))
(deffunction MAIN::get-ip
             (?target)
             (inspect-register ?target
                               15))

(deffunction MAIN::print-registers
             "Print out all of the cisc0 registers"
             (?target)
             (printout t 
                       "Registers" crlf
                       "-----------------------------------------------------------" crlf)
             (loop-for-count (?i 0 15) do
                             (printout t tab "r" ?i " : " (int->hex (inspect-register ?target
                                                                                      ?i)) crlf))
             (printout t 
                       "-----------------------------------------------------------" crlf))


(deffunction MAIN::parse-and-install-asm
             (?target ?file)
             (send ?target 
                   install-values 
                   (parse-file cisc0-assembler 
                               ?file)))


(deffunction MAIN::decode-installation-values
             (?list)
             (bind ?tmp
                   ?list)
             (bind ?out
                   (create$))
             (while (<> (length$ ?tmp)
                        0) do
                    (bind ?out
                          ?out
                          (int->hex (nth$ 1 
                                          ?tmp))
                          (int->hex (nth$ 2
                                          ?tmp)))
                    (bind ?tmp
                          (delete$ ?tmp
                                   1
                                   2)))
             ?out)

(deffunction MAIN::print-installation-map
             (?list)
             (printout t 
                       "Address" tab "Value" crlf)
             (while (<> (length$ ?list)
                        0) do
                    (printout t 
                              (nth$ 1 
                                    ?list)
                              tab
                              (nth$ 2
                                    ?list) crlf)
                    (bind ?list
                          (delete$ ?list
                                   1
                                   2))))

(deffunction MAIN::diff-and-step
             (?x)
             (print-registers ?x)
             (step ?x)
             (print-registers ?x))
