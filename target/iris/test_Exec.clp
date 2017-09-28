;------------------------------------------------------------------------------
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
;------------------------------------------------------------------------------
; test_Exec.clp - test running instructions
;------------------------------------------------------------------------------
(batch* cortex.clp)
(batch* test.clp)
(batch* target/iris/Base.clp)
(defmodule MAIN
           (import cortex
                   ?ALL)
           (import test
                   ?ALL))
(deffacts MAIN::testsuite-info
          (testsuite iris-exec-tests))
(deffunction MAIN::mk-register
             (?index)
             (str-cat r
                      ?index))

(deffunction MAIN::register-index-to-hex
             (?index)
             (format nil
                     (if (< ?index
                            16) then
                       "0%x"
                       else
                       "%x")
                     ?index))

(deffunction MAIN::skip-three$
             (?mf)
             (rest$ (rest$ (rest$ ?mf))))

(deffunction MAIN::extract-triple$
             (?mf)
             (create$ (nth$ 1
                            ?mf)
                      (nth$ 2
                            ?mf)
                      (nth$ 3
                            ?mf)))

(deffunction MAIN::expect-num-instructions
             "Make an assertion about the number of instructions encoded"
             (?id ?description ?expected-num-instructions ?encoded-values)
             (assert (testcase (id ?id)
                               (description ?description))
                     (testcase-assertion (parent ?id)
                                         (expected (* ?expected-num-instructions
                                                      3))
                                         (actual-value (length$ ?encoded-values)))))

(deffunction MAIN::check-register-value
             (?core ?register ?expected)
             (assert (testcase (id (bind ?id
                                         (sym-cat iris:check-register-value:
                                                  ?register)))
                               (description (str-cat "Check "
                                                     ?register
                                                     " to see if it is set to "
                                                     ?expected)))
                     (testcase-assertion (parent ?id)
                                         (expected ?expected)
                                         (actual-value (iris-get-register ?core
                                                                          ?register)))))
(deffunction MAIN::parse-instruction-mix
             (?core ?uid $?lines)
             (bind ?asm
                   (new iris-assembler))
             (iris-parse-instructions ?asm
                                      ?lines)
             (iris-resolve-assembler-labels ?asm)
             (expect-num-instructions (sym-cat iris:parse-instruction-mix-
                                               ?uid
                                               :num-words)
                                      "make sure the number of encoded words for this instruction mix is correct!"
                                      (length$ ?lines)
                                      (bind ?result
                                            (iris-get-encoded-instructions ?asm)))
             (while (> (length$ ?result) 0) do
                    (iris-write-memory ?core
                                       (expand$ (extract-triple$ ?result)))
                    (bind ?result
                          (skip-three$ ?result))))
(deffunction MAIN::run-instruction-mix
             (?core $?lines)
             (iris-cycle ?core
                         (length$ ?lines)))



(deffunction MAIN::test-instruction-mix0
             "Set two registers and check the result"
             (?core)
             (progn$ (?register (create$ r0
                                         r1
                                         r2))
                     (check-register-value ?core
                                           ?register
                                           0))
             (bind ?lines
                   (create$ "set r0 0x1"
                            "set r1 0x2"
                            "add r2 r0 r1"))
             (parse-instruction-mix ?core
                                    simple0
                                    ?lines)
             (run-instruction-mix ?core
                                  ?lines)
             ; now we take a look at r0, r1, and r2
             (check-register-value ?core
                                   r0
                                   1)
             (check-register-value ?core
                                   r1
                                   2)
             (check-register-value ?core
                                   r2
                                   3))

(deffunction MAIN::invoke-test
             ()
             (bind ?core
                   (new iris-core))
             (iris-initialize ?core)
             (test-instruction-mix0 ?core)
             (iris-shutdown ?core))


