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
; test_Base.clp - routines to make interfacing with the raw iris external address
; far simpler
;------------------------------------------------------------------------------
(batch* lib/cortex.clp)
(batch* lib/test.clp)
(batch* lib/target/iris/Base.clp)
(defmodule MAIN
           (import cortex
                   ?ALL)
           (import test
                   ?ALL))
(deffunction MAIN::test-instruction-encode-routines
             (?testcase ?instruction ?expected)
             (assert (testcase-assertion (actual-value (iris-parse-and-encode-instruction ?instruction))
                                         (expected ?expected)
                                         (parent ?testcase))))

(deffunction MAIN::test-instruction-decode-routines
             (?testcase ?core ?instruction ?expected)
             (assert (testcase-assertion (actual-value (iris-decode-instruction ?core
                                                                                ?instruction))
                                         (expected ?expected)
                                         (parent ?testcase))))


(deffunction MAIN::test-memory-space-read-generic
             (?testcase ?core ?space ?address ?expected)
             (assert (testcase-assertion (parent ?testcase)
                                         (expected ?expected)
                                         (actual-value (iris-read-memory ?core
                                                                         ?space
                                                                         ?address)))))
(deffunction MAIN::test-memory-space-write-generic
             (?testcase ?core ?space ?address ?value)
             ; check to make sure it passed
             (assert (testcase-assertion (parent ?testcase)
                                         (expected TRUE)
                                         (actual-value (iris-write-memory ?core
                                                                          ?space
                                                                          ?address
                                                                          ?value)))))

(deffunction MAIN::test-memory-manipulation-routines
             (?core ?space)
             (bind ?data-address0
                   (hex->int 0xFDED))
             (bind ?expected-value
                   81)
             (bind ?common-prefix
                   iris-memtest-)
             (bind ?m0
                   (sym-cat ?common-prefix
                            ?space
                            -space-read-initial))
             (bind ?m1
                   (sym-cat ?common-prefix
                            ?space
                            -space-write-value))
             (bind ?m2
                   (sym-cat ?common-prefix
                            ?space
                            -space-read-written-value))
             (assert (testcase (id ?m0)
                               (description (format nil
                                                    "Make sure that the data at the specified address is zero in space %s"
                                                    ?space)))
                     (testcase (id ?m1)
                               (description (format nil
                                                    "Test writing to %s space"
                                                    ?space)))
                     (testcase (id ?m2)
                               (description (format nil
                                                    "Test read from %s space after write"
                                                    ?space))))

             (test-memory-space-read-generic ?m0
                                             ?core
                                             ?space
                                             ?data-address0
                                             0)
             (test-memory-space-write-generic ?m1
                                              ?core
                                              ?space
                                              ?data-address0
                                              ?expected-value)
             (test-memory-space-read-generic ?m2
                                             ?core
                                             ?space
                                             ?data-address0
                                             ?expected-value))

(deffunction MAIN::test-register-manipulation-routines
             (?core)
             (bind ?register-index
                   71)
             (bind ?value
                   (hex->int 0xFDED))
             (bind ?common-prefix
                   iris-register-test)
             (bind ?m0
                   (sym-cat ?common-prefix
                            -initial-register-check))
             (bind ?m1
                   (sym-cat ?common-prefix
                            -register-write))
             (bind ?m2
                   (sym-cat ?common-prefix
                            -register-read-after-write))
             (assert (testcase (id ?m0)
                               (description "Make sure that the register is initially zero!"))
                     (testcase (id ?m1)
                               (description "Attempt to set a register to a given value"))
                     (testcase (id ?m2)
                               (description "Make sure that the result of the previous write has stuck")))
             (assert (testcase-assertion (parent ?m0)
                                         (expected 0)
                                         (actual-value (iris-get-register ?core
                                                                          ?register-index)))
                     (testcase-assertion (parent ?m1)
                                         (expected TRUE)
                                         (actual-value (iris-set-register ?core
                                                                          ?register-index
                                                                          ?value)))
                     (testcase-assertion (parent ?m2)
                                         (expected ?value)
                                         (actual-value (iris-get-register ?core
                                                                          ?register-index)))))

(deffunction MAIN::test-predicate-register-manipulation-routines
             (?core)
             (bind ?register-index
                   12)
             (bind ?value
                   TRUE)
             (bind ?common-prefix
                   iris-predicate-register-test)
             (bind ?m0
                   (sym-cat ?common-prefix
                            -initial-predicate-register-check))
             (bind ?m1
                   (sym-cat ?common-prefix
                            -predicate-register-write))
             (bind ?m2
                   (sym-cat ?common-prefix
                            -predicate-register-read-after-write))
             (assert (testcase (id ?m0)
                               (description "Make sure that the predicate register is initially FALSE!"))
                     (testcase (id ?m1)
                               (description "Attempt to set a predicate register to a given value"))
                     (testcase (id ?m2)
                               (description "Make sure that the result of the previous write has stuck")))
             (assert (testcase-assertion (parent ?m0)
                                         (expected FALSE)
                                         (actual-value (iris-get-predicate-register ?core
                                                                                    ?register-index)))
                     (testcase-assertion (parent ?m1)
                                         (expected TRUE)
                                         (actual-value (iris-set-predicate-register ?core
                                                                          ?register-index
                                                                          ?value)))
                     (testcase-assertion (parent ?m2)
                                         (expected ?value)
                                         (actual-value (iris-get-predicate-register ?core
                                                                                    ?register-index)))))
(deffacts MAIN::encode-tests
          (testcase (id iris-simple-encode-instruction0)
                    (description "Make sure that 'add r0 r0 r0' works correctly!"))
          (testcase (id iris-simple-encode-instruction1)
                    (description "Make sure that 'set r0 0xFDED' works correctly!"))
          (testcase (id iris-simple-encode-instruction2)
                    (description "Make sure that 'add r127 r67 r227' works correctly!")))
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

(deffunction MAIN::run-all-add-operations
             ()
             (printout t "Generating all 2^24 register add combinations as tests... this will take a while!" crlf)
             (loop-for-count (?di 0 255) do
                             (bind ?destIndex
                                   (register-index-to-hex ?di))
                             (bind ?rd
                                   (mk-register ?di))
                             (bind ?prefix0
                                   (sym-cat iris-add-instruction- ?rd))
                             (bind ?hex0
                                   (sym-cat ?destIndex
                                            "00"))
                             (loop-for-count (?s0 0 255) do
                                             (bind ?s0index
                                                   (register-index-to-hex ?s0))
                                             (bind ?rs0
                                                   (mk-register ?s0))
                                             (bind ?prefix1
                                                   (sym-cat ?prefix0 - ?rs0))
                                             (bind ?hex1
                                                   (sym-cat ?s0index
                                                            ?hex0))
                                             (loop-for-count (?s1 0 255) do
                                                             (bind ?s1index
                                                                   (register-index-to-hex ?s1))
                                                             (bind ?rs1
                                                                   (mk-register ?s1))
                                                             (bind ?tc
                                                                   (sym-cat ?prefix1 - ?rs1))
                                                             (bind ?hex
                                                                   (sym-cat 0x
                                                                            ?s1index
                                                                            ?hex1))
                                                             (assert (testcase (id ?tc)
                                                                               (description (str-cat "Make sure that '"
                                                                                                     (bind ?inst
                                                                                                           (str-cat "add " ?rd " " ?rs0 " " ?rs1))
                                                                                                     "' works correctly"))))
                                                             (test-instruction-encode-routines ?tc
                                                                                               ?inst
                                                                                               (hex->int (sym-cat 0x
                                                                                                                  ?s1index
                                                                                                                  ?hex1))))
                                             ; there are so many combos that we have to do this here!
                                             ; otherwise it takes forever to do!
                                             (focus test)
                                             (run))))

(deffunction MAIN::test-instruction-parsing-and-encoding
             (?core)
             (test-register-manipulation-routines ?core)
             (test-predicate-register-manipulation-routines ?core)
             (test-instruction-encode-routines iris-simple-encode-instruction0
                                               "add r0 r0 r0"
                                               0)
             (test-instruction-encode-routines iris-simple-encode-instruction1
                                               "set r0 0xFDED"
                                               (hex->int 0xFDED0009))
             (test-instruction-encode-routines iris-simple-encode-instruction2
                                               "add r127 r67 r227"
                                               (hex->int 0xE3437F00))
             (loop-for-count (?pos 0 255) do
                             (bind ?index
                                   (register-index-to-hex ?pos))
                             (bind ?reg
                                   (mk-register ?pos))
                             (bind ?tc0
                                   (sym-cat iris-add-instruction-test-dest- ?reg))
                             (bind ?tc1
                                   (sym-cat iris-add-instruction-test-src0- ?reg))
                             (bind ?tc2
                                   (sym-cat iris-add-instruction-test-src1- ?reg))
                             (bind ?h0
                                   (sym-cat 0x0000 ?index "00"))
                             (bind ?h1
                                   (sym-cat 0x00 ?index "0000"))
                             (bind ?h2
                                   (sym-cat 0x ?index "000000"))
                             (assert (testcase (id ?tc0)
                                               (description (str-cat "test the destination field being " ?reg)))
                                    (testcase (id ?tc1)
                                               (description (str-cat "test the source 0 field being " ?reg)))
                                    (testcase (id ?tc2)
                                               (description (str-cat "test the source 1 field being " ?reg))))

                             (test-instruction-encode-routines ?tc0
                                                               (str-cat "add " ?reg " r0 r0")
                                                               (hex->int ?h0))
                             (test-instruction-encode-routines ?tc1
                                                               (str-cat "add r0 " ?reg " r0")
                                                               (hex->int ?h1))
                             (test-instruction-encode-routines ?tc2
                                                               (str-cat "add r0 r0 " ?reg)
                                                               (hex->int ?h2))))
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


(deffunction MAIN::test-instruction-mix0
             "Set two registers and check the result"
             (?core)
             (iris-initialize ?core)
             (progn$ (?register (create$ r0
                                         r1
                                         r2))
                     (check-register-value ?core
                                           ?register
                                           0))
             (bind ?asm
                   (new iris-assembler))
             (bind ?lines
                   (create$ "set r0 0x1"
                            "set r1 0x2"
                            "add r2 r0 r1"))
             (iris-parse-instructions ?asm
                                      ?lines)
             (iris-resolve-assembler-labels ?asm)
             (expect-num-instructions iris:check-instruction-mix0:num-words
                                      "make sure the number of encoded words is correct!"
                                      (length$ ?lines)
                                      (bind ?result
                                            (iris-get-encoded-instructions ?asm)))
             (while (> (length$ ?result) 0) do
                    (iris-write-memory ?core
                                       (expand$ (extract-triple$ ?result)))
                    (bind ?result
                          (skip-three$ ?result)))
             ; now we go through and execute three cycles
             (iris-cycle ?core
                         (length$ ?lines))
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
             (progn$ (?space (create$ data
                                      code
                                      stack))
                     (test-memory-manipulation-routines ?core
                                                        ?space))
             (test-instruction-parsing-and-encoding ?core)
             (test-instruction-mix0 ?core))


