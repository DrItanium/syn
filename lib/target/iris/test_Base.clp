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
          (testcase (id simple-encode-instruction0)
                    (description "Make sure that 'add r0 r0 r0' works correctly!")))
(deffunction MAIN::invoke-test
             ()
             (bind ?core
                   (new iris-core))
             (progn$ (?space (create$ data
                                      code
                                      stack))
                     (test-memory-manipulation-routines ?core
                                                        ?space))
             (test-register-manipulation-routines ?core)
             (test-predicate-register-manipulation-routines ?core)
             (test-instruction-encode-routines simple-encode-instruction0
                                               "add r0 r0 r0"
                                               0))
