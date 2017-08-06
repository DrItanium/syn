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
; test_Base.clp - routines to make interfacing with the raw cisc0 external address 
; far simpler 
;------------------------------------------------------------------------------
(batch* lib/cortex.clp)
(batch* lib/test.clp)
(batch* lib/target/cisc0/Base.clp)
(defmodule MAIN
           (import cortex 
                   ?ALL)
           (import test
                   ?ALL))
(deffunction MAIN::test-memory-space-read-generic
             (?testcase ?core ?address ?expected)
             (assert (testcase-assertion (parent ?testcase)
                                         (expected ?expected)
                                         (actual-value (cisc0-read-memory ?core
                                                                          ?address)))))
(deffunction MAIN::test-memory-space-write-generic
             (?testcase ?core ?address ?value)
             ; check to make sure it passed
             (assert (testcase-assertion (parent ?testcase)
                                         (expected TRUE)
                                         (actual-value (cisc0-write-memory ?core
                                                                           ?address
                                                                           ?value)))))

(deffunction MAIN::test-memory-manipulation-routines
             (?core)
             (bind ?data-address0
                   (hex->int 0xFDED))
             (bind ?expected-value
                   81)
             (bind ?common-prefix
                   cisc0-memtest-)
             (bind ?m0 
                   (sym-cat ?common-prefix
                            -space-read-initial))
             (bind ?m1
                   (sym-cat ?common-prefix
                            -space-write-value))
             (bind ?m2
                   (sym-cat ?common-prefix
                            -space-read-written-value))
             (assert (testcase (id ?m0)
                               (description "Make sure that the data at the specified address is zero in memory"))
                     (testcase (id ?m1)
                               (description "Test writing to memory"))
                     (testcase (id ?m2)
                               (description "Test reading from memory after write")))

             (test-memory-space-read-generic ?m0
                                             ?core
                                             ?data-address0
                                             0)
             (test-memory-space-write-generic ?m1 
                                              ?core
                                              ?data-address0
                                              ?expected-value)
             (test-memory-space-read-generic ?m2 
                                             ?core
                                             ?data-address0
                                             ?expected-value))
(deffunction MAIN::test-register-manipulation-routines
             (?core)
             (bind ?register-index
                   7)
             (bind ?value
                   (hex->int 0xABCDFDED))
             (bind ?common-prefix
                   cisc0-register-test)
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
                                         (actual-value (cisc0-get-register ?core
                                                                           ?register-index)))
                     (testcase-assertion (parent ?m1)
                                         (expected TRUE)
                                         (actual-value (cisc0-set-register ?core
                                                                           ?register-index
                                                                           ?value)))
                     (testcase-assertion (parent ?m2)
                                         (expected ?value)
                                         (actual-value (cisc0-get-register ?core
                                                                           ?register-index)))))


(deffunction MAIN::invoke-core-test
             (?core-id)
             (bind ?core
                   (new ?core-id))
             ; need to make sure we can setup the io controller first before we can uncomment this
             ;(test-memory-manipulation-routines ?core)
             (test-register-manipulation-routines ?core))

(deffunction MAIN::invoke-test
             ()
             (map invoke-core-test
                  cisc0-core-model0
                  cisc0-core-model1))
