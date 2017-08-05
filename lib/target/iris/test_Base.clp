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
             (bind ?m0 
                   (sym-cat memtest- 
                            ?space 
                            -space-read-initial))
             (bind ?m1
                   (sym-cat memtest- 
                            ?space 
                            -space-write-value))
             (bind ?m2
                   (sym-cat memtest- 
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
(deffunction MAIN::invoke-test
             ()
             (bind ?core
                   (new iris-core))
             (progn$ (?space (create$ data
                                      code
                                      stack))
                     (test-memory-manipulation-routines ?core
                                                        ?space)))
