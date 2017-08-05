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
             (?core ?space ?address ?expected)
             (check-result ?expected
                           (iris-read-memory ?core
                                             ?space
                                             ?address)))
(deffunction MAIN::test-memory-space-write-generic
             (?core ?space ?address ?value)
             (iris-write-memory ?core
                                ?space
                                ?address
                                ?value)
             (test-memory-space-read-generic ?core
                                             ?space
                                             ?address
                                             ?value))

(deffunction MAIN::test-memory-manipulation-routines
             (?core)
             (bind ?data-address0
                   (hex->int 0xFDED))
             (bind ?expected-value
                   81)
             (testcase t
                       memtest-data-space-write0
                       "Test writing to data space"
                       test-memory-space-write-generic 
                       ?core
                       data
                       ?data-address0
                       ?expected-value)
             (testcase t
                       memtest-data-space-read0
                       "Test reading from data space"
                       test-memory-space-read-generic
                       ?core
                       data
                       ?data-address0
                       ?expected-value))

(deffunction MAIN::invoke-test
             ()
             (bind ?core
                   (new iris-core))
             (test-memory-manipulation-routines ?core))




