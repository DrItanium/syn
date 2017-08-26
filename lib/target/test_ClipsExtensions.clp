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
; test_ClipsExtensions.clp - Test the extra UDFs found in ClipsExtensions.cc
;------------------------------------------------------------------------------
(batch* lib/cortex.clp)
(batch* lib/test.clp)
(defmodule MAIN
           (import cortex
                   ?ALL)
           (import test
                   ?ALL))
(deffacts MAIN::clips-extensions-tests
          (testcase (id hex->int:basic)
                    (description "make sure that converting a hex symbol to a number works"))
          (testcase-assertion (parent hex->int:basic)
                              (expected 255)
                              (actual-value (hex->int 0xFF)))
          (testcase (id binary->int:basic)
                    (description "make sure that converting a binary symbol to a number works"))
          (testcase-assertion (parent binary->int:basic)
                              (expected 255)
                              (actual-value (binary->int 0b11111111)))
          (testcase (id bitmask->int:basic)
                    (description "make sure that converting a bitmask symbol to a number works"))
          (testcase-assertion (parent bitmask->int:basic)
                              (expected 255)
                              (actual-value (bitmask->int 0m11111111)))
          (testcase (id left-shift)
                    (description "make sure that << or left-shift works correctly"))
          (testcase-assertion (parent left-shift)
                              (expected 2)
                              (actual-value (left-shift 1 1)))
          (testcase (id right-shift)
                    (description "make sure that << or right-shift works correctly"))
          (testcase-assertion (parent right-shift)
                              (expected 1)
                              (actual-value (right-shift 2 1)))
          (testcase (id expand-bit:FALSE)
                    (description "make sure that expand bit when input is FALSE that we get zero"))
          (testcase-assertion (parent expand-bit:FALSE)
                              (expected 0)
                              (actual-value (expand-bit 0)))
          (testcase (id expand-bit:TRUE)
                    (description "make sure that expand bit when input is TRUE that we get zero"))
          (testcase-assertion (parent expand-bit:TRUE)
                              (expected (hex->int 0xFF))
                              (actual-value (expand-bit 1)))
          (testcase (id break-apart-number:simple)
                    (description "carve a simple number apart into constituent pieces"))
          (testcase-assertion (parent break-apart-number:simple)
                              (expected (hex->int 0xED)
                                        (hex->int 0xFD)
                                        0
                                        0
                                        0
                                        0
                                        0
                                        0)
                              (actual-value (break-apart-number (hex->int 0xFDED))))

          )
;TODO: add tests for the functions found in functional.cc
(deffunction MAIN::invoke-test
             ())

