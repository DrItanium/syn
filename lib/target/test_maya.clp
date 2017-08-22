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
; test_maya.clp - Test the extra maya user defined functions
;------------------------------------------------------------------------------
(batch* lib/cortex.clp)
(batch* lib/test.clp)
(defmodule MAIN
           (import cortex
                   ?ALL)
           (import test
                   ?ALL))
(deffacts MAIN::boost-test-cases
          (testcase (id boost-has-prefix0)
                    (description "does has-prefix work?"))
          (testcase (id boost-has-suffix0)
                    (description "does has-suffix work?"))
          (testcase (id boost-string-trim0)
                    (description "does string-trim work?"))
          (testcase (id boost-string-trim-front0)
                    (description "does string-trim-front work?"))
          (testcase (id boost-string-trim-back0)
                    (description "does string-trim-back work?"))
          (testcase-assertion (parent boost-has-prefix0)
                              (expected TRUE)
                              (actual-value (bind ?prefix-test
                                                  (has-prefix donuts
                                                              do))))
          (testcase-assertion (parent boost-has-suffix0)
                              (expected TRUE)
                              (actual-value (bind ?suffix-test
                                                  (has-suffix donuts
                                                              nuts))))
          (testcase-assertion (parent boost-string-trim0)
                              (expected "donuts")
                              (actual-value (string-trim "   donuts   ")))
          (testcase-assertion (parent boost-string-trim-front0)
                              (expected "donuts   ")
                              (actual-value (string-trim-front "   donuts   ")))
          (testcase-assertion (parent boost-string-trim-back0)
                              (expected "   donuts")
                              (actual-value (string-trim-back "   donuts   "))))



(deffunction MAIN::invoke-test
             ()
             )

