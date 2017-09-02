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
(deffacts MAIN::testsuite-info
          (testsuite boost-and-functional-tests))
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
          (testcase (id boost-fs-path-exists0)
                    (description "does path-exists work on a file?"))
          (testcase (id boost-fs-path-exists1)
                    (description "does path-exists work on a directory?"))
          (testcase (id boost-fs-directoryp0)
                    (description "does directoryp work?"))
          (testcase (id boost-fs-regular-filep0)
                    (description "does regular-filep work?"))
          (testcase (id boost-clamp0)
                    (description "test the range clamp"))
          (testcase-assertion (parent boost-fs-path-exists0)
                              (expected TRUE)
                              (actual-value (path-exists "lib/cortex.clp")))
          (testcase-assertion (parent boost-fs-path-exists1)
                              (expected TRUE)
                              (actual-value (path-exists "lib/")))
          (testcase-assertion (parent boost-fs-directoryp0)
                              (expected TRUE)
                              (actual-value (directoryp "lib/")))
          (testcase-assertion (parent boost-fs-regular-filep0)
                              (expected TRUE)
                              (actual-value (regular-filep "lib/cortex.clp")))
          (testcase-assertion (parent boost-has-prefix0)
                              (expected TRUE)
                              (actual-value (has-prefix donuts
                                                        do)))
          (testcase-assertion (parent boost-has-suffix0)
                              (expected TRUE)
                              (actual-value (has-suffix donuts
                                                        nuts)))
          (testcase-assertion (parent boost-string-trim0)
                              (expected "donuts")
                              (actual-value (string-trim "   donuts   ")))
          (testcase-assertion (parent boost-string-trim-front0)
                              (expected "donuts   ")
                              (actual-value (string-trim-front "   donuts   ")))
          (testcase-assertion (parent boost-string-trim-back0)
                              (expected "   donuts")
                              (actual-value (string-trim-back "   donuts   ")))
          (testcase-assertion (parent boost-clamp0)
                              (expected 0)
                              (actual-value (clamp 0 -1 1)))
          (testcase-assertion (parent boost-clamp0)
                              (expected 1)
                              (actual-value (clamp 0 1 2)))
          (testcase-assertion (parent boost-clamp0)
                              (expected 2)
                              (actual-value (clamp 89 1 2))))
(deffunction MAIN::greater-than-four
             (?a)
             (> ?a 4))
(deffacts MAIN::functional-test-cases
          (testcase (id filter-function:deffunction)
                    (description "filter using a deffunction"))
          (testcase-assertion (parent filter-function:deffunction)
                              (expected 5 6 7 9)
                              (actual-value (filter greater-than-four
                                                    1 2 3 4 5 6 7 9)))
          (testcase (id filter-function:native-function-checks)
                    (description "use a native function like lexemep instead of a deffunction"))
          (testcase-assertion (parent filter-function:native-function-checks)
                              (expected a b c d)
                              (actual-value (filter lexemep
                                                    1 a 2 b 3 c 4 d)))
          (testcase-assertion (parent filter-function:native-function-checks)
                              (expected 1 2 3 4)
                              (actual-value (filter numberp
                                                    1 a 2 b 3 c 4 d)))
          (testcase (id map-function:defmethod:increment)
                    (description "map using a defmethod increment"))
          (testcase-assertion (parent map-function:defmethod:increment)
                              (expected 2 3 4 5)
                              (actual-value (map increment
                                                 1 2 3 4)))
          (testcase (id map-function:defmethod:decrement)
                    (description "map using a defmethod decrement"))
          (testcase-assertion (parent map-function:defmethod:decrement)
                              (expected 0 1 2 3)
                              (actual-value (map decrement
                                                 1 2 3 4)))
          (testcase (id map-function:defmethod:int->hex)
                    (description "map using a defmethod int->hex"))
          (testcase-assertion (parent map-function:defmethod:int->hex)
                              (expected 0x1 0x2 0x3 0x4)
                              (actual-value (map int->hex
                                                 1 2 3 4)))
          (testcase (id map-function:native:numberp)
                    (description "check each number to see if it is a number"))
          (testcase-assertion (parent map-function:native:numberp)
                              (expected TRUE
                                        TRUE
                                        TRUE
                                        TRUE)
                              (actual-value (map numberp
                                                 1
                                                 2
                                                 3
                                                 4)))
          (testcase (id map-function:native:lexemep)
                    (description "check each number to see if it is a number"))
          (testcase-assertion (parent map-function:native:lexemep)
                              (expected FALSE
                                        FALSE
                                        FALSE
                                        FALSE)
                              (actual-value (map lexemep
                                                 1
                                                 2
                                                 3
                                                 4)))
          (testcase (id exists-function:native:numberp)
                    (description "see if there is a number in the given set"))
          (testcase-assertion (parent exists-function:native:numberp)
                              (expected TRUE)
                              (actual-value (exists numberp
                                                    1
                                                    2
                                                    3
                                                    4)))
          (testcase (id exists-function:native:lexemep)
                    (description "See if there is a lexeme in the given set"))
          (testcase-assertion (parent exists-function:native:lexemep)
                              (expected FALSE)
                              (actual-value (exists lexemep
                                                    1
                                                    2
                                                    3
                                                    4)))
          (testcase (id not-exists-function:native:numberp)
                    (description "see if there are no numbers in the input set"))
          (testcase-assertion (parent not-exists-function:native:numberp)
                              (expected FALSE)
                              (actual-value (not-exists numberp
                                                        1
                                                        2
                                                        3
                                                        4)))
          (testcase (id not-exists-function:native:lexemep)
                    (description "See if there is no lexeme in the input set"))
          (testcase-assertion (parent not-exists-function:native:lexemep)
                              (expected TRUE)
                              (actual-value (not-exists lexemep
                                                        1
                                                        2
                                                        3
                                                        4)))
          )




;TODO: add tests for the functions found in functional.cc
(deffunction MAIN::invoke-test
             ())

