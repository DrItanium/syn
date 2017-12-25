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
(defmodule test
           "Testing related operations"
           (import cortex
                   ?ALL)
           (export ?ALL))

(deftemplate test::testcase
             (slot id
                   (type SYMBOL)
                   (default ?NONE))
             (slot description
                   (type LEXEME)
                   (default ?NONE))
             (slot router
                   (type SYMBOL)
                   (default-dynamic t)))

(deftemplate test::testcase-assertion
             (slot parent
                   (type SYMBOL)
                   (default ?NONE))
             (multislot expected
                        (default ?NONE))
             (multislot actual-value
                        (default ?NONE))
             (slot outcome
                   (type SYMBOL)
                   (allowed-symbols ANALYZE
                                    TRUE
                                    FALSE)))


(defrule test::unpack-boolean-assertion:assert-result-true
         "Define shorthand conversion facts for making boolean checks"
         (declare (salience ?*priority:first*))
         ?f <- (expect-true ?parent ?result)
         =>
         (retract ?f)
         (assert (testcase-assertion (parent ?parent)
                                     (actual-value ?result)
                                     (expected TRUE))))

(defrule test::unpack-boolean-assertion:assert-result-false
         "Define shorthand conversion facts for making boolean checks"
         (declare (salience ?*priority:first*))
         ?f <- (expect-false ?parent ?result)
         =>
         (retract ?f)
         (assert (testcase-assertion (parent ?parent)
                                     (actual-value ?result)
                                     (expected FALSE))))

(defrule test::evaluate-assertion
         (declare (salience ?*priority:one*))
         ?f <- (testcase-assertion (outcome ANALYZE)
                                   (expected $?expected)
                                   (actual-value $?actual))
         =>
         (bind ?check
               (eq ?expected
                   ?actual))
         (modify ?f
                 (outcome ?check)))

(defrule test::printout-testcase
         "Once all assertions have been checked, print out the result"
         ?f <- (testcase (id ?id)
                         (description ?description)
                         (router ?router))
         (not (testcase-assertion (parent ?id)
                                  (outcome ANALYZE)))
         =>
         (retract ?f)
         (assert (target-router ?router))
         (bind ?sentences
               "Testcase " ?id ": " ?description crlf)
         (bind ?failed
               FALSE)
         (delayed-do-for-all-facts ((?ta testcase-assertion))
                                   (eq ?ta:parent
                                       ?id)
                                   (if (not ?ta:outcome) then
                                     (bind ?failed
                                           TRUE)
                                     (bind ?sentences
                                           ?sentences
                                           tab tab "CHECK FAILED: " crlf
                                           tab tab tab "expected: " ?ta:expected crlf
                                           tab tab tab "actual value: " ?ta:actual-value crlf))
                                   (retract ?ta))
         (bind ?sentences
               ?sentences
               tab "Result: "
               (if ?failed then
                 FAILED
                 else
                 PASSED) crlf)
         ; we want to terminate if we failed!
         (if ?failed then
           (printout ?router
                     (expand$ ?sentences))
           (exit 1)))


(defrule test::testsuite-was-successful
         (declare (salience -10000))
         ?f <- (target-router ?router)
         ?f2 <- (testsuite ?name)
         =>
         (retract ?f
                  ?f2)
         (printout ?router
                   "testsuite " ?name ": All tests passed!" crlf))
