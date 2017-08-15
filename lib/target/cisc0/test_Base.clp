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

(deffunction MAIN::parse-assembly-line
             (?id ?description ?line $?expected)
             (bind ?core
                   (new cisc0-assembler))
             (assert (testcase (id ?id)
                               (description ?description))
                     (testcase-assertion (parent ?id)
                                         (expected TRUE)
                                         (actual-value (cisc0-parse-line ?core
                                                                         ?line)))
                     (testcase-assertion (parent ?id)
                                         (expected TRUE)
                                         (actual-value (cisc0-resolve-instructions ?core))))
             ; with cisc0, we get a variable length architecture going on
             (bind ?result
                   (cisc0-get-encoded-instructions ?core))
             (bind ?has-elements
                   (not ?result))
             (assert (testcase-assertion (parent ?id)
                                         (expected FALSE)
                                         (actual-value ?has-elements))
                     (testcase-assertion (parent ?id)
                                         (expected (length$ ?expected))
                                         (actual-value (div (length$ ?result)
                                                            2))))
             (progn$ (?a ?result)
                     (if (evenp ?a-index) then
                       (assert (testcase-assertion (parent ?id)
                                                   (expected (nth$ (div ?a-index
                                                                        2)
                                                                   ?expected))
                                                   (actual-value ?a))))))

(deffunction MAIN::testcase-id-asm-parsing
             (?postfix)
             (sym-cat cisc0-assembly-parsing-test- ?postfix))
(deffunction MAIN::parse-asm-test
             (?postfix ?desc ?line ?hex)
             (parse-assembly-line (testcase-id-asm-parsing ?postfix)
                                  ?desc
                                  ?line
                                  (hex->int ?hex)))
(deffunction MAIN::register
             (?index)
             (sym-cat r
                      ?index))
(defglobal MAIN
           ?*register-to-hex-translation* = (create$ 0 1 2 3 4 5 6 7 8 9 A B C
                                                     D E F)
           ?*masks* = (create$ 0m0000
                               0m0001
                               0m0010
                               0m0011
                               0m0100
                               0m0101
                               0m0110
                               0m0111
                               0m1000
                               0m1001
                               0m1010
                               0m1011
                               0m1100
                               0m1101
                               0m1110
                               0m1111)
           ?*arithmetic-ops* = (create$ add
                                        sub
                                        mul
                                        div
                                        rem
                                        min
                                        max)
           ?*groups* = (create$ memory
                                arithmetic
                                shift
                                logical
                                compare
                                branch
                                move
                                set
                                swap
                                return
                                complex))


(deffunction MAIN::group-to-index
             (?group)
             (- (member$ ?group
                         ?*groups*) 1))
(deffunction MAIN::register-index-to-symbol
             (?index)
             (nth$ (+ ?index 1)
                   ?*register-to-hex-translation*))
(deffunction MAIN::group-to-hex
             (?group)
             (register-index-to-symbol (group-to-index ?group)))
(deffunction MAIN::form-word-as-hex
             (?n0 ?n1 ?n2 ?n3)
             (sym-cat 0x ?n3 ?n2 ?n1 ?n0))
(deffunction MAIN::test-all-register-combinations:single-word
             (?postfix ?nyb0 ?nyb1 ?operation ?desc-name)
             (loop-for-count (?di 0 15) do
                             (bind ?rdest
                                   (register ?di))
                             (loop-for-count (?si 0 15) do
                                             (bind ?rsrc
                                                   (register ?si))
                                             (bind ?desc
                                                   (format nil
                                                           "Parse a %s operation with the register %s and %s"
                                                           ?desc-name
                                                           ?rdest
                                                           ?rsrc))
                                             (bind ?hex
                                                   (form-word-as-hex ?nyb0
                                                                     ?nyb1
                                                                     (register-index-to-symbol ?si)
                                                                     (register-index-to-symbol ?di)))
                                             (parse-asm-test (sym-cat ?postfix
                                                                      -
                                                                      ?rdest
                                                                      -
                                                                      ?rsrc)
                                                             ?desc
                                                             (format nil
                                                                     "%s %s %s"
                                                                     ?operation
                                                                     ?rdest
                                                                     ?rsrc)
                                                             ?hex))))



(deffunction MAIN::bitmask-to-int
             (?mask)
             (register-index-to-symbol (- (member$ ?mask
                                                   ?*masks*)
                                          1)))
(deffunction MAIN::asm-parsing-tests
             ()
             (parse-asm-test return
                             "Parse a return instruction"
                             "return"
                             0x9)
             (parse-asm-test arithmetic-simple
                             "Parse a single arithmetic instruction"
                             "arithmetic add r0 r1"
                             0x101)
             (progn$ (?sub-op ?*arithmetic-ops*)
                     (test-all-register-combinations:single-word (sym-cat arithmetic-
                                                                          ?sub-op)
                                                                 (group-to-hex arithmetic)
                                                                 (register-index-to-symbol (left-shift (- ?sub-op-index
                                                                                                          1)
                                                                                                       1))
                                                                 (bind ?op
                                                                       (format nil
                                                                               "arithmetic %s"
                                                                               ?sub-op))
                                                                 ?op))

             (test-all-register-combinations:single-word swap
                                                         (group-to-hex swap)
                                                         0
                                                         swap
                                                         swap)
             (progn$ (?mask ?*masks*)
                     (test-all-register-combinations:single-word (sym-cat move-
                                                                          ?mask)
                                                                 (group-to-hex move)
                                                                 (bitmask-to-int ?mask)
                                                                 (format nil
                                                                         "move %s"
                                                                         ?mask)
                                                                 (str-cat "move with bitmask "
                                                                          ?mask))))

(deffunction MAIN::invoke-test
             ()
             (map invoke-core-test
                  cisc0-core-model0
                  cisc0-core-model1)
             (asm-parsing-tests))
