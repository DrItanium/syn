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
; BinaryLogicUnit.clp - A simple and/or/not/nor/nand/xor/shift-left/shift-right logic unit
;------------------------------------------------------------------------------
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* order.clp)
(batch* SimpleServer.clp)
;----------------------------------------------------------------
; commands are: add, sub, mul, div, rem, shift-left/left-shift, shift-right/right-shift, shutdown, list-commands
;----------------------------------------------------------------


(defrule MAIN::and-result
         (stage (current dispatch))
         ?f <- (action and ?v0 ?v1 callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (binary-and ?v0 ?v1)))))

(defrule MAIN::or-result
         (stage (current dispatch))
         ?f <- (action or ?v0 ?v1 callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (binary-or ?v0 ?v1)))))

(defrule MAIN::nor-result
         (stage (current dispatch))
         ?f <- (action nor ?v0 ?v1 callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (binary-nor ?v0 ?v1)))))

(defrule MAIN::nand-result
         (stage (current dispatch))
         ?f <- (action nand ?v0 ?v1 callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (binary-nand ?v0 ?v1)))))

(defrule MAIN::xor-result
         (stage (current dispatch))
         ?f <- (action xor ?v0 ?v1 callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (binary-xor ?v0 ?v1)))))

(defrule MAIN::shift-left-result
         (stage (current dispatch))
         ?f <- (action shift-left|left-shift ?value ?shift callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (integer (left-shift ?value
                                                               ?shift))))))
(defrule MAIN::shift-right-result
         (stage (current dispatch))
         ?f <- (action shift-right|right-shift ?value ?shift callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (integer (right-shift ?value
                                                                ?shift))))))

(defrule MAIN::not
         (stage (current dispatch))
         ?f <- (action not ?value callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command (binary-not ?value)))))
(defmethod MAIN::get-command-list
  ()
  (create$ or
           and
           xor
           nor
           nand
           not
           left-shift shift-left
           right-shift shift-right))

