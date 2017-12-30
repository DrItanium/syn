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
; ComparatorUnit.clp - A simple comparison unit
;------------------------------------------------------------------------------
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* order.clp)
(batch* SimpleServer.clp)
;----------------------------------------------------------------

(defrule MAIN::convert-boolean-value
         (declare (salience 1))
         ?f <- (command-writer (command boolean ?value))
         =>
         (modify ?f
                 (command (if ?value then 1 else 0))))

(defrule MAIN::eq
         (stage (current dispatch))
         ?f <- (action eq|equals  ?v0 ?v1 callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command boolean 
                                          (= ?v0 ?v1)))))

(defrule MAIN::neq
         (stage (current dispatch))
         ?f <- (action not-equals|neq ?v0 ?v1 callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command boolean 
                                          (<> ?v0 ?v1)))))
(defrule MAIN::less-than
         (stage (current dispatch))
         ?f <- (action less-than|lt ?v0 ?v1 callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command boolean 
                                          (< ?v0 ?v1)))))

(defrule MAIN::greater-than
         (stage (current dispatch))
         ?f <- (action greater-than|gt ?v0 ?v1 callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command boolean 
                                          (> ?v0 ?v1)))))

(defrule MAIN::less-than-or-equal-to
         (stage (current dispatch))
         ?f <- (action less-than-or-equal-to|le ?v0 ?v1 callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command boolean 
                                          (<= ?v0 ?v1)))))

(defrule MAIN::greater-than-or-equal-to
         (stage (current dispatch))
         ?f <- (action greater-than-or-equal-to|ge ?v0 ?v1 callback ?callback)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command boolean 
                                          (>= ?v0 ?v1)))))


(defmethod MAIN::get-command-list
  ()
  (create$ eq equals
           neq not-equals
           lt less-than
           gt greater-than
           le less-than-or-equal-to
           ge greater-than-or-equal-to))

