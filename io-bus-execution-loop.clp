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
(defglobal MAIN
           ?*result* = 0)

(defrule MAIN::perform-read-operation
         ?f <- (read ?address)
         ?q <- (object (is-a io-device))
         (test (send ?q
                     responds-to
                     ?address))
         =>
         (retract ?f)
         (bind ?*result*
               (send ?q
                     read
                     ?address)))

(defrule MAIN::perform-write-operation
         ?f <- (write ?address
                      ?value)
         ?q <- (object (is-a io-device))
         (test (send ?q
                     responds-to
                     ?address))
         =>
         (retract ?f)
         (send ?q
               write
               ?address
               ?value))

(defrule MAIN::no-read-match
         (declare (salience -1))
         ?f <- (read ?address)
         =>
         (retract ?f)
         (printout werror
                   "Can't read from " ?address crlf)
         (funcall illegal-read-operation))

(defrule MAIN::no-write-match
         (declare (salience -1))
         ?f <- (write ?address
                      ?)
         =>
         (retract ?f)
         (printout werror
                   "Can't write to " ?address crlf)
         (funcall illegal-write-operation))

(deffunction MAIN::process-io-event
             ()
             (run)
             ?*result*)

(deffunction MAIN::read-from-io-address
             (?address)
             (assert (read ?address))
             (process-io-event))

(deffunction MAIN::write-to-io-address
             (?address ?value)
             (assert (write ?address
                            ?value))
             (process-io-event))
