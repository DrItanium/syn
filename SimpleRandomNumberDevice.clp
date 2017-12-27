
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
; SimpleRandomNumberDevice.clp - a separate device process meant to respond on
; named pipes to requests
;------------------------------------------------------------------------------
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* order.clp)

(defglobal MAIN
           ?*current-seed* = (hex->int 0xFDED))

(deffacts MAIN::stage-order
          (stage (current system-init)
                 (rest read
                       dispatch
                       restart)))
(defrule MAIN::startup-and-seed
         (stage (current system-init))
         =>
         (seed ?*current-seed*))
(defrule MAIN::setup-device-connection
         (stage (current system-init))
         ?f <- (setup connection ?path)
         (not (connection established to ?))
         =>
         (retract ?f)
         (if (set-socket-name ?path) then
           (system (format nil 
                           "rm -f %s"
                           ?path))
           (setup-connection)
           (assert (connection established to ?path))))

(defrule MAIN::terminate-execution-on-missing-connection
         (declare (salience -1))
         ?f <- (stage (current system-init))
         (not (connection established to ?))
         =>
         (retract ?f)
         (printout stderr
                   "Connection not defined! Terminating Execution!" crlf))


(defrule MAIN::read-input
         (stage (current read))
         =>
         (assert (action (explode$ (read-command)))))

(defrule MAIN::terminate-execution
         ?z <- (stage (current dispatch))
         ?k <- (action EOF|shutdown)
         =>
         (retract ?k
                  ?z)
         (shutdown-connection))

(defrule MAIN::seed-device
         (stage (current dispatch))
         ?k <- (action seed ?seed)
         (test (integerp ?seed))
         =>
         (retract ?k)
         (seed (bind ?*current-seed*
                     ?seed)))
(defrule MAIN::seed-device:ignore
         (stage (current dispatch))
         ?k <- (action seed ?seed)
         (test (not (integerp ?seed)))
         =>
         ; TODO: add support for another channel for writing back
         (retract ?k))

(defrule MAIN::ignore-command
         (declare (salience -1))
         (stage (current dispatch))
         ?f <- (action $?command)
         =>
         (printout t 
                   "NOTE: Ignoring " ?command crlf)
         (retract ?f))

(defrule MAIN::generate-random-value
         (stage (current dispatch))
         ?f <- (action read to ?address)
         =>
         (retract ?f)
         (write-command ?address
                        (str-cat (random))))

(defrule MAIN::skip-random-value
         (stage (current dispatch))
         ?f <- (action skip)
         =>
         (retract ?f)
         (random))

(defrule MAIN::restart-process
         ?f <- (stage (current restart))
         =>
         (modify ?f
                 (current read)
                 (rest dispatch
                       restart)))
