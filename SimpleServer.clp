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
; SimpleServer.clp - A series of rules, facts and other concepts to handle the setup
; and maintenance of a CLIPS server
;------------------------------------------------------------------------------
(deftemplate MAIN::command-writer
             (slot target
                   (type LEXEME)
                   (default ?NONE))
             (multislot command
                        (default ?NONE)))
(deffacts MAIN::stage-order
          (stage (current system-init)
                 (rest read
                       dispatch
                       restart)))

(defrule MAIN::setup-device-connection
         (stage (current system-init))
         ?f <- (setup connection ?path)
         (not (connection established to ?))
         =>
         (retract ?f)
         (if (set-socket-name ?path) then
           (printout t "Socket name is now " 
                     (get-socket-name ) crlf)
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
(defrule MAIN::read-raw-input
         (stage (current read))
         =>
         (assert (action (explode$ (read-command)))
                 (inspect action)))

(defrule MAIN::retract-inspect-action
         (declare (salience -9999))
         (stage (current read))
         ?f <- (inspect action)
         =>
         (retract ?f))

(defrule MAIN::ignore-command
         (declare (salience -1))
         (stage (current dispatch))
         ?f <- (action $?command)
         =>
         (printout t 
                   "NOTE: Ignoring " ?command crlf)
         (retract ?f))

(defrule MAIN::terminate-execution
         ?z <- (stage (current dispatch))
         ?k <- (action EOF|shutdown $?)
         =>
         (retract ?k
                  ?z)
         (shutdown-connection))

(defrule MAIN::perform-write
         (stage (current dispatch))
         ?f <- (command-writer (target ?callback)
                               (command $?command))
         =>
         (retract ?f)
         (write-command ?callback
                        (implode$ ?command)))

(defrule MAIN::restart-process
         ?f <- (stage (current restart))
         =>
         (modify ?f
                 (current read)
                 (rest dispatch
                       restart)))
(defgeneric MAIN::get-command-list)
(defrule MAIN::list-commands
         "Return a list of commands to the requester"
         (stage (current dispatch))
         ?f <- (action list-commands|commands callback ?callback $?)
         =>
         (retract ?f)
         (assert (command-writer (target ?callback)
                                 (command list-commands
                                          commands
                                          shutdown
                                          (get-command-list)))))
