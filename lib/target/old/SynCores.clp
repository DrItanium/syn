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

; Define the base wrapper classes for the different cores

(deffunction MAIN::inform-then-halt
             ($?msg)
             (printout werror
                       (expand$ ?msg)
                       crlf)
             (halt)
             FALSE)
(defgeneric MAIN::new-core)
(defgeneric MAIN::shutdown-core)
(defgeneric MAIN::initialize-core)
(defgeneric MAIN::call-core)
(defgeneric MAIN::cycle-core
            "Run a single core cycle!")
(defgeneric MAIN::run-core
            "Run the core until it finishes itself! Cannot be interrupted!")

(defmethod MAIN::new-core
  ((?title SYMBOL)
   (?args MULTIFIELD))
  (new ?title
       (expand$ ?args)))

(defmethod MAIN::new-core
  ((?title SYMBOL)
   $?args)
  (new-core ?title
            ?args))

(defmethod MAIN::initialize-core
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        initialize))
(defmethod MAIN::shutdown-core
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        shutdown))

(defmethod MAIN::call-core
  ((?core EXTERNAL-ADDRESS)
   (?operation SYMBOL)
   (?arguments MULTIFIELD))
  (call ?core
        ?operation
        (expand$ ?arguments)))

(defmethod MAIN::call-core
  ((?core EXTERNAL-ADDRESS)
   (?operation SYMBOL)
   $?args)
  (call-core ?core
             ?operation
             ?args))

(defmethod MAIN::cycle-core
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        cycle))

(defmethod MAIN::run-core
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        run))

(defclass MAIN::core
  (is-a USER)
  (slot reference
        (type EXTERNAL-ADDRESS)
        (visibility public)
        (storage local))
  (slot type
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (default PLEASE-OVERRIDE-IN-SUBTYPES))
  (multislot init-arguments
             (visibility public)
             (storage local)
             (default ?NONE))
  (message-handler clear primary)
  (message-handler cycle primary)
  (message-handler run primary)
  (message-handler init after)
  (message-handler delete before)
  (message-handler call primary))
(defmessage-handler MAIN::core clear primary
                    ()
                    (shutdown-core ?self:reference)
                    (initialize-core (bind ?self:reference
                                           (new-core (dynamic-get type)
                                                     (dynamic-get init-arguments)))))
(defmessage-handler MAIN::core init after
                    ()
                    (initialize-core (bind ?self:reference
                                           (new-core (dynamic-get type)
                                                     (dynamic-get init-arguments)))))

(defmessage-handler MAIN::core delete before
                    ()
                    (shutdown-core ?self:reference))

(defmessage-handler MAIN::core call primary
                    (?operation $?args)
                    (call-core ?self:reference
                               ?operation
                               ?args))

(defmessage-handler MAIN::core cycle primary
                    ()
                    (cycle-core ?self:reference))

(defmessage-handler MAIN::core run primary
                    ()
                    (run-core ?self:reference))


(defclass MAIN::register-core
  (is-a core)
  (message-handler install-values primary)
  (message-handler get-register primary)
  (message-handler set-register primary)
  (message-handler read-memory primary)
  (message-handler write-memory primary))

(defmessage-handler MAIN::register-core get-register primary
                    (?index)
                    (call ?self:reference
                          get-register
                          ?index))
(defmessage-handler MAIN::register-core set-register primary
                    (?index ?value)
                    (call ?self:reference
                          set-register
                          ?index
                          ?value))

(defmessage-handler MAIN::register-core read-memory primary
                    (?address)
                    (call ?self:reference
                          read-memory
                          ?address))
(defmessage-handler MAIN::register-core write-memory primary
                    (?address ?value)
                    (call ?self:reference
                          write-memory
                          ?address
                          ?value))


(defclass MAIN::cisc0-core
  (is-a register-core)
  (slot type
        (source composite)
        (default cisc0-core))
  (message-handler install-values primary))

(defmessage-handler MAIN::cisc0-core install-values primary
                    ($?values)
                    (if (oddp (length$ ?values)) then
                      (return (inform-then-halt "Invalid value list!")))
                    (bind ?list
                          ?values)
                    (while (> (length$ ?list) 0) do
                           (bind ?address
                                 (nth$ 1
                                       ?list))
                           (bind ?value
                                 (nth$ 2
                                       ?list))
                           (if (not (call ?self:reference
                                          write-memory
                                          ?address
                                          ?value)) then
                             (return (inform-then-halt "Couldn't write "
                                                       ?value
                                                       " to "
                                                       ?address
                                                       "!")))
                           (bind ?list
                                 (delete$ ?list
                                          1
                                          2)))
                    TRUE)

(defclass MAIN::iris-core
  (is-a register-core)
  (slot type
        (source composite)
        (default iris-core))
  (message-handler install-values primary)
  (message-handler get-predicate-register primary)
  (message-handler set-predicate-register primary)
  (message-handler read-memory primary)
  (message-handler write-memory primary)
  (message-handler read-io-memory primary)
  (message-handler write-io-memory primary)
  (message-handler read-data-memory primary)
  (message-handler write-data-memory primary)
  (message-handler read-code-memory primary)
  (message-handler write-code-memory primary)
  (message-handler read-stack-memory primary)
  (message-handler write-stack-memory primary))


(defmessage-handler MAIN::iris-core read-memory primary
                    (?address)
                    (call ?self:reference
                          read-data-memory
                          ?address))


(defmessage-handler MAIN::iris-core write-memory primary
                    (?address ?value)
                    (call ?self:reference
                          write-data-memory
                          ?address
                          ?value))

(defmessage-handler MAIN::iris-core read-data-memory primary
                    (?address)
                    (call ?self:reference
                          read-data-memory
                          ?address))


(defmessage-handler MAIN::iris-core write-data-memory primary
                    (?address ?value)
                    (call ?self:reference
                          write-data-memory
                          ?address
                          ?value))
(defmessage-handler MAIN::iris-core read-stack-memory primary
                    (?address)
                    (call ?self:reference
                          read-stack-memory
                          ?address))


(defmessage-handler MAIN::iris-core write-stack-memory primary
                    (?address ?value)
                    (call ?self:reference
                          write-stack-memory
                          ?address
                          ?value))
(defmessage-handler MAIN::iris-core read-io-memory primary
                    (?address)
                    (call ?self:reference
                          read-io-memory
                          ?address))


(defmessage-handler MAIN::iris-core write-io-memory primary
                    (?address ?value)
                    (call ?self:reference
                          write-io-memory
                          ?address
                          ?value))

(defmessage-handler MAIN::iris-core read-code-memory primary
                    (?address)
                    (call ?self:reference
                          read-code-memory
                          ?address))


(defmessage-handler MAIN::iris-core write-code-memory primary
                    (?address ?value)
                    (call ?self:reference
                          write-code-memory
                          ?address
                          ?value))

(defmessage-handler MAIN::iris-core get-predicate-register primary
                    (?address)
                    (call ?self:reference
                          get-predicate-register
                          ?address))


(defmessage-handler MAIN::iris-core set-predicate-register primary
                    (?address ?value)
                    (call ?self:reference
                          set-predicate-register
                          ?address
                          ?value))
(defmessage-handler MAIN::iris-core install-values primary
                    ($?values)
                    (if (or (evenp (length$ ?values))
                            (<> (mod (length$ ?values)
                                     3)
                                0)) then
                      (return (inform-then-halt "Invalid value list!")))
                    (bind ?list
                          ?values)
                    (while (> (length$ ?list) 0) do
                           (if (not (bind ?target-operation
                                          (switch (nth$ 1
                                                        ?list)
                                                  (case 0 then write-code-memory)
                                                  (case 1 then write-data-memory)
                                                  (case 2 then write-stack-memory)
                                                  (case 3 then write-io-memory)
                                                  (default FALSE)))) then
                             (return (inform-then-halt "Invalid code space!")))
                           (bind ?address
                                 (nth$ 2
                                       ?list))
                           (bind ?value
                                 (nth$ 3
                                       ?list))
                           (if (not (call ?self:reference
                                          ?target-operation
                                          ?address
                                          ?value)) then
                             (return (inform-then-halt "Invalid action occurred!")))
                           (bind ?list
                                 (delete$ ?list
                                          1
                                          3)))

                    TRUE)
