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
; SimpleMemoryBlock.clp - a separate device process meant to respond on
; named pipes to requests
;------------------------------------------------------------------------------
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* ExternalAddressWrapper.clp)
(batch* Device.clp)
(batch* MemoryBlock.clp)
(batch* Paragraph.clp)
(batch* order.clp)
(batch* SimpleServer.clp)

(defgeneric MAIN::make-section)
(defgeneric MAIN::make-paragraph)
(defgeneric MAIN::make-page)
(defmethod MAIN::make-paragraph
  ((?count INTEGER
           (>= 8 
               ?current-argument
               1)))
  (bind ?result
        (create$))
  (loop-for-count (?i 1 ?count) do
                  (bind ?result
                        ?result
                        (make-instance of encyclopedia-sentence)))
  (make-instance of encyclopedia-paragraph
                 (children ?result)))

(defmethod MAIN::make-paragraph
  ()
  (make-paragraph 8))

(defmethod MAIN::make-page
  ()
  (make-page 256))

(defmethod MAIN::make-page
  ((?count INTEGER
           (>= 256
               ?current-argument
               1)))

  (bind ?result
        (create$))
  (loop-for-count (?i 1 ?count) do
                  (bind ?result
                        ?result
                        (make-paragraph)))
  (make-instance of encyclopedia-page
                 (children ?result)))

(defmethod MAIN::make-section
  ()
  (make-section 256))
(defmethod MAIN::make-section
  ((?num-pages INTEGER
               (>= 256 ?current-argument
                   1)))
  (bind ?result
        (create$))
  (loop-for-count (?i 1 ?num-pages) do
                  (bind ?result
                        ?result
                        (make-page)))
  (make-instance of encyclopedia-section
                 (children ?result)))
(definstances MAIN::main-memory
              (main-memory of iris64-encyclopedia 
                           (children)))

(deffacts MAIN::sections
          (make-section for [main-memory] (gensym*))
          (make-section for [main-memory] (gensym*))
          (make-section for [main-memory] (gensym*)))

(deffacts MAIN::stage-order
          (stage (current system-init)
                 (rest read
                       dispatch
                       restart)))
(defrule MAIN::construct-section
         (stage (current system-init))
         ?f <- (make-section for ?mem ?)
         (object (is-a iris64-encyclopedia)
                 (name ?mem)
                 (children $?children))
         =>
         (retract ?f)
         ; have to do this outside the modify instance to keep performance up
         (bind ?section
               (make-section))
         (modify-instance ?mem
                          (children $?children
                                    ?section)))



(defrule MAIN::add-memory-location-to-command
         (stage (current read))
         ?f <- (inspect action)
         ?k <- (action $?body)
         (object (is-a iris64-encyclopedia)
                 (name ?target))
         =>
         (retract ?f ?k)
         (assert (action ?body from ?target)))

; TODO: add support for restarting execution
;----------------------------------------------------------------
; Commands are - read, write, shutdown

(defrule MAIN::read-memory
         (stage (current dispatch))
         ?k <- (action read ?address callback ?callback from ?target)
         (object (is-a iris64-encyclopedia)
                 (name ?target))
         =>
         (retract ?k)
         (assert (command-writer (target ?callback)
                                 (command (send ?target
                                                read
                                                ?address)))))

(defrule MAIN::write-memory
         (stage (current dispatch))
         ?k <- (action write ?address ?value callback ?callback from ?target)
         (object (is-a iris64-encyclopedia)
                 (name ?target))
         =>
         (retract ?k)
         (assert (command-writer (target ?callback)
                                 (command (send ?target
                                                write
                                                ?address
                                                ?value)))))

;(deffacts MAIN::connection-info
;          (setup connection /tmp/syn/memory))
