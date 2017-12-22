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
; test-encyclopedia-scheme.clp
;------------------------------------------------------------------------------
(batch* cortex.clp)
(batch* MainModuleDeclaration.clp)
(batch* ExternalAddressWrapper.clp)
(batch* Device.clp)
(batch* MemoryBlock.clp)
(batch* Paragraph.clp)
(batch* order.clp)

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
(deffacts MAIN::execution-flow 
          (stage (current initialize)
                 (rest setup-tables
                       check)))

(defrule MAIN::construct-section
         (stage (current initialize))
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
(defrule MAIN::setup-interrupt-table
         "During initial setup, write a given value to the target address"
         (stage (current setup-tables))
         ?f <- (write ?value to ?address)
         =>
         (retract ?f)
         (assert (verify write ?value to ?address))
         (send [main-memory]
               write
               ?address
               ?value))
; the first paragraph (64k) is for the bios and other such things
(defglobal MAIN
           ?*system-structure-base-address* = (hex->int 0x0400) ; the interrupt table starts the second sentence, the first sentence is for a simple boot loader
           ?*interrupt-table-base-address* = (+ ?*system-structure-base-address*
                                                0)
           ?*interrupt-table-layout* = (create$ illegal-instruction
                                                divide-by-zero
                                                user))



(defrule MAIN::initialize-interrupt-table-entries
         (declare (salience ?*priority:first*))
         (stage (current initialize))
         =>
         (progn$ (?table ?*interrupt-table-layout*)
                 (assert (interrupt-table-entry ?table
                                                (+ ?*interrupt-table-base-address*
                                                   (- ?table-index
                                                      1))))))

(defrule MAIN::translate-interrupt-table-write
         (stage (current setup-tables))
         ?f <- (interrupt ?name ->
                          ?value)
         (interrupt-table-entry ?name
                                ?address)
         =>
         (retract ?f)
         (printout t 
                   "Setting interrupt " ?name " at " ?address " to " ?value crlf)
         (assert (write ?value to ?address)))

(deffacts MAIN::interrupt-table-layout
          (interrupt illegal-instruction -> (hex->int 0x2000))
          (interrupt divide-by-zero -> (hex->int 0x2100))
          (interrupt user -> (hex->int 0x2200)))

(defrule MAIN::verify-writes
         (stage (current check))
         ?f <- (verify write ?value to ?address)
         =>
         (retract ?f)
         (bind ?from-memory
               (send [main-memory]
                     read
                     ?address))
         (if (neq ?from-memory
                  ?value) then
             (halt)
             (printout werror
                       "ERROR: write to address " ?address " failed!" crlf
                       tab "Expected: " ?value " but got " ?from-memory " instead!" crlf)))


