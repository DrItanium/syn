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
; Base.clp - routines to make interfacing with the raw cisc0 external address 
; far simpler 
;------------------------------------------------------------------------------
(batch* lib/target/AssemblerBase.clp)
(defgeneric MAIN::cisc0-decode-instruction
            "Given an instruction encoded as an integer, translate it back to a string form")
(defgeneric MAIN::cisc0-initialize)
(defgeneric MAIN::cisc0-shutdown)
(defgeneric MAIN::cisc0-run)
(defgeneric MAIN::cisc0-cycle)
(defgeneric MAIN::cisc0-write-memory)
(defgeneric MAIN::cisc0-read-memory)
(defgeneric MAIN::cisc0-get-register)
(defgeneric MAIN::cisc0-set-register)
(defgeneric MAIN::cisc0-parse-line)
(defgeneric MAIN::cisc0-resolve-instructions)
(defgeneric MAIN::cisc0-get-encoded-instructions)
(defgeneric MAIN::cisc0-parse-file)

(defmethod MAIN::cisc0-decode-instruction
  ((?core EXTERNAL-ADDRESS)
   (?instruction INTEGER))
  (call ?core 
        translate-instruction
        ?instruction))
(defmethod MAIN::cisc0-initialize
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        initialize))
(defmethod MAIN::cisc0-shutdown
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        shutdown))

(defmethod MAIN::cisc0-run
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        run))

(defmethod MAIN::cisc0-cycle
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        cycle))

(defmethod MAIN::cisc0-cycle
  "Run the core the provided number of times!"
  ((?core EXTERNAL-ADDRESS)
   (?count INTEGER
           (>= ?current-argument 1)))
  (bind ?last-result
        TRUE)
  (loop-for-count (?c 1 ?count)
                  (bind ?last-result
                        (cisc0-cycle ?core))
                  (if (not ?last-result) then
                    (break)))
  ?last-result)


(defmethod MAIN::cisc0-write-memory
  ((?core EXTERNAL-ADDRESS)
   (?address INTEGER)
   (?value INTEGER))
  (call ?core
        write-memory
        ?address
        ?value))
(defmethod MAIN::cisc0-read-memory
  ((?core EXTERNAL-ADDRESS)
   (?address INTEGER))
  (call ?core 
        read-memory
        ?address))

(defmethod MAIN::cisc0-get-register
  ((?core EXTERNAL-ADDRESS)
   (?index INTEGER))
  (call ?core
        get-register
        ?index))
(defmethod MAIN::cisc0-set-register
  ((?core EXTERNAL-ADDRESS)
   (?index INTEGER)
   (?value INTEGER))
  (call ?core
        set-register
        ?index
        ?value))

(defmethod MAIN::cisc0-parse-line
  ((?asm EXTERNAL-ADDRESS)
   (?line LEXEME))
  (asm-parse-line ?asm
                  ?line))
(defmethod MAIN::cisc0-resolve-instructions
  ((?asm EXTERNAL-ADDRESS))
  (asm-resolve-labels ?asm))
(defmethod MAIN::cisc0-get-encoded-instructions
  ((?asm EXTERNAL-ADDRESS))
  (asm-get-encoded-instructions ?asm))

(defmethod MAIN::cisc0-parse-file
  ((?asm EXTERNAL-ADDRESS)
   (?path LEXEME))
  (asm-parse-file ?asm
                  ?path))

(defmethod MAIN::cisc0-parse-file
  ((?path LEXEME))
  (bind ?asm
        (new cisc0-assembler))
  (if (cisc0-parse-file ?asm
                        ?path) then
    (cisc0-resolve-instructions ?asm)
    (cisc0-get-encoded-instructions ?asm)))

