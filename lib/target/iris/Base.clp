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
; Base.clp - routines to make interfacing with the raw iris external address
; far simpler
;------------------------------------------------------------------------------
(batch* lib/target/ExternalAddressWrapper.clp)
(batch* lib/target/CoreBase.clp)
(batch* lib/target/AssemblerBase.clp)
(defgeneric MAIN::iris-decode-instruction
            "Given an instruction encoded as an integer, translate it back to a string form")
(defgeneric MAIN::iris-initialize)
(defgeneric MAIN::iris-shutdown)
(defgeneric MAIN::iris-run)
(defgeneric MAIN::iris-cycle)
(defgeneric MAIN::iris-write-memory)
(defgeneric MAIN::iris-read-memory)
(defgeneric MAIN::iris-get-register)
(defgeneric MAIN::iris-set-register)
(defgeneric MAIN::iris-get-predicate-register)
(defgeneric MAIN::iris-set-predicate-register)
(defgeneric MAIN::iris-parse-instruction)
(defgeneric MAIN::iris-parse-instructions)
(defgeneric MAIN::iris-resolve-assembler-labels)
(defgeneric MAIN::iris-get-encoded-instructions)
(defgeneric MAIN::iris-parse-and-encode-instruction)
(defgeneric MAIN::iris-parse-file)

(defmethod MAIN::iris-decode-instruction
  ((?core EXTERNAL-ADDRESS)
   (?instruction INTEGER))
  (call ?core
        translate-instruction
        ?instruction))
(defmethod MAIN::iris-initialize
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        initialize))
(defmethod MAIN::iris-shutdown
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        shutdown))

(defmethod MAIN::iris-run
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        run))

(defmethod MAIN::iris-cycle
  ((?core EXTERNAL-ADDRESS))
  (call ?core
        cycle))

(defmethod MAIN::iris-cycle
  "Run the core the provided number of times!"
  ((?core EXTERNAL-ADDRESS)
   (?count INTEGER
           (>= ?current-argument 1)))
  (bind ?last-result
        TRUE)
  (loop-for-count (?c 1 ?count)
                  (bind ?last-result
                        (iris-cycle ?core))
                  (if (not ?last-result) then
                    (break)))
  ?last-result)


(defmethod MAIN::iris-write-memory
  ((?core EXTERNAL-ADDRESS)
   (?space SYMBOL
           (not (neq ?current-argument
                     data
                     code
                     io
                     stack)))
   (?address INTEGER)
   (?value INTEGER))
  (call ?core
        (sym-cat write- ?space -memory)
        ?address
        ?value))
(defglobal MAIN
           ?*iris-memory-spaces* = (create$ code
                                            data
                                            stack
                                            io))
(defmethod MAIN::iris-write-memory
  ((?core EXTERNAL-ADDRESS)
   (?space INTEGER)
   (?address INTEGER)
   (?value INTEGER))
  (iris-write-memory ?core
                     (nth$ (+ ?space 1)
                           ?*iris-memory-spaces*)
                     ?address
                     ?value))

(defmethod MAIN::iris-read-memory
  ((?core EXTERNAL-ADDRESS)
   (?space SYMBOL
           (not (neq ?current-argument
                     data
                     code
                     io
                     stack)))
   (?address INTEGER))
  (call ?core
        (sym-cat read- ?space -memory)
        ?address))
(defmethod MAIN::iris-get-register
  ((?core EXTERNAL-ADDRESS)
   (?index INTEGER))
  (call ?core
        get-register
        ?index))
(defmethod MAIN::iris-set-register
  ((?core EXTERNAL-ADDRESS)
   (?index INTEGER)
   (?value INTEGER))
  (call ?core
        set-register
        ?index
        ?value))

(defmethod MAIN::iris-get-predicate-register
  ((?core EXTERNAL-ADDRESS)
   (?index INTEGER))
  (call ?core
        get-predicate-register
        ?index))
(defmethod MAIN::iris-set-predicate-register
  ((?core EXTERNAL-ADDRESS)
   (?index INTEGER)
   (?value INTEGER))
  (call ?core
        set-predicate-register
        ?index
        ?value))
(defmethod MAIN::iris-set-predicate-register
  ((?core EXTERNAL-ADDRESS)
   (?index INTEGER)
   (?value SYMBOL))
  (iris-set-predicate-register ?core
                               ?index
                               (if ?value then
                                 (hex->int 0xFFFF)
                                 else
                                 0)))

(defmethod MAIN::iris-parse-instruction
  ((?asm EXTERNAL-ADDRESS)
   (?statement LEXEME))
  (call ?asm
        parse
        ?statement))

(defmethod MAIN::iris-parse-instructions
  ((?asm EXTERNAL-ADDRESS)
   (?lines MULTIFIELD))
  (progn$ (?line ?lines)
          (iris-parse-instruction ?asm
                                  ?line)))
(defmethod MAIN::iris-parse-instructions
  ((?asm EXTERNAL-ADDRESS)
   $?lines)
  (iris-parse-instructions ?asm
                           ?lines))

(defmethod MAIN::iris-resolve-assembler-labels
  ((?asm EXTERNAL-ADDRESS))
  (call ?asm
        resolve))

(defmethod MAIN::iris-get-encoded-instructions
  ((?asm EXTERNAL-ADDRESS))
  (call ?asm
        get))
(defmethod MAIN::iris-parse-and-encode-instruction
  ((?statement LEXEME))
  (bind ?asm
        (new iris-assembler))
  (iris-parse-instruction ?asm
                          ?statement)
  (iris-resolve-assembler-labels ?asm)
  ; grab the actual encoded value
  (nth$ 3
        (iris-get-encoded-instructions ?asm)))

(defmethod MAIN::iris-decode-instruction
  ((?core EXTERNAL-ADDRESS)
   (?value INTEGER))
  (call ?core
        decode-instruction
        ?value))
(defmethod MAIN::iris-parse-file
  ((?asm EXTERNAL-ADDRESS)
   (?path LEXEME))
  (asm-parse-file ?asm
                  ?path))

(defmethod MAIN::iris-parse-file
  ((?path LEXEME))
  (bind ?asm
        (new iris-assembler))
  (if (iris-parse-file ?asm
                       ?path) then
    (iris-resolve-assembler-labels ?asm)
    (iris-get-encoded-instructions ?asm)))
(defmethod MAIN::iris-get-register
  ((?core EXTERNAL-ADDRESS)
   (?register SYMBOL
              (eq (str-index r
                             ?current-argument)
                  1)))
  (iris-get-register ?core
                     (string-to-field (sub-string 2
                                                  (str-length ?register)
                                                  ?register))))

(defmethod MAIN::iris-set-register
  ((?core EXTERNAL-ADDRESS)
   (?register SYMBOL
              (eq (str-index r
                             ?current-argument)
                  1))
   (?value INTEGER))

  (iris-set-register ?core
                     (string-to-field (sub-string 2
                                                  (str-length ?register)
                                                  ?register))
                     ?value))
(defclass MAIN::iris-assembler
  (is-a assembler)
  (role concrete)
  (pattern-match reactive)
  (slot backing-type
        (source composite)
        (storage shared)
        (default iris-assembler)))

(defclass MAIN::iris-core
  (is-a core)
  (role concrete)
  (pattern-match reactive)
  (slot backing-type
        (source composite)
        (storage shared)
        (default iris-core)))
