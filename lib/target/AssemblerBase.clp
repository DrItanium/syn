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
; AssemblerBase.clp - Common routines for interfacing with the assembler external
; address type. It is common for all assembler types.
;------------------------------------------------------------------------------
(defgeneric MAIN::asm-parse-line
            "Parse an assembler line")
(defgeneric MAIN::asm-resolve-labels
            "Second phase: replace labels with addresses")
(defgeneric MAIN::asm-get-encoded-instructions
            "Return instructions encoded into the format of the target core")
(defgeneric MAIN::asm-parse-file
            "Parse the given file using the given assembler target")

(defmethod MAIN::asm-parse-line
  ((?asm EXTERNAL-ADDRESS)
   (?line LEXEME))
  (call ?asm
        parse
        ?line))
(defmethod MAIN::asm-resolve-labels
  ((?asm EXTERNAL-ADDRESS))
  (call ?asm
        resolve))
(defmethod MAIN::asm-get-encoded-instructions
  ((?asm EXTERNAL-ADDRESS))
  (call ?asm
        get))

(defmethod MAIN::asm-parse-file
  ((?asm EXTERNAL-ADDRESS)
   (?path LEXEME))
  (if (open ?path
            (bind ?name
                  (gensym*))
            "r") then
    (while (neq (bind ?line
                      (readline ?name))
                EOF) do
           (asm-parse-line ?asm
                           ?line))
    (close ?name)
    TRUE))
