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

; Define the base wrapper classes for the different assemblers

(defgeneric MAIN::new-assembly-parser)
(defgeneric MAIN::parse-assembly-line)
(defgeneric MAIN::resolve-assembly)
(defgeneric MAIN::get-converted-assembly)
(defgeneric MAIN::parse-assembly-lines)

(defmethod MAIN::parse-assembly-lines
  ((?a EXTERNAL-ADDRESS)
   (?lines MULTIFIELD))
  (progn$ (?line ?lines)
          (if (not (parse-assembly-line ?a
                                        ?line)) then
            (return FALSE)))
  TRUE)

(defmethod MAIN::parse-assembly-lines
  ((?a EXTERNAL-ADDRESS)
   $?lines)
  (parse-assembly-lines ?a
                        ?lines))

(defmethod MAIN::new-assembly-parser
  ((?title LEXEME)
   (?args MULTIFIELD))
  (new ?title
       (expand$ ?args)))

(defmethod MAIN::new-assembly-parser
  ((?title LEXEME)
   $?args)
  (new-assembly-parser ?title
                       ?args))
(defmethod MAIN::parse-assembly-line
  ((?assembler EXTERNAL-ADDRESS)
   (?line LEXEME))
  (call ?assembler
        parse
        ?line))

(defmethod MAIN::resolve-assembly
  ((?assembler EXTERNAL-ADDRESS))
  (call ?assembler
        resolve))

(defmethod MAIN::get-converted-assembly
  ((?assembler EXTERNAL-ADDRESS))
  (call ?assembler
        get))

(defclass MAIN::assembler
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
  (message-handler parse primary)
  (message-handler parse-lines primary)
  (message-handler resolve primary)
  (message-handler clear primary)
  (message-handler get primary)
  (message-handler init after))

(defmessage-handler MAIN::assembler clear primary
                    ()
                    (bind ?self:reference
                          (new-assembly-parser (dynamic-get type)
                                               (dynamic-get init-arguments))))

(defmessage-handler MAIN::assembler init after
                    ()
                    (bind ?self:reference
                          (new-assembly-parser (dynamic-get type)
                                               (dynamic-get init-arguments))))

(defmessage-handler MAIN::assembler parse primary
                    (?input)
                    (parse-assembly-line ?self:reference
                                         ?input))

(defmessage-handler MAIN::assembler resolve primary
                    ()
                    (resolve-assembly ?self:reference))

(defmessage-handler MAIN::assembler get primary
                    ()
                    (get-converted-assembly ?self:reference))

(defmessage-handler MAIN::assembler parse-lines primary
                    ($?lines)
                    (parse-assembly-lines ?self:reference
                                          ?lines))

(defclass MAIN::cisc0-assembler
  (is-a assembler)
  (slot type
        (source composite)
        (default cisc0-assembler)))

(defclass MAIN::iris-assembler
  (is-a assembler)
  (slot type
        (source composite)
        (default iris-assembler)))

(defgeneric MAIN::parse-file)

(defmethod MAIN::parse-file
  ((?target SYMBOL)
   (?file LEXEME))
  (bind ?tmp
        (new-assembly-parser ?target))
  (if (not ?tmp) then
    (return FALSE))
  (bind ?router
        (gensym*))
  (if (open ?file 
            ?router 
            "r") then
    (while (neq (bind ?line
                      (readline ?router))
                EOF) do
           (if (not (parse-assembly-line ?tmp
                                         ?line)) then 
             (return FALSE)))
    (close ?router)
    (resolve-assembly ?tmp)
    (get-converted-assembly ?tmp)
    else
    (printout werror "Couldn't open " ?file " for reading!" crlf)
    FALSE))
