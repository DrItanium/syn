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


(defmodule MAIN
           (import lisp-parse
                   ?ALL))
(defgeneric MAIN::translate-file)
(defmethod MAIN::translate-file
  ((?path LEXEME)
   (?router SYMBOL))
  (assert (parse-request (path ?path))
          (output-router (name ?router)))
  (focus lisp-parse)
  (run)
  TRUE)

(defmethod MAIN::translate-file
  ((?path LEXEME))
  (translate-file ?path
                  t))


(defgeneric MAIN::open-file-for-writing
            "Open a new file for writing")

(defmethod MAIN::open-file-for-writing
 ((?path LEXEME)
  (?router-name SYMBOL))
 (if (open ?path 
           ?router-name
           "w") then
     ?router-name
     else
     (printout werror 
      "Could not open " ?path " for writing!" crlf)
     FALSE))

(defmethod MAIN::open-file-for-writing
  ((?path LEXEME))
  (open-file-for-writing ?path
                         (gensym*)))
