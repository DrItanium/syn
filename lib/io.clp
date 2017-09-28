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


; IO related operations
(defgeneric MAIN::include
            "Iterate through a search path to try and find the first the instance of a given file!")
(defgeneric MAIN::include:batch
            "Perform the include using the batch command!")
(defgeneric MAIN::include:batch*
            "Perform the include using the batch* command!")
(defgeneric MAIN::include:load
            "Perform the include using the load command!")
(defgeneric MAIN::include:load*
            "Perform the include using the load* command!")

(defgeneric MAIN::add-to-search-path)

(defglobal MAIN
           ?*legal-io-load-commands* = (create$ batch
                                                batch*
                                                load
                                                load*)
           ?*search-path* = (create$ .))

(defmethod MAIN::include
  "Try to load the given file using a specified clips operation"
  ((?operation SYMBOL
               (not (neq ?current-argument
                         (expand$ ?*legal-io-load-commands*))))
   (?path LEXEME))
  ; concat the path fragment together with the different search paths
  ; assume a unix environment
  (progn$ (?prefix ?*search-path*)
          (if (funcall ?operation
                       (str-cat ?prefix / ?path)) then
            (return TRUE)))
  FALSE)

(defmethod MAIN::add-to-search-path
  ((?location LEXEME))
  (bind ?*search-path*
        ?*search-path*
        ?location))

(defmethod MAIN::include:batch
  ((?path LEXEME))
  (include batch
           ?path))
(defmethod MAIN::include:batch*
  ((?path LEXEME))
  (include batch*
           ?path))
(defmethod MAIN::include:load
  ((?path LEXEME))
  (include load
           ?path))
(defmethod MAIN::include:load*
  ((?path LEXEME))
  (include load*
           ?path))
