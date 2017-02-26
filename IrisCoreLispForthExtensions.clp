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

(defrule lower::translate-unknown-calls-to-branch-immediates
         "If we encounter a list which doesn't map to any known instruction then it must be a BIL call!"
         ?f <- (object (is-a list)
                       (contents ?unknown))
         (not (zero-register-operation ?unknown))
         =>
         (modify-instance ?f
                          (contents bil
                                    ?unknown)))

(defrule lower::parse-loop-construct
         "If we see (loop ?label $?body) then just add a bi at the bottom to the label!"
         ?f <- (object (is-a list)
                       (contents loop 
                                 ?title
                                 $?body)
                       (name ?n))
         =>
         (modify-instance ?f
                          (contents label 
                                    ?title 
                                    $?body 
                                    (mk-list ?n
                                             bi
                                             ?title))))
(defrule lower::handle-goto
         ?f <- (object (is-a list)
                       (contents goto
                                 ?label))
         =>
         (modify-instance ?f
                          (contents bi ?label)))
(defrule lower::flatten-words
         ?f <- (object (is-a list)
                       (contents words
                                 ?first
                                 $?rest)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (bind ?contents
               (create$ (mk-list ?n 
                                 word ?first)))

         (progn$ (?a $?rest)
                 (bind ?contents
                       ?contents
                       (mk-list ?n
                                word
                                ?a)))
         (mk-container ?n
                       ?p
                       ?contents))

(defrule lower::parse-string
         "write the target string to a file and then read it back in character by character"
         ?f <- (object (is-a list)
                       (contents string
                                 ?str))
         =>
         (bind ?file
               (str-cat /tmp/ 
                        (bind ?id
                              (gensym*))))
         (if (open ?file ?id "w") then
           ; save the string to a file
           (format ?id 
                   "%s" 
                   (str-cat ?str))
           (close ?id)
           (if (open ?file ?id "r") then
             (bind ?contents 
                   (create$))
             (while (<> (bind ?char
                              (get-char ?id))
                        -1) do
                    (bind ?contents
                          ?contents
                          (sym-cat (format nil 
                                           "0x%x" 
                                           ?char))))
             (close ?id)
             (remove ?file)
             (modify-instance ?f 
                              (contents words
                                        ?contents
                                        0x00))

             else
             (printout werror "Couldn't open " ?file " for reading!" crlf)
             (halt))
           else
           (printout werror "Couldn't open " ?file " for writing!" crlf)
           (halt)))
