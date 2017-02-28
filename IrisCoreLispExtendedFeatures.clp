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
; more wrapper features
;------------------------------------------------------------------------------

(defrule lower::bind-macro-alias
         ?f <- (object (is-a list)
                       (contents bind
                                 ?register
                                 ?value))
         =>
         (modify-instance ?f
                          (contents set
                                    ?register
                                    ?value)))

(defrule lower::parse-increment-macro
         ?f <- (object (is-a list)
                       (contents incr
                                 ?destination
                                 ?register))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f
                          (contents addi
                                    ?destination
                                    ?register
                                    1)))
(defrule lower::parse-increment-self-macro
         ?f <- (object (is-a list)
                       (contents incr
                                 ?register))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f
                          (contents incr
                                    ?register
                                    ?register)))



(defrule lower::parse-decrement-macro
         ?f <- (object (is-a list)
                       (contents decr
                                 ?destination
                                 ?register))
         (object (is-a register)
                 (name ?register))
         =>
         (modify-instance ?f
                          (contents subi
                                    ?destination
                                    ?register
                                    1)))

(defrule lower::parse-decrement-macro:one-arg
         ?f <- (object (is-a list)
                       (contents decr
                                 ?register))
         =>
         (modify-instance ?f
                          (contents decr
                                    ?register
                                    ?register)))
(defrule lower::parse-double-macro
         ?f <- (object (is-a list)
                       (contents double
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents add
                                    ?destination
                                    ?register
                                    ?register)))

(defrule lower::parse-double-macro:one-arg
         ?f <- (object (is-a list)
                       (contents double
                                 ?register))
         =>
         (modify-instance ?f
                          (contents double
                                    ?register
                                    ?register)))

(defrule lower::parse-triple-macro
         ?f <- (object (is-a list)
                       (contents triple
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents muli
                                    ?destination
                                    ?register
                                    3)))

(defrule lower::parse-triple-macro:one-arg
         ?f <- (object (is-a list)
                       (contents triple
                                 ?register))
         =>
         (modify-instance ?f
                          (contents triple
                                    ?register
                                    ?register)))

(defrule lower::parse-halve-macro
         ?f <- (object (is-a list)
                       (contents halve
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents divi
                                    ?destination
                                    ?register
                                    2)))

(defrule lower::parse-halve-macro:one-arg
         ?f <- (object (is-a list)
                       (contents halve
                                 ?register))
         =>
         (modify-instance ?f
                          (contents halve
                                    ?register
                                    ?register)))
(defrule lower::parse-square-macro
         ?f <- (object (is-a list)
                       (contents square
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents mul
                                    ?destination
                                    ?register
                                    ?register)))

(defrule lower::parse-square-macro:one-arg
         ?f <- (object (is-a list)
                       (contents square
                                 ?register))
         =>
         (modify-instance ?f
                          (contents square
                                    ?register
                                    ?register)))

(defrule lower::parse-cube-macro
         ?f <- (object (is-a list)
                       (contents cube
                                 ?destination
                                 ?register))
         =>
         (modify-instance ?f
                          (contents mul
                                    ?destination
                                    ?register
                                    ?register
                                    ?register)))

(defrule lower::parse-cube-macro:one-arg
         ?f <- (object (is-a list)
                       (contents cube
                                 ?register))
         =>
         (modify-instance ?f
                          (contents cube
                                    ?register
                                    ?register)))


(defrule lower::parse-when-goto-as-bic
         "A special form of bic which states (when ?predicate goto ?address)"
         ?f <- (object (is-a list)
                       (contents when
                                 ?predicate
                                 goto
                                 ?target))
         =>
         (modify-instance ?f
                          (contents bic
                                    ?predicate
                                    ?target)))

(defrule lower::parse-when-branch-as-bc
         "A special form of bc which states (when ?predicate branch ?register)"
         ?f <- (object (is-a list)
                       (contents when
                                 ?predicate
                                 branch
                                 ?register))
         =>
         (modify-instance ?f
                          (contents bc
                                    ?predicate
                                    ?register)))
(defrule lower::parse-copy-as-move
         ?f <- (object (is-a list)
                       (contents copy
                                 ?first
                                 ?second))
         =>
         (modify-instance ?f
                          (contents move
                                    ?first
                                    ?second)))
(defrule lower::load:one-arg
         ?f <- (object (is-a list)
                       (contents ld
                                 ?register))
         =>
         (modify-instance ?f
                          (contents ld
                                    ?register
                                    ?register)))

(defrule lower::translate-unknown-calls-to-branch-immediates
         "If we encounter a list which doesn't map to any known instruction then it must be a BIL call!"
         ?f <- (object (is-a list)
                       (contents ?unknown))
         (not (zero-register-operation ?unknown))
         =>
         (modify-instance ?f
                          (contents call-immediate
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
                          (contents bi
                                    ?label)))

(defrule lower::handle-call-immediate
         ?f <- (object (is-a list)
                       (contents call-immediate
                                 ?target))
         =>
         (modify-instance ?f
                          (contents bil
                                    ?target)))

(defrule lower::handle-call-indirect
         ?f <- (object (is-a list)
                       (contents call
                                 ?target))
         =>
         (modify-instance ?f
                          (contents bl
                                    ?target)))

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
                                 word
                                 ?first)))

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

(defrule lower::is-zero
         "Macro for checking to see if a given number is equal to zero"
         ?f <- (object (is-a list)
                       (contents is-zero
                                 ?on-true
                                 ?on-false
                                 ?register))
         =>
         (modify-instance ?f
                          (contents eqi
                                    ?on-true
                                    ?on-false
                                    ?register
                                    0x00)))

(defrule lower::is-not-zero
         ?f <- (object (is-a list)
                       (contents is-not-zero
                                 ?on-true
                                 ?on-false
                                 ?register))
         =>
         (modify-instance ?f
                          (contents nei
                                    ?on-true
                                    ?on-false
                                    ?register
                                    0x00)))

(defrule lower::greater-than-macro
         ?f <- (object (is-a list)
                       (contents >
                                 ?on-true
                                 ?on-false
                                 ?registerA
                                 ?registerB))
         =>
         (modify-instance ?f
                          (contents gt
                                    ?on-true
                                    ?on-false
                                    ?registerA
                                    ?registerB)))

(defrule lower::greater-than-immediate-macro
         ?f <- (object (is-a list)
                       (contents >
                                 ?on-true
                                 ?on-false
                                 ?registerA
                                 immediate
                                 ?immediate))
         =>
         (modify-instance ?f
                          (contents gti
                                    ?on-true
                                    ?on-false
                                    ?registerA
                                    ?immediate)))

(defrule lower::less-than-macro
         ?f <- (object (is-a list)
                       (contents <
                                 ?on-true
                                 ?on-false
                                 ?registerA
                                 ?registerB))
         =>
         (modify-instance ?f
                          (contents lt
                                    ?on-true
                                    ?on-false
                                    ?registerA
                                    ?registerB)))

(defrule lower::less-than-immediate-macro
         ?f <- (object (is-a list)
                       (contents <
                                 ?on-true
                                 ?on-false
                                 ?registerA
                                 immediate
                                 ?immediate))
         =>
         (modify-instance ?f
                          (contents lti
                                    ?on-true
                                    ?on-false
                                    ?registerA
                                    ?immediate)))

(defrule lower::nop-macro
         ?f <- (object (is-a list)
                       (contents nop))
         =>
         (modify-instance ?f
                          (contents addi
                                    iv0
                                    iv0
                                    0)))
(defrule lower::zero-register-macro
         ?f <- (object (is-a list)
                       (contents clear
                                 ?register))
         =>
         (modify-instance ?f
                          (contents set
                                    ?register
                                    0x0000)))
