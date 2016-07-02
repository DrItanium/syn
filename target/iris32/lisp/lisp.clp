; iris
; Copyright (c) 2013-2015, Joshua Scoggins and Contributors
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
(defmodule lisp
           (import iris32 ?ALL)
           (export ?ALL))


(defgeneric lisp::set)
(defgeneric lisp::putc)
(defgeneric lisp::getc)
(defgeneric lisp::terminate)
(defgeneric lisp::defun)

(defgeneric lisp::save)
(defgeneric lisp::restore)
(defmethod lisp::defun
  ((?title SYMBOL)
   $?body)
  (create$ (@label ?title)
           $?body
           (j lr)))

(defmethod lisp::terminate
  ()
  (system-op 0 r0 r0))

(defmethod lisp::putc
  ((?reg SYMBOL
         (registerp ?reg)))
  (system-op 2 ?reg ?reg))
(defmethod lisp::putc
  ((?reg SYMBOL
         (register-aliasp ?current-argument)))
  (putc (send (symbol-to-instance-name ?reg) get-refers-to)))

(defmethod lisp::getc
  ((?reg SYMBOL
         (registerp ?reg)))
  (system-op 1 ?reg ?reg))

(defmethod lisp::getc
  ((?reg SYMBOL
         (register-aliasp ?current-argument)))
  (getc (send (symbol-to-instance-name ?reg) get-refers-to)))

(defmethod lisp::set
  ((?destination SYMBOL)
   (?address (immediatep ?current-argument)))
  (create$ (setl ?destination
                 ?address)
           (setu ?destination
                 ?address)))


(defmethod lisp::save
  ($?registers)
  (bind ?output
        (create$))
  (progn$ (?r ?registers)
          (bind ?output
                ?output
                (push ?r)))
  ?output)
(defmethod lisp::restore
  ($?registers)
  (bind ?output
        (create$))
  (progn$ (?r ?registers)
          (bind ?output
                ?output
                (pop ?r)))
  ?output)

(defclass lisp::data-element
  (is-a register)
  (slot type
        (type SYMBOL)
        (allowed-symbols number 
                         char
                         string
                         list)
        (visibility public)
        (storage local)
        (default ?NONE)))


(deffunction lisp::bind-to-register
             (?collection 
              ?start-register 
              ?max
              ?error-message)
             (if (> (length$ ?collection)
                    ?max) then
                 (printout werror ?error-message crlf)
                 FALSE
                 else
                 (progn$ (?c ?collection)
                         (send ?c put-refers-to 
                               (sym-cat ?start-register
                                        (- ?c-index 1))))
                 ?collection))
(deffunction lisp::args
             ($?args)
             (bind-to-register ?args
                               in
                               16
                               "Too many input arguments"))
(deffunction lisp::returns
             ($?returns)
             (bind-to-register ?returns
                               out
                               16
                               "Too many output arguments"))
(deffunction lisp::locals
             ($?locals)
             (bind-to-register ?locals
                               temp
                               32
                               "Too many locals"))

(deffunction lisp::string
             (?title)
             (make-instance ?title of data-element
                           (type string)))
(deffunction lisp::number
             (?title)
             (make-instance ?title of data-element
                            (type number)))
(deffunction lisp::list
             (?title)
             (make-instance ?title of data-element
                            (type list)))


(defmethod lisp::defun
  ((?name SYMBOL)
   (?args MULTIFIELD
          (<= (length$ ?current-argument)
              16))
   (?returns MULTIFIELD
             (<= (length$ ?current-argument)
                 16))
   (?locals MULTIFIELD
            (<= (length$ ?current-argument)
                32))
   $?body)
  (printout t 
   "?name = " ?name crlf
   "?args = " ?args crlf
   "?returns = " ?returns crlf
   "?locals = " ?locals crlf
   "?body = " ?body crlf)

    
        
  FALSE
  )

