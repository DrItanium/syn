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

(defglobal lisp
           ?*address-variable* = r251)
(defgeneric lisp::ret)
(defgeneric lisp::call)
(defgeneric lisp::set)
(defgeneric lisp::putc)
(defgeneric lisp::getc)
(defgeneric lisp::terminate)

(defmethod lisp::terminate
  ()
  (system-op 0 r0 r0))
(defmethod lisp::putc
  ((?reg SYMBOL
         (registerp ?reg)))
  (system-op 2 ?reg ?reg))
(defmethod lisp::getc
  ((?reg SYMBOL
         (registerp ?reg)))
  (system-op 1 ?reg ?reg))

(defmethod lisp::call
  ((?address (immediatep ?current-argument)))
  (create$ (set ?address)
           (jl ?*address-variable*)))

(defmethod lisp::call
  ((?address SYMBOL
             (registerp ?current-argument)))
  (jl ?address))

(defmethod lisp::ret
  ((?register SYMBOL
              (registerp ?current-argument)))
  (j ?register))

(defmethod lisp::ret
  ()
  (ret lr))

(defmethod lisp::set
  ((?address SYMBOL
             (immediatep ?current-argument)))
  (create$ (setl ?*address-variable* 
                 ?address)
           (setu ?*address-variable*
                 ?address)))
