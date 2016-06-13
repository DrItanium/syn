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
           ?*address-variable* = r251
           ?*arg0* = r250
           ?*arg1* = r249
           ?*arg2* = r248
           ?*arg3* = r247
           ?*arg4* = r246
           ?*arg5* = r245
           ?*arg6* = r244
           ?*arg7* = r243
           ?*ret0* = r242
           ?*ret1* = r241
           ?*ret2* = r240
           ?*ret3* = r239
           ?*tmp0* = r238
           ?*tmp1* = r237
           ?*tmp2* = r236
           ?*tmp3* = r235)

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


(defgeneric lisp::str-length:function)
(defgeneric lisp::new-function)

(defmethod lisp::new-function
  ((?title SYMBOL)
   $?body)
  (create$ (@code)
           (@label ?title)
           $?body))
(defmethod lisp::str-length:function
  ()
  (new-function strlength
                (push ?*tmp0*)
                (push ?*tmp1*)
                (push ?*tmp2*)
                (set ?*ret0* 0)
                (set ?*tmp2* 
                     strlength_loop_top)
                (@label strlength_loop_top)
                (ld ?*tmp0*
                    ?*arg0*)
                (nei ?*tmp1* ?*tmp0* 0)
                (jf ?*tmp1* 
                    strlength_done)
                (incr ?*ret0*)
                (incr ?*arg0*)
                (j ?*tmp2*)
                (@label strlength_done)
                (pop ?*tmp2*)
                (pop ?*tmp1*)
                (pop ?*tmp0*)
                (ret)))
(defmethod lisp::str-length.
  "Call strlength"
  ((?ptr SYMBOL
         (registerp ?ptr)))
  (create$ (push ?*arg0*)
           (move ?*arg0* ?ptr)
           (call strlength)
           (pop ?*arg0*)))

(defmethod lisp::str-length.
  ((?ptr (immediatep ?ptr)))
  (create$ (push ?*arg0*)
           (set ?ptr)
           (move ?*arg0* 
                 ?*address-variable*)
           (call strlength)
           (pop ?*arg0*)))
(defmethod lisp::multi-op
  ((?op SYMBOL)
   (?destination SYMBOL
                 (registerp ?current-argument))
   (?source0 SYMBOL
             (registerp ?current-argument))
   (?source1 SYMBOL
             (registerp ?current-argument))
   $?rest)
  (create$ (funcall ?op 
                    ?destination 
                    ?source0 
                    ?source1)
           (if (> (length$ ?rest) 0) then
             (multi-op ?op 
                       ?destination 
                       ?destination 
                       (expand$ ?rest))
             else
             (create$))))

