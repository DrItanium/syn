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


(defclass lower::constant-declaration
          (is-a node)
          (slot value
                (visibility public)
                (storage local)
                (default ?NONE))
          (message-handler resolve primary))

(defmessage-handler lower::constant-declaration resolve primary
                    ()
                    (send ?self:value
                          resolve))

(defrule lower::construct-constant-declaration
         (declare (salience ?*priority:first*))
         ?f <- (object (is-a list)
                       (contents let
                                 ?title
                                 mean|be
                                 ?value)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?title of constant-declaration
                        (parent ?parent)
                        (value ?value)))

(defrule lower::replace-constant-declaration
         (declare (salience 100))
         ?f <- (object (is-a list)
                       (contents $?a
                                 ?constant&:(symbolp ?constant)
                                 $?b))
         (object (is-a constant-declaration)
                 (name =(symbol-to-instance-name ?constant)))
         =>
         (modify-instance ?f
                          (contents $?a
                                    (symbol-to-instance-name ?constant)
                                    $?b)))
