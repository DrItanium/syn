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
; ExternalAddressWrapper.clp - Common class concept for external address wrappers
;------------------------------------------------------------------------------
(defclass MAIN::external-address-wrapper
  (is-a USER)
  (slot backing-type
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot backing-store
        (type EXTERNAL-ADDRESS)
        (storage local)
        (visibility public))
  (multislot constructor-args
             (storage local)
             (visibility public))
  (message-handler init around)
  (message-handler call primary))

(defmessage-handler MAIN::external-address-wrapper init around
                    ()
                    (call-next-handler)
                    (bind ?self:backing-store
                          (new (dynamic-get backing-type)
                               (expand$ (dynamic-get constructor-args)))))

(defmessage-handler MAIN::external-address-wrapper call primary
                    (?cmd $?args)
                    (call (dynamic-get backing-store)
                          ?cmd
                          (expand$ ?args)))

(defmethod call
  ((?target external-address-wrapper)
   (?cmd SYMBOL)
   (?arguments MULTIFIELD))
  (send ?target
        call
        ?cmd
        ?arguments))
(defmethod call
  ((?target external-address-wrapper)
   (?cmd SYMBOL)
   $?arguments)
  (call ?target
        ?cmd
        ?arguments))
