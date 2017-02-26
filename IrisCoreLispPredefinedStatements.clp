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

(definstances lower::predefined-aliases
              (of list
                  (parent FALSE)
                  (contents alias
                            r239
                            as
                            iv0))
              (of list
                  (parent FALSE)
                  (contents alias
                            r238
                            as
                            sp))
              (of list
                  (parent FALSE)
                  (contents alias
                            r237
                            as
                            iv1))
              (of list
                  (parent FALSE)
                  (contents alias
                            r236
                            as
                            arg0))
              (of list
                  (parent FALSE)
                  (contents alias
                            r235
                            as
                            arg1))
              (of list
                  (parent FALSE)
                  (contents alias
                            r234
                            as
                            arg2))
              (of list
                  (parent FALSE)
                  (contents alias
                            r233
                            as
                            arg3))
              (of list
                  (parent FALSE)
                  (contents alias
                            r232
                            as
                            ret0))
              (of list
                  (parent FALSE)
                  (contents alias
                            r231
                            as
                            ret1))
              (of list
                  (parent FALSE)
                  (contents alias
                            r230
                            as
                            ret2))
              (of list
                  (parent FALSE)
                  (contents alias
                            r229
                            as
                            ret3))
              (of list
                  (parent FALSE)
                  (contents alias
                            r228
                            as
                            code-stack))
              (of list
                  (parent FALSE)
                  (contents alias
                            code-stack
                            as
                            return-stack))
              (of list
                  (parent FALSE)
                  (contents alias
                            code-stack
                            as
                            cs))
              (of list
                  (parent FALSE)
                  (contents alias
                            return-stack
                            as
                            rs))
              (of list
                  (parent FALSE)
                  (contents alias
                            r227
                            as 
                            data-stack))
              (of list
                  (parent FALSE)
                  (contents alias
                            data-stack
                            as
                            ds))
              (of list
                  (parent FALSE)
                  (contents alias
                            data-stack
                            as
                            parameter-stack))
              (of list
                  (parent FALSE)
                  (contents alias
                            parameter-stack
                            as
                            ps))
              (of list
                  (parent FALSE)
                  (contents alias
                            r226
                            as
                            zero-register))
              (of list
                  (parent FALSE)
                  (contents alias
                            zero-register
                            as
                            zr))
              )
(defglobal lower
           ?*primary-secondary-storage-base* = 0x000A
           ?*error-dispatch-vector-address* = 0x00FF
           ?*unused0-dispatch-vector-address* = 0x00FE
           ?*unused1-dispatch-vector-address* = 0x00FD
           ?*custom-instruction-dispatch-vector* = 0x00FC
           ?*divide-by-zero-dispatch-vector* = 0x00FB
           ?*terminate-port-address* = 0x0000
           ?*get-c-port-address* = 0x0001
           ?*put-c-port-address* = 0x0002
           ?*seed-random-port-address* = 0x0003
           ?*next-random-port-address* = 0x0004)

(definstances lower::predefined-let-statements
              (of list
                  (parent FALSE)
                  (contents let
                            ZeroAddress
                            be
                            0x0000))
              (of list
                  (parent FALSE)
                  (contents let
                            ErrorDispatchVector
                            be
                            ?*error-dispatch-vector-address*))
              (of list
                  (parent FALSE)
                  (contents let
                            Unused0DispatchVector 
                            be
                            ?*unused0-dispatch-vector-address*))
              (of list
                  (parent FALSE)
                  (contents let
                            Unused1DispatchVector 
                            be
                            ?*unused1-dispatch-vector-address*))
              (of list
                  (parent FALSE)
                  (contents let
                            CustomInstructionDispatchVector
                            be
                            ?*custom-instruction-dispatch-vector*))
              (of list
                  (parent FALSE)
                  (contents let
                            TerminatePort 
                            be 
                            ?*terminate-port-address*))
              (of list
                  (parent FALSE)
                  (contents let
                            GetCPort
                            be
                            ?*get-c-port-address*))
              (of list
                  (parent FALSE)
                  (contents let
                            PutCPort
                            be
                            ?*put-c-port-address*))
              (of list
                  (parent FALSE)
                  (contents let 
                            SeedRandomPort 
                            be 
                            ?*seed-random-port-address*))
              (of list
                  (parent FALSE)
                  (contents let 
                            NextRandomPort 
                            be 
                            ?*next-random-port-address*))
              (of list
                  (parent FALSE)
                  (contents let 
                            PrimarySecondaryStorageBasePort 
                            be 
                            ?*primary-secondary-storage-base*))
              (of list
                  (parent FALSE)
                  (contents let
                            PrimarySecondaryStorageSectorPort
                            be
                            ?*primary-secondary-storage-base*))
              (of list
                  (parent FALSE)
                  (contents let
                            PrimarySecondaryStorageIndexPort
                            be
                            (int->hex (+ (hex->int ?*primary-secondary-storage-base*)
                                         1))))
              (of list
                  (parent FALSE)
                  (contents let
                            PrimarySecondaryStorageDataPort
                            be
                            (int->hex (+ (hex->int ?*primary-secondary-storage-base*)
                                         2))))
              )
