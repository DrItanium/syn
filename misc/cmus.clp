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

; cmus.clp - routines to interface with the cmus music player via cmus-remote

(defgeneric MAIN::cmus-remote)
(defgeneric MAIN::cmus)
(defmethod MAIN::cmus-remote
 ((?cmdline MULTIFIELD))
 (system (format nil
                 "cmus-remote %s"
                 (implode$ ?cmdline))))
(defmethod MAIN::cmus-remote
 ($?cmdline)
 (cmus-remote ?cmdline))

(defmethod MAIN::cmus
 ((?cmd SYMBOL
        (eq ?current-argument
            status)))
 (cmus-remote -C ?cmd))
(defmethod MAIN::cmus
 ((?cmd SYMBOL
   (eq ?current-argument
       add))
  (?path LEXEME))
 (cmus-remote ?path))
(defmethod MAIN::cmus
 ((?cmd SYMBOL
   (not (neq ?current-argument
             repeat
             toggle-repeat))))
 (cmus-remote --repeat))

(defmethod MAIN::cmus
 ((?cmd SYMBOL
   (not (neq ?current-argument
             shuffle
             toggle-shuffle))))
 (cmus-remote --shuffle))

(defmethod MAIN::cmus
 ((?cmd SYMBOL
   (not (neq ?current-argument
             prev
             previous))
 (cmus-remote --prev))

(defmethod MAIN::cmus
  ((?cmd SYMBOL
         (eq ?current-argument
             next)))
  (cmus-remote --next))
(defmethod MAIN::cmus
  ((?cmd SYMBOL
         (eq ?current-argument
             play)))
  (cmus-remote --play))
(defmethod MAIN::cmus
  ((?cmd SYMBOL
         (eq ?current-argument
             pause)))
  (cmus-remote --pause))
(defmethod MAIN::cmus
  ((?cmd SYMBOL
         (eq ?current-argument
             pause-playback)))
  (cmus-remote --pause-playback))
(defmethod MAIN::cmus
  ((?cmd SYMBOL
         (eq ?current-argument
             stop)))
  (cmus-remote --stop))
(defmethod MAIN::cmus
  ((?cmd SYMBOL
         (not (neq ?current-argument
             vol
             volume
             set-volume)))
   (?level INTEGER
           LEXEME))
  (cmus-remote --volume 
               ?level))

(defmethod MAIN::cmus
  ((?cmd SYMBOL
    (eq ?current-argument
        clear)))
  (cmus-remote --clear))
