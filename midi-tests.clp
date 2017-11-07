(batch* midi.clp)
(batch* system.clp)
(defmodule MAIN
           (import os
                   ?ALL)
           (import midi
                   ?ALL))
(deffunction test-scale
             (?tone ?dev ?velocity)
             (call ?dev
                   write
                   (change-patch 0 0 0
                                 (gm-patch-name->program-number ?tone)))
             (progn$ (?key (construct-octave 3))
                     (call ?dev
                           write
                           (note-on 0
                                    (key->note-number ?key)
                                    ?velocity))
                     (sleep 1)
                     (call ?dev
                           write
                           (note-off 0
                                     (key->note-number ?key)
                                     ?velocity))))
(defglobal MAIN
           ?*default-velocity* = 100
           ?*default-sleep-duration* = 2)
(defgeneric print-all-notes-for-sound
            "print all keys for a given patch sound")
(defmethod print-all-notes-for-sound
           ((?dev EXTERNAL-ADDRESS)
            (?tone LEXEME)
            (?velocity INTEGER)
            (?sleep NUMBER))
             (call ?dev
                   write
                   (program-change 0 ?tone))
             (printout t
                       "Changing to patch: " ?tone crlf)
             (progn$ (?key ?*key-to-note-table*)
                     (printout t
                               "Playing key: " ?key crlf)
                     (call ?dev
                           write
                           (note-on 0
                                    ?key
                                    ?velocity))
                     (sleep ?sleep)
                     (call ?dev
                           write
                           (note-off 0
                                     ?key
                                     ?velocity)))
             FALSE)
(defmethod print-all-notes-for-sound
  ((?dev EXTERNAL-ADDRESS)
   (?tone LEXEME)
   (?velocity INTEGER))
  (print-all-notes-for-sound ?dev
                             ?tone
                             ?velocity
                             ?*default-sleep-duration*))

(defmethod print-all-notes-for-sound
  ((?dev EXTERNAL-ADDRESS)
   (?tone LEXEME))
  (print-all-notes-for-sound ?dev
                             ?tone
                             ?*default-velocity*))
