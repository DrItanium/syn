(batch* midi.clp)
(defmodule MAIN
           (import midi
                   ?ALL))
(deffunction sleep
             (?count)
             (system (str-cat "sleep " ?count)))
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
                             2))

(defmethod print-all-notes-for-sound 
  ((?dev EXTERNAL-ADDRESS)
   (?tone LEXEME))
  (print-all-notes-for-sound ?dev
                             ?tone
                             100))
