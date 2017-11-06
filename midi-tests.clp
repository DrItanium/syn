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
