(deffunction to-char-list
             (?uid)
             (bind ?output
                   (create$))
             (while (<> (bind ?char
                              (get-char ?uid))
                        -1) do
                    (bind ?output
                          ?output
                          ?char))
             ?output)
(deffunction string-to-char-list
             "convert the given string to a char list"
             (?str)
             (bind ?path
                   (str-cat "/tmp/" 
                            (gensym*)))
             (bind ?uid
                   (gensym*))
             (bind ?outcome
                   (create$))
             (if (open ?path 
                       ?uid
                       "w") then
               (close ?uid)
               (open ?path
                     ?uid
                     "r")
               (bind ?outcome
                     (to-char-list ?uid))
               (close ?uid)
               (remove ?path))
             ?outcome)


