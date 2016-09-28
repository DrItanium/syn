(deffunction defascii-table
             (?title)
             (bind ?numbers
                   (create$))
             (loop-for-count (?ind 0 255) do
                             (bind ?numbers
                                   ?numbers
                                   ?ind))
             (jump-table ?title
                         ?numbers))
(deffunction defchar-compare
             (?title ?desc ?value)
             (defunc ?title
                     (describe-arg value
                                   ?desc)
                     (compare-op ==
                                 none
                                 immediate
                                 value
                                 ?value)))
(deffunction defis-lparen
             ()
             (defchar-compare IsLParen
                              "Is it a (?"
                              0x28))
(deffunction defis-rparen
             ()
             (defchar-compare IsRParen
                              "Is it a )?"
                              0x29))

(deffunction defis-forward-slash
             ()
             (defchar-compare IsForwardSlash
                              "Is it a /?"
                              0x2f))
(deffunction defis-digit
             ()
             (defunc CharIsNumber
                     (describe-arg value
                                   "The character to check for numerical relevance")
                     (comment (compare-op >=
                                          none
                                          immediate
                                          value
                                          0x30)
                              "Are we looking at something before '0'?")
                     (comment (compare-op <=
                                          and
                                          immediate
                                          value
                                          0x39)
                              "Okay, now see if it below '9' or equal to")))
(deffunction defis-space
             ()
             (defchar-compare IsSpace
                              "Is it a ' '?"
                              0x20))

(deffunction defis-semicolon
             ()
             (defchar-compare IsSemiColon
                              "Is it a ';'?"
                              0x3b))
