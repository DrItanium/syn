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

(defgeneric is-space
            "Macro operation to check and see if the given register contains an ascii space code")
(defgeneric is-lparen)
(defgeneric is-rparen)

(defmethod is-space
  ((?register SYMBOL)
   (?combine SYMBOL)
   (?comment STRING))
  (comment (is-space ?register
                     ?combine)
           ?comment))
(defmethod is-space
  ((?register SYMBOL)
   (?combine SYMBOL))
  (compare-op ==
              ?combine
              immediate
              ?register
              0x20))

(defmethod is-space
  ((?register SYMBOL))
  (is-space ?register
            none))

(defgeneric store-register)
(defgeneric load-register)
;NOTE: Since address and value are fixed registers we have to swap the contents
;      temporarily. This is much cheaper than save and restore
(defmethod store-register
  ((?address SYMBOL)
   (?value SYMBOL)
   (?bitmask SYMBOL)
   (?offset SYMBOL
            NUMBER)
   (?comment STRING))
  (create$ (comment ?comment)
           (store-register ?address
                           ?value
                           ?bitmask
                           ?offset)))
(defmethod store-register
  ((?address SYMBOL)
   (?value SYMBOL)
   (?bitmask SYMBOL)
   (?offset SYMBOL))
  (body (swap ?address
              address)
        (swap ?value
              value)
        (store-value ?bitmask ?offset)
        (swap ?address
              address)
        (swap ?value
              value)))

(defmethod store-register
  ((?address SYMBOL)
   (?value SYMBOL)
   (?bitmask SYMBOL))
  (store-register ?address
                  ?value
                  ?bitmask
                  0x0))
(defmethod store-register
  ((?address SYMBOL)
   (?value SYMBOL))
  (store-register ?address
                  ?value
                  0m1111))
(defmethod load-register
  ((?address SYMBOL)
   (?value SYMBOL)
   (?bitmask SYMBOL)
   (?offset SYMBOL
            NUMBER)
   (?comment STRING))
  (create$ (comment ?comment)
           (load-register ?address
                           ?value
                           ?bitmask
                           ?offset)))
(defmethod load-register
  ((?address SYMBOL)
   (?value SYMBOL)
   (?bitmask SYMBOL)
   (?offset SYMBOL))
  (body (swap ?address
              address)
        (swap ?value
              value)
        (load-value ?bitmask ?offset)
        (swap ?address
              address)
        (swap ?value
              value)))

(defmethod load-register
  ((?address SYMBOL)
   (?value SYMBOL)
   (?bitmask SYMBOL))
  (load-register ?address
                  ?value
                  ?bitmask
                  0x0))
(defmethod load-register
  ((?address SYMBOL)
   (?value SYMBOL))
  (load-register ?address
                  ?value
                  0m1111))
