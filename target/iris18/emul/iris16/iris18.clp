(deffunction memory-op
             (?router ?action ?bitmask ?offset)
             (format ?router
                     "memory %s %s %s%n"
                     ?action
                     ?bitmask
                     (str-cat ?offset)))
(deffunction load-value
             (?router ?bitmask ?offset)
             (memory-op ?router
                        load
                        ?bitmask
                        ?offset)
             value)
(deffunction push
             (?router ?bitmask ?reg)
             (memory-op ?router
                        push
                        ?bitmask
                        ?reg)
             ?reg)
(deffunction pop
             (?router ?bitmask ?reg)
             (memory-op ?router
                        pop
                        ?bitmask
                        ?reg)
             ?reg)
(deffunction store-value
             (?router ?bitmask ?offset)
             (memory-op ?router
                        store
                        ?bitmask
                        ?offset)
             value)

(deffunction arithmetic-op
             (?router ?action ?immediate ?dest ?src)
             (format ?router
                     "arithmetic %s %s %s %s%n"
                     ?action
                     (if ?immediate then
                       immediate
                       else
                       "")
                     ?dest
                     ?src)
             ?dest)
(deffunction add
             (?router ?dest ?src)
             (arithmetic-op ?router
                            add
                            FALSE
                            ?dest
                            ?src))
(deffunction add-immediate
             (?router ?dest ?src)
             (arithmetic-op ?router
                            add
                            TRUE
                            ?dest
                            (str-cat ?src)))


(deffunction sub
             (?router ?dest ?src)
             (arithmetic-op ?router
                            sub
                            FALSE
                            ?dest
                            ?src))
(deffunction sub-immediate
             (?router ?dest ?src)
             (arithmetic-op ?router
                            sub
                            TRUE
                            ?dest
                            (str-cat ?src)))
(deffunction mul
             (?router ?dest ?src)
             (arithmetic-op ?router
                            mul
                            FALSE
                            ?dest
                            ?src))
(deffunction mul-immediate
             (?router ?dest ?src)
             (arithmetic-op ?router
                            mul
                            TRUE
                            ?dest
                            (str-cat ?src)))
(deffunction divide
             (?router ?dest ?src)
             (arithmetic-op ?router
                            div
                            FALSE
                            ?dest
                            ?src))
(deffunction divide-immediate
             (?router ?dest ?src)
             (arithmetic-op ?router
                            div
                            TRUE
                            ?dest
                            (str-cat ?src)))
(deffunction rem
             (?router ?dest ?src)
             (arithmetic-op ?router
                            rem
                            FALSE
                            ?dest
                            ?src))
(deffunction rem-immediate
             (?router ?dest ?src)
             (arithmetic-op ?router
                            rem
                            TRUE
                            ?dest
                            (str-cat ?src)))

(deffunction load16
             (?router ?addr)
             (load-value ?router 0m0011 0))
(deffunction load32
             (?router ?addr)
             (load-value ?router 0m1111 0))
(deffunction assign
             (?router ?bitmask ?register ?value)
             (format ?router
                     "set %s %s %s%n"
                     ?bitmask
                     ?register
                     (str-cat ?value))
             ?register)
(deffunction load-data-address
             (?router ?address)
             (load16 ?router
                     (add ?router
                          (assign ?router
                                  0m1111
                                  addr
                                  Iris16Data)
                          ?address)))
(deffunction ret
             (?router ?return)
             (printout ?router
                       "return"
                       crlf)
             ?return)
