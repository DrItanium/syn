;------------------------------------------------------------------------------
; Stack macro operations
;------------------------------------------------------------------------------
(defrule lower::parse-push-operation-style1
         ?f <- (object (is-a list)
                       (contents push
                                 ?target
                                 into|onto
                                 ?stack))
         =>
         (modify-instance ?f
                          (contents push
                                    ?stack
                                    ?target)))

(defrule lower::parse-pop-operation-style1
         ?f <- (object (is-a list)
                       (contents pop
                                 ?stack
                                 into
                                 ?target)
                       (name ?n)
                       (parent ?p))
         =>
         (modify-instance ?f
                          (contents pop
                                    ?target
                                    ?stack)))


