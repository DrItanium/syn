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

(defrule lower::pop-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents pop
                                 lr
                                 ?stack)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of simple-container
                        (parent ?p)
                        (body (make-instance of list
                                             (parent ?n)
                                             (contents pop
                                                       iv0
                                                       ?stack))
                              (make-instance of list
                                             (parent ?n)
                                             (contents move
                                                       lr
                                                       iv0)))))

(defrule lower::push-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents push
                                 ?stack
                                 lr)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of simple-container
                        (parent ?p)
                        (body (make-instance of list
                                             (parent ?n)
                                             (contents move
                                                       iv0
                                                       lr))
                              (make-instance of list
                                             (parent ?n)
                                             (contents push
                                                       ?stack
                                                       iv0)))))

(defrule lower::pop-predicate-registers
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents pop
                                 predicates
                                 ?stack)
                       (name ?n)
                       (parent ?p))
         (object (is-a register)
                 (name ?stack))
         =>
         (unmake-instance ?f)
         (make-instance ?n of simple-container
                        (parent ?p)
                        (body (make-instance of list
                                             (parent ?n)
                                             (contents pop
                                                       iv0
                                                       ?stack))
                              (make-instance of list
                                             (parent ?n)
                                             (contents move
                                                       predicates
                                                       iv0)))))
(defrule lower::push-predicate-registers
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents push
                                 ?stack
                                 predicates)
                       (name ?n)
                       (parent ?p))
         (object (is-a register)
                 (name ?stack))
         =>
         (unmake-instance ?f)
         (make-instance ?n of simple-container
                        (parent ?p)
                        (body (make-instance of list
                                             (parent ?n)
                                             (contents move
                                                       iv0
                                                       predicates))
                              (make-instance of list
                                             (parent ?n)
                                             (contents push
                                                       ?stack
                                                       iv0)))))
