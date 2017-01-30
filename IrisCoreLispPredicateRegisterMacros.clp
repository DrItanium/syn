;------------------------------------------------------------------------------
; Predicate registers macro operations
;------------------------------------------------------------------------------
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
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                pop
                                iv0
                                ?stack)
                       (mk-move-op ?n
                                   predicates
                                   iv0)))

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
         (mk-container ?n
                       ?p
                       (mk-move-op ?n
                                   iv0
                                   predicates)
                       (mk-list ?n
                                push
                                ?stack
                                iv0)))

(defrule lower::move-predicates-to-register
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 ?register
                                 predicates))
         =>
         (modify-instance ?f
                          (contents svcr
                                    ?register)))

(defrule lower::move-register-to-predicates
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents move
                                 predicates
                                 ?register))
         =>
         (modify-instance ?f
                          (contents recr 
                                    ?register)))


(defrule lower::store-predicate-registers
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents st
                                 ?address
                                 predicates)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-move-op ?n
                                   iv0
                                   predicates)
                       (mk-list ?n
                                st
                                ?address
                                iv0)))


(defrule lower::load-predicate-registers
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents ld
                                 predicates
                                 ?address)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                ld
                                iv0
                                ?address)
                       (mk-move-op ?n
                                   predicates
                                   iv0)))
(defrule lower::swap-predicate-registers
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents swap
                                 predicates
                                 ?register)
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?name
                       ?p
                       (mk-move-op ?name
                                   iv0
                                   predicates)
                       (mk-list ?name
                                swap
                                iv0
                                ?register)
                       (mk-move-op ?name
                                   predicates
                                   iv0)))

(defrule lower::swap-predicate-registers:predicates-second
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents swap
                                 ?register
                                 predicates)
                       (name ?name)
                       (parent ?p))
         =>
         (modify-instance ?f
                          (contents swap
                                    predicates
                                    ?register)))

