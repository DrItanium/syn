;------------------------------------------------------------------------------
; Link Register macro operations 
;------------------------------------------------------------------------------
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
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                pop
                                iv0
                                ?stack)
                       (mk-list ?n
                                move
                                lr
                                iv0)))

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
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                move
                                iv0
                                lr)
                       (mk-list ?n
                                push 
                                ?stack 
                                iv0)))

(defrule lower::store-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents st
                                 ?address
                                 lr)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (mk-container ?n
                       ?p
                       (mk-list ?n
                                move
                                iv0
                                lr)
                       (mk-list ?n
                                st
                                ?address
                                iv0)))

(defrule lower::load-lr
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents ld
                                 lr
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
                       (mk-list ?n
                                move
                                lr
                                iv0)))
