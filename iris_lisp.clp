(defclass lisp->intermediary::function
  (is-a node
        has-body
        has-arguments
        has-title))
(defrule lisp->intermediary::build-function
         ?f <- (object (is-a list)
                       (contents deffunction ?title 
                                 ?args
                                 $?body)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of function
                        (parent ?parent)
                        (body ?body)
                        (arguments ?args)
                        (title ?title)))
(defrule lisp->intermediary:flatten-arguments
         ?f <- (object (is-a has-arguments)
                       (arguments $?b ?arg $?a)
                       (parent ?p))
         ?f2 <- (object (is-a list)
                        (name ?arg)
                        (contents $?children))
         =>
         (progn$ (?c ?children)
                 (send ?c 
                       put-parent 
                       ?p))
         (modify-instance ?f
                          (arguments $?b ?children $?a))
         (unmake-instance ?f2))



