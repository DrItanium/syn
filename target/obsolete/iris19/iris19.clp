; iris19 language parser
(defclass invocation
  (is-a node)
  (slot function
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot arguments
             (storage local)
             (visibility public)))
(defclass binary-operation
  (is-a invocation)
  (slot destination
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot operation
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass unary-operation
  (is-a invocation)
  (slot source
        (storage local)
        (visibility public)
        (default ?NONE)))

(defrule parse-unary-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents ?function
                                 ?source)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of unary-operation
                        (function ?function)
                        (source ?source)
                        (parent ?p)))
(defrule parse-binary-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (name ?n)
                       (parent ?p)
                       (contents ?function
                                 ?destination
                                 ?operation))
         =>
         (unmake-instance ?f)
         (make-instance ?n of binary-operation
                        (parent ?p)
                        (function ?function)
                        (destination ?destination)
                        (operation ?operation)))
(defclass if-condition
  (is-a invocation)
  (slot function
        (source composite)
        (storage shared)
        (default if))
  (slot condition
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot then
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot else
        (storage local)
        (visibility public)
        (default ?NONE)))
(defrule construct-if-condition
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents if ?condition then
                                 ?then
                                 else
                                 ?else)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of if-condition
                        (parent ?p)
                        (condition ?condition)
                        (then ?then)
                        (else ?else)))
(defrule mark-push-and-immediate-pops
         (stage (current parse))
         (object (is-a invocation)
                 (function pop)
                 (name ?name))
         (object (is-a invocation)
                 (function push)
                 (parent ?name)
                 (name ?child))
         =>
         (assert (note pop ?name push ?child optimization)))
