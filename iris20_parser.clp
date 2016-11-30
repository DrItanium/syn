(defclass lisp->intermediary::body
          (is-a composite-node
                has-title))
(defclass lisp->intermediary::molecule
  (is-a composite-node))
(defclass lisp->intermediary::atom
  (is-a composite-node
        has-title))


(defrule lisp->intermediary::construct-body
         ?f <- (object (is-a list)
                       (parent ?p)
                       (contents body ?title
                                 $?children)
                       (name ?list))
         =>
         (unmake-instance ?f)
         (make-instance ?list of body
                        (parent ?p)
                        (title ?title)
                        (contents $?children)))
(defrule lisp->intermediary::construct-molecule
         ?f <- (object (is-a list)
                       (parent ?p)
                       (contents molecule $?rest)
                       (name ?list))
         =>
         (unmake-instance ?f)
         (make-instance ?list of molecule
                        (parent ?p)
                        (contents ?rest)))

(defrule list->intermediary::construct-atom
         ?f <- (object (is-a list)
                       (parent ?molecule)
                       (contents ?title $?rest)
                       (name ?atom))
         (object (is-a molecule)
                 (name ?molecule))
         =>
         (unmake-instance ?f)
         (make-instance ?atom of atom
                        (parent ?molecule)
                        (title ?title)
                        (contents $?rest)))

