(defclass lisp->intermediary::body
  (is-a composite-node
        has-title))
(defclass lisp->intermediary::molecule
  (is-a composite-node))
(defclass lisp->intermediary::atom
  (is-a composite-node
        has-title)
  (slot double-wide
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (visibility public)
        (storage local)))
(defclass lisp->intermediary::set-operation
  (is-a atom)
  (slot destination
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot immediate-value
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot title
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default set)))

(defclass lisp->intermediary::special-register-action
  (is-a node
        has-title)
  (slot target-register
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass lisp->intermediary::data-value
  (is-a node
        has-title)
  (slot value
        (visibility public)
        (storage local)
        (default ?NONE)))

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

(defrule lisp->intermediary::construct-atom
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

(defrule lisp->intermediary::mark-atom-double-wide-operation-from-immediate
         ?f <- (object (is-a atom)
                       (double-wide FALSE)
                       (contents $? ?wide-op))
         (object (is-a data-value)
                 (name ?wide-op)
                 (title int32|uint32|int64|uint64|address))
         =>
         (modify-instance ?f
                          (double-wide TRUE)))



(defrule lisp->intermediary::mark-special-register-action
         ?f <- (object (is-a list)
                       (parent ?atom)
                       (contents ?title&stack|memory ?target)
                       (name ?special))
         (object (is-a atom)
                 (name ?atom))
         =>
         (unmake-instance ?f)
         (make-instance ?special of special-register-action
                        (parent ?atom)
                        (title ?title)
                        (target-register ?target)))

(defrule lisp->intermediary::mark-data-value
         ?f <- (object (is-a list)
                       (parent ?p)
                       (contents ?width&:(not (neq ?width
                                                   address
                                                   byte
                                                   int8
                                                   uint8
                                                   int16
                                                   uint16
                                                   int32
                                                   uint32
                                                   int48
                                                   uint48
                                                   int64
                                                   uint64))
                                 ?value)
                       (name ?dat))
         =>
         (unmake-instance ?f)
         (make-instance ?dat of data-value
                        (parent ?p)
                        (title ?width)
                        (value ?value)))

(defrule lisp->intermediary::mark-atom-double-wide-operation-from-immediate
         ?f <- (object (is-a set-operation)
                       (double-wide FALSE)
                       (immediate-value ?wide-op))
         (object (is-a data-value)
                 (name ?wide-op)
                 (title =int32|uint32|int64|uint64|address))
         =>
         (modify-instance ?f
                          (double-wide TRUE)))

(defrule lisp->intermediary::generate-set-operation
         (declare (salience 1))
         ?f <- (object (is-a atom)
                       (title set)
                       (double-wide ?dw)
                       (parent ?p)
                       (name ?set)
                       (contents ?destination
                                 ?immediate
                                 $?rest))
         =>
         (unmake-instance ?f)
         (make-instance ?set of set-operation
                        (parent ?p)
                        (double-wide ?dw)
                        (destination ?destination)
                        (immediate-value ?immediate)
                        (contents ?rest)))
