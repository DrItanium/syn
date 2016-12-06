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
(defclass lisp->intermediary::has-destination
  (is-a thing)
  (slot destination
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass lisp->intermediary::has-source0
  (is-a thing)
  (slot source0
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass lisp->intermediary::has-source1
  (is-a thing)
  (slot source1
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass lisp->intermediary::two-argument-operation
  (is-a atom
        has-destination
        has-source0))



(defclass lisp->intermediary::three-argument-operation
  (is-a atom
        has-destination
        has-source0
        has-source1))

(defclass lisp->intermediary::set-operation
  (is-a two-argument-operation)
  (slot title
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default set)))
(defclass lisp->intermediate::branch-instruction
 (is-a USER))
(defclass lisp->intermediary::relative-jump-operation
  (is-a two-argument-operation
        branch-instruction)
  (slot title
        (source composite)
        (storage-shared)
        (access read-only)
        (create-accessor read)
        (default branch)))


(defclass lisp->intermediary::move-operation
  (is-a two-argument-operation)
  (slot title
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default move)))

(defclass lisp->intermediary::not-operation
  (is-a two-argument-operation)
  (slot title
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default not)))

(defclass lisp->intermediary::swap-operation
  (is-a two-argument-operation)
  (slot title
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default swap)))

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
                 (title =int32|uint32|int48|uint48|address))
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
                        (source ?immediate)
                        (contents ?rest)))

(defrule lisp->intermediary::generate-move-operation
         (declare (salience 1))
         ?f <- (object (is-a atom)
                       (title move)
                       (double-wide ?dw)
                       (parent ?p)
                       (name ?move)
                       (contents ?destination
                                 ?immediate
                                 $?rest))
         =>
         (unmake-instance ?f)
         (make-instance ?move of move-operation
                        (parent ?p)
                        (double-wide ?dw)
                        (destination ?destination)
                        (source ?immediate)
                        (contents ?rest)))
(defrule lisp->intermediary::generate-not-operation
         (declare (salience 1))
         ?f <- (object (is-a atom)
                       (title not)
                       (double-wide ?dw)
                       (parent ?p)
                       (name ?not)
                       (contents ?destination
                                 ?immediate
                                 $?rest))
         =>
         (unmake-instance ?f)
         (make-instance ?not of not-operation
                        (parent ?p)
                        (double-wide ?dw)
                        (destination ?destination)
                        (source ?immediate)
                        (contents ?rest)))

(defrule lisp->intermediary::generate-swap-operation
         (declare (salience 1))
         ?f <- (object (is-a atom)
                       (title swap)
                       (double-wide ?dw)
                       (parent ?p)
                       (name ?swap)
                       (contents ?destination
                                 ?immediate
                                 $?rest))
         =>
         (unmake-instance ?f)
         (make-instance ?swap of swap-operation
                        (parent ?p)
                        (double-wide ?dw)
                        (destination ?destination)
                        (source ?immediate)
                        (contents ?rest)))
