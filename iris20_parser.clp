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

(defclass lisp->intermediary::one-argument-operation
  (is-a atom
        has-destination))

(defclass lisp->intermediary::three-argument-operation
  (is-a atom
        has-destination
        has-source0
        has-source1))


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

(deffacts lisp->intermediary::three-argument-ops
          (three-argument-instruction add aliases: !add)
          (three-argument-instruction sub aliases: !sub)
          (three-argument-instruction div aliases: !div)
          (three-argument-instruction rem aliases: !rem)
          (three-argument-instruction mul aliases: !mul)
          (three-argument-instruction shift-left aliases: !shl)
          (three-argument-instruction shift-right aliases: !shr)
          (three-argument-instruction binary-and aliases: !and)
          (three-argument-instruction binary-or aliases: !or)
          (three-argument-instruction binary-xor aliases: !xor)
          (three-argument-instruction binary-nand aliases: !nand)
          (three-argument-instruction branch-if-then-else aliases: !if)
          (three-argument-instruction branch-if-then-else-link aliases: !if-link)
          (three-argument-instruction equals aliases: !eq)
          (three-argument-instruction not-equals aliases: !neq)
          (three-argument-instruction less-than aliases: !lt)
          (three-argument-instruction greater-than aliases: !gt)
          (three-argument-instruction less-than-or-equal aliases: !le)
          (three-argument-instruction greater-than-or-equal aliases: !ge)
          (three-argument-instruction system-call aliases: !sys))


(deffacts lisp->intermediary::two-argument-ops
          (two-argument-instruction swap aliases: !swap)
          (two-argument-instruction set aliases: !set)
          (two-argument-instruction move aliases: !move)
          (two-argument-instruction binary-not aliases: !not)
          (two-argument-instruction conditional-branch aliases: !branch-if)
          (two-argument-instruction conditional-branch-link aliases: !branch-if-link))

(deffacts lisp->intermediary::one-argument-ops
          (one-argument-instruction unconditional-branch aliases: !branch))

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
         ?f <- (object (is-a atom)
                       (double-wide FALSE)
                       (source0 ?wide-op))
         (object (is-a data-value)
                 (name ?wide-op)
                 (title =int32|uint32|int48|uint48|address))
         =>
         (modify-instance ?f
                          (double-wide TRUE)))


(defrule lisp->intermediary::generate-three-argument-operation
         (declare (salience 2))
         ?f <- (object (is-a atom)
                       (title ?title)
                       (double-wide ?dw)
                       (parent ?p)
                       (name ?op)
                       (contents ?dest
                                 ?src0
                                 ?src1
                                 $?rest))
         (three-argument-instruction ?real-title aliases: $? ?title $?)
         =>
         (unmake-instance ?f)
         (make-instance ?op of three-argument-operation
                        (title ?real-title)
                        (double-wide ?dw)
                        (parent ?p)
                        (destination ?dest)
                        (source0 ?src0)
                        (source1 ?src1)
                        (contents $?rest)))

(defrule lisp->intermediary::generate-two-argument-operation
         (declare (salience 2))
         ?f <- (object (is-a atom)
                       (title ?title)
                       (double-wide ?dw)
                       (parent ?p)
                       (name ?op)
                       (contents ?dest
                                 ?src
                                 $?rest))
         (two-argument-instruction ?real-title aliases: $? ?title $?)
         =>
         (unmake-instance ?f)
         (make-instance ?op of two-argument-operation
                        (title ?real-title)
                        (double-wide ?dw)
                        (parent ?p)
                        (destination ?dest)
                        (source0 ?src)
                        (contents $?rest)))

(defrule lisp->intermediary::generate-one-argument-operation
         (declare (salience 2))
         ?f <- (object (is-a atom)
                       (title ?title)
                       (double-wide ?dw)
                       (parent ?p)
                       (name ?op)
                       (contents ?dest
                                 $?rest))
         (one-argument-instruction ?real-title aliases: $? ?title $?)
         =>
         (unmake-instance ?f)
         (make-instance ?op of one-argument-operation
                        (title ?real-title)
                        (double-wide ?dw)
                        (parent ?p)
                        (destination ?dest)
                        (contents $?rest)))

