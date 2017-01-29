(defclass lower::has-reference
  (is-a thing)
  (slot reference
        (type INSTANCE)
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmessage-handler SYMBOL resolve-alias primary
                    ()
                    ?self)

(defclass lower::reference-node
  (is-a node
        has-title
        has-reference))
(defclass lower::composite-reference-node
  (is-a reference-node
        composite-node))
(defclass lower::body
  (is-a composite-node
        has-title))
(defclass lower::molecule
  (is-a composite-node))
(defclass lower::atom
  (is-a composite-node
        has-title)
  (slot double-wide
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (visibility public)
        (storage local)))
(defclass lower::has-destination
  (is-a thing)
  (slot destination
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass lower::has-source0
  (is-a thing)
  (slot source0
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass lower::has-source1
  (is-a thing)
  (slot source1
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass lower::two-argument-operation
  (is-a atom
        has-destination
        has-source0))

(defclass lower::one-argument-operation
  (is-a atom
        has-destination))

(defclass lower::three-argument-operation
  (is-a atom
        has-destination
        has-source0
        has-source1))


(defclass lower::special-register-action
  (is-a node
        has-title)
  (slot target-register
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass lower::data-value
  (is-a node
        has-title)
  (slot value
        (visibility public)
        (storage local)
        (default ?NONE)))

(deffacts lower::three-argument-ops
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


(deffacts lower::two-argument-ops
          (two-argument-instruction swap aliases: !swap)
          (two-argument-instruction set aliases: !set)
          (two-argument-instruction move aliases: !move)
          (two-argument-instruction binary-not aliases: !not)
          (two-argument-instruction conditional-branch aliases: !branch-if)
          (two-argument-instruction conditional-branch-link aliases: !branch-if-link))

(deffacts lower::one-argument-ops
          (one-argument-instruction unconditional-branch aliases: !branch))

(defrule lower::construct-body
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
(defrule lower::construct-molecule
         ?f <- (object (is-a list)
                       (parent ?p)
                       (contents molecule $?rest)
                       (name ?list))
         =>
         (unmake-instance ?f)
         (make-instance ?list of molecule
                        (parent ?p)
                        (contents ?rest)))

(defrule lower::construct-atom
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

(defrule lower::mark-atom-double-wide-operation-from-immediate
         ?f <- (object (is-a atom)
                       (double-wide FALSE)
                       (contents $? ?wide-op))
         (object (is-a data-value)
                 (name ?wide-op)
                 (title int32|uint32|int64|uint64|address))
         =>
         (modify-instance ?f
                          (double-wide TRUE)))



(defrule lower::mark-special-register-action
         ?f <- (object (is-a list)
                       (parent ?atom)
                       (contents ?title&stack|memory ?target)
                       (name ?special))
         =>
         (unmake-instance ?f)
         (make-instance ?special of special-register-action
                        (parent ?atom)
                        (title ?title)
                        (target-register ?target)))

(defrule lower::mark-data-value
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

(defrule lower::mark-atom-double-wide-operation-from-immediate
         ?f <- (object (is-a atom)
                       (double-wide FALSE)
                       (source0 ?wide-op))
         (object (is-a data-value)
                 (name ?wide-op)
                 (title =int32|uint32|int48|uint48|address))
         =>
         (modify-instance ?f
                          (double-wide TRUE)))


(defrule lower::generate-three-argument-operation:atom
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

(defrule lower::generate-two-argument-operation:atom
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

(defrule lower::generate-one-argument-operation:atom
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

(defrule lower::generate-three-argument-operation:list
         (declare (salience 2))
         ?f <- (object (is-a list)
                       (parent ?p)
                       (name ?op)
                       (contents ?title
                                 ?dest
                                 ?src0
                                 ?src1
                                 $?rest))
         (three-argument-instruction ?real-title aliases: $? ?title $?)
         =>
         (unmake-instance ?f)
         (make-instance ?op of three-argument-operation
                        (title ?real-title)
                        (parent ?p)
                        (destination ?dest)
                        (source0 ?src0)
                        (source1 ?src1)
                        (contents $?rest)))

(defrule lower::generate-two-argument-operation:list
         (declare (salience 2))
         ?f <- (object (is-a list)
                       (parent ?p)
                       (name ?op)
                       (contents ?title
                                 ?dest
                                 ?src
                                 $?rest))
         (two-argument-instruction ?real-title aliases: $? ?title $?)
         =>
         (unmake-instance ?f)
         (make-instance ?op of two-argument-operation
                        (title ?real-title)
                        (parent ?p)
                        (destination ?dest)
                        (source0 ?src)
                        (contents $?rest)))

(defrule lower::generate-one-argument-operation:list
         (declare (salience 2))
         ?f <- (object (is-a list)
                       (parent ?p)
                       (name ?op)
                       (contents ?title
                                 ?dest
                                 $?rest))
         (one-argument-instruction ?real-title aliases: $? ?title $?)
         =>
         (unmake-instance ?f)
         (make-instance ?op of one-argument-operation
                        (title ?real-title)
                        (parent ?p)
                        (destination ?dest)
                        (contents $?rest)))


(defclass lower::macro
  (is-a composite-node
        has-title)
  (multislot arguments))

(defclass lower::macro-call
  (is-a composite-reference-node))

(defrule lower::parse-macro
         (declare (salience 2))
         ?f <- (object (is-a list)
                       (parent ?nocare)
                       (contents macro
                                 ?title
                                 ?args
                                 $?rest)
                       (name ?t))
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (parent ?t)
                        (contents $?vars))
         =>
         (unmake-instance ?f ?f2)
         (progn$ (?arg $?vars)
                 (if (instancep ?arg) then
                   (modify-instance ?arg
                                    (parent ?t))))
         (make-instance ?t of macro
                        (arguments $?vars)
                        (title ?title)
                        (parent ?nocare)
                        (contents $?rest)))

(defrule lower::identify-macro-call
         (declare (salience 3))
         ?f <- (object (is-a list)
                       (parent ?nocare)
                       (contents ?first
                                 $?args)
                       (name ?list))
         (object (is-a file)
                 (name ?file))
         (test (send ?f
                     parent-is
                     ?file))
         (object (is-a macro)
                 (title ?first)
                 (name ?raw-macro)
                 (parent ?file))
         (test (send ?raw-macro
                     parent-is
                     ?file))
         =>
         (unmake-instance ?f)
         (make-instance ?list of macro-call
                        (parent ?nocare)
                        (title ?first)
                        (contents $?args)
                        (reference ?raw-macro)))


(defclass lower::alias
  (is-a node
        has-title)
  (slot value
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler resolve-alias primary))
(defmessage-handler lower::alias resolve-alias primary
                    ()
                    (send ?self:value
                          resolve-alias))

(defclass lower::alias-reference
  (is-a node
        has-title
        has-reference)
  (message-handler resolve-alias primary))
(defmessage-handler lower::alias-reference resolve-alias primary
                    ()
                    (send ?self:reference
                          resolve-alias))

(defrule lower::parse-alias
         (declare (salience 2))
         ?f <- (object (is-a list)
                       (contents alias
                                 ?title
                                 ?name)
                       (parent ?nocare)
                       (name ?l))
         =>
         (unmake-instance ?f)
         (make-instance ?l of alias
                        (parent ?nocare)
                        (title ?title)
                        (value ?name)))


(defrule lower::identify-alias:list
         (declare (salience 4))
         (object (is-a list)
                 (contents ?f $?a ?item&:(symbolp ?item) $?b)
                 (name ?list))
         (object (is-a file)
                 (name ?file))
         (test (send ?list
                     parent-is
                     ?file))
         (object (is-a alias)
                 (title ?item)
                 (name ?alias))
         (test (send ?alias
                     parent-is
                     ?file))
         =>
         (modify-instance ?list
                          (contents ?f
                                    ?a
                                    (make-instance of alias-reference
                                                   (parent ?list)
                                                   (title ?item)
                                                   (reference ?alias))
                                    ?b)))

(defrule lower::identify-alias:source0
         (declare (salience 4))
         (object (is-a has-source0)
                 (source0 ?item&:(lexemep ?item))
                 (name ?list))
         (object (is-a file)
                 (name ?file))
         (test (send ?list
                     parent-is
                     ?file))
         (object (is-a alias)
                 (title ?item)
                 (name ?alias))
         (test (send ?alias
                     parent-is
                     ?file))
         =>
         (modify-instance ?list
                          (source0 (make-instance of alias-reference
                                                  (parent ?list)
                                                  (title ?item)
                                                  (reference ?alias)))))

(defrule lower::identify-alias:source1
         (declare (salience 4))
         (object (is-a has-source1)
                 (source1 ?item&:(lexemep ?item))
                 (name ?list))
         (object (is-a file)
                 (name ?file))
         (test (send ?list
                     parent-is
                     ?file))
         (object (is-a alias)
                 (title ?item)
                 (name ?alias))
         (test (send ?alias
                     parent-is
                     ?file))
         =>
         (modify-instance ?list
                          (source1 (make-instance of alias-reference
                                                  (parent ?list)
                                                  (title ?item)
                                                  (reference ?alias)))))

(defrule lower::identify-alias:destination
         (declare (salience 4))
         (object (is-a has-destination)
                 (destination ?item&:(lexemep ?item))
                 (name ?list))
         (object (is-a file)
                 (name ?file))
         (test (send ?list
                     parent-is
                     ?file))
         (object (is-a alias)
                 (title ?item)
                 (name ?alias))
         (test (send ?alias
                     parent-is
                     ?file))
         =>
         (modify-instance ?list
                          (destination (make-instance of alias-reference
                                                      (parent ?list)
                                                      (title ?item)
                                                      (reference ?alias)))))

(defrule lower::identify-alias:alias
         (declare (salience 4))
         ?f <- (object (is-a alias)
                       (value ?v))
         (object (is-a alias)
                 (title ?v)
                 (name ?other))
         =>
         (modify-instance ?f
                          (value ?other)))


(defclass lower::macro-argument-reference
  (is-a reference-node))

(defrule lower::make-macro-argument-reference:composite-node
         (object (is-a macro)
                 (arguments $? ?aref $?)
                 (name ?macro))
         (object (is-a singlefield-variable|multifield-variable)
                 (name ?aref)
                 (parent ?macro)
                 (value ?arg))
         (object (is-a composite-node)
                 (name ?cnode&:(send ?cnode
                                     parent-is
                                     ?macro))
                 (contents $? ?arf0 $?))
         (object (is-a singlefield-variable|multifield-variable)
                 (name ?arf0)
                 (value ?arg)
                 (parent ?cnode))
         =>
         (unmake-instance ?arf0)
         (make-instance ?arf0 of macro-argument-reference
                        (parent ?cnode)
                        (title ?arg)
                        (reference ?aref)))

(defrule lower::make-macro-argument-reference:list
         (object (is-a macro)
                 (arguments $? ?aref $?)
                 (name ?macro))
         (object (is-a singlefield-variable|multifield-variable)
                 (name ?aref)
                 (parent ?macro)
                 (value ?arg))
         (object (is-a list)
                 (name ?cnode&:(send ?cnode
                                     parent-is
                                     ?macro))
                 (contents $? ?arf0 $?))
         (object (is-a singlefield-variable|multifield-variable)
                 (name ?arf0)
                 (value ?arg)
                 (parent ?cnode))
         =>
         (unmake-instance ?arf0)
         (make-instance ?arf0 of macro-argument-reference
                        (parent ?cnode)
                        (title ?arg)
                        (reference ?aref)))

;(defmessage-handler lower::macro copy-body primary
;                    (?parent $?args)
;                    ; first bind the args to the arguments inside the class
;                    ; temporarily
;                    ; TODO: figure out the binding
;                    ; once that is done, then make a copy of the body with the
;                    ; new parent in place of the macro itself
;                    (bind ?output
;                          (create$))
;                    (progn$ (?content $?contents)
;                            (bind ?output
;                                  ?output
;                                  (send ?content
;                                        clone
;                                        ?parent)))
;                    ?output)
;
