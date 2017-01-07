(deffunction lisp->intermediary::defsimpleinherit
             (?title ?parent)
             (buildf "(defclass lisp->intermediary::%s (is-a %s))"
                     ?title
                     ?parent))
(defclass lisp->intermediary::action
  (is-a node
        has-arguments)
  (slot operation
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass lisp->intermediary::section
  (is-a node
        has-title
        has-body))

(defclass lisp->intermediary::alias
  (is-a node)
  (slot alias 
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot target
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass lisp->intermediary::special-symbol
  (is-a node)
  (slot value
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE)))
(deffunction lisp->intermediary::defsimple-symbol 
             (?title)
             (defsimpleinherit ?title
                               special-symbol))
(defsimple-symbol general-purpose-register)
(defsimple-symbol predicate-register)


(deffunction lisp->intermediary::make-range
             (?prefix ?count)
             (bind ?output
                   (create$))
             (loop-for-count (?index 0 (- ?count 
                                          1)) do
                             (bind ?output
                                   ?output
                                   (sym-cat ?prefix ?index)))
             ?output)
(defglobal lisp->intermediary
           ?*base-actions* = (create$ add
                                      sub
                                      mul
                                      div
                                      rem
                                      shift-left
                                      shift-right
                                      and
                                      or
                                      not
                                      xor
                                      nand
                                      bind
                                      move
                                      pop
                                      push
                                      load
                                      store
                                      ; more to follow
                                      )
           ?*predicate-registers* = (make-range p
                                                16)
           ?*registers* = (make-range r
                                      256)
           ?*sections* = (create$ code
                                  stack
                                  data))

(defrule lisp->intermediary::make-section
         ?f <- (object (is-a list)
                       (contents section
                                 ?section&:(member$ ?section
                                                    ?*sections*)
                                 $?body)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of section
                        (parent ?p)
                        (title ?section)
                        (body $?body)))
(defrule lisp->intermediary::make-action
         ?f <- (object (is-a list)
                       (contents ?action&:(member$ ?action
                                                   ?*base-actions*)
                                 $?rest)
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of action
                        (parent ?p)
                        (operation ?action)
                        (arguments $?rest)))

(defrule lisp->intermediary::make-gpr-in-list
         (declare (salience ?*priority:first*))
         ?f <- (object (is-a list)
                       (contents $?a 
                                 ?register&:(member$ ?register
                                                     ?*registers*)
                                 $?b)
                       (name ?n))
         =>
         (modify-instance ?f
                          (contents ?a 
                                    (make-instance of general-purpose-register
                                                   (parent ?n)
                                                   (value ?register))
                                    ?b)))

(defrule lisp->intermediary::make-predicate-in-list
         (declare (salience ?*priority:first*))
         ?f <- (object (is-a list)
                       (contents $?a 
                                 ?register&:(member$ ?register
                                                     ?*predicate-registers*)
                                 $?b)
                       (name ?n))
         =>
         (modify-instance ?f
                          (contents ?a 
                                    (make-instance of predicate-register
                                                   (parent ?n)
                                                   (value ?register))
                                    ?b)))
(defrule lisp->intermediary::make-alias
         (declare (salience 1))
         ?f <- (object (is-a list)
                       (contents alias
                                 ?alias
                                 ?reference)
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of alias
                        (parent ?p)
                        (alias ?alias)
                        (target ?reference)))
(defrule lisp->intermediary::update-alias-references
         ?f <- (object (is-a alias)
                       (target ?other-alias)
                       (name ?a))
         (object (is-a alias)
                 (alias ?other-alias)
                 (name ?target))
         =>
         (modify-instance ?f
                          (target (make-instance of reference
                                                 (parent ?a)
                                                 (value ?target)
                                                 (expand TRUE)))))

(defrule lisp->intermediary::bad-alias-references-self
         (declare (salience 2))
         (object (is-a alias)
                 (alias ?title)
                 (target ?title))
         =>
         (printout werror "ERROR: cyclic reference to alias, the following alias is now allowed: (alias " ?title " " ?title ")" crlf)
         (halt))

(defrule lisp->intermediary::bad-alias-register-to-register
         ?f <- (object (is-a alias)
                       (alias ?thingy))
         (test (and (instancep ?thingy)
                    (not (neq (class ?thingy)
                              general-purpose-register
                              predicate-register))))
         =>
         (printout werror 
                   "ERROR: CAN'T MAKE AN ALIAS OUT OF THIS: " 
                   (send ?thingy
                         get-value)
                   " IT IS A PREDEFINED REGISTER!" 
                   crlf)
         (halt))







