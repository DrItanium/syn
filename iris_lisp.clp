(deffunction lisp->intermediary::defsimpleinherit
             (?title ?parent)
             (buildf "(defclass lisp->intermediary::%s (is-a %s))"
                     ?title
                     ?parent))
(defclass lisp->intermediary::environment
  "Top level wrapper of everything that is code related!"
  (is-a node
        has-body))
(defclass lisp->intermediary::label
  (is-a node
        has-title
        has-body))
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
(defclass lisp->intermediary::org
  (is-a node
        has-body)
  (slot address
        (storage local)
        (visibility public)
        (default ?NONE)))

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

(defclass lisp->intermediary::macro
  (is-a node
        has-title
        has-arguments
        has-body))
(defclass lisp->intermediary::macro-call
  (is-a reference
        has-arguments))
(defclass lisp->intermediary::if-condition
  (is-a node)
  (slot condition
        (type INSTANCE)
        (allowed-classes predicate-register)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot link
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (visibility public)
        (storage local))
  (slot on-true
        (type INSTANCE)
        (allowed-classes general-purpose-register)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot on-false
        (type INSTANCE)
        (allowed-classes general-purpose-register)
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass lisp->intermediary::word
  (is-a scalar-node))

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
                                      move
                                      set
                                      pop
                                      push
                                      load
                                      store
                                      ; more to follow
                                      call
                                      goto
                                      stio
                                      ldio
                                      stc
                                      ldc
                                      jlr
                                      call-lr

                                      mflr
                                      mtlr

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
         (declare (salience ?*priority:right-after-first*))
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



(defrule lisp->intermediary::make-org-directive
         ?f <- (object (is-a list)
                       (contents org
                                 ?address
                                 $?rest)
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of org
                        (parent ?p)
                        (address ?address)
                        (body $?rest)))

(defrule lisp->intermediary::mark-alias-reference-in-action
         ?f <- (object (is-a action)
                       (arguments $?a ?alias $?b)
                       (name ?action))
         (object (is-a alias)
                 (alias ?alias)
                 (name ?target))
         =>
         (modify-instance ?f
                          (arguments ?a
                                     (make-instance of reference
                                                    (parent ?action)
                                                    (value ?target)
                                                    (expand TRUE))
                                     ?b)))

(defrule lisp->intermediary::make-macro
         ?f <- (object (is-a list)
                       (name ?name)
                       (parent ?p)
                       (contents macro 
                                 ?title
                                 ?args
                                 $?body))
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (contents $?children))
         =>
         (progn$ (?c ?children) 
                 (send ?c 
                       put-parent
                       ?name))
         (unmake-instance ?f 
                          ?f2)
         (make-instance ?name of macro
                        (parent ?p)
                        (title ?title)
                        (arguments ?children)
                        (body ?body)))


(defrule lisp->intermediary::mark-macro-call
         ?f <- (object (is-a list)
                       (contents ?macro-op
                                 $?arguments)
                       (name ?n)
                       (parent ?p))
         (object (is-a macro)
                 (title ?macro-op)
                 (name ?mac))
         =>
         (unmake-instance ?f)
         (make-instance ?n of macro-call
                        (parent ?p)
                        (value ?mac)
                        (arguments ?arguments)))

(defrule lisp->intermediary::mark-if-conditional
         ?f <- (object (is-a list)
                       (contents if 
                                 ?predicate 
                                 then 
                                 ?on-true 
                                 else 
                                 ?on-false)
                       (name ?name)
                       (parent ?p))
         (object (is-a predicate-register)
                 (name ?predicate))
         (object (is-a general-purpose-register)
                 (name ?on-true))
         (object (is-a general-purpose-register)
                 (name ?on-false))
         =>
         (unmake-instance ?f)
         (make-instance ?name of if-condition
                        (parent ?p)
                        (condition ?predicate)
                        (on-true ?on-true)
                        (on-false ?on-false)))
(defrule lisp->intermediary::mark-if-conditional-link
         ?f <- (object (is-a list)
                       (contents if 
                                 ?predicate 
                                 then 
                                 call ?on-true 
                                 else 
                                 call ?on-false)
                       (name ?name)
                       (parent ?p))
         (object (is-a predicate-register)
                 (name ?predicate))
         (object (is-a general-purpose-register)
                 (name ?on-true))
         (object (is-a general-purpose-register)
                 (name ?on-false))
         =>
         (unmake-instance ?f)
         (make-instance ?name of if-condition
                        (parent ?p)
                        (link TRUE)
                        (condition ?predicate)
                        (on-true ?on-true)
                        (on-false ?on-false)))
(defrule lisp->intermediary::make-label
         ?f <- (object (is-a list)
                       (contents label
                                 ?label
                                 $?body)
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of label
                        (parent ?p)
                        (title ?label)
                        (body $?body)))
(defrule lisp->intermediary::make-word
         ?f <- (object (is-a list)
                       (contents word
                                 ?value)
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of word
                        (parent ?p)
                        (value ?value)))
(defrule lisp->intermediary::make-environment
         ?f <- (object (is-a list)
                       (contents environment $?rest)
                       (name ?n)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?n of environment
                        (parent ?p)
                        (body ?rest)))
