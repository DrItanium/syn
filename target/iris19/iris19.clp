; iris18 addons
(defclass defn
  (is-a node)
  (slot title
        (type SYMBOL)
        (default ?NONE))
  (slot args
        (default ?NONE))
  (multislot body))
(defclass macro
  (is-a node)
  (slot title
        (type SYMBOL)
        (default ?NONE))
  (slot args
        (default ?NONE))
  (multislot body))
(defclass args
  (is-a node)
  (multislot contents
             (visibility public)
             (storage local)))

(defmessage-handler defn get-return-value primary
                    ()
                    ; find the last element of the body
                    (send (nth$ (length$ ?self:body)
                                ?self:body)
                          get-return-value))

(defrule parse-defn
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defn
                                 ?name
                                 ?args
                                 $?body)
                       (name ?title)
                       (parent ?parent))
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (parent ?title)
                        (contents $?contents))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?title of defn
                        (parent ?parent)
                        (title ?name)
                        (args (make-instance ?args of args
                                             (contents ?contents)
                                             (parent ?title)))
                        (body ?body)))

(defrule too-many-args-to-target-defn
         (stage (current parse))
         (object (is-a args)
                 (contents $?args&:(> (length$ ?args)
                                      4))
                 (parent ?defn))
         (object (is-a defn)
                 (name ?defn)
                 (title ?op))
         =>
         (printout werror "ERROR: only four arguments are allowed for a given defn. Bad function is " ?op crlf)
         (halt))

(defclass chained-operation
  (is-a node)
  (message-handler get-return-value primary))
(defclass special-operation
  (is-a chained-operation)
  (slot operation
        (visibility public)
        (storage local)
        (default ?NONE))
  (multislot args
             (storage local)
             (visibility public)))

(defclass memory-operation
  (is-a special-operation)
  (slot bitmask
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass stack-operation
  (is-a memory-operation)
  (slot pointer
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot target
        (storage local)
        (visibility public)
        (default ?NONE)))
(defmessage-handler stack-operation get-return-value primary
                    ()
                    ?self:target)
(defclass load-store-operation
  (is-a memory-operation)
  (slot offset
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot address
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE)))
(defmessage-handler load-store-operation get-return-value primary
                    ()
                    ?self:value)

(defclass store-operation
  (is-a load-store-operation)
  (slot operation
        (source composite)
        (storage shared)
        (default store)))

(defclass load-operation
  (is-a load-store-operation)
  (slot operation
        (source composite)
        (storage shared)
        (default load)))

(defclass push-operation
  (is-a stack-operation)
  (slot operation
        (source composite)
        (storage shared)
        (default push)))

(defclass pop-operation
  (is-a stack-operation)
  (slot operation
        (source composite)
        (storage shared)
        (default pop)))


(defrule parse-store-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents *memory
                                 store
                                 ?bitmask
                                 ?offset
                                 ?address
                                 ?value
                                 $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of store-operation
                        (bitmask ?bitmask)
                        (parent ?parent)
                        (address ?address)
                        (value ?value)
                        (args $?rest)
                        (offset ?offset)))

(defrule parse-load-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents *memory
                                 load
                                 ?bitmask
                                 ?offset
                                 ?address
                                 ?value
                                 $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of load-operation
                        (bitmask ?bitmask)
                        (parent ?parent)
                        (address ?address)
                        (value ?value)
                        (args $?rest)
                        (offset ?offset)))
(defrule parse-pop-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents *memory
                                 pop
                                 ?bitmask
                                 ?target
                                 ?pointer
                                 $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of pop-operation
                        (bitmask ?bitmask)
                        (parent ?parent)
                        (pointer ?pointer)
                        (args $?rest)
                        (target ?target)))

(defrule parse-push-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents *memory
                                 push
                                 ?bitmask
                                 ?target
                                 ?pointer
                                 $?rest)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of push-operation
                        (bitmask ?bitmask)
                        (parent ?parent)
                        (pointer ?pointer)
                        (args $?rest)
                        (target ?target)))

(defclass loop
  (is-a node)
  (slot id
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot body
             (storage local)
             (visibility public)))

(defrule parse-loop
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents loop
                                 ?id
                                 $?body)
                       (name ?name)
                       (parent ?parent))
         ?f2 <- (object (is-a list)
                        (name ?id)
                        (contents label ?id))
         =>
         (unmake-instance ?f
                          ?f2)
         (make-instance ?name of loop
                        (parent ?parent)
                        (id ?id)
                        (body ?body)))
(defclass arithmetic-operation
  (is-a special-operation)
  (slot immediate
        (type SYMBOL)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE
                         TRUE))
  (slot destination
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot source
        (visibility public)
        (storage local)
        (default ?NONE)))

(defmessage-handler arithmetic-operation get-return-value primary
                    ()
                    ?self:destination)
(defrule parse-arithmetic-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents *arithmetic
                                 ?subop
                                 ?reg0
                                 ?reg1)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of arithmetic-operation
                        (parent ?parent)
                        (operation ?subop)
                        (destination ?reg0)
                        (source ?reg1)))
(defclass register
  (is-a USER))
(deffunction registerp
             (?reg)
             (and (instance-existp (bind ?tag
                                         (symbol-to-instance-name ?reg)))
                  (eq (class ?tag)
                      register)))
(defrule tag-registers
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents $?a ?reg $?b))
         (test (and (symbolp ?reg)
                    (registerp ?reg)))
         =>
         (modify-instance ?f
                          (contents $?a
                                    (symbol-to-instance-name ?reg)
                                    $?b)))
(definstances register-declarations
              (r0 of register)
              (r1 of register)
              (r2 of register)
              (r3 of register)
              (r4 of register)
              (r5 of register)
              (r6 of register)
              (r7 of register)
              (r8 of register)
              (r9 of register)
              (r10 of register)
              (r11 of register)
              (r12 of register)
              (r13 of register)
              (r14 of register)
              (r15 of register)
              (addr of register)
              (ip of register)
              (sp of register)
              (value of register)
              (cr of register))

(defclass fcall
  (is-a node)
  (slot operation
        (visibility public)
        (storage local)
        (default ?NONE))
  (multislot arguments
             (visibility public)
             (storage local)))

(defrule error-not-a-registered-function-call
         (stage (current associate))
         (object (is-a defn)
                 (body $? ?list $?))
         (object (is-a list)
                 (name ?list)
                 (contents ?operation
                           $?body))
         (not (object (is-a defn|macro)
                      (title ?operation)))
         =>
         (printout werror "ERROR: The function " ?operation " is not defined before use!" crlf)
         (halt))

(defrule register-function-call
         (stage (current associate))
         (object (is-a defn)
                 (body $? ?list $?))
         ?f <- (object (is-a list)
                       (name ?list)
                       (contents ?operation
                                 $?body)
                       (parent ?parent))

         (object (is-a defn)
                 (title ?operation)
                 (name ?defn))
         =>
         (unmake-instance ?f)
         (make-instance ?list of fcall
                        (operation ?defn)
                        (arguments $?body)
                        (parent ?parent)))
(defrule too-many-arguments-to-the-fcall
         (stage (current associate))
         ?f <- (object (is-a fcall)
                       (operation ?defn)
                       (arguments $?body)
                       (parent ?parent))
         (object (is-a defn)
                 (name ?defn)
                 (title ?going-to-call))
         (object (is-a args)
                 (parent ?defn)
                 (contents $?args))
         (test (<> (length$ ?body)
                   (length$ ?args)))
         (object (is-a defn)
                 (name ?calling-defn)
                 (title ?op))
         (test (send ?f parent-is ?calling-defn))
         =>
         (printout werror "ERROR: in function " ?op ", argument mismatch when calling " ?going-to-call crlf)
         (halt))

(defclass compare-operation
  (is-a special-operation)
  (slot combine
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot destination
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot source
        (storage local)
        (visibility public)
        (default ?NONE)))
(defmessage-handler compare-operation get-return-value primary
                    ()
                    [cr])

(defrule generate-compare-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (name ?name)
                       (parent ?parent)
                       (contents *compare
                                 ?op
                                 ?combine
                                 ?destination
                                 ?source))
         =>
         (unmake-instance ?f)
         (make-instance ?name of compare-operation
                        (parent ?parent)
                        (operation ?op)
                        (combine ?combine)
                        (destination ?destination)
                        (source ?source)))


(defrule parse-macro
         (stage (current parse))
         ?f <- (object (is-a list)
                       (name ?name)
                       (parent ?parent)
                       (contents defmac
                                 ?title
                                 ?args
                                 $?body))
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (parent ?name)
                        (contents $?elements))
         =>
         (unmake-instance ?f
                          ?f2)
         (make-instance ?name of macro
                        (parent ?parent)
                        (body ?body)
                        (args (make-instance ?args of args
                                             (parent ?name)
                                             (contents ?elements)))
                        (title ?title)))
(defclass argument-association
  (is-a node)
  (slot title
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot current-value
        (storage local)
        (visibility public)))

(defrule associate-argument
         (stage (current associate))
         (object (is-a macro|defn)
                 (name ?macro)
                 (args ?args))
         (object (is-a args)
                 (name ?args)
                 (contents $? ?arg $?))
         ?f <- (object (is-a singlefield-variable)
                       (name ?arg)
                       (value ?value&:(not (instancep ?value))))

         =>
         (modify-instance ?f
                          (value (make-instance of argument-association
                                                (parent ?arg)
                                                (title ?value)))))
(defrule register-argument
         (stage (current associate))
         ?f <- (object (is-a singlefield-variable)
                       (parent ?p)
                       (value ?cv&:(not (instancep ?cv))))
         (test (neq (class ?p)
                    args))
         (object (is-a macro|defn)
                 (name ?mac)
                 (args ?args))
         (test (send ?f parent-is ?mac))
         (object (is-a args)
                 (name ?args)
                 (contents $? ?arg $?))
         (object (is-a singlefield-variable)
                 (name ?arg)
                 (value ?v))
         (object (is-a argument-association)
                 (name ?v)
                 (parent ?arg)
                 (title ?cv))
         =>
         (modify-instance ?f
                          (value ?v)))


(defrule update-argument-associations:arg0
         (stage (current associate))
         (object (is-a defn)
                 (args ?args))
         (object (is-a args)
                 (name ?args)
                 (contents ?first $?))
         ?f <- (object (is-a argument-association)
                       (parent ?first)
                       (current-value nil))
         =>
         (modify-instance ?f
                          (current-value [value])))

(defrule update-argument-associations:arg1
         (stage (current associate))
         (object (is-a defn)
                 (args ?args))
         (object (is-a args)
                 (name ?args)
                 (contents ? ?curr $?))
         ?f <- (object (is-a argument-association)
                       (parent ?curr)
                       (current-value nil))
         =>
         (modify-instance ?f
                          (current-value [r8])))
(defrule update-argument-associations:arg2
         (stage (current associate))
         (object (is-a defn)
                 (args ?args))
         (object (is-a args)
                 (name ?args)
                 (contents ? ? ?curr $?))
         ?f <- (object (is-a argument-association)
                       (parent ?curr)
                       (current-value nil))
         =>
         (modify-instance ?f
                          (current-value [r7])))

(defrule update-argument-associations:arg3
         (stage (current associate))
         (object (is-a defn)
                 (args ?args))
         (object (is-a args)
                 (name ?args)
                 (contents ? ? ? ?curr $?))
         ?f <- (object (is-a argument-association)
                       (parent ?curr)
                       (current-value nil))
         =>
         (modify-instance ?f
                          (current-value [r6])))


(defclass mcall
  (is-a node)
  (slot operation
        (visibility public)
        (storage local)
        (default ?NONE))
  (multislot arguments
             (visibility public)
             (storage local)))

(defrule register-macro-call
         (stage (current associate))
         (object (is-a defn|macro)
                 (body $? ?list $?))
         ?f <- (object (is-a list)
                       (name ?list)
                       (contents ?operation
                                 $?body)
                       (parent ?parent))

         (object (is-a macro)
                 (title ?operation)
                 (name ?mac))
         =>
         (unmake-instance ?f)
         (make-instance ?list of mcall
                        (operation ?mac)
                        (arguments $?body)
                        (parent ?parent)))

(defclass branch-operation
  (is-a special-operation))
(defclass call-operation
  (is-a branch-operation))
(defclass funcall
  (is-a branch-operation))
(defclass if-operation
  (is-a branch-operation))
(defclass set-operation
  (is-a special-operation)
  (slot bitmask
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot destination
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot source
        (visibility public)
        (storage local)
        (default ?NONE)))
(defrule generate-set-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents *set
                                 ?bitmask
                                 ?destination
                                 ?source)
                       (name ?name)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of set-operation
                        (parent ?parent)
                        (bitmask ?bitmask)
                        (destination ?destination)
                        (source ?source)))

