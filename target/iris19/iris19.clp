; iris19 addons
(defclass asm-operation
  (is-a node)
  (slot group
        (type SYMBOL)
        (visibility public)
        (default ?NONE))
  (multislot properties))
(defclass asm-operation-property
  (is-a node)
  (slot key
        (type SYMBOL)
        (visibility public)
        (default ?NONE))
  (multislot value
             (visibility public)))

(defclass sub-type-property
  (is-a asm-operation-property)
  (slot key
        (source composite)
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (default sub-type))
  (slot sub-type
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass asm-operation-field
  (is-a asm-operation-property)
  (slot target
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot is-stack
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public))
  (slot is-indirect
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public)))

(deffunction build-field-registration-rules
             (?key)
             (bind ?rule-base
                   (format nil
                           "(defrule parse-asm-operation-field:%s:%%s
                                     (declare (salience 1))
                                     (object (is-a asm-operation)
                                             (properties $? ?p $?))
                                     ?f <- (object (is-a list)
                                                   (name ?p)
                                                   (parent ?k)
                                                   (contents %s
                                                             ?reg
                                                             %%s))
                                     =>
                                     (unmake-instance ?f)
                                     (make-instance ?p of asm-operation-field
                                                    (parent ?k)
                                                    (key %s)
                                                    (target ?reg)
                                                    (is-stack %%s)
                                                    (is-indirect %%s)))"
                           ?key
                           ?key
                           ?key))
             (build (format nil
                            ?rule-base
                            none
                            ""
                            FALSE
                            FALSE))
             (build (format nil
                            ?rule-base
                            stack
                            stack
                            TRUE
                            FALSE))
             (build (format nil
                            ?rule-base
                            indirect
                            indirect
                            FALSE
                            TRUE)))

(build-field-registration-rules destination)
(build-field-registration-rules source0)
(build-field-registration-rules source1)



(defrule parse-asm-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents **asm
                                 ?group
                                 $?properties)
                       (name ?title)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?title of asm-operation
                        (parent ?parent)
                        (group ?group)
                        (properties ?properties)))
(defrule parse-asm-sub-type-property
         (declare (salience 1))
         (object (is-a asm-operation)
                 (properties $? ?property $?))
         ?f <- (object (is-a list)
                       (name ?property)
                       (parent ?parent)
                       (contents sub-type
                                 ?stype))
         =>
         (unmake-instance ?f)
         (make-instance ?property of sub-type-property
                        (sub-type ?stype)
                        (parent ?parent)))

(defrule parse-asm-operation-property:generic
         (stage (current parse))
         (object (is-a asm-operation)
                 (properties $? ?property $?))
         ?f <- (object (is-a list)
                       (name ?property)
                       (parent ?parent)
                       (contents ?key
                                 $?values))
         =>
         (unmake-instance ?f)
         (make-instance ?property of asm-operation-property
                        (parent ?parent)
                        (key ?key)
                        (value ?values)))


