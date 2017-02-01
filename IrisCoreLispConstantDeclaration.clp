(defclass lower::constant-declaration
          (is-a node)
          (slot value
                (visibility public)
                (storage local)
                (default ?NONE))
          (message-handler resolve primary))

(defmessage-handler lower::constant-declaration resolve primary
                    ()
                    (send ?self:value
                          resolve))

(defrule lower::construct-constant-declaration
         (declare (salience ?*priority:first*))
         ?f <- (object (is-a list)
                       (contents let
                                 ?title
                                 mean|be
                                 ?value)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?title of constant-declaration
                        (parent ?parent)
                        (value ?value)))

(defrule lower::replace-constant-declaration
         (declare (salience 100))
         ?f <- (object (is-a list)
                       (contents $?a
                                 ?constant&:(symbolp ?constant)
                                 $?b))
         (object (is-a constant-declaration)
                 (name =(symbol-to-instance-name ?constant)))
         =>
         (modify-instance ?f
                          (contents $?a
                                    (symbol-to-instance-name ?constant)
                                    $?b)))
