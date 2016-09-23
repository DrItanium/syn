(deffunction memory-op
             (?action ?bitmask ?offset)
             (format nil
                     "memory %s %s %s"
                     ?action
                     ?bitmask
                     (str-cat ?offset)))
(deffunction load-value
             (?bitmask ?offset)
             (memory-op load
                        ?bitmask
                        ?offset))
(deffunction push
             (?bitmask ?reg)
             (memory-op push
                        ?bitmask
                        ?reg))
(deffunction pop
             (?bitmask ?reg)
             (memory-op pop
                        ?bitmask
                        ?reg))
(deffunction store-value
             (?bitmask ?offset)
             (memory-op store
                        ?bitmask
                        ?offset))

(deffunction arithmetic-op
             (?action ?immediate ?dest ?src)
             (format nil
                     "arithmetic %s %s %s %s"
                     ?action
                     (if ?immediate then
                       immediate
                       else
                       "")
                     ?dest
                     ?src))
(deffunction add
             (?immediate ?dest ?src)
             (arithmetic-op add
                            (neq ?immediate
                                 FALSE)
                            ?dest
                            ?src))

(deffunction sub
             (?immediate ?dest ?src)
             (arithmetic-op sub
                            (neq ?immediate
                                 FALSE)
                            ?dest
                            ?src))
(deffunction mul
             (?immediate ?dest ?src)
             (arithmetic-op mul
                            (neq ?immediate
                                 FALSE)
                            ?dest
                            ?src))
(deffunction divide
             (?immediate ?dest ?src)
             (arithmetic-op div
                            (neq ?immediate
                                 FALSE)
                            ?dest
                            ?src))
(deffunction rem
             (?immediate ?dest ?src)
             (arithmetic-op rem
                            (neq ?immediate
                                 FALSE)
                            ?dest
                            ?src))

(deffunction load16
             (?addr)
             (load-value 0m0011
                         0))
(deffunction load32
             (?addr)
             (load-value 0m1111
                         0))
(deffunction assign
             (?bitmask ?register ?value)
             (format nil
                     "set %s %s %s"
                     ?bitmask
                     ?register
                     (str-cat ?value)))

(deffunction ret
             ()
             return)

(deffunction deflabel
             (?title)
             (format nil
                     "@label %s"
                     ?title))
(deffunction select-immediate
             (?cond)
             (if ?cond then
               immediate
               else
               ""))
(deffunction select-call
             (?cond)
             (if ?cond then
               call
               else
               ""))
(deffunction select-cond
             (?cond)
             (if ?cond then
               cond
               else
               ""))

(defgeneric branch-call)
(defgeneric branch)
(defgeneric branch-if)
(defgeneric branch-cond)

(defmethod branch-if
  ((?call SYMBOL)
   (?onTrue LEXEME)
   (?onFalse LEXEME))
  (format nil
          "branch if %s %s %s"
          (select-call ?call)
          ?onTrue
          ?onFalse))

(defmethod branch-call
  ((?immediate SYMBOL)
   (?target LEXEME))
  (format nil
          "branch call %s %s"
          (select-immediate ?immediate)
          (str-cat ?target)))
(defmethod branch
  ((?immediate SYMBOL)
   (?target LEXEME))
  (format nil
          "branch %s %s"
          (select-immediate ?immediate)
          (str-cat ?target)))

(defmethod branch-cond
  ((?immediate SYMBOL)
   (?target LEXEME))
  (format nil
          "branch cond %s %s"
          (select-immediate ?immediate)
          (str-cat ?target)))

(defgeneric compare-op)
(defmethod compare-op
  ((?compare LEXEME)
   (?combine LEXEME)
   (?immediate SYMBOL)
   (?arg0 LEXEME
          INTEGER)
   (?arg1 LEXEME
          INTEGER))
  (format nil
          "compare %s %s %s %s %s"
          (str-cat ?compare)
          (str-cat ?combine)
          (select-immediate ?immediate)
          (str-cat ?arg0)
          (str-cat ?arg1)))

(defgeneric use-register)
(defgeneric save-register)
(defgeneric restore-register)
(defmethod use-register
  ((?register SYMBOL)
   (?body MULTIFIELD))
  (create$ (save-register ?register)
           ?body
           (restore-register ?register)))
(defmethod use-register
  ((?register SYMBOL)
   $?body)
  (use-register ?register
                ?body))
(defmethod save-register
  ((?register SYMBOL))
  (push 0m1111
        ?register))

(defmethod restore-register
  ((?register SYMBOL))
  (pop 0m1111
       ?register))
(defgeneric defunc)
(defmethod defunc
  ((?name SYMBOL)
   (?entries MULTIFIELD))
  (create$ (deflabel ?name)
           ?entries
           (ret)))
(defmethod defunc
  ((?name SYMBOL)
   $?entries)
  (defunc ?name
          ?entries))
(defgeneric output)
(defmethod output
  ((?router SYMBOL)
   (?lines MULTIFIELD))
  (progn$ (?line ?lines)
          (printout ?router
                    ?line crlf)))
(defmethod output
  ((?router SYMBOL)
   $?lines)
  (output ?router
          ?lines))
