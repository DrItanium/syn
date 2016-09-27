(defgeneric comment)
(defmethod comment
  ((?op LEXEME)
   (?comment STRING))
  (format nil
          "%s ; %s"
          ?op
          ?comment))
(defmethod comment
  ((?comment STRING))
  (format nil
          "    ; %s"
          ?comment))

(defmethod todo
  ((?message STRING))
  (comment (format nil
                   "TODO: %s"
                   ?message)))
(deffunction generate-note-text
             (?msg)
             (format nil
                     "NOTE: %s"
                     ?msg))
(defmethod note
  ((?op LEXEME)
   (?message STRING))
  (comment ?op
           (generate-note-text ?message)))
(defmethod note
  ((?message STRING))
  (comment (generate-note-text ?message)))

(defmethod describe-arg
  ((?argument LEXEME)
   (?description STRING))
  (comment (format nil
                   "%s - %s"
                   ?argument
                   ?description)))
(deffunction memory-op
             (?action ?bitmask ?offset)
             (format nil
                     "    memory %s %s %s"
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
(deffunction store16
             (?offset)
             (store-value 0m0011 ?offset))

(deffunction store32
             (?offset)
             (store-value 0m1111 ?offset))


(deffunction arithmetic-op
             (?action ?immediate ?dest ?src)
             (format nil
                     "    arithmetic %s %s %s %s"
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
             (?offset)
             (load-value 0m0011
                         ?offset))
(deffunction load32
             (?offset)
             (load-value 0m1111
                         ?offset))
(defgeneric assign)
(defmethod assign
  ((?bitmask SYMBOL)
   (?register LEXEME)
   (?value LEXEME
           INTEGER))
  (format nil
          "    set %s %s %s"
          ?bitmask
          ?register
          (str-cat ?value)))

(defmethod assign8
  ((?register LEXEME)
   (?value LEXEME
           INTEGER))
  (assign 0m0001
          ?register
          ?value))
(defmethod assign16
  ((?register LEXEME)
   (?value LEXEME
           INTEGER))
  (assign 0m0011
          ?register
          ?value))
(defmethod assign24
  ((?register LEXEME)
   (?value LEXEME
           INTEGER))
  (assign 0m0111
          ?register
          ?value))
(defmethod assign32
  ((?register LEXEME)
   (?value LEXEME
           INTEGER))
  (assign 0m1111
          ?register
          ?value))

(defgeneric move)
(defmethod move
  ((?bitmask SYMBOL)
   (?dest LEXEME)
   (?source LEXEME))
  (format nil
          "    move %s %s %s"
          ?bitmask
          ?dest
          ?source))
(defmethod copy
  ((?dest LEXEME)
   (?source LEXEME))
  (move 0m1111
        ?dest
        ?source))

(deffunction ret
             ()
             (note (pop 0m1111
                        ip)
                   "return macro"))

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
          "    branch if %s %s %s"
          (select-call ?call)
          ?onTrue
          ?onFalse))

(defmethod branch-call
  ((?immediate SYMBOL)
   (?target LEXEME))
  (format nil
          "    branch call %s %s"
          (select-immediate ?immediate)
          (str-cat ?target)))
(defmethod branch
  ((?immediate SYMBOL)
   (?target LEXEME))
  (format nil
          "    branch %s %s"
          (select-immediate ?immediate)
          (str-cat ?target)))

(defmethod branch-cond
  ((?immediate SYMBOL)
   (?target LEXEME))
  (format nil
          "    branch cond %s %s"
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
          "    compare %s %s %s %s %s"
          (str-cat ?compare)
          (str-cat ?combine)
          (select-immediate ?immediate)
          (str-cat ?arg0)
          (str-cat ?arg1)))

(defgeneric use-register)
(defgeneric save-register)
(defgeneric restore-register)
(defgeneric scope
            "Create a scope for formatting purposes")
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
  (scope ?name
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

(defmethod zero
  ((?register LEXEME))
  (assign 0m0000
          ?register
          0x0))

(defmethod syscall
  ((?arg LEXEME))
  (format nil
          "    system %s"
          ?arg))
(defmethod syscall
  ((?arg LEXEME)
   (?index INTEGER))
  (create$ (assign 0m0001
                   addr
                   ?index)
           (syscall ?arg)))
(defmethod terminate
  ()
  (create$ (zero addr)
           (syscall r0)))


(defmethod scope
  ((?name LEXEME)
   (?body MULTIFIELD))
  (create$ (deflabel ?name)
           ?body))
(defmethod scope
  ((?name LEXEME)
   $?body)
  (scope ?name
         ?body))
(defgeneric shift)
(defmethod shift
  ((?direction SYMBOL)
   (?immediate SYMBOL)
   (?dest LEXEME)
   (?value LEXEME
           INTEGER))
  (format nil
          "    shift %s %s %s %s"
          ?direction
          (select-immediate ?immediate)
          ?dest
          (str-cat ?value)))
(defmethod increment
  ((?register SYMBOL))
  (note (add immediate
             ?register
             0x1)
        "increment macro"))
(defmethod decrement
  ((?register SYMBOL))
  (note (sub immediate
             ?register
             0x1)
        "decrement macro"))

(defmethod dword
  ((?value LEXEME
           INTEGER))
  (format nil
          "    @dword %s"
          (str-cat ?value)))
(defmethod word
  ((?value LEXEME
           INTEGER))
  (format nil
          "    @word %s"
          (str-cat ?value)))
(defmethod memory-location
  ((?value LEXEME
           NUMBER))
  (format nil
          "@org %s"
          (str-cat ?value)))



(defmethod at-memory-location
  ((?value LEXEME
           NUMBER)
   (?body MULTIFIELD))
  (create$ (memory-location ?value)
           ?body))
(defmethod at-memory-location
  ((?value LEXEME
           NUMBER)
   $?body)
  (at-memory-location ?value
                      ?body))

