(defgeneric move)
(defgeneric branch-call)
(defgeneric branch)
(defgeneric branch-if)
(defgeneric branch-cond)
(defgeneric compare-op)
(defgeneric shift)

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

(defmethod ret
  ()
  (note (pop 0m1111
             ip)
        "return macro"))

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

(defmethod save-register
  ((?register SYMBOL))
  (push 0m1111
        ?register))

(defmethod restore-register
  ((?register SYMBOL))
  (pop 0m1111
       ?register))

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




