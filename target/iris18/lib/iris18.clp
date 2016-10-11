(defgeneric move)
(defgeneric branch-call)
(defgeneric branch)
(defgeneric branch-if)
(defgeneric branch-cond)
(defgeneric compare-op)
(defgeneric shift)
(defgeneric swap)
(defmethod swap
  ((?ra SYMBOL)
   (?rb SYMBOL)
   (?comment STRING))
  (comment (swap ?ra
                 ?rb)
           ?comment))
(defmethod swap
  ((?ra SYMBOL)
   (?rb SYMBOL))
  (format nil
          "    swap %s %s"
          ?ra
          ?rb))
(deffunction memory-op
             (?action ?bitmask ?offset ?extra0 ?extra1)
             (format nil
                     "    memory %s %s %s %s"
                     ?action
                     ?bitmask
                     (str-cat ?offset)
                     (str-cat ?extra0)
                     (str-cat ?extra1)))
(deffunction load-value
             (?bitmask ?offset ?addr ?value)
             (memory-op load
                        ?bitmask
                        ?offset
                        ?addr
                        ?value))

(deffunction push
             (?bitmask ?reg ?sp)
             (memory-op push
                        ?bitmask
                        ?reg
                        ?sp
                        ""))

(deffunction pop
             (?bitmask ?reg ?sp)
             (memory-op pop
                        ?bitmask
                        ?reg
                        ?sp
                        ""))

(deffunction store-value
             (?bitmask ?offset ?addr ?value)
             (memory-op store
                        ?bitmask
                        ?offset
                        ?addr
                        ?value))
(deffunction store16
             (?offset ?addr ?value)
             (store-value 0m0011 ?offset ?addr ?value))

(deffunction store32
             (?offset ?addr ?value)
             (store-value 0m1111 ?offset ?addr ?value))

(deffunction load16
             (?offset ?addr ?value)
             (load-value 0m0011 ?offset ?addr ?value))

(deffunction load32
             (?offset ?addr ?value)
             (load-value 0m1111 ?offset ?addr ?value))


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
        ?register
        sp))

(defmethod restore-register
  ((?register SYMBOL))
  (pop 0m1111
       ?register
       sp))

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
  ((?index INTEGER)
   (?arg SYMBOL))
  (format nil
          "    system %d %s"
          ?index
          ?arg))
(defmethod terminate
  ()
  (syscall 0
           r0))
(defmethod putc
  ((?register SYMBOL))
  (syscall 2
           ?register))
(defmethod getc
  ((?register SYMBOL))
  (syscall 1
           ?register))


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




(defmethod set-address
  ((?value INTEGER
           SYMBOL))
  (assign32 addr
            ?value))

(defmethod logical-not
  ((?arg0 SYMBOL))
  (format nil
          "    logical not %s" 
          ?arg0))
(defmethod logical-op
  ((?action SYMBOL)
   (?arg0 SYMBOL)
   (?arg1 SYMBOL))
  (format nil
          "    logical %s %s %s"
          ?action
          ?arg0
          ?arg1))


(defmethod logical-op:immediate
  ((?action SYMBOL
            (neq ?current-argument
                 not))
   (?bitmask SYMBOL)
   (?arg0 SYMBOL)
   (?arg1 LEXEME
          INTEGER))
  (format nil
          "    logical %s immediate %s %s %s"
          ?action
          ?bitmask
          ?arg0
          (str-cat ?arg1)))


