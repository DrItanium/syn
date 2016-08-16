(defmodule iris17
           (export ?ALL))
(defglobal iris17
           ?*address-register* = addr
           ?*link-register* = lr
           ?*value-register* = value
           ?*instruction-pointer* = ip
           ?*condition-register* = cr
           ?*stack-pointer* = sp
           ?*arg0* = r9
           ?*arg1* = r8
           ?*arg2* = r7
           ?*arg3* = r6
           ?*temp0* = r5
           ?*temp1* = r4
           ?*temp2* = r3
           ?*temp3* = r2
           ?*return-register* = ?*arg0*)
(deffunction iris17::output-base-instruction
             (?op $?rest)
             (format nil
                     "%s %s"
                     ?op
                     (implode$ ?rest)))
(defgeneric iris17::terminate
            "terminate the simulation")
(defgeneric iris17::readchar)
(defgeneric iris17::putchar)

(defgeneric iris17::op:clear)
(defgeneric iris17::op:system)
(defgeneric iris17::op:arithmetic)
(defgeneric iris17::op:set)
(defgeneric iris17::op:increment)
(defgeneric iris17::op:decrement)
(defgeneric iris17::op:double)
(defgeneric iris17::op:halve)
(defgeneric iris17::op:zero?)
(defgeneric iris17::op:compare)
(defgeneric iris17::op:memory)
(defgeneric iris17::op:load)
(defgeneric iris17::op:merge)
(defgeneric iris17::op:push)
(defgeneric iris17::op:pop)
(defgeneric iris17::op:add)
(defgeneric iris17::op:sub)
(defgeneric iris17::op:mul)
(defgeneric iris17::op:div)
(defgeneric iris17::op:rem)
(defgeneric iris17::op:shift)
(defgeneric iris17::op:shift-left)
(defgeneric iris17::op:shift-right)
(defgeneric iris17::op:move)
(defgeneric iris17::op:swap)
(defgeneric iris17::use-register)
(defgeneric iris17::op:return)
(defgeneric iris17::op:nop)
(defgeneric iris17::defunc)
(defgeneric iris17::@label)
(defgeneric iris17::op:logical)
(defgeneric iris17::op:not)
(defgeneric iris17::op:logical-or)
(defgeneric iris17::op:logical-and)
(defgeneric iris17::op:logical-xor)
(defgeneric iris17::op:logical-nand)
(defgeneric iris17::op:branch)
(defgeneric iris17::op:if)
(defgeneric iris17::op:call)
(defgeneric iris17::op:jump)
(defmethod iris17::op:branch
  ((?args MULTIFIELD))
  (output-base-instruction branch
                           ?args))
(defmethod iris17::op:branch
  ($?args)
  (op:branch ?args))

(defmethod iris17::op:if
  ((?call-flag SYMBOL
               (eq ?current-argument
                   call))
   (?onTrue SYMBOL)
   (?onFalse SYMBOL))
  (op:branch if
             ?call-flag
             ?onTrue
             ?onFalse))
(defmethod iris17::op:if
  ((?onTrue SYMBOL)
   (?onFalse SYMBOL))
  (op:branch if
             ?onTrue
             ?onFalse))
(defmethod iris17::op:call
  ((?imm-flag SYMBOL
              (eq ?current-argument
                  immediate))
   (?target INTEGER
            SYMBOL))
  (op:branch call
             ?imm-flag
             ?target))
(defmethod iris17::op:call
  ((?target SYMBOL))
  (op:branch call
             ?target))
(defmethod iris17::op:jump
           ((?immediate-flag SYMBOL
                             (eq ?current-argument
                                 immediate))
            (?target INTEGER
                     SYMBOL))
           (op:branch ?immediate-flag
                      ?target))
(defmethod iris17::op:jump
  ((?cond SYMBOL
          (eq ?current-argument
              cond))
   (?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?target INTEGER
            SYMBOL))
  (op:branch ?cond
             ?immediate-flag
             ?target))
(defmethod iris17::op:jump
  ((?cond SYMBOL
          (eq ?current-argument
              cond))
   (?target SYMBOL))
  (op:branch ?cond
             ?target))
(defmethod iris17::op:jump
  ((?target SYMBOL))
  (op:branch ?target))


(defmethod iris17::op:not
  ((?reg SYMBOL))
  (output-base-instruction logical
                           not
                           ?reg))

(defmethod iris17::op:logical
  ((?sub-type SYMBOL)
   (?imm-flag SYMBOL
              (eq ?current-argument
                  immediate))
   (?bitmask SYMBOL)
   (?destination SYMBOL)
   (?source INTEGER
            SYMBOL))
  (output-base-instruction logical
                           ?sub-type
                           ?imm-flag
                           ?bitmask
                           ?destination
                           ?source))

(defmethod iris17::op:logical
  ((?sub-type SYMBOL)
   (?destination SYMBOL)
   (?source SYMBOL))
  (output-base-instruction logical
                           ?sub-type
                           ?destination
                           ?source))

(defmethod iris17::op:logical-xor
  ((?imm-flag SYMBOL
              (eq ?current-argument
                  immediate))
   (?bitmask SYMBOL)
   (?destination SYMBOL)
   (?source INTEGER
            SYMBOL))
  (op:logical xor
              ?imm-flag
              ?bitmask
              ?destination
              ?source))
(defmethod iris17::op:logical-xor
  ((?destination SYMBOL)
   (?source SYMBOL))
  (op:logical xor
              ?destination
              ?source))
(defmethod iris17::op:logical-nand
  ((?imm-flag SYMBOL
              (eq ?current-argument
                  immediate))
   (?bitmask SYMBOL)
   (?destination SYMBOL)
   (?source INTEGER
            SYMBOL))
  (op:logical nand
              ?imm-flag
              ?bitmask
              ?destination
              ?source))
(defmethod iris17::op:logical-nand
  ((?destination SYMBOL)
   (?source SYMBOL))
  (op:logical nand
              ?destination
              ?source))

(defmethod iris17::op:logical-or
  ((?imm-flag SYMBOL
              (eq ?current-argument
                  immediate))
   (?bitmask SYMBOL)
   (?destination SYMBOL)
   (?source INTEGER
            SYMBOL))
  (op:logical or
              ?imm-flag
              ?bitmask
              ?destination
              ?source))
(defmethod iris17::op:logical-or
  ((?destination SYMBOL)
   (?source SYMBOL))
  (op:logical or
              ?destination
              ?source))

(defmethod iris17::op:logical-and
  ((?imm-flag SYMBOL
              (eq ?current-argument
                  immediate))
   (?bitmask SYMBOL)
   (?destination SYMBOL)
   (?source INTEGER
            SYMBOL))
  (op:logical and
              ?imm-flag
              ?bitmask
              ?destination
              ?source))

(defmethod iris17::op:logical-and
  ((?destination SYMBOL)
   (?source SYMBOL))
  (op:logical and
              ?destination
              ?source))



(defmethod iris17::@label
  ((?name SYMBOL))
  (output-base-instruction @label
                           ?name))

(defmethod iris17::op:return
  ()
  (output-base-instruction return))

(defmethod iris17::op:nop
  ()
  (output-base-instruction nop))

(defmethod iris17::op:swap
  ((?reg0 SYMBOL)
   (?reg1 SYMBOL))
  (output-base-instruction swap
                           ?reg0
                           ?reg1))
(defmethod iris17::op:move
  ((?bitmask SYMBOL)
   (?reg0 SYMBOL)
   (?reg1 SYMBOL))
  (output-base-instruction move
                           ?bitmask
                           ?reg0
                           ?reg1))
(defmethod iris17::op:move
  ((?reg0 SYMBOL)
   (?reg1 SYMBOL))
  (op:move 0m1111
           ?reg0
           ?reg1))

(defmethod iris17::op:shift
  ((?direction SYMBOL
               (not (neq ?current-argument
                         left
                         right)))
   (?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (output-base-instruction shift
                           ?direction
                           ?immediate-flag
                           ?register
                           ?immediate))

(defmethod iris17::op:shift
  ((?direction SYMBOL
               (not (neq ?current-argument
                         left
                         right)))
   (?dest SYMBOL)
   (?src SYMBOL))
  (output-base-instruction shift
                           ?direction
                           ?dest
                           ?src))

(defmethod iris17::op:shift-left
  ((?dest SYMBOL)
   (?immediate INTEGER))
  (op:shift left
            ?dest
            ?immediate))
(defmethod iris17::op:shift-left
  ((?dest SYMBOL)
   (?src SYMBOL))
  (op:shift left
            ?dest
            ?src))

(defmethod iris17::op:shift-right
  ((?dest SYMBOL)
   (?immediate INTEGER))
  (op:shift right
            immediate
            ?dest
            ?immediate))

(defmethod iris17::op:shift-right
  ((?dest SYMBOL)
   (?src SYMBOL))
  (op:shift right
            ?dest
            ?src))

(defmethod iris17::op:add
  ((?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (op:arithmetic add
                 ?immediate-flag
                 ?register
                 ?immediate))

(defmethod iris17::op:add
  ((?destination SYMBOL)
   (?source SYMBOL))
  (op:arithmetic add
                 ?destination
                 ?source))

(defmethod iris17::op:mul
  ((?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (op:arithmetic mul
                 ?immediate-flag
                 ?register
                 ?immediate))

(defmethod iris17::op:mul
  ((?destination SYMBOL)
   (?source SYMBOL))
  (op:arithmetic mul
                 ?destination
                 ?source))

(defmethod iris17::op:div
  ((?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (op:arithmetic div
                 ?immediate-flag
                 ?register
                 ?immediate))
(defmethod iris17::op:div
  ((?destination SYMBOL)
   (?source SYMBOL))
  (op:arithmetic div
                 ?destination
                 ?source))

(defmethod iris17::op:rem
  ((?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (op:arithmetic rem
                 ?immediate-flag
                 ?register
                 ?immediate))
(defmethod iris17::op:rem
  ((?destination SYMBOL)
   (?source SYMBOL))
  (op:arithmetic rem
                 ?destination
                 ?source))

(defmethod iris17::op:sub
  ((?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (op:arithmetic sub
                 ?immediate-flag
                 ?register
                 ?immediate))

(defmethod iris17::op:sub
  ((?destination SYMBOL)
   (?source SYMBOL))
  (op:arithmetic sub
                 ?destination
                 ?source))


(defmethod iris17::op:memory
  ((?sub-type SYMBOL)
   (?bitmask SYMBOL)
   (?value INTEGER
           SYMBOL))
  (output-base-instruction memory
                           ?sub-type
                           ?bitmask
                           ?value))

(defmethod iris17::op:load
  (
   (?bitmask SYMBOL)
   (?offset INTEGER))
  (op:memory load
             ?bitmask
             ?offset))

(defmethod iris17::op:merge
  ((?bitmask SYMBOL)
   (?offset INTEGER))
  (op:memory merge
             ?bitmask
             ?offset))

(defmethod iris17::op:push
  ((?bitmask SYMBOL)
   (?register SYMBOL))
  (op:memory push
             ?bitmask
             ?register))

(defmethod iris17::op:pop
  ((?bitmask SYMBOL)
   (?register SYMBOL))
  (op:memory pop
             ?bitmask
             ?register))

(defmethod iris17::op:pop
  ((?register SYMBOL))
  (op:pop 0m1111
          ?register))

(defmethod iris17::op:push
  ((?register SYMBOL))
  (op:push 0m1111
           ?register))

(defmethod iris17::op:compare
  ((?rest MULTIFIELD))
  (output-base-instruction compare
                           ?rest))
(defmethod iris17::op:compare
  ($?rest)
  (op:compare ?rest))

(defmethod iris17::op:zero?
  ((?register SYMBOL))
  (op:compare ==
              none
              immediate
              ?register
              0))

(defmethod iris17::op:increment
  ((?register SYMBOL))
  (op:add immediate
          ?register
          1))
(defmethod iris17::op:decrement
  ((?register SYMBOL))
  (op:sub immediate
          ?register
          1))

(defmethod iris17::op:double
  ((?register SYMBOL))
  (op:mul immediate
          ?register
          2))

(defmethod iris17::op:halve
  ((?register SYMBOL))
  (op:div immediate
          ?register
          2))

(defmethod iris17::op:arithmetic
  ((?sub-type SYMBOL)
   (?uses-immediate SYMBOL
                    (eq ?uses-immediate
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER
               SYMBOL))
  (output-base-instruction arithmetic
                           ?sub-type
                           ?uses-immediate
                           ?register
                           ?immediate))

(defmethod iris17::op:arithmetic
  ((?sub-type SYMBOL)
   (?dest SYMBOL
              (neq ?current-argument
                   immediate))
   (?src SYMBOL))
  (output-base-instruction arithmetic
                           ?sub-type
                           ?dest
                           ?src))


(defmethod iris17::op:system
  ((?register SYMBOL))
  (output-base-instruction system
                           ?register))

(defmethod iris17::op:set
  ((?bitmask SYMBOL)
   (?register SYMBOL)
   (?value SYMBOL
           INTEGER))
  (output-base-instruction set
                           ?bitmask
                           ?register
                           ?value))
(defmethod iris17::op:clear
  ((?register SYMBOL))
  (op:set 0m0000
          ?register
          0))

(defmethod iris17::terminate
           ()
           (create$ (op:clear ?*address-register*)
                    (op:system ?*value-register*)))

(defmethod iris17::putchar
  ((?register SYMBOL))
  (create$ (op:set
                   0m0001
                   ?*address-register*
                   1)
           (op:system ?register)))
(defmethod iris17::getchar
  ((?register SYMBOL))
  (create$ (op:set 0m0001
                   ?*address-register*
                   2)
           (op:system ?register)))
(defmethod iris17::use-register
  ((?registers MULTIFIELD
               (> (length$ ?current-argument)
                  0))
   $?body)
  (use-register (expand$ (first$ ?registers))
                (use-register (rest$ ?registers)
                              $?body)))
(defmethod iris17::use-register
  ((?registers MULTIFIELD
               (= (length$ ?current-argument)
                  0))
   $?body)
  ?body)

(defmethod iris17::use-register
  ((?register SYMBOL)
   $?body)
  (create$ (op:push 0m1111
                    ?register)
           $?body
           (op:pop 0m1111
                   ?register)))
(defmethod iris17::defunc
  ((?name SYMBOL)
   $?body)
  (create$ (@label ?name)
           (use-register ?*link-register*
                         $?body)
           (op:return)
           (@label (sym-cat ?name _end))))

(defmethod iris17::decode-bits-fn
  ()
  (defunc decode-bits
          (use-register (create$ (bind ?data
                                       r9)
                                 (bind ?mask
                                       r8)
                                 (bind ?shift
                                       r7))
                        (op:logical-and
                          ?data
                          ?mask)
                        (op:shift-left
                          ?data
                          ?shift))))

(defmethod iris17::encode-bits-fn
  ()
  (defunc encode-bits
          (use-register (create$ (bind ?input
                                       r9)
                                 (bind ?add-value
                                       r8)
                                 (bind ?mask
                                       r7)
                                 (bind ?shift
                                       r6))
                        (op:not ?mask)
                        (op:logical-and ?input
                                        ?mask)
                        (op:shift-left ?add-value
                                       ?shift)
                        (op:logical-or ?input
                                       ?add-value))))

(defmethod iris17::print-string-fn
  ()
  (bind ?loop-start
        printString_LoopStart)
  (bind ?loop-done
        printString_LoopEnd)
  (defunc printString
          (use-register (create$ (bind ?address
                                       r9)
                                 ?*address-register*)

                        (op:move ?*address-register*
                                 ?address)
                        (@label ?loop-start)
                        (op:load 0m0011
                                 0)
                        (op:zero? ?*value-register*)
                        (op:jump cond
                                 immediate
                                 ?loop-done)
                        (use-register ?*address-register*
                                      (putchar ?*value-register*))
                        (op:increment ?*address-register*)
                        (op:jump immediate
                                 ?loop-start)
                        (@label ?loop-done))))



