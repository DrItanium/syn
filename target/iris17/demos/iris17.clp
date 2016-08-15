(defmodule iris17
           (export ?ALL))
(defglobal iris17
           ?*address-register* = addr
           ?*link-register* = lr
           ?*value-register* = value)
(deffunction iris17::output-base-instruction
             (?router ?op $?rest)
             (format ?router
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
(defgeneric iris17::defun)
(defgeneric iris17::@label)

(defmethod iris17::@label
  ((?router SYMBOL)
   (?name SYMBOL))
  (output-base-instruction ?router
                           @label
                           ?name))

(defmethod iris17::op:return
  ((?router SYMBOL))
  (output-base-instruction ?router
                           return))

(defmethod iris17::op:nop
  ((?router SYMBOL))
  (output-base-instruction ?router
                           nop))

(defmethod iris17::op:swap
  ((?router SYMBOL)
   (?reg0 SYMBOL)
   (?reg1 SYMBOL))
  (output-base-instruction ?router
                           swap
                           ?reg0
                           ?reg1))
(defmethod iris17::op:move
  ((?router SYMBOL)
   (?bitmask SYMBOL)
   (?reg0 SYMBOL)
   (?reg1 SYMBOL))
  (output-base-instruction ?router
                           move
                           ?bitmask
                           ?reg0
                           ?reg1))
(defmethod iris17::op:move
  ((?router SYMBOL)
   (?reg0 SYMBOL)
   (?reg1 SYMBOL))
  (op:move ?router
           0m1111
           ?reg0
           ?reg1))

(defmethod iris17::op:shift
  ((?router SYMBOL)
   (?direction SYMBOL
               (not (neq ?current-argument
                         left
                         right)))
   (?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (output-base-instruction ?router
                           shift
                           ?direction
                           ?immediate-flag
                           ?register
                           ?immediate))

(defmethod iris17::op:shift
  ((?router SYMBOL)
   (?direction SYMBOL
               (not (neq ?current-argument
                         left
                         right)))
   (?dest SYMBOL)
   (?src SYMBOL))
  (output-base-instruction ?router
                           shift
                           ?direction
                           ?dest
                           ?src))
(defmethod iris17::op:shift-left
  ((?router SYMBOL)
   (?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?dest SYMBOL)
   (?immediate INTEGER))
  (op:shift ?router
            left
            ?immediate-flag
            ?dest
            ?immediate))
(defmethod iris17::op:shift-left
  ((?router SYMBOL)
   (?dest SYMBOL)
   (?src SYMBOL))
  (op:shift ?router
            left
            ?dest
            ?src))

(defmethod iris17::op:shift-right
  ((?router SYMBOL)
   (?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?dest SYMBOL)
   (?immediate INTEGER))
  (op:shift ?router
            right
            ?immediate-flag
            ?dest
            ?immediate))

(defmethod iris17::op:shift-right
  ((?router SYMBOL)
   (?dest SYMBOL)
   (?src SYMBOL))
  (op:shift ?router
            right
            ?dest
            ?src))

(defmethod iris17::op:add
  ((?router SYMBOL)
   (?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (op:arithmetic ?router
                 add
                 ?immediate-flag
                 ?register
                 ?immediate))
(defmethod iris17::op:add
  ((?router SYMBOL)
   (?destination SYMBOL)
   (?source SYMBOL))
  (op:arithmetic ?router
                 add
                 ?destination
                 ?source))
(defmethod iris17::op:mul
  ((?router SYMBOL)
   (?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (op:arithmetic ?router
                 mul
                 ?immediate-flag
                 ?register
                 ?immediate))
(defmethod iris17::op:mul
  ((?router SYMBOL)
   (?destination SYMBOL)
   (?source SYMBOL))
  (op:arithmetic ?router
                 mul
                 ?destination
                 ?source))

(defmethod iris17::op:div
  ((?router SYMBOL)
   (?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (op:arithmetic ?router
                 div
                 ?immediate-flag
                 ?register
                 ?immediate))
(defmethod iris17::op:div
  ((?router SYMBOL)
   (?destination SYMBOL)
   (?source SYMBOL))
  (op:arithmetic ?router
                 div
                 ?destination
                 ?source))

(defmethod iris17::op:rem
  ((?router SYMBOL)
   (?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (op:arithmetic ?router
                 rem
                 ?immediate-flag
                 ?register
                 ?immediate))
(defmethod iris17::op:rem
  ((?router SYMBOL)
   (?destination SYMBOL)
   (?source SYMBOL))
  (op:arithmetic ?router
                 rem
                 ?destination
                 ?source))

(defmethod iris17::op:sub
  ((?router SYMBOL)
   (?immediate-flag SYMBOL
                    (eq ?current-argument
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER))
  (op:arithmetic ?router
                 sub
                 ?immediate-flag
                 ?register
                 ?immediate))
(defmethod iris17::op:sub
  ((?router SYMBOL)
   (?destination SYMBOL)
   (?source SYMBOL))
  (op:arithmetic ?router
                 sub
                 ?destination
                 ?source))


(defmethod iris17::op:memory
  ((?router SYMBOL)
   (?sub-type SYMBOL)
   (?bitmask SYMBOL)
   (?value INTEGER
           SYMBOL))
  (output-base-instruction ?router
                           memory
                           ?sub-type
                           ?bitmask
                           ?value))
(defmethod iris17::op:load
  ((?router SYMBOL)
   (?bitmask SYMBOL)
   (?offset INTEGER))
  (op:memory load
             ?bitmask
             ?offset))

(defmethod iris17::op:merge
  ((?router SYMBOL)
   (?bitmask SYMBOL)
   (?offset INTEGER))
  (op:memory merge
             ?bitmask
             ?offset))

(defmethod iris17::op:push
  ((?router SYMBOL)
   (?bitmask SYMBOL)
   (?register SYMBOL))
  (op:memory ?router
             push
             ?bitmask
             ?register))
(defmethod iris17::op:pop
  ((?router SYMBOL)
   (?bitmask SYMBOL)
   (?register SYMBOL))
  (op:memory ?router
             pop
             ?bitmask
             ?register))
(defmethod iris17::op:pop
  ((?router SYMBOL)
   (?register SYMBOL))
  (op:pop ?router
          0m1111
          ?register))

(defmethod iris17::op:push
  ((?router SYMBOL)
   (?register SYMBOL))
  (op:push ?router
           0m1111
           ?register))

(defmethod iris17::op:compare
  ((?router SYMBOL)
   (?rest MULTIFIELD))
  (output-base-instruction ?router
                           compare
                           ?rest))
(defmethod iris17::op:compare
  ((?router SYMBOL)
   $?rest)
  (op:compare ?router
              ?rest))

(defmethod iris17::op:zero?
  ((?router SYMBOL)
   (?register SYMBOL))
  (op:compare ?router
              ==
              none
              immediate
              ?register
              0))

(defmethod iris17::op:increment
  ((?router SYMBOL)
   (?register SYMBOL))
  (op:add ?router
          immediate
          ?register
          1))
(defmethod iris17::op:decrement
  ((?router SYMBOL)
   (?register SYMBOL))
  (op:sub ?router
          immediate
          ?register
          1))

(defmethod iris17::op:double
  ((?router SYMBOL)
   (?register SYMBOL))
  (op:mul ?router
          immediate
          ?register
          2))

(defmethod iris17::op:halve
  ((?router SYMBOL)
   (?register SYMBOL))
  (op:div ?router
          immediate
          ?register
          2))

(defmethod iris17::op:arithmetic
  ((?router SYMBOL)
   (?sub-type SYMBOL)
   (?uses-immediate SYMBOL
                    (eq ?uses-immediate
                        immediate))
   (?register SYMBOL)
   (?immediate INTEGER
               SYMBOL))
  (output-base-instruction ?router
                           arithmetic
                           ?sub-type
                           ?uses-immediate
                           ?register
                           ?immediate))

(defmethod iris17::op:arithmetic
  ((?router SYMBOL)
   (?sub-type SYMBOL)
   (?dest SYMBOL
              (neq ?current-argument
                   immediate))
   (?src SYMBOL))
  (output-base-instruction ?router
                           arithmetic
                           ?sub-type
                           ?dest
                           ?src))


(defmethod iris17::op:system
  ((?router SYMBOL)
   (?register SYMBOL))
  (output-base-instruction ?router
                           system
                           ?register))

(defmethod iris17::op:set
  ((?router SYMBOL)
   (?bitmask SYMBOL)
   (?register SYMBOL)
   (?value SYMBOL
           INTEGER))
  (output-base-instruction ?router
                           set
                           ?bitmask
                           ?register
                           ?value))
(defmethod iris17::op:clear
  ((?router SYMBOL)
   (?register SYMBOL))
  (op:set ?router
          0m0000
          ?register
          0))

(defmethod iris17::terminate
           ((?router SYMBOL))
           (create$ (op:clear ?router
                              ?*address-register*)
                    (op:system ?router
                               ?*value-register*)))
(defmethod iris17::putchar
  ((?router SYMBOL)
   (?register SYMBOL))
  (create$ (op:set ?router
                   0m0001
                   ?*address-register*
                   1)
           (op:system ?router
                      ?register)))
(defmethod iris17::getchar
  ((?router SYMBOL)
   (?register SYMBOL))
  (create$ (op:set ?router
                   0m0001
                   ?*address-register*
                   2)
           (op:system ?router
                      ?register)))
(defmethod iris17::use-register
  ((?router SYMBOL)
   (?registers MULTIFIELD
               (> (length$ ?current-argument)
                  0))
   $?body)
  (use-register ?router
                (expand$ (first$ ?registers))
                (use-register ?router
                              (rest$ ?registers)
                              $?body)))
(defmethod iris17::use-register
  ((?router SYMBOL)
   (?registers MULTIFIELD
               (= (length$ ?current-argument)
                  0))
   $?body)
  ?body)

(defmethod iris17::use-register
  ((?router SYMBOL)
   (?register SYMBOL)
   $?body)
  (create$ (op:push ?router
                    0m1111
                    ?register)
           $?body
           (op:pop ?router
                   0m1111
                   ?register)))
(defmethod iris17::defun
  ((?router SYMBOL)
   (?name SYMBOL)
   $?body)
  (create$ (@label ?router
                   ?name)
           (use-register ?router
                         lr
                         $?body)
           (op:return ?router)
           (@label ?router
                   (sym-cat ?name _end))))


