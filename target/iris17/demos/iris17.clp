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
  (op:arithmetic ?router
                 add
                 immediate
                 ?register
                 1))
(defmethod iris17::op:decrement
  ((?router SYMBOL)
   (?register SYMBOL))
  (op:arithmetic ?router
                 sub
                 immediate
                 ?register
                 1))

(defmethod iris17::op:double
  ((?router SYMBOL)
   (?register SYMBOL))
  (op:arithmetic ?router
                 mul
                 immediate
                 ?register
                 2))

(defmethod iris17::op:halve
  ((?router SYMBOL)
   (?register SYMBOL))
  (op:arithmetic ?router
                 div
                 immediate
                 ?register
                 2))

(defmethod iris17::op:arithmetic
  ((?router SYMBOL)
   (?rest MULITIFIELD))
  (output-base-instruction ?router
                           arithmetic
                           ?rest))

(defmethod iris17::op:arithmetic
  ((?router SYMBOL)
   $?rest)
  (op:arithmetic ?router
                 ?rest))

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

