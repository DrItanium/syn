; execution pipeline
(defmodule iris2
           (import cortex 
                   ?ALL)
           (import ucode
                   ?ALL)
           (export ?ALL))
(defclass iris2::core
  (is-a thing)
  (slot parent
        (source composite)
        (default FALSE))
  (slot registers
        (type INSTANCE
              SYMBOL)
        (visibility public)
        (storage local)
        (default-dynamic FALSE))
  (slot code 
        (type INSTANCE
              SYMBOL)
        (visibility public)
        (storage local)
        (default-dynamic FALSE))
  (slot data 
        (type INSTANCE
              SYMBOL)
        (visibility public)
        (storage local)
        (default-dynamic FALSE))
  (slot stack
        (type INSTANCE
              SYMBOL)
        (visibility public)
        (storage local)
        (default-dynamic FALSE))
  (message-handler init after))


(defmessage-handler iris2::core init after
                    ()
                    (bind ?this
                          (instance-name ?self))
                    (bind ?self:registers
                          (make-instance of memory-space
                                         (parent ?this)
                                         (type word16u)
                                         (capacity 16)))
                    (bind ?self:code
                          (make-instance of memory-space
                                         (parent ?this)
                                         (type word32u)
                                         (capacity 65536)))
                    (bind ?self:data
                          (make-instance of memory-space
                                         (parent ?this)
                                         (type word16u)
                                         (capacity 65536)))
                    (bind ?self:stack
                          (make-instance of memory-space
                                         (parent ?this)
                                         (type word16u)
                                         (capacity 65536))))

(deffunction iris2::defsection
             (?title ?mask ?shift)
             (create$ ?title 
                      ?mask
                      ?shift))

(defglobal iris2
           ?*instruction-sections* = (create$ (defsection group (hex->int 0x00000007) 0)
                                              (defsection operation (hex->int 0x000000F8) 3)
                                              (defsection immediate (hex->int 0xFFFF0000) 16)
                                              (defsection half-immediate (hex->int 0xFF000000) 24)
                                              (defsection destination (hex->int 0x0000FF00) 8)
                                              (defsection source0 (hex->int 0x00FF0000) 16)
                                              (defsection source1 (hex->int 0xFF000000) 24)
                                              (defsection control (hex->int 0x000000FF) 0)))

(deffunction iris2::decode-instruction
             (?section ?value)
             (if (bind ?ind
                       (member$ ?section 
                                ?*instruction-sections*)) then
               (decode-bits ?value
                            (nth$ (+ ?ind 1)
                                  ?*instruction-sections*)
                            (nth$ (+ ?ind 2)
                                  ?*instruction-sections*))))

(deffunction iris2::encode-instruction
             (?section ?value ?add)
             (if (bind ?ind
                       (member$ ?section 
                                ?*instruction-sections*)) then
               (encode-bits ?value
                            ?add
                            (nth$ (+ ?ind 1)
                                  ?*instruction-sections*)
                            (nth$ (+ ?ind 2)
                                  ?*instruction-sections*))))

(deffunction iris2::int->symbol
             (?index ?collection)
             (nth$ (+ ?index 1)
                   ?collection))
(deffunction iris2::translate-group
             (?value)
             (int->symbol ?value
                          ?*groups*))

(deffunction iris2::decode-and-translate
             (?section ?value)
             (funcall (sym-cat translate- 
                               ?section)
                      (decode-instruction ?section
                                          ?value)))



(defclass iris2::instruction
  (is-a thing)
  (slot raw-value
        (type INTEGER)
        (default ?NONE))
  (slot group
        (default-dynamic FALSE))
  (multislot operation)
  (multislot destination)
  (multislot source0)
  (multislot source1)
  (message-handler init after))

(defmessage-handler iris2::instruction init after
                    ()
                    (bind ?self:group
                          (translate-group (decode-instruction group
                                                               ?self:raw-value))))
(deffunction iris2::push-value
             (?memory ?sp ?value)
             (memory-store ?memory
                           (bind ?next
                                 (+ ?sp 1))
                           ?value)
             ?next)
(deffunction iris2::pop-value
             (?memory ?sp)
             (create$ (- ?sp 1)
                      (memory-load ?memory
                                   ?sp)))


(defmodule iris2-decode
           (import cortex
                   ?ALL)
           (import ucode
                   ?ALL)
           (import iris2
                   ?ALL))
(defrule iris2-decode::retrieve-instruction
         (object (is-a core)
                 (register ?registers)
                 (code ?c)
                 (name ?parent))
         (object (is-a memory-space)
                 (name ?c)
                 (raw-pointer ?code))
         (object (is-a memory-space)
                 (name ?registers)
                 (raw-pointer ?regs))
         =>
         (bind ?this 
               (make-instance of instruction
                              (parent ?parent)
                              (raw-value (bind ?rvalue
                                               (memory-load ?code
                                                            (memory-load ?regs
                                                                         0))))))
         (assert (decode ?this
                         operation 
                         (decode-instruction operation
                                             ?rvalue))
                 (decode ?this
                         destination
                         (decode-instruction destination
                                             ?rvalue))
                 (decode ?this
                         source0
                         (decode-instruction source0
                                             ?rvalue))
                 (decode ?this
                         source1
                         (decode-instruction source1
                                             ?rvalue))))


(defrule iris2-decode::decode-operation:jump
         ?f <- (decode ?inst
                       operation
                       ?value)
         (object (is-a instruction)
                 (name ?inst)
                 (group jump))
         =>
         (retract ?f)
         (modify-instance ?inst
                          (operation (bool (decode-bits ?value
                                                        (binary->int 0b00001)
                                                        0))
                                     (bool (decode-bits ?value
                                                        (binary->int 0b00010)
                                                        1))
                                     (bool (decode-bits ?value
                                                        (binary->int 0b00100)
                                                        2))
                                     (bool (decode-bits ?value
                                                        (binary->int 0b01000)
                                                        3))
                                     (bool (decode-bits ?value
                                                        (binary->int 0b10000)
                                                        4)))))
(defrule iris2-decode::decode-operation:other
         ?f <- (decode ?inst
                       operation
                       ?value)
         (object (is-a instruction)
                 (name ?inst)
                 (group ~jump))
         =>
         (retract ?f)
         (modify-instance ?inst
                          (operation (decode-bits ?value
                                                  (binary->int 0b11111)
                                                  2))))

(defrule iris2-decode::decode-
