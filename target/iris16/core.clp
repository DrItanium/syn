
(defmodule iris16
           (import cortex
                   ?ALL)
           (import ucode
                   ?ALL)
           (export defgeneric 
                   encode-instruction
                   decode-instruction
                   new-core)
           (export defclass
                   core))
(defgeneric iris16::init)

(defglobal iris16
           ?*init-called* = FALSE
           ?*stack-pointer* = 253
           ?*link-register* = 254
           ?*instruction-pointer* = 255)

(defmethod iris16::init
  "microcode startup"
  ()
  (if (not ?*init-called*) then
    (bind ?*init-called*
          TRUE)
    (init arithmetic)))

(defclass iris16::core
  (is-a thing)
  (slot registers
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot data
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot code
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot stack
        (type EXTERNAL-ADDRESS
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local))
  (slot data-capacity
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot code-capacity
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot stack-capacity
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot advance-ip
        (type SYMBOL)
        (allowed-symbols TRUE
                        FALSE)
        (visibility private)
        (storage local)
        (default-dynamic TRUE))
  (message-handler init after)
  (message-handler init before)
  (message-handler set-code primary)
  (message-handler set-data primary)
  (message-handler set-stack primary)
  (message-handler dump-image primary)
  (message-handler run primary))
(defmessage-handler iris16::core set-code primary
                    (?address ?value)
                    (memory-store ?self:code
                                  ?address
                                  ?value))
(defmessage-handler iris16::core set-data primary
                    (?address ?value)
                    (memory-store ?self:data
                                  ?address 
                                  ?value))
(defmessage-handler iris16::core set-stack primary
                    (?address ?value)
                    (memory-store ?self:stack
                                  ?address
                                  ?value))

(defmessage-handler iris16::core push-value primary
                    (?value)
                    (memory-increment ?self:registers
                                      ?*stack-pointer*)
                    (memory-store ?self:stack
                                  (memory-load ?self:registers
                                               ?*stack-pointer*)
                                  ?value))

                   
                    
 

(defmessage-handler iris16::core init after
                    ()
                    (memory-zero 
                      (bind ?self:registers
                            (alloc word16u 
                                   256)))
                    ; setup the stack pointer
                    (memory-store ?self:registers
                                  ?*stack-pointer*
                                  (hex->int 0xFFFF))
                    (memory-zero
                      (bind ?self:data
                            (alloc word16u
                                   ?self:data-capacity)))
                    (memory-zero
                      (bind ?self:stack
                            (alloc word16u
                                   ?self:stack-capacity)))
                    (memory-zero
                      (bind ?self:code
                            (alloc word32u
                                   ?self:code-capacity))))

(defmessage-handler iris16::core init before
                    "Make sure that the backing code has been initialized before initializing"
                    () 
                    (init))

(defgeneric iris16::new-core)

(defmethod iris16::new-core
  ((?data-cap INTEGER)
   (?code-cap INTEGER)
   (?stack-cap INTEGER))
  (make-instance of core
                 (parent FALSE)
                 (data-capacity ?data-cap)
                 (code-capacity ?code-cap)
                 (stack-capacity ?stack-cap)))
(defmethod iris16::new-core
  ()
  (new-core (hex->int 0xFFFF)
            (hex->int 0xFFFF)
            (hex->int 0xFFFF)))
; instruction decoding routines
(deffunction iris16::decode-group
             (?value)
             (decode-bits ?value 
                          (hex->int 0x00000007) 
                          0))
(deffunction iris16::decode-operation
             (?value)
             (decode-bits ?value
                          (hex->int 0x000000F8)
                          3))
(deffunction iris16::decode-immediate
             (?value)
             (decode-bits ?value
                          (hex->int 0xFFFF0000)
                          16))
(deffunction iris16::decode-half-immediate
             (?value)
             (decode-bits ?value
                          (hex->int 0xFF000000)
                          24))
(deffunction iris16::decode-destination
             (?value)
             (decode-bits ?value
                          (hex->int 0x0000FF00)
                          8))
(deffunction iris16::decode-source0
             (?value)
             (decode-bits ?value
                          (hex->int 0x00FF0000)
                          16))
(deffunction iris16::decode-source1
             (?value)
             (decode-bits ?value
                          (hex->int 0xFF000000)
                          24))

(deffunction iris16::decode-control
             (?value)
             (decode-bits ?value
                          (hex->int 0x000000FF)
                          0))
(deffunction iris16::encode-control
             (?value ?control)
             (encode-bits ?value
                          ?control
                          (hex->int 0x000000FF)
                          0))
(deffunction iris16::encode-half-immediate
             (?value ?immediate)
             (encode-bits ?value
                          ?immediate
                          (hex->int 0xFF000000)
                          24))
(deffunction iris16::encode-immediate
             (?value ?immediate)
             (encode-bits ?value
                          ?immediate
                          (hex->int 0xFFFF0000)
                          16))
(deffunction iris16::encode-group
             (?value ?group)
             (encode-bits ?value
                          ?group
                          (hex->int 0x00000007)
                          0))

(deffunction iris16::encode-destination
             (?value ?destination)
             (encode-bits ?value
                          ?destination
                          (hex->int 0x0000FF00)
                          8))

(deffunction iris16::encode-source0
             (?value ?source0)
             (encode-bits ?value
                          ?source0
                          (hex->int 0x00FF0000)
                          16))
(deffunction iris16::encode-source1
             (?value ?source1)
             (encode-bits ?value
                          ?source1
                          (hex->int 0xFF000000)
                          24))
(deffunction iris16::encode-operation
             (?value ?op)
             (encode-bits ?value
                          ?op
                          (hex->int 0x000000F8)
                          3))
(deffunction iris16::decode-upper8
             (?value)
             (decode-bits ?value
                          (hex->int 0x0000FF00)
                          8))
(deffunction iris16::decode-lower8
             (?value)
             (decode-bits ?value
                          (hex->int 0x000000FF)
                          0))
(deffunction iris16::dump16
             (?router ?mem ?capacity)
             (progn$ (?curr ?capacity)
                     (bind ?current
                           (memory-load ?mem
                                        ?curr))
                     (put-char ?router
                               (decode-lower8 ?current))
                     (put-char ?router
                               (decode-upper8 ?current))))
(deffunction iris16::dump32
             (?router ?mem ?capacity)
             (progn$ (?curr ?capacity)
                     (bind ?current 
                           (memory-load ?mem
                                        ?curr))
                     (put-char ?router
                               (decode-bits ?current
                                            (hex->int 0x000000FF)
                                            0))
                     (put-char ?router
                               (decode-bits ?current
                                            (hex->int 0x0000FF00)
                                            8))
                     (put-char ?router
                               (decode-bits ?current
                                            (hex->int 0x00FF0000)
                                            16))
                     (put-char ?router
                               (decode-bits ?current
                                            (hex->int 0xFF000000)
                                            24))))
(defmessage-handler iris16::core dump-image primary
                    (?router)
                    (dump16 ?router
                            ?self:registers
                            256)
                    (dump32 ?router
                            ?self:code
                            ?self:code-capacity)
                    (dump16 ?router
                            ?self:data
                            ?self:data-capacity)
                    (dump16 ?router
                            ?self:stack
                            ?self:stack-capacity))
(defgeneric iris16::encode-instruction)
(defgeneric iris16::decode-instruction)
(defmethod iris16::decode-instruction
  ((?value INTEGER))
  (bind ?casted 
        (cast word32u
              ?value))
  (create$ (decode-group ?casted)
           (decode-operation ?casted)
           (decode-destination ?casted)
           (decode-source0 ?casted)
           (decode-source1 ?casted)
           (decode-immediate ?casted)))
(defmethod iris16::encode-instruction
  ((?group INTEGER)
   (?op INTEGER)
   (?destination INTEGER)
   (?source0 INTEGER)
   (?source1 INTEGER))
  (encode-source1
    (encode-source0
      (encode-destination 
        (encode-operation
          (encode-group 0 ?group)
          ?op)
        ?destination)
      ?source0)
    ?source1))
(defmethod iris16::encode-instruction
  ((?group INTEGER)
   (?op INTEGER)
   (?destination INTEGER)
   (?immediate INTEGER))
  (encode-instruction ?group
                      ?op
                      ?destination
                      (decode-lower8 ?immediate)
                      (decode-upper8 ?immediate)))
(deffunction iris16::register-binary-operation
             (?register-file ?instruction ?op)
             (memory-load-binary-op-store ?register-file
                                          (decode-destination ?instruction)
                                          (decode-source0 ?instruction)
                                          (decode-source1 ?instruction)
                                          ?op))
(deffunction iris16::memory-load-op-store-with-offset
             (?register-file ?instruction ?op)
             (memory-store ?register-file
                           (decode-destination ?instruction)
                           (funcall ?op
                                    (memory-load ?register-file
                                                 (decode-source0 ?instruction))
                                    (decode-half-immediate ?instruction))))
(defglobal iris16
           ?*dual-arithmetic-operations* = (create$ add-word16u
                                                    sub-word16u
                                                    mul-word16u
                                                    div-word16u
                                                    rem-word16u
                                                    left-shift
                                                    right-shift)
           ?*binary-manipulation-operations* = (create$ binary-and
                                                        binary-op
                                                        binary-not
                                                        binary-xor)
           ?*arithmetic-operations* = (create$))
(defmethod iris16::init
  ((?class SYMBOL
           (eq ?class
               arithmetic)))
  (bind ?immediate-forms
        (create$))
  (bind ?arithmetics
        (create$))
  (progn$ (?op ?*dual-arithmetic-operations*)
          ; build immediate operations
          (build (format nil
                         "(deffunction iris16::%s-with-offset
                                       (?r ?i)
                                       (memory-load-op-store-with-offset ?r 
                                                                         ?i 
                                                                         %s))"
                         ?op 
                         ?op))
          (bind ?immediate-forms
                ?immediate-forms
                (sym-cat ?op
                         -with-offset))
          ; build basic operations
          (build (format nil
                         "(deffunction iris16::call-%s
                                       (?r ?i)
                                       (register-binary-operation ?r 
                                                                  ?i 
                                                                  %s))"
                         ?op
                         ?op))
                (bind ?arithmetics
                      ?arithmetics
                      (sym-cat call- ?op)))
          (bind ?*arithmetic-operations*
                ?arithmetics
                ?*binary-manipulation-operations*
                ?immediate-forms))




(defmethod iris16::binary-not
  ((?first INTEGER)
   (?unused INTEGER))
  (binary-not ?first))


(deffunction iris16::arithmetic-operation
             (?register-file ?instruction)
             (funcall (nth$ (decode-operation ?instruction)
                            ?*arithmetic-operations*)
                      ?register-file
                      ?instruction))
(deffunction iris16::get-register
             (?register ?index)
             (memory-load ?register
                          ?index))
(deffunction iris16::set-register
             (?register ?index ?value)
             (memory-store ?register
                           ?index
                           ?value))
(deffunction iris16::swap-registers
             (?register ?a ?b)
             (memory-swap ?register
                          ?a 
                          ?b))


(deffunction iris16::set-destination-register
             (?registers ?instruction ?value)
             (set-register ?registers
                           (decode-destination ?instruction)
                           ?value))
(deffunction iris16::get-source0-register
             (?registers ?instruction)
             (get-register ?registers
                           (decode-source0 ?instruction)))

(deffunction iris16::load-generic
             (?registers ?data ?instruction ?address)
             (set-register ?registers
                           (decode-destination ?instruction)
                           (memory-load ?data
                                        ?address)))
(deffunction iris16::get-stack-pointer
             (?register-file)
             (get-register ?register-file
                           ?*stack-pointer*))

(deffunction iris16::push-generic
             (?registers ?stack ?value)
             ; two operations in one, add followed by a store
             (memory-increment ?registers
                               ?*stack-pointer*)
             (memory-store ?stack
                           (get-stack-pointer ?registers)
                           ?value))

(deffunction iris16::store-generic
             (?registers ?data ?instruction ?value)
             (memory-store ?data
                           (get-register ?registers
                                         (decode-destination  ?instruction))
                           ?value))


(defglobal iris16
           ?*move-operations* = (create$ move-operation
                                         set-immediate 
                                         swap-operation
                                         load-operation
                                         load-immediate-operation
                                         store-operation
                                         store-immediate-operation
                                         push-operation
                                         push-immediate-operation
                                         pop-operation
                                         load-code
                                         store-code))

(deffunction iris16::move-group-operation
             (?registers ?code ?data ?stack ?instruction)
             (switch (nth$ (decode-operation ?instruction)
                           ?*move-operations*)
                     (case move-operation then
                       (set-destination-register ?registers
                                                 ?instruction
                                                 (get-source0-register ?registers
                                                                       ?instruction)))
                     (case set-immediate then
                       (set-destination-register ?registers
                                                 ?instruction
                                                 (decode-immediate ?instruction)))
                     (case swap-operation then
                       (swap-registers ?registers
                                       (decode-destination ?instruction)
                                       (decode-source0 ?instruction)))
                     (case load-operation then
                       (load-generic ?registers
                                     ?data
                                     ?instruction
                                     (get-source0-register ?registers
                                                           ?instruction)))
                     (case load-immediate-operation then
                       (load-generic ?registers
                                     ?data 
                                     ?instruction
                                     (decode-immediate ?instruction)))
                     (case store-operation then
                       (store-generic ?registers
                                      ?data 
                                      ?instruction
                                      (get-source0-register ?registers
                                                            ?instruction)))

                     (case store-immediate-operation then
                       (store-generic ?registers
                                      ?data
                                      ?instruction
                                      (decode-immediate ?instruction)))
                     (case push-operation then
                       (push-generic ?registers
                                     ?stack
                                     (get-register ?registers
                                                   (decode-destination ?instruction))))
                     (case push-immediate-operation then
                       (push-generic ?registers
                                     ?stack
                                     (decode-immediate ?instruction)))
                     (case pop-operation then
                       (set-destination-register ?registers
                                                 ?instruction
                                                 (memory-load ?stack
                                                              (get-stack-pointer ?registers)))
                       (memory-decrement ?registers
                                         ?*stack-pointer*))
                     (case load-code then
                       (bind ?value
                             (memory-load ?code
                                          (get-register ?registers
                                                        (decode-destination ?instruction))))
                       (set-register ?registers
                                     (decode-source0 ?instruction)
                                     (word32u-lower-half ?value))
                       (set-register ?registers
                                     (decode-source1 ?instruction)
                                     (word32u-upper-half ?value)))
                     (case store-code then
                       (memory-load-binary-op-store ?code
                                                    (get-register ?registers
                                                                  (decode-destination ?instruction))
                                                    ?registers
                                                    (decode-source0 ?instruction)
                                                    ?registers
                                                    (decode-source1 ?instruction)
                                                    make-word32u))
                     (default (printout werror "Illegal move group operation " (decode-operation ?instruction) crlf))))


(deffunction iris16::set-instruction-pointer
             (?registers ?value)
             (set-register ?registers
                           ?*instruction-pointer*
                           ?value))

(deffunction iris16::get-instruction-pointer
             (?registers)
             (get-register ?registers
                           ?*instruction-pointer*))
(deffunction iris16::jump-operation
             (?registers ?instruction)
             ; the operation field has a total of 5 bits which we use to determine what kind of operation we are looking at
             ; 0 - if then else
             ; 1 - conditional
             ; 2 - if-false
             ; 3 - immediate
             ; 4 - link
             (bind ?ifthenelse
                   (bool (decode-bits ?instruction
                                      (binary->int 0b00001)
                                      0)))
             (bind ?conditional
                   (bool (decode-bits ?instruction
                                      (binary->int 0b00010)
                                      1)))
             (bind ?iffalse
                   (bool (decode-bits ?instruction
                                      (binary->int 0b00100)
                                      2)))
             (bind ?immediate
                   (bool (decode-bits ?instruction
                                      (binary->int 0b01000)
                                      3)))
             (bind ?link
                   (bool (decode-bits ?instruction
                                      (binary->int 0b10000)
                                      4)))
             (bind ?ip 
                   (get-instruction-pointer ?registers))
             (bind ?cond 
                   TRUE)
             (set-instruction-pointer ?registers
                                      (if ?conditional then
                                        (bind ?dest
                                              (get-register ?registers
                                                            (decode-destination ?instruction)))
                                        (bind ?cond 
                                              (if ?iffalse then
                                                (= ?dest 0)
                                                else
                                                (<> ?dest 0)))
                                        (if ?ifthenelse then
                                          (get-register ?registers
                                                        (if ?cond then 
                                                          (decode-source0 ?instruction)
                                                          else
                                                          (decode-source1 ?instruction)))
                                          else
                                          (if ?cond then
                                            (if ?immediate then
                                              (decode-immediate ?instruction)
                                              else
                                              (get-register ?registers
                                                            (decode-source0 ?instruction)))
                                            else
                                            (+ ?ip 
                                               1)))
                                        else
                                        (if ?immediate then 
                                          (decode-immediate ?instruction)
                                          else
                                          (get-register ?registers
                                                        (decode-destination ?instruction)))))
             (if (and ?link 
                      ?cond) then
               (set-register ?registers
                             ?*link-register*
                             (+ ?ip 1))))

(defglobal iris16
           ?*compare-operations* = (create$ =
                                            <>
                                            <
                                            <=
                                            >=))
(deffunction iris16::compare-operation
             (?registers ?instruction)
             (set-destination-register ?registers
                                       ?instruction
                                       (funcall (nth$ (decode-bits (decode-operation ?instruction)
                                                                   (binary->int 0b01110)
                                                                   1)
                                                      ?*compare-operations*)
                                                (get-source0-register ?registers
                                                                      ?instruction)
                                                (if (bool (decode-bits (decode-operation ?instruction)
                                                                       (binary->int 0b00001)
                                                                       0)) then
                                                  (decode-half-immediate ?instruction)
                                                  else
                                                  (get-register ?registers
                                                                (decode-source1 ?instruction))))))

(deffunction iris16::misc-operation
             (?registers ?instruction)
             (if (= (decode-operation ?instruction) 0) then
               (switch (decode-destination ?instruction)
                       (case 0 then (halt))
                       (case 1 then 
                         (set-register ?registers
                                       (decode-source0 ?instruction)
                                       (cast word16u 
                                             (get-char))))
                       (case 2 then 
                         (put-char (get-source0-register ?registers
                                                         ?instruction)))
                       (default (printout werror "Undefined system call " (decode-destination ?instruction) crlf)
                                (halt)))
               else 
               (printout werror "undefined misc operation " (decode-operation ?instruction) crlf)
               (halt)))

(defmessage-handler iris16::core run primary
                    (?cycle-count)
                    (loop-for-count (?c ?cycle-count) do
                                    (bind ?instruction
                                          (memory-load ?self:code
                                                       (get-instruction-pointer ?self:registers)))
                                    (switch (decode-group ?instruction)
                                            (case 0 then 
                                              (arithmetic-operation ?self:registers 
                                                                    ?instruction))
                                            (case 1 then
                                              (move-group-operation ?self:registers
                                                                    ?self:code
                                                                    ?self:data
                                                                    ?self:stack
                                                                    ?instruction))
                                            (case 2 then 
                                              (bind ?self:advance-ip 
                                                    FALSE)
                                              (jump-operation ?self:registers
                                                              ?instruction))
                                            (case 3 then
                                              (compare-operation ?self:registers
                                                                 ?instruction))
                                            (case 4 then
                                              (misc-operation ?self:registers
                                                              ?instruction))
                                            (default (printout werror 
                                                               "Undefined group: " 
                                                               (decode-group ?instruction) crlf)
                                                     (halt)))
                                    (if ?self:advance-ip then
                                      (memory-increment ?self:registers
                                                        ?*instruction-pointer*)
                                      else
                                      (bind ?self:advance-ip
                                            TRUE))))

