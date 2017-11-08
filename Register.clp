; syn
; Copyright (c) 2013-2017, Joshua Scoggins and Contributors
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; Register.clp - wrapper class for the register concept
(defmessage-handler PRIMITIVE get-value primary
                    ()
                    ?self)

(defmethod decode-bits
  ((?value INTEGER)
   (?mask INTEGER))
  (decode-bits ?value
               ?mask
               0))

(defmethod encode-bits
  ((?value INTEGER)
   (?insert INTEGER)
   (?mask INTEGER))
  (encode-bits ?value
               ?insert
               ?mask
               0))

(defclass MAIN::register
  (is-a USER)
  (slot mask
        (type INTEGER)
        (storage local)
        (visibility public)
        (default-dynamic -1))
  (slot value
        (type INTEGER)
        (storage local)
        (visibility public)
        (default-dynamic 0))
  (message-handler increment primary)
  (message-handler put-mask after)
  (message-handler put-value around)
  (message-handler decode primary)
  (message-handler encode primary))
(defmessage-handler MAIN::register increment primary
                    ()
                    (send ?self
                          put-value
                          (+ (dynamic-get value)
                             1)))

(defmessage-handler MAIN::register put-value around
                    "Mask the value before storing it!"
                    (?value)
                    (override-next-handler (decode-bits ?value
                                                        (dynamic-get mask)
                                                        0)))


(defmessage-handler MAIN::register put-mask after
                    "update the value after updating the mask!"
                    (?mask)
                    ; when we initialize this class, the slot for value is nil
                    ; so we have to make sure that we've initialized correctly before hand!
                    (bind ?old-value
                          (dynamic-get value))
                    (if (integerp ?old-value) then
                      (dynamic-put value
                                   (decode-bits ?old-value
                                                ?mask))))

(defmessage-handler MAIN::register decode primary
                    (?mask $?shift)
                    (decode-bits (dynamic-get value)
                                 ?mask
                                 (expand$ (first$ ?shift))))

(defmessage-handler MAIN::register encode primary
                    (?new-value ?mask $?shift)
                    (decode-bits (dynamic-get value)
                                 ?new-value
                                 ?mask
                                 (expand$ (first$ ?shift))))
(defmethod MAIN::move
  ((?src register)
   (?dest register
          (neq ?src
               ?current-argument)))
  (send ?dest
        put-value
        (send ?src
              get-value)))

(defmethod MAIN::move
  ((?src register)
   (?dest register
          (eq ?src
              ?current-argument))))

(defmethod MAIN::move
  ((?src register)
   (?dest EXTERNAL-ADDRESS)
   (?address INTEGER))
  (call ?dest
        write
        ?address
        (send ?src
              get-value)))

(defmethod MAIN::move
  ((?src EXTERNAL-ADDRESS)
   (?address INTEGER)
   (?dest register))
  (send ?dest
        put-value
        (call ?src
              read
              ?address)))

(defmethod MAIN::move
  ((?src register)
   (?dest device)
   (?address INTEGER))
  (send ?dest
        write
        ?address
        (send ?src
              get-value)))

(defmethod MAIN::move
  ((?src device)
   (?address INTEGER)
   (?dest register))
  (send ?dest
        put-value
        (send ?src
              read
              ?address)))

(defmethod MAIN::move
  ((?src register)
   (?src-address INTEGER)
   (?dest EXTERNAL-ADDRESS
          device)
   (?dest-address INTEGER))
  (move ?src
        ?dest
        ?dest-address))

(defmethod MAIN::move
  ((?src EXTERNAL-ADDRESS
         device)
   (?src-address INTEGER)
   (?dest register)
   (?dest-address INTEGER))
  (move ?src
        ?src-address
        ?dest))

(defmethod MAIN::swap
  "Swap data between a memory block and a register"
  ((?src register)
   (?dest EXTERNAL-ADDRESS)
   (?dest-address INTEGER))
  (bind ?value
        (call ?dest
              read
              ?dest-address))
  (call ?dest
        write
        ?dest-address
        (send ?src
              get-value))
  (send ?src
        put-value
        ?value))

(defmethod MAIN::swap
  "Swap data between a memory block and a register"
  ((?src register)
   (?dest device)
   (?dest-address INTEGER))
  (bind ?value
        (send ?dest
              read
              ?dest-address))
  (send ?dest
        write
        ?dest-address
        (send ?src
              get-value))
  (send ?src
        put-value
        ?value))

(defmethod MAIN::swap
  ((?src register)
   (?dest register
          (neq ?src
               ?current-argument)))
  (bind ?k
        (send ?src
              get-value))
  (send ?src
        put-value
        (send ?dest
              get-value))
  (send ?dest
        put-value
        ?k))

(defmethod MAIN::swap
  ((?src register)
   (?dest register
          (eq ?src
              ?current-argument))))

(defmethod MAIN::swap
  "Swap data between a memory block and a register"
  ((?src register)
   (?src-address INTEGER) ; ignore this
   (?dest EXTERNAL-ADDRESS
          device)
   (?dest-address INTEGER))
  (swap ?src
        ?dest
        ?dest-address))

(defmethod MAIN::swap
  "Swap data between a memory block and a register"
  ((?dest EXTERNAL-ADDRESS
          device)
   (?dest-address INTEGER) ; ignore this
   (?src register)
   (?src-address INTEGER))
  ; reverse the arguments and call it as we expect it
  (swap ?src
        ?src-address
        ?dest
        ?dest-address))

(defmethod MAIN::swap
  ((?src register)
   (?src-address INTEGER)
   (?dest register)
   (?dest-address INTEGER))
  (swap ?src
        ?dest))

(defmethod +
  ((?a register)
   (?b register))
  (+ (send ?a
           get-value)
     (send ?b
           get-value)))
(defmethod +
  ((?a register)
   (?b NUMBER))
  (+ (send ?a
           get-value)
     ?b))
(defmethod +
  ((?a NUMBER)
   (?b register))
  (+ ?a
     (send ?b
           get-value)))

(defmethod -
  ((?a register)
   (?b register))
  (- (send ?a
           get-value)
     (send ?b
           get-value)))
(defmethod -
  ((?a register)
   (?b NUMBER))
  (- (send ?a
           get-value)
     ?b))
(defmethod -
  ((?a NUMBER)
   (?b register))
  (- ?a
     (send ?b
           get-value)))

(defmethod *
  ((?a register)
   (?b register))
  (* (send ?a
           get-value)
     (send ?b
           get-value)))

(defmethod *
  ((?a register)
   (?b NUMBER))
  (* (send ?a
           get-value)
     ?b))

(defmethod *
  ((?a NUMBER)
   (?b register))
  (* ?a
     (send ?b
           get-value)))


(defmethod /
  ((?a register)
   (?b register))
  (/ (send ?a
           get-value)
     (send ?b
           get-value)))

(defmethod /
  ((?a register)
   (?b NUMBER))
  (/ (send ?a
           get-value)
     ?b))

(defmethod /
  ((?a NUMBER)
   (?b register))
  (/ ?a
     (send ?b
           get-value)))

(defmethod div
  ((?a register)
   (?b register))
  (div (send ?a
             get-value)
       (send ?b
             get-value)))

(defmethod div
  ((?a register)
   (?b NUMBER))
  (div (send ?a
             get-value)
       ?b))

(defmethod div
  ((?a NUMBER)
   (?b register))
  (div ?a
       (send ?b
             get-value)))

(defmethod mod
  ((?a register)
   (?b register))
  (mod (send ?a
             get-value)
       (send ?b
             get-value)))

(defmethod mod
  ((?a register)
   (?b NUMBER))
  (mod (send ?a
             get-value)
       ?b))

(defmethod mod
  ((?a NUMBER)
   (?b register))
  (mod ?a
       (send ?b
             get-value)))

(defmethod left-shift
  ((?a register)
   (?b INTEGER))
  (left-shift (send ?a
                    get-value)
              ?b))
(defmethod left-shift
  ((?a INTEGER)
   (?b register))
  (left-shift ?a
              (send ?b
                    get-value)))
(defmethod left-shift
  ((?a register)
   (?b register))
  (left-shift (send ?a
                    get-value)
              (send ?b
                    get-value)))

(defmethod right-shift
  ((?a register)
   (?b INTEGER))
  (right-shift (send ?a
                     get-value)
               ?b))
(defmethod right-shift
  ((?a INTEGER)
   (?b register))
  (right-shift ?a
               (send ?b
                     get-value)))
(defmethod right-shift
  ((?a register)
   (?b register))
  (right-shift (send ?a
                     get-value)
               (send ?b
                     get-value)))

(defmethod circular-shift-left
  ((?a register)
   (?b INTEGER))
  (circular-shift-left (send ?a
                             get-value)
                       ?b))
(defmethod circular-shift-left
  ((?a INTEGER)
   (?b register))
  (circular-shift-left ?a
                       (send ?b
                             get-value)))
(defmethod circular-shift-left
  ((?a register)
   (?b register))
  (circular-shift-left (send ?a
                             get-value)
                       (send ?b
                             get-value)))

(defmethod circular-shift-right
  ((?a register)
   (?b INTEGER))
  (circular-shift-right (send ?a
                              get-value)
                        ?b))
(defmethod circular-shift-right
  ((?a INTEGER)
   (?b register))
  (circular-shift-right ?a
                        (send ?b
                              get-value)))
(defmethod circular-shift-right
  ((?a register)
   (?b register))
  (circular-shift-right (send ?a
                              get-value)
                        (send ?b
                              get-value)))

(defmethod binary-nand
  ((?a register)
   (?b INTEGER))
  (binary-nand (send ?a
                     get-value)
               ?b))
(defmethod binary-nand
  ((?a INTEGER)
   (?b register))
  (binary-nand ?a
               (send ?b
                     get-value)))
(defmethod binary-nand
  ((?a register)
   (?b register))
  (binary-nand (send ?a
                     get-value)
               (send ?b
                     get-value)))
(defmethod binary-and
  ((?a register)
   (?b INTEGER))
  (binary-and (send ?a
                    get-value)
              ?b))
(defmethod binary-and
  ((?a INTEGER)
   (?b register))
  (binary-and ?a
              (send ?b
                    get-value)))
(defmethod binary-and
  ((?a register)
   (?b register))
  (binary-and (send ?a
                    get-value)
              (send ?b
                    get-value)))
(defmethod binary-xor
  ((?a register)
   (?b INTEGER))
  (binary-xor (send ?a
                    get-value)
              ?b))
(defmethod binary-xor
  ((?a INTEGER)
   (?b register))
  (binary-xor ?a
              (send ?b
                    get-value)))
(defmethod binary-xor
  ((?a register)
   (?b register))
  (binary-xor (send ?a
                    get-value)
              (send ?b
                    get-value)))
(defmethod binary-nor
  ((?a register)
   (?b INTEGER))
  (binary-nor (send ?a
                    get-value)
              ?b))
(defmethod binary-nor
  ((?a INTEGER)
   (?b register))
  (binary-nor ?a
              (send ?b
                    get-value)))
(defmethod binary-nor
  ((?a register)
   (?b register))
  (binary-nor (send ?a
                    get-value)
              (send ?b
                    get-value)))
(defmethod binary-or
  ((?a register)
   (?b INTEGER))
  (binary-or (send ?a
                   get-value)
             ?b))
(defmethod binary-or
  ((?a INTEGER)
   (?b register))
  (binary-or ?a
             (send ?b
                   get-value)))
(defmethod binary-or
  ((?a register)
   (?b register))
  (binary-or (send ?a
                   get-value)
             (send ?b
                   get-value)))

(defmethod binary-not
  ((?a register))
  (binary-not (send ?a
                    get-value)))

(defmethod upper-half
  ((?a register))
  (upper-half (send ?a
                    get-value)))
(defmethod lower-half
  ((?a register))
  (lower-half (send ?a
                    get-value)))

(defmethod ones-complement
  ((?a register))
  (ones-complement (send ?a
                         get-value)))
(defmethod twos-complement
  ((?a register))
  (twos-complement (send ?a
                         get-value)))

(defmethod multiply-add
  ((?a register)
   (?b register
       INTEGER)
   (?c register
       INTEGER))
  (multiply-add (send ?a
                      get-value)
                (send ?b
                      get-value)
                (send ?c
                      get-value)))


(defmethod break-apart-number
  ((?a register))
  (break-apart-number (send ?a
                            get-value)))


(defmethod decode-bits
  ((?value register)
   (?mask register
          INTEGER)
   (?shift register
           INTEGER))
  (decode-bits (send ?value
                     get-value)
               (send ?mask
                     get-value)
               (send ?shift
                     get-value)))

(defmethod decode-bits
  ((?value register)
   (?mask register
          INTEGER))
  (decode-bits (send ?value
                     get-value)
               (send ?mask
                     get-value)))


(defmethod encode-bits
  ((?value register)
   (?insert register
            INTEGER)
   (?mask register
          INTEGER)
   (?shift register
           INTEGER))
  (encode-bits (send ?value
                     get-value)
               (send ?insert
                     get-value)
               (send ?mask
                     get-value)
               (send ?shift
                     get-value)))
(defmethod encode-bits
  ((?value register)
   (?insert register
            INTEGER)
   (?mask register
          INTEGER))
  (encode-bits (send ?value
                     get-value)
               (send ?insert
                     get-value)
               (send ?mask
                     get-value)))

(defmethod min
  ((?a register)
   (?b register))
  (min (send ?a
             get-value)
       (send ?b
             get-value)))
(defmethod min
  ((?a INTEGER)
   (?b register))
  (min ?a
       (send ?b
             get-value)))

(defmethod min
  ((?a register)
   (?b INTEGER))
  (min (send ?a
             get-value)
       ?b))

(defmethod max
  ((?a register)
   (?b register))
  (max (send ?a
             get-value)
       (send ?b
             get-value)))
(defmethod max
  ((?a INTEGER)
   (?b register))
  (max ?a
       (send ?b
             get-value)))

(defmethod max
  ((?a register)
   (?b INTEGER))
  (max (send ?a
             get-value)
       ?b))

(defmethod MAIN::zero
  ((?a register))
  (send ?a
        put-value
        0))

(defmethod MAIN::load-value
  ((?memory EXTERNAL-ADDRESS
            device)
   (?address register))
  (load-value ?memory
              (send ?address
                    get-value)))

(defmethod MAIN::store-value
  ((?memory EXTERNAL-ADDRESS
            device)
   (?address register)
   (?value register))
  (store-value ?memory
    (send ?address
          get-value)
    (send ?value
          get-value)))

(defmethod MAIN::store-value
  ((?memory EXTERNAL-ADDRESS
            device)
   (?address register)
   (?value INTEGER))
  (store-value ?memory
    (send ?address
          get-value)
    ?value))

(defmethod MAIN::store-value
  ((?memory EXTERNAL-ADDRESS
            device)
   (?address INTEGER)
   (?value register))
  (store-value ?memory
    ?address
    (send ?value
          get-value)))

(defgeneric MAIN::store-value-and-increment)
(defgeneric MAIN::store-value-and-decrement)
(defgeneric MAIN::load-value-and-increment)
(defgeneric MAIN::load-value-and-decrement)

(defmethod MAIN::store-value-and-increment
  ((?memory EXTERNAL-ADDRESS
            device)
   (?address register)
   (?value INTEGER
           register))
  (store-value ?memory
    ?address
    ?value)
  (send ?address
        increment))

(defmethod MAIN::store-value-and-decrement
  ((?memory EXTERNAL-ADDRESS
            device)
   (?address register)
   (?value INTEGER
           register))
  (store-value ?memory
    ?address
    ?value)
  (send ?address
        decrement))

(defmethod MAIN::load-value-and-increment
  ((?memory EXTERNAL-ADDRESS
            device)
   (?address register)
   (?value INTEGER
           register))
  (bind ?x
        (load-value ?memory
                    ?address
                    ?value))
  (send ?address
        increment)
  ?x)

(defmethod MAIN::load-value-and-decrement
  ((?memory EXTERNAL-ADDRESS
            device)
   (?address register)
   (?value INTEGER
           register))
  (bind ?x
        (load-value ?memory
                    ?address
                    ?value))
  (send ?address
        decrement)
  ?x)
