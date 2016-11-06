(defmodule parsing
           (import cortex
                   ?ALL)
           (export ?ALL))

(defgeneric parsing::registerp
            "Is the given symbol a register?")
(defgeneric parsing::register-to-index
            "convert the given register to an index value")
(defgeneric parsing::index-to-register
            "convert the given index to a register symbol")
(defmethod parsing::registerp
  ((?sym SYMBOL))
  (and (has-prefix ?sym r)
       (integerp 
         (string-to-field
           (sub-string 2 (length$ ?sym)
                       ?sym)))))

(defmethod parsing::register-to-index
  ((?sym SYMBOL
         (registerp ?sym)))
  (string-to-field 
    (sub-string 2 (length$ ?sym)
                ?sym)))

(defmethod parsing::index-to-register
  ((?index INTEGER
           (>= ?index 0)))
  (sym-cat r 
           ?index))
