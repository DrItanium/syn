(defmodule lisp
           (import iris17 ?ALL))
(defglobal lisp
           ?*tag-orderings* = (create$ integer
                                       list
                                       float
                                       lexeme))
(defgeneric lisp::load-data-object)
(defgeneric lisp::mark)
(defgeneric lisp::use-new-data-object)
(defgeneric lisp::tag-type-check-function)
(defgeneric lisp::marked?)
(defgeneric lisp::integer?)
(defgeneric lisp::list?)
(defgeneric lisp::float?)
(defgeneric lisp::lexeme?)

(defmethod lisp::use-new-data-object
  ($?body)
  (use-register (create$ ?*tag*
                         ?*this*
                         ?*next*)
                $?body))

(defmethod lisp::integer?
  ()
  (tag-type-check-function integerp
                           integer))
(defmethod lisp::list?
  ()
  (tag-type-check-function listp
                           list))
(defmethod lisp::float?
  ()
  (tag-type-check-function floatp
                           float))

(defmethod lisp::lexeme?
  ()
  (tag-type-check-function lexemep
                           lexeme))

(defmethod lisp::tag-type-check-function
  ((?title SYMBOL)
   (?tag SYMBOL
         (member$ ?tag
                  ?*tag-orderings*)))
  (defunc ?title
          (use-register (create$ ?*link-register*
                                 ?*args*
                                 ?*condition-register*)
                        (call-decode-bits ?*tag*
                                          0b00011110
                                          0x1)
                        (op:compare ==
                                    none
                                    immediate
                                    ?*return-register*
                                    (- (nth$ ?tag
                                             ?*tag-orderings*)
                                       1))
                        (op:move ?*return-register*
                                 ?*condition-register*))))




(defmethod lisp::load-data-object
  ()
  (bind ?address
        ?*arg0*)
  (defunc loadDataObject
          (use-address-and-value (op:move ?*address-register*
                                          ?address)
                                 (op:load 0m1111
                                          0x0)
                                 (op:move ?*tag*
                                          ?*value-register*)
                                 (op:load 0m1111
                                          0x2)
                                 (op:move ?*this*
                                          ?*value-register*)
                                 (op:load 0m1111
                                          0x4)
                                 (op:move ?*next*
                                          ?*value-register*))))
(defmethod lisp::marked?
  ()
  (bind ?data
        ?*arg0*)
  (bind ?mask
        ?*arg1*)
  (bind ?shift
        ?*arg2*)
  (defunc markedp
          (use-register (create$ ?*link-register*
                                 ?*condition-register*
                                 ?data
                                 ?mask
                                 ?shift)
                        (call-decode-bits ?*tag*
                                          0x1
                                          0)
                        (op:not-zero? ?*return-register*)
                        (op:move ?*return-register*
                                 ?*condition-register*))))
(defmethod lisp::mark
  ()
  (bind ?input
        ?*arg0*)
  (bind ?value
        ?*arg1*)
  (bind ?mask
        ?*arg2*)
  (bind ?shift
        ?*arg3*)
  (defunc mark
          (use-register (create$ ?*link-register*
                                 ?*args*
                                 ?*return-register*)
                        (op:move ?input
                                 ?*tag*)
                        (op:set-one ?value)
                        (op:move ?mask
                                 ?value)
                        (op:clear ?shift)
                        (call immediate
                              encode_bits)
                        (op:move ?*tag*
                                 ?*return-register*))))

