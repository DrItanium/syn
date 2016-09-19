(defmodule lisp
           (import iris17 ?ALL))
(defglobal lisp
           ?*tag-orderings* = (create$ nil
                                       integer
                                       float
                                       lexeme
                                       list
                                       node))
(defgeneric lisp::mark)
(defgeneric lisp::use-data-object-registers)
(defgeneric lisp::tag-type-check-function)
(defgeneric lisp::marked?)
(defgeneric lisp::integer?)
(defgeneric lisp::list?)
(defgeneric lisp::float?)
(defgeneric lisp::lexeme?)
(defgeneric lisp::load-data-object)
(defgeneric lisp::store-data-object)
(defgeneric lisp::new-data-object)
(defgeneric lisp::next-cell)
(defgeneric lisp::nil?)
(defgeneric lisp::cons)
(defgeneric lisp::node?)
(defgeneric lisp::nil-cell)
;(defmethod lisp::nil-cell
;  ()
;  (create$ (@label nil)
;           (@dword 0x1) ; always marked
;           (@dword 0x0)
;           (@dword nil))) ; loop back on self

(defmethod lisp::nil?
  ()
  (tag-type-check-function nilp
                           nil))

(defmethod lisp::next-cell
           ()
           (defunc nextCell
                   (use-register ?*arg0*
                                 (op:move ?*arg0*
                                          ?*next*)
                                 (op:call immediate
                                          loadDataObject))))

(defmethod lisp::use-data-object-registers
  ($?body)
  (use-register (create$ ?*tag*
                         ?*this*
                         ?*next*)
                $?body))

(defmethod lisp::store-data-object
  ()
  (defunc storeDataObject
          (use-address-and-value (op:move ?*address-register*
                                          ?*arg0*)
                                 (op:move ?*value-register*
                                          ?*tag*)
                                 (op:store 0m1111
                                           0x0)
                                 (op:move ?*value-register*
                                          ?*this*)
                                 (op:store 0m1111
                                           0x2)
                                 (op:move ?*value-register*
                                          ?*next*)
                                 (op:store 0m1111
                                           0x4))))


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
          (use-register (create$ ?*args*
                                 ?*condition-register*)
                        (call-decode-bits ?*tag*
                                          0b00011110
                                          0x1)
                        (op:compare ==
                                    none
                                    immediate
                                    ?*return-register*
                                    (- (member$ ?tag
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
          (use-register (create$ ?*condition-register*
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
  (defunc mark
          (use-register (create$ ?*args*
                                 ?*return-register*)
                        (call-encode-bits ?*tag*
                                          1
                                          1
                                          0)
                        (op:move ?*tag*
                                 ?*return-register*))))

