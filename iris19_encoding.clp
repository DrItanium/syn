(defmodule iris19
           (export defclass
                   instruction-encoding))
(defclass iris19::instruction-encoding
  (is-a USER)
  (slot currentLine
        (type INTEGER))
  (slot address
        (type INTEGER))
  (slot type
        (type LEXEME))
  (slot immediate
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (slot shift-left
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (slot is-if
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (slot is-call
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (slot is-conditional
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (slot is-indirect
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (slot read-next-word
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (slot bitmask
        (type INTEGER))
  (slot arg0)
  (slot arg1)
  (slot arg2)
  (slot is-label
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (slot label-value
        (type SYMBOL))
  (slot sub-type
        (type SYMBOL
              INTEGER))
  (slot full-immediate
        (type INTEGER))
  (message-handler encode primary)
  (message-handler clear primary)
  (message-handler num-word primary))
(defglobal iris19
 ?*arithmetic-operation* = (iris19:convertEnumToInt_Operation Arithmetic)
 )
(deffunction iris19::encode-control
             (?value ?type)
             (iris19:encodeControl ?value
                                   (iris19:convertEnumToInt_Operation ?type)))
(deffunction iris19::try-encode-source1
             (?value ?immediate ?source1)
             (if ?immediate then
               (iris19:encodeShortImmediate ?value
                                           ?source1)
               else
               (iris19:encodeSource1Index ?value
                                          ?source1)))
(deffunction iris19::encode-destination
             (?value ?dest)
             (iris19:encodeDestinationIndex ?value
                                            ?dest))
(deffunction iris19::encode-source0
             (?value ?src0)
             (iris19:encodeSource0Index ?value
                                        ?src0))
(deffunction iris19::encode-single-word
             (?value ?immediate ?dest ?src0 ?src1)
             (create$ (try-encode-source1
                        (encode-source0
                          (encode-destination ?value
                                              ?dest)
                          ?src0)
                        ?immediate
                        ?src1))
(deffunction iris19::make-control
             (?type)
             (encode-control 0
                             ?type))

(deffunction iris19::encode-arithmetic
             (?sub-type ?immediate)
             (encode-immediate
               (iris19:encodeArithmeticFlagType 
                 (make-control Arithmetic)
                 (iris19:convertEnumToInt_ArithmeticOps ?sub-type))
               ?immediate))
(deffunction iris19::encode-shift
             (?shift-left ?immediate)
             (encode-immediate
                 (iris19:encodeShiftFlagLeft
                   (make-control Shift)
                   ?shift-left)
                 ?immediate))
(deffunction iris19::encode-immediate
             (?value ?immediate)
             (iris19:encodeImmediateFlag ?value
                                         ?immediate))
(deffunction iris19::encode-compare
             (?sub-type ?immediate)
             (encode-immediate 
               (iris19:encodeCompareType 
                 (make-control Compare)
                 (iris19:convertEnumToInt_CompareStyle ?sub-type))
               ?immediate))
(deffunction iris19::encode-move
             (?sub-type ?is-set)
             (iris19:encodeMoveSubtype (make-control Move)
                                       (iris19:convertEnumToInt_MoveOperation ?sub-type)))
(deffunction iris19::encode-branch
             (?immediate ?is-conditional ?is-call ?is-if)
             (encode-immediate
               (iris19:encodeBranchFlagIsConditional
                 (iris19:encodeBranchFlagIsCallForm
                   (iris19:encodeBranchFlagIsIfForm 
                     (make-control Branch)
                     ?is-if)
                   ?is-call)
                 ?is-conditional)
               ?immediate))
(deffunction iris19::encode-if
             (?value ?source0 ?source1)
             (create$ (encode-source0
                        (encode-source1 ?value
                                        ?source1)
                        ?source0)))
(defmessage-handler iris19::instruction-encoding encode primary
                    ()
                    (switch ?self:type
                            (case Branch then
                              (send ?self 
                               branch-encoding))
                            (case Arithmetic then 
                             (send ?self
                                   single-word-encoding
                                   (encode-arithmetic ?self:sub-type
                                                      ?self:immediate))
                            (case Shift then
                              (send ?self
                                    single-word-encoding
                                    (encode-shift ?self:sub-type
                                                  ?self:immediate))
                            (case Compare then
                              (send ?self
                                    single-word-encoding
                                    (encode-compare ?self:sub-type
                                                    ?self:immediate)))
                            (case Move then)
                            (case Logical then)
                            (default (create$))))


(defmessage-handler iris19::instruction-encode single-word-encoding primary
                    (?value)
                    (encode-single-word ?value
                                        ?self:immediate
                                        ?self:destination
                                        ?self:source0
                                        ?self:source1))
(defmessage-handler iris19::instruction-encode branch-encoding primary
                    ()
                    (bind ?cbits
                          (encode-destination 
                            (encode-branch ?self:immediate
                                           ?self:is-conditional
                                           ?self:is-call
                                           ?self:is-if)
                            ?self:destination))
                    (if ?self:is-if then
                      (encode-if ?cbits
                                 ?self:source0
                                 ?self:source1)
                      else
                      (if ?self:immediate then
                        (create$ ?cbits
                                 (send ?self lower-half)
                                 (send ?self upper-half))
                        else
                        (create$ (encode-source0 ?cbits
                                                 ?self:source0))))))
