(defmodule iris19
           (export deffunction
                   *stack
                   *indirect)
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
  (message-handler encode primary))
(deffunction iris19::encode-control
             (?value ?type)
             (iris19:encodeControl ?value
                                   (iris19:convertEnumToInt_Operation ?type)))
(deffunction iris19::encode-source1 
             (?value ?source1)
             (iris19:encodeSource1Index ?value
                                        ?source1))
(deffunction iris19::try-encode-source1
             (?value ?immediate ?source1)
             (if ?immediate then
               (iris19:encodeShortImmediate ?value
                                            ?source1)
               else
               (encode-source1 ?value
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
                        ?src1)))
(deffunction iris19::make-control
             (?type)
             (encode-control 0
                             ?type))

(deffunction iris19::bool->int
             (?value)
             (if ?value then 1 else 0))
(deffunction iris19::encode-immediate
             (?value ?immediate)
             (iris19:encodeImmediateFlag ?value
                                         (bool->int ?immediate)))
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
                 (bool->int ?shift-left))
               ?immediate))
(deffunction iris19::encode-compare
             (?sub-type ?immediate)
             (encode-immediate 
               (iris19:encodeCompareType 
                 (make-control Compare)
                 (iris19:convertEnumToInt_CompareStyle ?sub-type))
               ?immediate))
(deffunction iris19::encode-branch
             (?immediate ?is-conditional ?is-call ?is-if)
             (encode-immediate
               (iris19:encodeBranchFlagIsConditional
                 (iris19:encodeBranchFlagIsCallForm
                   (iris19:encodeBranchFlagIsIfForm 
                     (make-control Branch)
                     (bool->int ?is-if))
                   (bool->int ?is-call))
                 (bool->int ?is-conditional))
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
                                                       ?self:immediate)))
                            (case Shift then
                              (send ?self
                                    single-word-encoding
                                    (encode-shift ?self:sub-type
                                                  ?self:immediate)))
                            (case Compare then
                              (send ?self
                                    single-word-encoding
                                    (encode-compare ?self:sub-type
                                                    ?self:immediate)))
                            (case Move then
                              (send ?self encode-move))
                            (case Logical then)
                            (default (create$))))
    (deffunction iris19::on-true
                 (?cond ?value)
                 (if ?cond then ?value else (create$)))
(defmessage-handler iris19::instruction-encoding encode-logical primary
                    ()
                    (bind ?bits
                          (encode-source0
                            (encode-destination
                              (encode-immediate
                                (iris19:encodeLogicalFlagType (make-control Logical)
                                                              (iris19:convertEnumToInt_LogicalOps ?self:sub-type))
                                ?self:immediate)
                              ?self:arg0)
                            ?self:arg1))
                    (if ?self:immediate then
                      (create$ (iris19:encodeRawBitmask ?bits
                                                        ?self:arg2)
                               (on-true (iris19:readLower ?self:bitmask)
                                        (iris19:maskedLowerHalf ?self:bitmask
                                                                ?self:full-immediate))
                               (on-true (iris19:readUpper ?self:bitmask)
                                        (iris19:maskedUpperHalf ?self:bitmask
                                                                ?self:full-immediate)))
                      else
                      (create$ (iris19:encodeSource1Index ?bits
                                                          ?self:arg2))))
(defmessage-handler iris19::instruction-encoding single-word-encoding primary
                    (?value)
                    (encode-single-word ?value
                                        ?self:immediate
                                        ?self:arg0
                                        ?self:arg1
                                        ?self:arg2))

(defmessage-handler iris19::instruction-encoding encode-move primary
                    ()
                    (create$ (encode-source0
                               (encode-destination
                                 (iris19:encodeRawBitmask 
                                   (iris19:encodeMoveSubtype (make-control Move)
                                                             (bind ?is-set
                                                                   (eq (bind ?mem-op
                                                                             (iris19:convertEnumToInt_MoveOperation ?self:sub-type))
                                                                       (iris19:convertEnumToInt_MoveOperation Set))))

                                   ?self:bitmask)
                                 ?self:arg0)
                               ?self:arg1)
                             (on-true (and ?is-set
                                           (iris19:readLower ?self:bitmask))
                                      (iris19:maskedLowerHalf ?self:bitmask 
                                                              ?self:full-immediate))
                             (on-true (and ?is-set
                                           (iris19:readUpper ?self:bitmask))
                                      (iris19:maskedUpperHalf ?self:bitmask
                                                              ?self:full-immediate))))





(defmessage-handler iris19::instruction-encoding branch-encoding primary
                    ()
                    (bind ?cbits
                          (encode-destination 
                            (encode-branch ?self:immediate
                                           ?self:is-conditional
                                           ?self:is-call
                                           ?self:is-if)
                            ?self:arg0))
                    (if ?self:is-if then
                      (encode-if ?cbits
                                 ?self:arg1
                                 ?self:arg2)
                      else
                      (if ?self:immediate then
                        (create$ ?cbits
                                 (iris19:RegisterValue_decodeLowerHalf ?self:full-immediate)
                                 (iris19:RegisterValue_decodeUpperHalf ?self:full-immediate))
                        else
                        (create$ (encode-source0 ?cbits
                                                 ?self:arg1)))))

(deffunction iris19::*stack
             (?index)
             (iris19:encodeRegisterIndex ?index 
                                         FALSE 
                                         TRUE))
(deffunction iris19::*indirect
             (?index)
             (iris19:encodeRegisterIndex ?index 
                                         TRUE 
                                         FALSE))
