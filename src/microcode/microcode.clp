; microcode v1

(defgeneric add:)
(defgeneric sub:)
(defgeneric mul:)
(defgeneric div:)
(defgeneric mod:)
(defgeneric shift-left:)
(defgeneric shift-right:)
(defgeneric binary-and:)
(defgeneric binary-or:)
(defgeneric binary-not:)
(defgeneric set:)

(defmethod add:
  ((?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (set-register ?dest (+ (get-register ?src0)
                         (get-register ?src1))))

(defmethod sub: 
  ((?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (set-register ?dest (- (get-register ?src0)
                         (get-register ?src1))))
(defmethod mul: 
  ((?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (set-register ?dest (* (get-register ?src0)
                         (get-register ?src1))))

(defmethod div:
  ((?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (set-register ?dest (div (get-register ?src0)
                           (get-register ?src1))))

(defmethod mod:
  ((?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (set-register ?dest (mod (get-register ?src0)
                           (get-register ?src1))))
(defmethod shift-left:
  ((?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (set-register ?dest (shift-left (get-register ?src0)
                                  (get-register ?src1))))
(defmethod shift-right:
  ((?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (set-register ?dest (shift-right (get-register ?src0)
                                   (get-register ?src1))))
(defmethod binary-and:
  ((?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (set-register ?dest (binary-and (get-register ?src0)
                                  (get-register ?src1))))
(defmethod binary-or:
  ((?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (set-register ?dest (binary-or (get-register ?src0)
                                 (get-register ?src1))))
(defmethod binary-or:
  ((?dest INTEGER)
   (?src0 INTEGER))
  (set-register ?dest (binary-not (get-register ?src0))))

(defmethod set:
  ((?dest INTEGER)
   (?value INTEGER))
  (set-register ?dest ?value))
