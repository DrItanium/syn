(defglobal MAIN
			  ?*instruction-list* = (create$ nop add sub mul div mod rightshift 
														leftshift binaryand binaryor 
														binarynot eq neq gt lt load store 
														branch set call ret)

			  ?*registers* = (create$ r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13
											  r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 
											  r25 r26 r27 r28 r29 r30 r31 r32 r33 r34 r35 
											  r36 r37 r38 r39 r40 r41 r42 r43 r44 r45 r46 
											  r47 r48 r49 r50 r51 r52 r53 r54 r55 r56 r57 
											  r58 r59 r60 r61 r62 r63 r64 r65 r66 r67 r68 
											  r69 r70 r71 r72 r73 r74 r75 r76 r77 r78 r79 
											  r80 r81 r82 r83 r84 r85 r86 r87 r88 r89 r90 
											  r91 r92 r93 r94 r95 r96 r97 r98 r99 r100 
											  r101 r102 r103 pi3 pi2 pi1 pi0 pfalse ptrue 
											  ps7 ps6 ps5 ps4 ps3 ps2 ps1 ps0 po0 po1 pfci 
											  ra pc cc rc pid true false))
(defgeneric translate/instruction)
(defgeneric translate/register)

(defmethod translate/instruction
  ((?index INTEGER (= ?index 254)))
  platform)
(defmethod translate/instruction
  ((?index INTEGER (= ?index 255)))
  termnate)

(defmethod translate/instruction
  ((?index INTEGER))
  (nth$ ?index ?*instruction-list*))

(defmethod translate/instruction
  ((?symbol LEXEME (eq ?symbol platform)))
  254)
(defmethod translate/instruction
  ((?symbol LEXEME (eq ?symbol terminate)))
  255)
(defmethod translate/instruction
  ((?symbol LEXEME))
  (member$ ?symbol ?*instruction-list*))


(defmethod translate/register
  ((?index INTEGER))
  (nth$ ?index ?*registers*))

(defmethod translate/register
  ((?symbol LEXEME))
  (member$ ?symbol ?*registers*))

; syntax of an instruction is 
; predicate op dest0 dest1 <- src0 src1
; predicate op dest0 <- src0 src1
; predicate op dest0 <- value
; predicate op dest0
; predicate op <- value
; predicate op

(defrule parse-dual-result-instruction
			(declare (salience 1))
			?f <- (instruction ?predicate ?op ?d0 ?d1 <- ?s0 ?s1)
			(test (not (neq ?op eq neq gt lt)))
			=>
			(retract ?f)
			(put-char t (translate/register ?predicate))
			(put-char t (translate/instruction ?op))
			(put-char t (translate/register ?d0))
			(put-char t (translate/register ?d1))
			(put-char t (translate/register ?s0))
			(put-char t (translate/register ?s1))
			(put-char t 0)
			(put-char t 0))

(defrule parse-single-result-instruction:dual-input
			(declare (salience 1))
			?f <- (instruction ?predicate ?op ?d0 <- ?s0 ?s1)
			(test (not (neq ?op add sub mul div mod rightshift 
								 leftshift binaryor binaryand)))
			=>
			(retract ?f)
			(put-char t (translate/register ?predicate))
			(put-char t (translate/instruction ?op))
			(put-char t (translate/register ?d0))
			(put-char t 0)
			(put-char t (translate/register ?s0))
			(put-char t (translate/register ?s1))
			(put-char t 0)
			(put-char t 0))

(defrule parse-single-result:single-input:binarynot
			(declare (salience 1))
			?f <- (instruction ?predicate binarynot ?dest <- ?value)
			=>
			(retract ?f)
			(put-char t (translate/register ?predicate))
			(put-char t (translate/instruction binarynot))
			(put-char t (translate/register ?op))
			(put-char t 0)
			(put-char t (translate/register ?value))
			(put-char t 0)
			(put-char t 0)
			(put-char t 0))
;TODO: continue
