(define (*arithmetic name op action)
  (string-append "case ArithmeticOp::" 
				 name
				 ": gpr[getDestination()] = " 
				 op
				 (cond ((equal? action 
								"None")
						"(gpr[getSource0()], gpr[getSource1()])")
					   ((equal? action 
								"Immediate")
						"(gpr[getSource0()], static_cast<word>(getSource1()))")
					   ((equal? action 
								"Unary")
						"(gpr[getSource0()])")
					   (else (/ 1 0)))
				 "; break; "))
(define (*arithmetic-unit name op action)
  (list name op action))

(define *arithmetic-units
  (list (*arithmetic-unit "Add" "iris::add" "None")
		(*arithmetic-unit "Sub" "iris::sub" "None")
		(*arithmetic-unit "Mul" "iris::mul" "None")
		(*arithmetic-unit "Div" "iris::div" "None")
		(*arithmetic-unit "Rem" "iris::rem" "None")
		(*arithmetic-unit "ShiftLeft" "iris::shiftLeft" "None")
		(*arithmetic-unit "ShiftRight" "iris::shiftRight" "None")
		(*arithmetic-unit "BinaryAnd" "iris::binaryAnd" "None")
		(*arithmetic-unit "BinaryOr" "iris::binaryOr" "None")
		(*arithmetic-unit "BinaryNot" "iris::binaryNot" "Unary")
		(*arithmetic-unit "BinaryXor" "iris::binaryXor" "None")
		(*arithmetic-unit "AddImmediate" "iris::add" "Immediate")
		(*arithmetic-unit "SubImmediate" "iris::sub" "Immediate")
		(*arithmetic-unit "MulImmediate" "iris::mul" "Immediate")
		(*arithmetic-unit "DivImmediate" "iris::div" "Immediate")
		(*arithmetic-unit "RemImmediate" "iris::rem" "Immediate")
		(*arithmetic-unit "ShiftLeftImmediate" "iris::shiftLeft" "Immediate")
		(*arithmetic-unit "ShiftRightImmediate" "iris::shiftRight" "Immediate")))

(define arithmetic-unit-code
  (open-output-file "iris16_arithmetic_logic.def"))
(define arithmetic-unit-enumeration
  (open-output-file "iris16_arithmetic_enum.def"))
(for-each (lambda (mf)
			(let ((str (*arithmetic (car mf)
									(car (cdr mf))
									(car (cdr (cdr mf))))))
			  (display str arithmetic-unit-code)))
		  *arithmetic-units)
(display (*def-enum "ArithmeticOp" "byte" "ArchitectureConstants::MaxOperations" *arithmetic-units)
		 arithmetic-unit-enumeration)
(close-port arithmetic-unit-code)
(close-port arithmetic-unit-enumeration)

