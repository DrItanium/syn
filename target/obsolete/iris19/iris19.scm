(define (field-to-string a)
 		(cond ((symbol? a)
			   (symbol->string a))
		 	  ((number? a)
			   (atom->string a))
			  ((integer? a)
			   (atom->string a))
			  ((real? a)
			   (atom->string))
			  ((string? a) a)
			  (else (atom->string a))))
(define (sym-cat a b)
 		(string->symbol (string-append (field-to-string a)
						 			   (field-to-string b))))
(define (symbol a)
 		(string->symbol (field-to-string a)))


(macro (defregister form)
	   `(define ,(cadr form)
		 (sym-cat "r" ,(caddr form))))

(macro (stack form)
	   `(string-append "stack " ,(field-to-string (cadr form))))
(macro (indirect form)
	   `(string-append "indirect " ,(field-to-string (cadr form))))
(macro (immediate form)
 		`(string-append "immediate " ,(field-to-string (cadr form)) " " ,(field-to-string (caadr form))))
(macro (short-immediate form)
 		`(string-append "immediate " ,(field-to-string (cadr form))))
						

(define (swap dest src)
  (list 'swap dest src))
(define (set bitmask dest imm)
  (list 'move bitmask dest 'immediate  imm))
(define (move bitmask dest src)
  (list 'move bitmask dest src))
(define (arithmetic subtype dest src0 src1)
  (list 'arithmetic subtype dest src0 src1))

(define (dup ptr)
 		(move '0m11111111
		 	  (stack ptr)
			  (indirect ptr)))
(define (drop ptr)
 		(arithmetic 'add
		 			 ptr
					 ptr
					 (short-immediate 2)))
(define (swap-top ptr)
 		(swap (stack ptr)
		 	  (stack ptr)))

(define (fetch dest ptr)
 (list (move '0m11111111 dest (stack ptr))
	   (move '0m11111111 dest (indirect dest))))


