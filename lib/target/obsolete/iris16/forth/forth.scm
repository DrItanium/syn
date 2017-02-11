(defregister ptop 252)
(defregister pend 251)
(defregister rtop 250)
(defregister rend 249)
(defregister spar0 248)
(defregister spar1 247)
(defregister spar2 246)
(defregister spar3 245)
(defregister sres0 244)
(defregister sres1 243)
(defregister sres2 242)
(defregister sres3 241)
(defregister stmp0 240)
(defregister stmp1 239)
(defregister stmp2 238)
(defregister stmp3 237)
(defregister space 236)
(defregister stmp4 235)
(defregister stmp5 234)
(defregister stmp6 233)
(defregister wlen 232)
(defregister stmp7 231)
(defregister zero 230)
(defregister fdcur 229)
(defregister inptr 228)
(defregister outptr 227)
(defregister is-negative 226)
(defregister nil-registers 225)

(define stack-setup 
  (list @data
		(!at-memory-location 0xFFFF
							 (!label ParameterStackBottom))
		(!at-memory-location 0x7FFF
							 (!label ReturnStackBottom))))

(define start-code 
  (!at-memory-location 0x0000
					   @code
					   (!set ptop "ParameterStackBottom")
					   (!set pend "ParameterStackBottom")
					   (!set rtop "ReturnStackBottom")
					   (!set rend "ReturnStackBottom")
					   (!set space 0x20)			; fixed register for the ascii space code
					   (!set wlen 62)))				; the maximum number of characters allowed in a single word (- 1 for the terminator)


(define (!print)
 		(!calli 'print))
(define fn-done
  (!loop "DONE"
   		  ; top of our "control loop", what he calls RETURN
		 (!set spar0 "WORD_BUFFER")		; load the front of the word buffer
		 (!calli "NUMBER")				; check and see we get a NUMBER from this input
		 (!move stmp1 sres1)			; extract the result field
		 (!jf stmp1 "PARSE_WORD")		;
		 (!move stmp0 sres0)			; extract the number field
		 (!jump "PRINT_RESULT")			;
		 (scope "PARSE_WORD"
				(!set spar0 "WORD_BUFFER") ; 
				(!calli "WORD"))		   ; read the next word
		 (scope "PRINT_RESULT"
				(!print)				   ; print it out
				(!set spar0 "msg_newline") ; 
				(!print))))				   ; print out a newline
(define fn-print
 		(!defunc "print"
		 		 (!loop "print_loop"
				  		(!ld stmp0 spar0) ; load the current char from memory into a register
						(!jf stmp0 'print_done) ; before printing, check to see that we should print, if it is zero then stop

				 )
 		

