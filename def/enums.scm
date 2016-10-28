(define (*def-enum title size capacity body)
  (string-append "enum class " title " : " size " {" 
   				 (apply string-append 
				  		(lambda (x) (string-append x ", "))
						body)
				 "Count, }; static_assert<static_cast<" size ">(" title "::Count) < static_cast<" size ">(" (atom->string capacity) "), \"too many entries of type \"" title "\" defined!\");"))


(define (*build-enum port title size capacity body)
  (if port 
	(begin (display (*def-enum title size capacity body)
					port)
		   (close-port port)
		   #t)
	#f))
