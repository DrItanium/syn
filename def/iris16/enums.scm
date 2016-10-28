(define (*def-enum title size capacity body)
  (string-append "enum class " title " : " size " {" 
   				 (apply string-append 
						(map (lambda (x) 
							   (string-append (car x)
											  ", "))
							 body))
				 "Count, }; static_assert<static_cast<" size ">(" title "::Count) < static_cast<" size ">(" (atom->string capacity) "), \"too many entries of type \"" title "\" defined!\");"))
