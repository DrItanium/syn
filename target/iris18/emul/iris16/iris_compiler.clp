; basic runtime for the iris base environment
; requires implementation details from a separate target file.
; However the iris environment uses 16-bit numbers as words
; There are also other constants for the environment as well.
; 1) Labels start with @label
; 2) The org directive starts with @org
; 3) 32bit values are @dword
; 4) 16bit values are @word
(defgeneric output
            "Output the input body to the target IO router")
(defgeneric deflabel
            "Declare a label which points to a given address")
(defgeneric memory-location
            "Wrapper over the org directive")
(defgeneric dword
            "Define a 32bit constant")
(defgeneric word
            "Define a 16bit constant")
(defgeneric comment
            "declare a comment")
(defgeneric note
            "Declare a note comment")
(defgeneric todo
            "Declare a todo comment")
(defgeneric scope
            "Declares a label targeted body of code")
(defgeneric defunc
            "defines a 'function' that is registered into the current iris environment")
(defgeneric jump-table
            "Define a series of addresses prefixed with a base label address")
(defgeneric at-memory-location
            "At the given memory address, output the following things")
(defgeneric describe-arg
            "Describe a given argument")
(defgeneric ret
            "Instruction(s) for returning from a procedure")
(defgeneric zero
            "Set the given register to zero")
(defgeneric assign32
            "Assign the given 32bit constant to the target register")
(defgeneric assign16
            "Assign the given 16bit constant to the target register")
(defgeneric assign24)
(defgeneric assign8)
(defgeneric assign
            "Assing the give constant to the target register")
(defgeneric use-register
            "Save the given register to the stack, perform the provided actions, and then restore the original value")
(defgeneric save-register
            "Save the given register to the stack")
(defgeneric restore-register
            "Restore the given register from the stack")
(defgeneric set-address)
            

(defmethod dword
  ((?value LEXEME
           INTEGER))
  (format nil
          "    @dword %s"
          (str-cat ?value)))

(defmethod word
  ((?value LEXEME
           INTEGER))
  (format nil
          "    @word %s"
          (str-cat ?value)))

(defmethod memory-location
  ((?value LEXEME
           NUMBER))
  (format nil
          "@org %s"
          (str-cat ?value)))

(defmethod at-memory-location
  ((?value LEXEME
           NUMBER)
   (?body MULTIFIELD))
  (create$ (memory-location ?value)
           ?body))
(defmethod at-memory-location
  ((?value LEXEME
           NUMBER)
   $?body)
  (at-memory-location ?value
                      ?body))

(defmethod scope
  ((?name LEXEME)
   (?body MULTIFIELD))
  (create$ (deflabel ?name)
           ?body))
(defmethod scope
  ((?name LEXEME)
   $?body)
  (scope ?name
         ?body))

(defmethod defunc
  ((?name SYMBOL)
   (?entries MULTIFIELD))
  (scope ?name
         ?entries
         (ret)))
(defmethod defunc
  ((?name SYMBOL)
   $?entries)
  (defunc ?name
          ?entries))
(defmethod comment
  ((?op LEXEME)
   (?comment STRING))
  (format nil
          "%s ; %s"
          ?op
          ?comment))
(defmethod comment
  ((?comment STRING))
  (format nil
          "    ; %s"
          ?comment))

(defmethod todo
  ((?message STRING))
  (comment (format nil
                   "TODO: %s"
                   ?message)))
(deffunction generate-note-text
             (?msg)
             (format nil
                     "NOTE: %s"
                     ?msg))
(defmethod note
  ((?op LEXEME)
   (?message STRING))
  (comment ?op
           (generate-note-text ?message)))
(defmethod note
  ((?message STRING))
  (comment (generate-note-text ?message)))

(defmethod describe-arg
  ((?argument LEXEME)
   (?description STRING))
  (comment (format nil
                   "%s - %s"
                   ?argument
                   ?description)))

(defmethod deflabel
  ((?title SYMBOL))
  (format nil
          "@label %s"
          ?title))

(defmethod output
  ((?router SYMBOL)
   (?lines MULTIFIELD))
  (progn$ (?line ?lines)
          (printout ?router
                    ?line crlf)))
(defmethod output
  ((?router SYMBOL)
   $?lines)
  (output ?router
          ?lines))

(defmethod use-register
  ((?register SYMBOL)
   (?body MULTIFIELD))
  (create$ (save-register ?register)
           ?body
           (restore-register ?register)))
(defmethod use-register
  ((?register SYMBOL)
   $?body)
  (use-register ?register
                ?body))

(defmethod jump-table
 ((?title SYMBOL)
  (?locations MULTIFIELD))
  (scope ?title
         (map dword
              (expand$ ?locations))))
(defmethod jump-table
 ((?title SYMBOL)
  $?locations)
 (jump-table ?title
             ?locations))

