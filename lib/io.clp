; IO related operations
(defmodule io
           (export defgeneric
                   include
                   include:batch
                   include:batch*
                   include:load
                   include:load*
                   add-to-search-path)
           (export defglobal
                   search-path))

(defgeneric io::include
            "Iterate through a search path to try and find the first the instance of a given file!")
(defgeneric io::include:batch
            "Perform the include using the batch command!")
(defgeneric io::include:batch*
            "Perform the include using the batch* command!")
(defgeneric io::include:load
            "Perform the include using the load command!")
(defgeneric io::include:load*
            "Perform the include using the load* command!")

(defgeneric io::add-to-search-path)

(defglobal io
           ?*legal-io-load-commands* = (create$ batch
                                                batch*
                                                load
                                                load*)
           ?*search-path* = (create$ .))

(defmethod io::include
  "Try to load the given file using a specified clips operation"
  ((?operation SYMBOL
               (not (neq ?current-argument
                         (expand$ ?*legal-io-load-commands*))))
   (?path LEXEME))
  ; concat the path fragment together with the different search paths
  ; assume a unix environment
  (progn$ (?prefix ?*search-path*)
          (if (funcall ?operation
                       (str-cat ?prefix / ?path)) then
            (return TRUE)))
  FALSE)

(defmethod io::add-to-search-path
  ((?location LEXEME))
  (bind ?*search-path*
        ?*search-path*
        ?location))

(defmethod io::include:batch
  ((?path LEXEME))
  (include batch
           ?path))
(defmethod io::include:batch*
  ((?path LEXEME))
  (include batch*
           ?path))
(defmethod io::include:load
  ((?path LEXEME))
  (include load
           ?path))
(defmethod io::include:load*
  ((?path LEXEME))
  (include load*
           ?path))
