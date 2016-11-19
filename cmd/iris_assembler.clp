(defmodule parser
           (import cortex
                   ?ALL)
           (export ?ALL))
(deffunction parser::create-register
             (?index)
             (sym-cat r ?index))
(deffunction parser::make-range
             (?from ?to)
             (bind ?a
                   (create$))
             (loop-for-count (?ind ?from ?to) do
                             (bind ?a
                                   ?a
                                   ?ind)))
(defclass parser::language
  (is-a thing))

(defclass parser::keyword
  (is-a thing)
  (slot word
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot class
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass parser::simple-value
  (is-a thing)
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass parser::number
  (is-a simple-value)
  (slot value
        (source composite)
        (type NUMBER))
  (slot specific-type
        (type SYMBOL)
        (allowed-symbols unknown
                         integer
                         float)
        (storage local)
        (visibility public))
  (message-handler init after))

(defmessage-handler parser::number init after
                    ()
                    (bind ?self:specific-type
                          (if (integerp ?self:value) then
                            integer
                            else
                            (if (floatp ?self:value) then
                              float
                              else
                              unknown))))

(defclass parser::lexeme
  (is-a simple-value)
  (slot value
        (source composite)
        (type LEXEME)))

(defclass parser::line
  (is-a indexed-thing-with-children)
  (slot original-line
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler init after))

(defmessage-handler parser::line init after
                    ()
                    (bind ?self:contents
                          (explode$ ?self:original-line)))

(defclass parser::file
  (is-a thing)
  (slot path
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot line-count
        (type INTEGER)
        (storage local)
        (visibility public)
        (range 0 ?VARIABLE))
  (slot open
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public))
  (slot router-id
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic (gensym*)))
  (slot current-line
        (type LEXEME)
        (storage local)
        (visibility public))
  (message-handler init after)) 

(defmessage-handler parser::file init after
                    ()
                    (bind ?self:open
                          (open ?self:path 
                                ?self:router-id
                                "r"))
                    (if ?self:open then
                      (bind ?self:current-line
                            (readline ?self:router-id))))



(defrule parser::close-file
         ?f <- (object (is-a file)
                       (open TRUE)
                       (current-line EOF)
                       (router-id ?r))
         =>
         (modify-instance ?f
                          (open FALSE))
         (close ?r))

(defrule parser::new-line
         ?f <- (object (is-a file)
                       (open TRUE)
                       (current-line ?line&~EOF)
                       (line-count ?count)
                       (router-id ?r)
                       (name ?file))
         =>
         (modify-instance ?f 
                          (line-count (+ ?count 
                                         1))
                          (current-line (readline ?r)))
         (make-instance of line
                        (parent ?file)
                        (original-line ?line)
                        (index ?count)))

(defrule parser::identify-keyword
         (declare (salience ?*priority:first*))
         (object (is-a keyword)
                 (word ?id)
                 (name ?k))
         ?f <- (object (is-a line)
                       (children $?a ?id $?b))
         =>
         (modify-instance ?f
                          (children ?a ?k ?b)))

(defrule parser::identify-number
         (declare (salience ?*priority:first*))
         ?f <- (object (is-a line)
                       (children $?a ?id&:(numberp ?id) $?b)
                       (name ?line))
         =>
         (modify-instance ?f
                          (children ?a 
                                    (make-instance of number
                                                   (value ?id)
                                                   (parent ?line))
                                    ?b)))


