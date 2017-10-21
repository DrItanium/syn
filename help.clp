; an expert system to wrap over a help document/documents
(batch* cortex.clp)
(defmodule MAIN
           (import cortex ?ALL))
(batch* order.clp)
(defclass MAIN::text-file
  (is-a USER)
  (slot path
        (type LEXEME)
        (default ?NONE))
  (message-handler init after)
  (message-handler delete before))
(defmessage-handler text-file init after
                    ()
                    (if (not (fetch ?self:path)) then
                      (assert (unmake (instance-name ?self)))
                      else
                      (print-region t ?self:path root)))
(defmessage-handler text-file delete before
                    ()
                    (toss ?self:path))

(deffacts MAIN::execution-order
          (stage (current startup)
                 (rest prompt
                       eval
                       print
                       loop))
          (current-file FALSE))

(defrule MAIN::load-basic-help-file
         (stage (current startup))
         =>
         (fetch doc/basic_help.txt)
         (print-region t doc/basic_help.txt root))

(defrule MAIN::restart-execution
         ?f <- (stage (current loop))
         =>
         (modify ?f
                 (current prompt)
                 (rest read
                       eval
                       loop)))

(defrule MAIN::prompt-input
         (stage (current prompt))
         =>
         (printout t "Action: " ))

(defrule MAIN::read-input
         (stage (current read))
         =>
         (assert (input (explode$ (readline)))))

(defrule MAIN::leave-input
         (declare (salience 10000))
         ?f <- (stage (current eval))
         ?k <- (input quit|exit|bye)
         =>
         (retract ?f ?k))
(defrule MAIN::fetch-file
         (stage (current eval))
         ?f <- (input fetch ?file)
         =>
         (retract ?f)
         (make-instance of text-file
                        (path ?file)))

(defrule MAIN::toss-file
         (stage (current eval))
         ?f <- (input toss ?file)
         ?k <- (object (is-a text-file)
                       (path ?file)
                       (name ?name))
         =>
         (retract ?f)
         (assert (unmake ?name)))

(defrule MAIN::set-current-file
         (stage (current eval))
         ?f <- (input current ?path)
         (object (is-a text-file)
                 (path ?path)
                 (name ?name))
         ?k <- (current-file ~?name)
         =>
         (retract ?k ?f)
         (assert (current-file ?name)))

(defrule MAIN::get-current-file:is-not-set
         (stage (current eval))
         ?f <- (input current)
         (current-file FALSE)
         =>
         (retract ?f)
         (printout t NONE crlf))

(defrule MAIN::get-current-file:is-set
         (stage (current eval))
         ?f <- (input current)
         (current-file ?current&~FALSE)
         (object (is-a text-file)
                 (name ?current)
                 (path ?path))
         =>
         (retract ?f)
         (printout t ?path crlf))


(defrule MAIN::unset-current-file:retract
         (stage (current eval))
         ?a <- (unmake ?name)
         ?f <- (current-file ?name)
         =>
         (unmake-instance ?name)
         (retract ?a
                  ?f)
         (assert (current-file FALSE)))

(defrule MAIN::unset-current-file:retract:no-match
         (stage (current eval))
         ?a <- (unmake ?name)
         (current-file ~?name)
         =>
         (unmake-instance ?name)
         (retract ?a))


(defrule MAIN::print-out-loaded-files
         (stage (current eval))
         ?f <- (input files)
         =>
         (retract ?f)
         (printout t "Loaded files:" crlf)
         (do-for-all-instances ((?file text-file))
                               (printout t tab ?file:path crlf)))

(defrule MAIN::inspect-document
         (stage (current eval))
         ?f <- (input print)
         (current-file ?name)
         (object (is-a text-file)
                 (name ?name)
                 (path ?path))
         =>
         (retract ?f)
         (print-region t ?path "?"))

(defrule MAIN::go-up-one-level-and-print
         (stage (current eval))
         ?f <- (input up)
         (current-file ?name)
         (object (is-a text-file)
                 (name ?name)
                 (path ?path))
         =>
         (retract ?f)
         (print-region t ?path ^ "?"))
(defrule MAIN::list-subtopics-cmdA
         (stage (current eval))
         ?f <- (input sub-topics)
         =>
         (retract ?f)
         (assert (list sub-topics)))

(defrule MAIN::list-subtopics-cmdB
         (stage (current eval))
         ?f <- (input topics)
         =>
         (retract ?f)
         (assert (list sub-topics)))

(defrule MAIN::list-subtopics-cmdC
         (stage (current eval))
         ?f <- (input list sub-topics)
         =>
         (retract ?f)
         (assert (list sub-topics)))

(defrule MAIN::list-subtopics-cmdD
         (stage (current eval))
         ?f <- (input list available)
         =>
         (retract ?f)
         (assert (list sub-topics)))

(defrule MAIN::list-subtopics
         (stage (current eval))
         ?f <- (list sub-topics)
         (current-file ?name)
         (object (is-a text-file)
                 (name ?name)
                 (path ?path))
         =>
         (retract ?f)
         (print-region t
                       ?path
                       sub-topics))

(defrule MAIN::enter-sub-topic
         (stage (current eval))
         ?f <- (input sub-topic ?topic)
         (current-file ?name)
         (object (is-a text-file)
                 (name ?name)
                 (path ?path))
         =>
         (retract ?f)
         (print-region t
                       ?path
                       ?topic))

(defrule MAIN::reload-file
         (stage (current eval))
         ?f <- (input reload ?path)
         ?z <- (object (is-a text-file)
                       (path ?path)
                       (name ?name))
         =>
         (retract ?f)
         (unmake-instance ?z)
         (make-instance ?name of text-file
                        (path ?path)))


(defrule MAIN::reload-file-from-current
         (stage (current eval))
         ?f <- (input reload)
         (current-file ?name)
         (object (is-a text-file)
                 (name ?name)
                 (path ?path))
         =>
         (retract ?f)
         (assert (input reload ?path)))

(defrule MAIN::unknown-input
         (declare (salience -1000))
         (stage (current eval))
         ?f <- (input $?)
         =>
         (retract ?f))
(defrule MAIN::print-help-file
         (stage (current eval))
         ?f <- (input help)
         =>
         (retract ?f)
         (print-region t
                       doc/basic_help.txt
                       list-commands))


(reset)
(run)
(exit)
