; an expert system to wrap over a help document/documents
(batch* cortex.clp)
(defmodule MAIN
           (import cortex ?ALL))
(batch* order.clp)
(deffacts MAIN::execution-order
          (stage (current startup)
                 (rest prompt
                       eval
                       print
                       loop))
          (loaded-files))

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
         ?k <- (loaded-files $?files)
         =>
         (retract ?f)
         (if (not (fetch ?file)) then
           (printout werror "Couldn't open " ?file crlf)
           else
           (retract ?k)
           (assert (loaded-files ?files
                                 ?file))))

(defrule MAIN::toss-file
         (stage (current eval))
         ?f <- (input toss ?file)
         ?k <- (loaded-files $?a ?file $?b)
         =>
         (retract ?f)
         (if (not (toss ?file)) then
           (printout werror "Couldn't open " ?file crlf)
           else
           (retract ?k)
           (assert (loaded-files ?a ?b))))

(defrule MAIN::print-out-loaded-files
         (stage (current eval))
         ?f <- (input list files)
         (loaded-files $?files)
         =>
         (retract ?f)
         (printout t "Loaded files:" crlf)
         (progn$ (?file ?files)
                 (printout t tab ?file crlf)))


(defrule MAIN::unknown-input
         (declare (salience -1000))
         (stage (current eval))
         ?f <- (input $?)
         =>
         (retract ?f))


