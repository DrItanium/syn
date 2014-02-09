(deftemplate message
             (slot from)
             (slot to)
             (slot action)
             (multislot values))
(defrule parse-mouse-input
         ?f <- (mouse ?buttons&:(!= ?buttons 4) ?x ?y ?msec)
         =>
         (retract ?f)
         (format t "mouse: %d %d %d %d%n" ?buttons ?x ?y ?msec))

(defrule parse-mouse-input:quit
         ?f <- (mouse ?buttons&:(= ?buttons 4) ?x ?y ?msec)
         =>
         (retract ?f)
         (format t "exiting%n")
         (exit))

(defrule send-message-on
         ?f <- (message (from ?from)
                        (to ?to&~mux)
                        (action ?action)
                        (values $?values))
         =>
         (retract ?f)
         (format ?to "(message (from %s) (to %s) (action %s) (values %s))"
                 ?from ?to ?action (implode$ ?values)))
