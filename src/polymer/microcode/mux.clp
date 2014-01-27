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
