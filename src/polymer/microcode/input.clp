(defrule process-mouse-input
         ?f <- (mouse ?a ?b ?c ?d)
         =>
         (retract ?f)
         (format mux "(mouse %d %d %d %d)" ?a ?b ?c ?d))

(defrule keyboard-input
         ?f <- (key ?key)
         =>
         (format t "pressed %d%n" ?key)
         (retract ?f))
