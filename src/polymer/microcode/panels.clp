(deftemplate message
             (slot from)
             (slot to)
             (slot action)
             (multislot values))

(defrule process-resize-message
         ?f <- (message (from gpu)
                        (action resize)
                        (values ? ?w ?h))
         =>
         (retract ?f)
         (printout t "resize to " ?w " " ?h crlf))
