(defclass MAIN::output-file
  (is-a USER)
  (multislot enum-entries)
  (multislot arg-counts)
  (multislot translations))
(deffacts MAIN::memory-block-operations
          (translation type -> Type)
          (translation size -> Size)
          (translation get -> Get)
          (translation set -> Set)
          (translation clear -> Clear)
          (translation zero -> Clear)
          (translation increment -> Increment)
          (translation decrement -> Decrement)
          (translation ++ -> Increment)
          (translation -- -> Decrement)
          (translation swap -> Swap)
          (translation move -> Move)
          (translation copy -> Move)
          (count Type -> 0)
          (count Size -> 0)
          (count Get -> 1)
          (count Set -> 2)
          (count Clear -> 0)
          (count Increment -> 1)
          (count Decrement -> 1)
          (count Swap -> 2)
          (count Move -> 2))
(definstances MAIN::output-files
              (of output-file))
(defrule MAIN::process-translation
         (declare (salience 2))
         ?f <- (translation ?title -> ?enum)
         =>
         (retract ?f)
         (assert (enum-entry (str-cat ?enum ,))
                 (translation-entry (format nil 
                                            "{ \"%s\", MemoryBlockOp:: %s },"
                                            ?title
                                            ?enum))))
(defrule MAIN::process-arg-count
         (declare (salience 2))
         ?f <- (count ?enum -> ?count)
         =>
         (retract ?f)
         (assert (enum-entry (str-cat ?enum ,))
                 (arg-count-entry (format nil
                                          "{ MemoryBlockOp:: %s, %d },"
                                          ?enum
                                          ?count))))

(defrule MAIN::implant-arg-count-entry
         (declare (salience 1))
         ?f <- (arg-count-entry ?str)
         ?q <- (object (is-a output-file)
                       (arg-counts $?ac))
         =>
         (retract ?f)
         (modify-instance ?q 
                          (arg-counts ?ac
                                      ?str)))
(defrule MAIN::implant-enum-entry
         (declare (salience 1))
         ?f <- (enum-entry ?msg)
         ?q <- (object (is-a output-file)
                       (enum-entries $?ee))
         =>
         (retract ?f)
         (modify-instance ?q 
                          (enum-entries $?ee ?msg)))

(defrule MAIN::implant-translation
         (declare (salience 1))
         ?f <- (translation-entry ?msg)
         ?q <- (object (is-a output-file)
                       (translations $?ee))
         =>
         (retract ?f)
         (modify-instance ?q 
                          (translations $?ee ?msg)))

(defrule MAIN::generate-final-output
         (object (is-a output-file)
                 (enum-entries $?ee)
                 (translations $?tt)
                 (arg-counts $?ac))
         =>
         (printout t "enum class MemoryBlockOp {" crlf)
         (progn$ (?e $?ee)
                 (printout t ?e crlf))
         (printout t "};" crlf)
         (printout t "static std::map<std::string, MemoryBlockOp> opTranslation = {" crlf)
         (progn$ (?t ?tt) 
                 (printout t ?t crlf))
         (printout t "};" crlf)
         (printout t "static std::map<MemoryBlockOp, int> opArgCounts = {" crlf)
         (progn$ (?a ?ac)
                 (printout t ?a crlf))
         (printout t "};" crlf))


