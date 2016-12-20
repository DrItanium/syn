(defglobal MAIN
           ?*program-contents* = (create$ stack-init-code
                                          memory-block-code
                                          setup-read-eval-print-loop
                                          setup-simple-funcs
                                          generate-defuncs))

;-----------------------------------------------------------------------------
; Rules
;-----------------------------------------------------------------------------
(defrule MAIN::build-program
         =>
         (make-instance of program
                        (contents (map funcall
                                       (expand$ ?*program-contents*)))))
;-----------------------------------------------------------------------------
; Code to generate
;-----------------------------------------------------------------------------
(deffunction MAIN::stack-init-code
             ()
             (create$ (set64 (register:stack-pointer-bottom)
                             ?*stack-bottom*
                             (op:move (stack-pointer)
                                      (register:stack-pointer-bottom)))
                      (set64 (register:stack-pointer-top)
                             ?*stack-top*)
                      (set64 (register:call-stack-bottom)
                             ?*call-stack-bottom*
                             (op:move (register:call-stack-pointer)
                                      (register:call-stack-bottom)))
                      (set64 (register:call-stack-top)
                             ?*call-stack-top*)))
(deffunction MAIN::setup-start-end-pair
             (?start ?start-addr ?end ?end-addr)
             (create$ (set64 (register ?start)
                             ?start-addr)
                      (set64 (register ?end)
                             ?end-addr)))

(deffunction MAIN::memory-block-code
             ()
             (create$
               (setup-start-end-pair (register:memory-space0-start)
                                     ?*memory0-start*
                                     (register:memory-space0-end)
                                     ?*memory0-end*)
               (setup-start-end-pair (register:memory-space1-start)
                                     ?*memory1-start*
                                     (register:memory-space1-end)
                                     ?*memory1-end*)
               (setup-start-end-pair (register:code-start)
                                     ?*code-start*
                                     (register:code-end)
                                     ?*code-end*)
               (set64 (register (register:address-table-base))
                      ?*addr-table-begin*
                      (op:move (register:address-table-pointer)
                               (register:address-table-base)))))
(deffunction MAIN::setup-read-eval-print-loop
             ()
             (!loop EvalBase
                    ; loop body goes here
                    ))

(deffunction MAIN::defunc:stack-empty
             ()
             (func stack-empty
                   (op:eq (register:ret0)
                          (stack-pointer)
                          (register:stack-pointer-bottom))))
(deffunction MAIN::defunc:stack-full
             ()
             (func stack-full
                   (op:eq (register:ret0)
                          (stack-pointer)
                          (register:stack-pointer-top))))

(deffunction MAIN::generate-defuncs
             ()
             (create$ (defunc:stack-full)
                      (defunc:stack-empty)))

(deffunction MAIN::register-func3
             (?title)
             (func ?title
                   (funcall (sym-cat op:
                                     (lowcase ?title))
                            (register:ret0)
                            (register:arg0)
                            (register:arg1))))
(deffunction MAIN::stack-and-register-func
             (?title)
             (create$ (stack-func ?title)
                      (register-func3 ?title)))
(deffunction MAIN::setup-simple-funcs
             ()
             (bind ?ssp
                   (stack (stack-pointer)))
             (create$ (map stack-and-register-func
                           eq
                           neq
                           add
                           sub
                           div
                           rem)
                      (stack-func shift-left
                                  shiftleft)
                      (stack-func shift-right
                                  shiftright)
                      (stack-func lt
                                  lessthan)
                      (stack-func gt
                                  greaterthan)
                      (stack-func le
                                  lessthanorequalto)
                      (stack-func ge
                                  greaterthanorequalto)
                      (func stack:incr
                            (op:increment ?ssp
                                          ?ssp))
                      (func incr
                            (op:increment (register:ret0)
                                          (register:arg0)))
                      (func stack:decr
                            (op:decrement ?ssp
                                          ?ssp))
                      (func decr
                            (op:decrement (register:ret0)
                                          (register:arg0)))
                      (func stack:halve
                            (op:halve ?ssp
                                      ?ssp))
                      (func halve
                            (op:halve (register:ret0)
                                      (register:arg0)))
                      (func stack:double
                            (op:double ?ssp
                                       ?ssp))
                      (func double
                            (op:double (register:ret0)
                                       (register:arg0)))
                      (func shutdown
                            (sys-terminate))
                      (func read-char
                            (sys-getc (register:ret0)))
                      (func put-char
                            (sys-putc (register:arg0)))))
