(defmodule system
           (import iris32 ?ALL)
           (import lisp ?ALL))

(deffunction system::setup-idle
             (?index)
             (create$ (@label (sym-cat thread ?index _idle))
                      (j r244)))
(deffunction system::stack-entry
             (?index)
             (create$ (@org (sym-cat 0x03F ?index "0000"))
                      (@label (sym-cat thread ?index _stack_base))))
(defgeneric system::decl-thread-control-block)
(defmethod system::decl-thread-control-block
  ((?index INTEGER)
   (?evaluate-jump SYMBOL)
   (?address-to-jump SYMBOL))
  (create$ (@label (sym-cat thread ?index))
           (@word (if ?evaluate-jump then 0xFFFFFFFF else 0x00000000))
           (@word ?address-to-jump)
           (@word (sym-cat thread ?index _stack_base))
           (@word 0x00000000)))

(defmethod system::decl-thread-control-block
  ((?index INTEGER))
  (decl-thread-control-block ?index
                             FALSE
                             (sym-cat thread ?index _idle)))

(defgeneric system::define)
(defmethod system::define
  ((?title SYMBOL)
   $?body)
  (create$ (@label ?title)
           $?body
           (j lr)))

(definstances system::register-conventions
              (tcbase of register (refers-to r243))
              (zero of register (refers-to r0))
              (one of register (refers-to r1))
              (basesp of register (refers-to r240))
              (thread-invoke-flag of register (refers-to r249))
              (thread-idle-address of register (refers-to r244))
              (nilptr of register (refers-to r209))
              (creg of register (refers-to r208))
              (thread-jump of register (refers-to r244)))

(defgeneric system::push-multiple)
(defmethod system::push-multiple
  ((?a MULTIFIELD))
  (map push
       (expand$ ?a)))
(defmethod system::push-multiple
  ($?a)
  (map push
       (expand$ ?a)))

(defgeneric system::pop-multiple)
(defmethod system::pop-multiple
  ((?a MULTIFIELD))
  (map pop (expand$ ?a)))
(defmethod system::pop-multiple
  ($?a)
  (map pop (expand$ ?a)))




(deffunction system::thread-evaluate-jump-fns
             ()
             (create$ (define thread_evaluate_jump_enable
                        (push-multiple temp0 
                                       temp1)
                        (muli temp0 in0 4)
                        (add temp1 tcbase temp0)
                        (st temp1 thread-invoke-flag)
                        (pop-multiple temp1
                                      temp0))
                      (define thread_evaluate_jump_disable
                        (push-multiple temp0
                                       temp1)
                        (muli temp0 in0 4)
                        (add temp1 tcbase temp0)
                        (st temp1 zero)
                        (pop-multiple temp1
                                      temp0))))

(deffunction system::thread-control-data
             ()
             (create$ (@org 0x03FF0000)
                      (@label thread_control_block_base)
                      (decl-thread-control-block 0 
                                                 TRUE 
                                                 main_thread)
                      (map decl-thread-control-block 
                           1 2 3 4 5 6 7)))
(deffunction system::terminate-execution-fn
             ()
             (create$ (@label terminate_execution)
                      (system-op 0 zero zero)))
(deffunction system::printchar-fn
             ()
             (define printchar
               (system-op 2 
                          r32 
                          r32)))
(deffunction system::readchar-fn
             ()
             (define printchar
               (system-op 1 r48 r48)))
(deffunction system::stack-data-fn
             ()
             (map stack-entry 0 1 2 3 4 5 6 7))

(deffunction system::main-thread-fn
             ()
             (create$ (@label main_thread)
                      ; (set r208 printstring)
                      ; (set r32 string)
                      ; (jl r208)
                      (set r208 
                           terminate_execution)
                      (j r208)))

(deffunction system::init-fn
             ()
             (create$ (set r243 thread_control_block_base) 
                      (muli r242 tid 4) 
                      (add r250 r243 r242) 
                      (addi r249 r250 2) 
                      (ld sp r249) 
                      (move r240 sp) 
                      (set r249 0xFFFFFFFF) 
                      (set r244 thread_idle) 
                      (set r209 nil) 
                      (set r0 0) 
                      (set r1 1) 
                      (@label thread_idle)
                      (ld r248 r250)
                      (addi r246 r250 1)
                      (ld r245 r246)
                      (eq r247 r249 r248)
                      (jt r247 r245)
                      (j r244)
                      (map setup-idle 
                           1 2 3 4 5 6 7)))
(deffunction system::printstring-fn
             ()
             (define printstring
               (push-multiple lr
                              temp0
                              temp1
                              temp2
                              temp3
                              temp4
                              temp5)
               (move temp0 in0)
               (set temp3 printchar)
               (set temp4 printstring_done)
               (set temp5 printstring_loop)
               (@label printstring_loop)
               (ld temp1 temp0)
               (ne temp2 temp1 zero)
               (jf temp2 temp4)
               (move in0 temp1)
               (jl temp3)
               (addi temp0 temp0 1)
               (j temp5)
               (@label printstring_done)
               (pop-multiple temp5
                             temp4
                             temp3
                             temp2
                             temp1
                             temp0
                             lr)))
