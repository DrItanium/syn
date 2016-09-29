(batch* iris_compiler.clp)
(batch* iris18.clp)
(batch* helpers.clp)
(reset)
;-------------------------------------------------
(code (scope startup
             (comment "Make sure that we setup memory and hand off to the runtime")
             )
      (comment "standard character checking routines")
      (defunc printstring
              (describe-arg value
                            "Pointer to the front of the string")
              (todo "FINISH")
              )


      )

