(batch* iris_compiler.clp)
(batch* iris18.clp)
(batch* helpers.clp)
(reset)
;-------------------------------------------------
(code (scope startup
             (comment "Make sure that we setup memory and hand off to the runtime")
             )
      (defis-lparen)
      (defis-rparen)
      (defis-space))

(compile)
