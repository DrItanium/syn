
(output t
  (defunc PushOntoStack
          (use-register value
                        (branch-call immediate
                                     IncrementStackPointer))
          (branch-call immediate
                       StoreStackTop))
  (defunc PopFromStack
          (branch-call immediate
                       LoadStackTop)
          (use-register value
                        (branch-call immediate
                                     DecrementStackPointer))))


