; Startup the machine and perform initial partitioning and register layout
; the convention is to pass all args through the stack
; That way I don't have to do anything register wise when wanting to establish a convention
(alias r225 as call-stack)
(alias call-stack as cs)
(alias r226 as data-stack)
(alias data-stack as ds)
(alias r0 as scratch-register0)
(alias r1 as scratch-register1)
(alias r2 as scratch-register2)
(alias r3 as scratch-register3)
(alias scratch-register0 as arg0)
(alias scratch-register1 as arg1)
(alias scratch-register2 as arg2)
(alias scratch-register3 as arg3)
(alias r200 as v0)
(alias r201 as v1)
(alias r202 as v2)
(alias r203 as v3)
(let InternalStackStart be 0xFFFF)
(let DataStackStart be 0x3FFF)
(let CallStackStart be 0x7FFF)
(section code
         (org 0x0000
              (label Startup
                     ; first setup our two stacks each having 32 k at this point
                     (set sp
                          InternalStackStart)
                     (set cs
                          CallStackStart)
                     (set ds
                          DataStackStart)
                     ; now that we have done that we are ready to use the computer
                     )
              )
         ; perform a call by using the address on the top of the call stack
            (label IndirectCall
                   (push sp
                         lr)
                   (pop arg0
                        ds)
                   (bl arg0)
                   ; Can fixed jump to this if we so desire!
                   (label IndirectCallReturn
                          (pop lr
                               sp)
                          (blr)))
         )
