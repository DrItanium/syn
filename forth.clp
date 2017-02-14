; Register descriptions:
; r0 - r31 - scratch / unused registers
; r
; setup the system as needed
; TODO: insert code for the error handler setup

(let ErrorDispatchVector be 0x00FF)
(let ZeroAddress be 0x0000)
(let MessageBufferStart be 0x0100)
(let GetCAddress be 0x0001)
(let PutCAddress be 0x0002)
(let TerminateAddress be 0x0000)
(let ROMRoutines be 0xFE00)
(let ErrorHandler be 0xFF00)
(alias r0 as scratch0)
(alias r1 as scratch1)
(alias r2 as scratch2)
(alias r3 as scratch3)
(alias r32 as internal-stack)
(alias p14 as rompredicate-true)
(alias p15 as rompredicate-false)
; in case our internal routines require return values, keep it off of the
; visible stack
(alias r96 as retval0)
(alias r97 as retval1)
(alias r98 as retval2)
(alias r99 as retval3)

(alias r128 as scratchrom0)
(alias r129 as scratchrom1)
(alias r130 as scratchrom2)
(alias r131 as scratchrom3)
(alias r132 as romarg0)
(alias r133 as romarg1)
(alias r134 as romarg2)
(alias r135 as romarg3)

(section code
         (org ROMRoutines
              (label TerminateExecution
                     ; if we are at this point then we can easily just use
                     ; whatever register we want!
                     (xor scratchrom0
                          scratchrom0
                          scratchrom0)
                     ; don't waste the space for the return
                     (stio scratchrom0
                           scratchrom0))
              (label ReadCharacter
                     (push internal-stack
                           scratchrom0)
                     (set scratchrom0
                          GetCAddress)
                     (ldio retval0
                           scratchrom0)
                     (pop scratchrom0
                          internal-stack)
                     (blr))
              (label WriteCharacter
                     (push internal-stack
                           scratchrom0)
                     (set scratchrom0
                          PutCAddress)
                     (stio scratchrom0
                           romarg0)
                     (pop scratchrom0
                          internal-stack)
                     (blr))
              (label ForceDivideByZero
                     (divi scratchrom0
                           scratchrom0
                           0x0)
                     (blr))
              (label IsSpace
                     (eqi rompredicate-true
                          rompredicate-false
                          romarg0
                          0x20)
                     (blr)))
         (org ErrorHandler
              (divi scratch0
                    scratch0
                    0x0)))
(section data
         (org ErrorDispatchVector
              (word ErrorHandler))
         (org MessageBufferStart
              (label MessageIOStorage
                     (label MessageIOLength
                            (word 0x0000))
                     (label MessageIOStorage
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000)
                            (word 0x0000))
                     )
              )
         )
