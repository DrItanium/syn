;-----------------------------------------------------------------------------
; An implementation of the language specified by Charles H. Moore's book on
; a Problem orientied langauge.
;-----------------------------------------------------------------------------
; Fixed registers
;	- InstructionPoiner : r255 (aka ip)
;	- LinkRegister: r254 (aka lr)
;	- StackPointer: r253 (aka sp)
;	- Parameter Stack Top: r252 (aka ptop)
;	- Parameter Stack End: r251 (aka pend)
;	- Return Stack Top: r250 (aka rtop)
;	- Return Stack End: r249 (aka rend)
;	These are used internally for passing arguments around to leaf functions
;	- Safe Param 0: r248 (spar0)
;	- Safe Param 1: r247 (spar1)
;	- Safe Param 2: r246 (spar2)
;	- Safe Param 3: r245 (spar3)
;	- Safe Result 0: r244 (sres0)
;	- Safe Result 1: r243 (sres1)
;	- Safe Result 2: r242 (sres2)
;	- Safe Result 3: r241 (sres3)
;	- Safe Temporary 0: r240 (stmp0)
;	- Safe Temporary 1: r239 (stmp1)
;	- Safe Temporary 2: r238 (stmp2)
;	- Safe Temporary 3: r237 (stmp3)
;	- ASCII Space: r236 (space)
;	- Safe Temporary 4: r235 (stmp4)
;	- Safe Temporary 5: r234 (stmp5)
;	- Safe Temporary 6: r233 (stmp6)
;	- Max Word Length: r232 (wlen)
;	- Safe Temporary 7: r231 (stmp7)
;	- Zero register: r230 (zero)
;	- FETCH / DEPOSIT current: r229 (fdcur)
;	- input pointer: r228 (inptr)
;	- output pointer: r227 (outptr)
;	- is negative : r226
;	- nil registers : r225
;-----------------------------------------------------------------------------
; Parameter passing conventions
;  If the given subroutine calls another subroutine then you need to save
;  the current lr onto the return stack, otherwise you can just use it.
;  lr will always be populated in the case of subroutine calls
;-----------------------------------------------------------------------------
; setup the parameter and return stack pointers inside stack memory
;-----------------------------------------------------------------------------
; First setup the memory addresses using the org directive so that it is easy
; to keep track of
;-----------------------------------------------------------------------------
; Startup the machine and perform initial partitioning and register layout
; The above register layouts are a suggestion not exact, I'll rewrite them as I go on!
;-----------------------------------------------------------------------------
(alias r225 as call-stack)
(alias call-stack as cs)
(alias r226 as data-stack)
(alias data-stack as ds)
(alias r0 as scratch-register0)
(alias r1 as scratch-register1)
(alias r2 as scratch-register2)
(alias r3 as scratch-register3)
(alias r32 as global-register0)
(alias r33 as global-register1)
(alias r34 as global-register2)
(alias r35 as global-register3)
(alias r200 as v0)
(alias r201 as v1)
(alias r202 as v2)
(alias r203 as v3)
(alias r128 as internal-register0)
(alias r129 as internal-register1)
(alias r130 as internal-register2)
(alias r131 as internal-register3)
(alias r132 as internal-register4)
(alias r133 as internal-register5)
(alias r134 as internal-register6)
(alias r135 as internal-register7)
(alias internal-register0 as safe-param0)
(alias internal-register1 as safe-param1)
(alias internal-register2 as safe-param2)
(alias internal-register3 as safe-param3)
(alias safe-param0 as spar0)
(alias safe-param1 as spar1)
(alias safe-param2 as spar2)
(alias safe-param3 as spar3)
(alias safe-param0 as sarg0)
(alias safe-param1 as sarg1)
(alias safe-param2 as sarg2)
(alias safe-param3 as sarg3)
(alias internal-register4 as safe-result0)
(alias internal-register5 as safe-result1)
(alias internal-register6 as safe-result2)
(alias internal-register7 as safe-result3)
(alias safe-result0 as sres0)
(alias safe-result1 as sres1)
(alias safe-result2 as sres2)
(alias safe-result3 as sres3)
(alias scratch-register0 as safe-temp0)
(alias scratch-register1 as safe-temp1)
(alias scratch-register2 as safe-temp2)
(alias scratch-register3 as safe-temp3)
(alias safe-temp0 as stmp0)
(alias safe-temp1 as stmp1)
(alias safe-temp2 as stmp2)
(alias safe-temp3 as stmp3)

(alias safe-temp0 as scratch0)
(alias safe-temp1 as scratch1)
(alias safe-temp2 as scratch2)
(alias safe-temp3 as scratch3)


(alias p0 as cond0-true)
(alias p1 as cond0-false)
(alias p2 as cond1-true)
(alias p3 as cond1-false)
(alias p4 as cond2-true)
(alias p5 as cond2-false)
(alias cond0-true as is-number)
(alias cond0-false as is-not-number)
(alias cond1-true as scratch-true)
(alias cond1-false as scratch-false)

(let InternalStackStart be 0xFFFF)
(let DataStackStart be 0x3FFF)
(let CallStackStart be 0x7FFF)
(let SpaceChar be 0x20)
(let WordLength be 62)
(let GetCPort be 0x0001)
(let PutCPort be 0x0002)
(let SeedRandomPort be 0x0003)
(let NextRandomPort be 0x0004)
(let SecondaryStorageBasePort be 0x000A)
(let SecondaryStorageSectorPortOffset be 0x0000)
(let SecondaryStorageIndexPortOffset be 0x0001)
(let SecondaryStorageDataPortOffset be 0x0002)
(section code
         (org 0x0000
              (label Startup
                     (set sp
                          InternalStackStart)
                     (set cs
                          CallStackStart)
                     (set ds
                          DataStackStart)
                     ; TODO: more registers to setup
                     )
              (label DONE               ; top of our control loop
                     (set spar0 WordBuffer)  ; load the front fo the word buffer
                     (bil NumberRoutine)     ; Check and see if we got a number from this input
                     (bic is-not-number
                          ParseWord)         ; If the is-not-number predicate register is true then it must be a word
                     (bi PrintResult)        ; Print the number
                     (label ParseWord
                            (set sarg0
                                 WordBuffer) 
                            (bil WordRoutine) ; read the next word
                            )
                     (label PrintResult
                            (bil Print)       ; print it out
                            (set sarg0
                                 NewlineChar) 
                            (bil Print)       ; add a newline
                            (bi DONE)))

              (label Print
                     ;-----------------------------------------------------------------------------
                     ; Prints a string character by character until we see a \0
                     ; Inputs:
                     ;	sarg0 - what to print out
                     ;-----------------------------------------------------------------------------
                     (set scratch1
                          PutCPort)
                     (label PrintLoop
                            (ld scratch0
                                sarg0)        ; load the current char from memory
                            (eqi scratch-true
                                 scratch-false
                                 scratch0
                                 0x00)        ; first check to see if we should stop printing (zero means stop)
                            (bic scratch-true
                                 PrintDone)
                            (stwo scratch1
                                  scratch0)  ; write it into io memory at the PutC port
                            (addi sarg0
                                  sarg0
                                  0x1)
                            (bi PrintLoop))
                     (label PrintDone
                            (blr)))
              (label AtVerb
                     ;-----------------------------------------------------------------------------
                     ; Treat the top of the stack as an address and loads its contents in place of
                     ; the original address
                     ;-----------------------------------------------------------------------------
                     (pop scratch0
                          ds)
                     (ld scratch0
                         scratch0)
                     (push ds
                           scratch0)
                     (blr))

                        

