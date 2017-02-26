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
(alias r4 as scratch-register4)
(alias r5 as scratch-register5)
(alias r6 as scratch-register6)
(alias r7 as scratch-register7)

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

(alias scratch-register0 as scratch0)
(alias scratch-register1 as scratch1)
(alias scratch-register2 as scratch2)
(alias scratch-register3 as scratch3)
(alias scratch-register4 as scratch4)
(alias scratch-register5 as scratch5)
(alias scratch-register6 as scratch6)
(alias scratch-register7 as scratch7)


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


(alias scratch0 as top)
(alias scratch1 as second)

(let InternalStackStart be 0xFFFF)
(let DataStackStart be 0x3FFF)
(let CallStackStart be 0x7FFF)
(let SpaceChar be 0x20)
(let WordLength be 62)
(let TerminatePort be 0x0000)
(let GetCPort be 0x0001)
(let PutCPort be 0x0002)
(let SeedRandomPort be 0x0003)
(let NextRandomPort be 0x0004)
(let SecondaryStorageBasePort be 0x000A)
(let SecondaryStorageSectorPortOffset be 0x0000)
(let SecondaryStorageIndexPortOffset be 0x0001)
(let SecondaryStorageDataPortOffset be 0x0002)

(alias r224 as fixed-purpose-register0)
(alias fixed-purpose-register0 as fetch-deposit-current)
(alias fetch-deposit-current as fdcurr)

(alias r223 as fixed-purpose-register1)
(alias fixed-purpose-register1 as input-pointer)
(alias input-pointer as inptr)

(alias r222 as fixed-purpose-register2)
(alias fixed-purpose-register2 as output-pointer)
(alias output-pointer as outptr)

(alias r221 as fixed-purpose-register3)
(alias fixed-purpose-register3 as zero)

(alias r220 as fixed-purpose-register4)
(alias fixed-purpose-register4 as ascii-space)

(alias r219 as fixed-purpose-register5)
(alias fixed-purpose-register5 as max-word-length)
(alias max-word-length as wlen)

(section code
         (org 0x0000
              (label Startup
                     (set zero
                          0x0000)
                     (set sp
                          InternalStackStart)
                     (set cs
                          CallStackStart)
                     (set ds
                          DataStackStart)
                     ; TODO: more registers to setup
                     )
              (label DONE               ; top of our control loop
                     (set sarg0 
                          WordBuffer)  ; load the front fo the word buffer
                     (bil NUMBER)     ; Check and see if we got a number from this input
                     (bic is-not-number
                          ParseWord)         ; If the is-not-number predicate register is true then it must be a word
                     (bi PrintResult)        ; Print the number
                     (label ParseWord
                            (set sarg0
                                 WordBuffer) 
                            (bil WORD) ; read the next word
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
                            (eq scratch-true
                                scratch-false
                                scratch0
                                zero)         ; first check to see if we should stop printing (zero means stop)
                            (bic scratch-true
                                 PrintDone)
                            (stio scratch1
                                  scratch0)  ; write it into io memory at the PutC port
                            (incr sarg0)
                            (bi PrintLoop))
                     (label PrintDone
                            (blr)))
              (label AtVerb
                     ;-----------------------------------------------------------------------------
                     ; Treat the top of the stack as an address and loads its contents in place of
                     ; the original address
                     ;-----------------------------------------------------------------------------
                     (pop top ds)   ; get the top element
                     (ld top 
                         top)
                     (push ds
                           top)
                     (blr))
              (label EqualsVerb
                     ;-----------------------------------------------------------------------------
                     ; Store second in memory at the address stored in top
                     ;-----------------------------------------------------------------------------
                     (pop top
                          ds) ; get the top element (address)
                     (pop second
                          ds) ; get the second element (value)
                     (st top
                         second) ; data[top] = second
                     (blr))
              (label ERROR
                     ;-----------------------------------------------------------------------------
                     ; Takes in the offending word, an error message, prints them, then clears the
                     ; stacks and then calls DONE.
                     ; Inputs are on r32, r33 for this routine
                     ;   sarg0 - word that did a bad thing
                     ;   sarg1 - error type
                     ;-----------------------------------------------------------------------------
                     ; error routine when something goes wrong!
                     (set ds
                          DataStackStart) ; overwrite the current parameter stack location with the bottom
                     (set cs
                          CallStackStart) ; overwrite the current return stack location with the bottom
                     (bil Print)          ; print the offending word that did the bad thing
                     (move sarg0 
                           sarg1)         ; need to print the error type so setup the arguments
                     (bil Print)          
                     (bi DONE)            ; And we're done!
                     )
              (label Shutdown
                     ; End the program and shutdown the machine
                     (label Die
                            (set scratch0
                                 TerminatePort)
                            (stio scratch0
                                  scratch0)))
              (label WORD
                     ;-----------------------------------------------------------------------------
                     ; WORD: Read the next word in the input
                     ;	sarg0 - pointer to temporary storage to save the current word
                     ;-----------------------------------------------------------------------------
                     (move scratch2 sarg0)                  ; Copy the pointer address to temporary storage so we can mess with it
                     (set scratch3 WORD_read_data)          ; where to jump if we see a space
                     (set scratch4 WORD_reassign_jumps)     ; where to jump to when wanting to handle storage
                     (move scratch5 zero)                   ; The number of characters in the word
                     (set scratch6 GetCPort)                ; The IO Port to load the next character from
                     (label WORD_read_data
                            (ldio scratch0 scratch6)        ; call "getc"
                            (eq scratch-true
                                scratch-false
                                scratch0
                                ascii-space)                ; Are we looking at a space?
                            ; If we are looking at a space then goto WORD_read_data (start the loop up again). This is done to 
                            ; "trim" the input of any number of spaces preceeding it
                            ; If we aren't looking at a space then we need to rebuild the jump table and then 
                            (if scratch-true
                              scratch3
                              scratch4)
                            (label WORD_reassign_jumps
                                   ; this code should only be executed once. We now terminate if we see another space at this point!
                                   (set scratch3
                                        WORD_done_reading)  
                                   (set scratch4
                                        WORD_store_word))
                            (label WORD_store_word
                                   ; the actual save operation, 
                                   (st scratch2 
                                       scratch0)   ; store the extracted character into the character buffer
                                   (incr scratch2) ; next character
                                   (gt scratch-true
                                       scratch-false
                                       scratch5
                                       wlen) ; did we go over the maximum word length?
                                   (bic scratch-true
                                        WORD_too_large_word_ERROR) ; welp, this is fucked get out of here!
                                   (incr scratch5)                 ; increment the word length count since we didn't error out
                                   (bi WORD_read_data)             ; check the next character
                                   ))
                     (label WORD_too_large_word_ERROR
                            ; we need to setup the pointers for error states since we got here!
                            (st scratch2
                                zero)     ; rewrite zero to the end of word entry
                            (set sarg1
                                 errmsg_WORD_too_large_word) ; load the error message
                            (bi ERROR))
                     (label WORD_done_reading
                            (st scratch2
                                zero)                ; put a zero in the current cell, or the last one
                            (blr)))
              (label TRANSLATE_HEX_DIGIT
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; Convert a hex digit to its numeric representation
                     ; sarg0 - the character to inspect
                     ;-----------------------------------------------------------------------------
                     (push sp
                           scratch0)
                     ; first check and see if the number is in 0-9
                     (label DONE_TRANSLATE_HEXDIGIT
                            (pop scratch0
                                 sp)
                            (blr)))
              (label NUMBER
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; Parse an unsigned hexadecimal number and construct a number out of it
                     ;-----------------------------------------------------------------------------
                     (push sp
                           lr)
                     (push sp
                           inptr)
                     (move inptr
                           sarg0)
                     (set scratch0
                          0x58) ; Capital X
                     ; be super lazy and just load all six characters
                     (bil Fetch) ; Load the first character and see if we're looking at a x or X
                     (eq scratch-true
                         scratch-false
                         fdcurr
                         scratch0) ; are we looking at an x?
                     (bic scratch-true
                          HEXPARSE_LOOP) ; we are so parse the number!
                     (label NATURAL
                            (label NATURAL_CHECK_CHARACTER
                                   (bil Fetch)
                                   (subi scratch0
                                         fdcurr 
                                         0x30)
                                   (gti is-number
                                        is-not-number
                                        scratch0
                                        0x9) ; if the result is greater than 9 (unsigned wraparound)
                                   (bic is-not-number
                                        END_NATURAL)
                                   (muli scratch1
                                         scratch1
                                         10)
                                   (add scratch1
                                        scratch1
                                        scratch0)
                                   (bi NATURAL_CHECK_CHARACTER)))
                     (label END_NATURAL
                            (move sres0
                                  scratch1)
                            (bi NUMBER_CHECK))
                     ; we aren't looking at any of that
                     (label HEXPARSE_LOOP
                            ; let's start parsing the hex loop and looking at four digits (must be four digits)
                            (bil Fetch) ; get the most significant digit
                            (subi scratch0
                                  fdcurr
                                  0x30) 
                            (lti is-number
                                 is-not-number
                                 scratch0
                                 0xA)  ; is it a natural digit?
                            (bic is-number
                                 COMBINE_NUMBER) ; it was successful so save it
                            (subi scratch0
                                  fdcurr
                                  0x41) ; see if it is a capital letter
                            (lti is-number
                                 is-not-number
                                 scratch0
                                 0x6)
                            (bic is-not-number
                                 CHECK_FOR_NOT_NUMBER_STATUS) ; we found an upper case digit
                            ; add 10 (0xA) to the number since it is a digit
                            (addi scratch0
                                  scratch0
                                  0xA) ; 
                            (bi COMBINE_NUMBER)
                            (label CHECK_FOR_NOT_NUMBER_STATUS
                                   (bic is-not-number
                                        NUMBER_END))
                            (label COMBINE_NUMBER
                                   (shli scratch1
                                         scratch1
                                         0x4)
                                   (or scratch1
                                       scratch0
                                       scratch1))
                            (bi HEXPARSE_LOOP))
                     (label NUMBER_CHECK
                            (eq is-number
                                is-not-number
                                fdcurr
                                ascii-space)
                            (move sres0
                                  scratch1))
                     (label NUMBER_END
                            (move sres0
                                  scratch1)
                            (pop inptr
                                 sp)
                            (pop lr
                                 sp)
                            (blr)))
              ; TODO: Numeric Output Conversion (3.4.3)
              (label Fetch
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; Load the character defined by the input pointer into the fdcur register, 
                     ; then advance inptr by one
                     ;-----------------------------------------------------------------------------
                     (ld fdcurr
                         inptr)
                     (incr inptr)
                     (blr))
              (label Deposit
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; Store the character in fdcur in the address described by outptr, then 
                     ; advance outptr by one
                     ;-----------------------------------------------------------------------------
                     (st outptr
                         inptr)
                     (incr outptr)
                     (blr))
              (label WordDrop
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; Pop a word off the top of the parameter stack
                     ;-----------------------------------------------------------------------------
                     (pop top
                          sp)
                     (blr))

              (label WordDup
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; duplicate the top of the parameter stack
                     ;-----------------------------------------------------------------------------
                     (pop top
                          ds)
                     (push ds 
                           top)
                     (push ds 
                           top)
                     (blr))

              (label WordSwap
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; swap the top of the parameter stack with the lower word
                     ;-----------------------------------------------------------------------------
                     (pop top
                          ds) 
                     (pop second
                          ds)
                     (push ds
                           top)
                     (push ds
                           second)
                     (blr))

              (label WordOver
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; push the lower word on the stack onto the stack (a b -- a b a)
                     ;-----------------------------------------------------------------------------
                     (pop top
                          ds) ; get the top and second
                     (pop second
                          ds)
                     (push ds 
                           second) ; push lower
                     (push ds
                           top)    ; push top
                     (push ds
                           second) ; push lower
                     (blr))

              )
         )
