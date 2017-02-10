(let ZeroAddress be 0x0000)
(let ErrorDispatchVector be 0x00FF)
(let Unused0DispatchVector be 0x00FE)
(let Unused1DispatchVector be 0x00FD)
(let CustomInstructionDispatchVector be 0x00FC)
(let HandleDivideByZeroVector be 0x00FB)
(let HandlerCodeBase be 0xD000)
(let TerminateAddress be 0x0000)
(let GetCAddress be 0x0001)
(let PutCAddress be 0x0002)
(let ErrorStack be 0x0000)
(let UnusedGroup0Handler be 0x00FE)
(let UnusedGroup1Handler be 0x00FD)
(let CustomInstructionHandler be 0x00FC)
(alias r3 as v3)
(alias r2 as v2)
(alias r1 as v1)
(alias r0 as v0)
(alias r255 as error-id)
(alias r254 as group)
(alias r253 as operation)
(alias r252 as destination)
(alias r251 as source0)
(alias r250 as source1)
(alias r249 as immediate)
(alias r248 as etmp0)
(alias r247 as etmp1)
(alias r246 as etmp2)
(alias r245 as etmp3)
(alias r244 as etmp4)
(alias r243 as epreds)
(alias r242 as estack)

(section code
         (org ZeroAddress
              ; setup the handler base and other tables
              (set v0
                   ErrorDispatchVector)
              (memset v0
                      HandlerCodeBase)
              (set v0
                   Unused0DispatchVector)
              (memset v0
                      CrashCPUInHandler)
              (set v0
                   Unused1DispatchVector)
              (memset v0
                      CrashCPUInHandler)
              (set v0
                   CustomInstructionDispatchVector)
              (memset v0
                      CrashCPUInHandler)
              (divi v0
                    v0
                    0x0)
              )
         (org HandlerCodeBase
              (label HandlerDiagnoseIssue
                     ; save the predicate registers to the emergency predicate
                     ; storage
                     (set estack
                          ErrorStack)
                     (move epreds
                           predicates)
                     (eqi p0 p1
                          error-id
                          0x5)
                     (bic p0
                          HandleCustomInstruction)
                     (eqi p0 p1
                          error-id
                          0x3)
                     (bic p0
                          HandleDivideByZero)
                     (bi CrashCPUInHandler)
                     (label HandlerDone)
                     (move predicates
                           epreds)
                     (rfe))
              (label HandleCustomInstruction
                     (gti p0 p1
                          group
                          0x4) ; make sure that we are looking at the upper 3 groups
                     (bic p1
                          CrashCPUInHandler)
                     (set etmp0
                          ErrorDispatchVector)
                     (subi etmp1
                           group
                           0x4) ; compute the offset
                     (sub etmp0
                          etmp0
                          etmp1) ; compute the address in the dispatch vector
                     (ld etmp2
                         etmp0)
                     (bl etmp2)
                     ; now we can go through and
                     (bi HandlerDone))
              (label HandleDivideByZero
                     ;TODO: figure out how the handler should work at this
                     ;      point
                     (bi CrashCPUInHandler)
                     (bi HandlerDone))
              (label CrashCPUInHandler
                     (divi r248
                           r248
                           0x0) ; terminate the cpu at this point
                     )
              ))
