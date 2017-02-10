(let ZeroAddress be 0x0000)
(let ErrorDispatchVector be 0xFFFF)
(let HandlerCodeBase be 0xD000)
(let TerminateAddress be 0x0000)
(let GetCAddress be 0x0001)
(let PutCAddress be 0x0002)
(let ErrorStack be 0xFF00)
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
              ; setup the handler base
              (set v0
                   HandlerCodeBase)
              (set v1
                   ErrorDispatchVector)
              (store v0
                     into
                     v1)
              )
         (org HandlerCodeBase
              (label HandlerDiagnoseIssue
                     ; save the predicate registers to the emergency predicate
                     ; storage
                     (set estack
                          ErrorStack)
                     (using (save-to estack)
                            (predicates)
                            (eqi p0 p1
                                 error-id
                                 0x5)
                            (bci p0
                                 HandleCustomInstruction)
                            (eqi p0 p1
                                 error-id
                                 0x3)
                            (bci p0
                                 HandleDivideByZero)
                            (divi r248
                                  r248
                                  0x0) ; force a divide by zero to terminate the CPU
                            (label HandlerDone))
                     (rfe))
              (label HandleCustomInstruction

                     (bi HandlerDone))
              (label HandleDivideByZero
                     ;TODO: figure out how the handler should work at this
                     ;      point
                     (divi r248
                           r248
                           0x0) ; terminate the cpu at this point

                     (bi HandlerDone))
              ))
