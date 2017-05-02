(let io-bus-start be 0xFF000000)
(let rng0 be 0xFF000001)
(let terminate-address be 0xFFFFFFFF)
(let ram3-base-address be 0x03FFFFFF)
(alias io-bus-start as stdin-out)
(section code
         (org 0xFE000000
              (label Startup

                     ; use /dev/ram3 as the ram disk at boot up
                     (set32 sp
                            ram3-base-address)
                     (set32 addr
                            ROM_Messages)
                     (direct-load32 0x00)
                     (copy arg0
                           value)
                     (branch call
                             immediate
                             PrintLine)
                     (direct-load32 0x02)
                     (copy arg0
                           value)
                     (branch call
                             immediate
                             PrintLine)
                     ; setup the seeding routines first
                     (set32 arg0
                            0xFDEDABCD)
                     (branch call
                             immediate
                             SeedRandom0)
                     (indirect-load32 0x08)
                     (copy arg0
                           value)
                     (branch unconditional
                             immediate
                             Shutdown)))
         (org 0xFE010000
              (label Shutdown
                     (set32 addr
                            terminate-address)
                     (set32 value
                            0xD0CEDB00)
                     (direct-store16l)
                     (direct-store16l))
              (label NextRandom0
                     (set32 arg0
                            rng0)
                     (incr arg0)
                     (branch unconditional
                             immediate
                             ReadWord))
              (label SkipRandom0
                     (set32 arg0
                            rng0)
                     (addi arg0
                           0x2)
                     ; don't care what is inside arg1, we just want to write it
                     ; as is!
                     (branch unconditional
                             immediate
                             WriteWord))
              (label SeedRandom0
                     ; arg0 - the new seed value
                     (copy arg1
                           arg0)
                     (set32 arg0
                            rng0)
                     (branch unconditional
                             immediate
                             WriteWord))
              (label ReadChar
                     (set32 arg0
                            stdin-out)
                     (defunc-basic ReadWord
                                   ; arg0 - address to read from
                                   (use-registers (addr value)
                                                  (copy addr
                                                        arg0)
                                                  (direct-load16l)
                                                  (copy result
                                                        value))))
              (defunc-basic ReadDword
                            ; arg0 - address to read from (will read two addresses)
                            (use-registers (addr value)
                                           (copy addr
                                                 arg0)
                                           (direct-load32)
                                           (copy result
                                                 value)))
              (defunc-basic WriteChar
                            ; arg0 - value to write
                            (use-registers (addr value)
                                           (set32 addr
                                                  stdin-out)
                                           (move 0m0001
                                                 value
                                                 arg0)
                                           (direct-store16l)))
              (defunc-basic WriteWord
                            ; arg0 - address to write to
                            ; arg1 - value to write
                            (use-registers (addr value)
                                           (copy addr
                                                 arg0)
                                           (copy value
                                                 arg1)
                                           (direct-store16l)))
              (defunc-basic WriteDword
                            ; arg0 - address to write to
                            ; arg1 - value to write
                            (use-registers (addr value)
                                           (copy addr
                                                 arg0)
                                           (copy value
                                                 arg1)
                                           (direct-store32)))
              (defunc-basic ReadLine
                            ; arg0 - address to store into
                            (use-registers (addr value)
                                           (copy addr
                                                 arg0)
                                           (label ReadLineLoopBody
                                                  (branch call
                                                          immediate
                                                          ReadChar)
                                                  (copy value
                                                        result)
                                                  (is-new-line value)
                                                  (branch conditional
                                                          immediate
                                                          ReadLine_Done)
                                                  (is-null-char value)
                                                  (branch conditional
                                                          immediate
                                                          ReadLine_Done)
                                                  (direct-store16l)
                                                  (incr addr)
                                                  (branch unconditional
                                                          immediate
                                                          ReadLineLoopBody))
                                           (label ReadLine_Done)
                                           (copy result
                                                 arg0)))
              (defunc-basic PrintLine
                            ; arg0 - address to read from
                            (use-registers (addr value)
                                           (copy addr
                                                 arg0)
                                           (label PrintLineLoopBody
                                                  (direct-load16l)
                                                  (is-null-char value)
                                                  (branch conditional
                                                          immediate
                                                          PrintLine_Done)
                                                  (copy arg0
                                                        value)
                                                  (branch call
                                                          immediate
                                                          WriteChar)
                                                  (incr addr)
                                                  (branch unconditional
                                                          immediate
                                                          PrintLineLoopBody))
                                           (label PrintLine_Done)
                                           (set16l arg0
                                                   0x0a)
                                           (branch call
                                                   immediate
                                                   WriteChar)))
              (defunc-basic ArrayLength
                            (use-registers (addr value)
                                           (copy addr
                                                 arg0)
                                           (direct-load32)
                                           (copy result
                                                 value)))
              (defunc-basic ArrayFront
                            (use-registers (addr value)
                                           (copy addr
                                                 arg0)
                                           (addi addr
                                                 0x2)
                                           (direct-load32)
                                           (copy result
                                                 value)))
              (defunc-basic DecodeDword
                            ; arg0 - value
                            ; arg1 - mask
                            ; arg2 - shift
                            (use-registers (addr mask shift value)
                                           (copy addr
                                                 arg0)
                                           (copy mask
                                                 arg1)
                                           (copy shift
                                                 arg2)
                                           (decode)
                                           (copy result
                                                 value)))
              (defunc-basic EncodeDword
                            ; arg0 - value
                            ; arg1 - insertion
                            ; arg2 - mask
                            ; arg3 - shift
                            (use-registers (addr value mask shift)
                                           (copy addr
                                                 arg0)
                                           (copy value
                                                 arg1)
                                           (copy mask
                                                 arg2)
                                           (copy shift
                                                 arg3)
                                           (encode)
                                           (copy result
                                                 addr)))
              (defunc-basic BitIsSet
                            ; arg0 - value
                            ; arg1 - index
                            (use-registers (addr field cond)
                                           (copy addr
                                                 arg0)
                                           (copy field
                                                 arg1)
                                           (bitset)
                                           (copy result
                                                 cond)))
              (defunc-basic BitIsUnset
                            ; arg0 - value
                            ; arg1 - index
                            (use-registers (addr field cond)
                                           (copy addr
                                                 arg0)
                                           (copy field
                                                 arg1)
                                           (bitunset)
                                           (copy result
                                                 cond)))
              )
         )
(section data
         (org 0xFE020000
              (label ScratchStorage
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)
                     (dword 0)))
         (org 0xFE020100
              (label InterruptTable))
         (org 0xFE0A0000
              (label ROM_Strings
                     (label Message0
                            (string "Starting up machine..."))
                     (label Message1
                            (string "Seeding random..."))
                     (label Message2
                            (string "Please wait...."))
                     (label Message3
                            (string "Done"))
                     (label Message4
                            (string "Shutting Down!")))
              (label ROM_Messages
                     (dword Message0)
                     (dword Message1)
                     (dword Message2)
                     (dword Message3)
                     (dword Message4)
                     ))
         ; data tables and such
         )
