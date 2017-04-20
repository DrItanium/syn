(let io-bus-start be 0xFF000000)
(let rng0 be 0xFF000001)
(alias io-bus-start as stdin-out)
(section code
         (org 0xFE010000
              (label NextRandom0
                     (set32 arg0
                            rng0)
                     (incr arg0)
                     (branch unconditional
                             immediate
                             ReadWord))
              (label ReadChar
                     (set32 arg0
                            stdin-out)
                     (label ReadWord
                            ; arg0 - address to read from
                            (push32 addr)
                            (push32 value)
                            (copy addr
                                  arg0)
                            (direct-load16l)
                            (copy result
                                  value)
                            (pop32 value)
                            (pop32 addr)
                            (return)))
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
              (label WriteChar
                     ; arg0 - value to write
                     (copy arg1
                           arg0)
                     (set32 arg0
                            stdin-out)
                     (label WriteWord
                            ; arg0 - address to write to
                            ; arg1 - value to write
                            (push32 addr)
                            (push32 value)
                            (copy addr
                                  arg0)
                            (copy value
                                  arg1)
                            (direct-store16l)
                            (pop32 value)
                            (pop32 addr)
                            (return)))
              (label ReadDword
                     ; arg0 - address to read from (will read two addresses)
                     (push32 addr)
                     (push32 value)
                     (copy addr
                           arg0)
                     (direct-load32)
                     (copy result
                           value)
                     (pop32 value)
                     (pop32 addr)
                     (return))
              (label WriteDword
                     ; arg0 - address to write to
                     ; arg1 - value to write
                     (push32 addr)
                     (push32 value)
                     (copy addr
                           arg0)
                     (copy value
                           arg1)
                     (direct-store32)
                     (pop32 value)
                     (pop32 addr)
                     (return))

              (label ReadLine
                     ; arg0 - address to store into
                     (push32 addr)
                     (push32 value)
                     (set32 addr
                            arg0)
                     (label ReadLineLoopBody
                            (branch call
                                    immediate
                                    ReadChar)
                            (copy value
                                  result)
                            (direct-store16l)
                            (incr addr)
                            (is-new-line value)
                            (not cond)
                            (branch conditional
                                    immediate
                                    ReadLineLoopBody)
                            (is-null-char value)
                            (not cond)
                            (branch conditional
                                    immediate
                                    ReadLineLoopBody))
                     (copy result
                           arg0)
                     (pop32 value)
                     (pop32 addr)
                     (return))
              (label WriteLine
                     ; arg0 - address to read from
                     (push32 addr)
                     (push32 value)
                     (set32 addr
                            arg0)
                     (label WriteLineLoopBody
                            (direct-load16l)
                            (copy arg0
                                  value)
                            (branch call
                                    immediate
                                    WriteChar)
                            (incr addr)
                            (is-null-char value)
                            (not cond)
                            (branch conditional
                                    immediate
                                    ReadLineLoopBody))
                     (set16l arg0
                             0x0a)
                     (branch call
                             immediate
                             WriteChar)
                     (pop32 value)
                     (pop32 addr)
                     (return))
              )
         )







