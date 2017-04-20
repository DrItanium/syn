(alias 0xFF000000 as io-bus-start)
(alias io-bus-start as stdin-out)
(section code
         (org 0xFE010000
              (label ReadChar
                     (push32 addr)
                     (push32 value)
                     (set32 addr
                            stdin-out)
                     (direct-load16l)
                     (copy result
                           value)
                     (pop32 value)
                     (pop32 addr)
                     (return))
              (label WriteChar
                     (push32 addr)
                     (push32 value)
                     (set32 value
                            arg0)
                     (set32 addr
                            stdin-out)
                     (direct-store16l)
                     (pop32 value)
                     (pop32 addr)
                     (return))))




