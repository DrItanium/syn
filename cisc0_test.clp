; a simple test translation program
; Setup the memory addresses into several sections
; the upper four bits
(let ZeroAddress be 0x00000000)
(let SystemMemoryStart be 0x00F00000)
(let StackStart be 0x00FFFFFF)
(let Zero be ZeroAddress)

(section code
         (org ZeroAddress
              (set32 sp
                     StackStart)))
