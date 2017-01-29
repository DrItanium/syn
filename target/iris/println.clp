(alias r32 as arg0)
(alias r33 as arg1)
(alias r96 as temp0)
(section code
         (func PrintCString
               (using (save-to sp)
                      (predicates temp0) 
                      ; arg0 contains the pointer to the front of the data section
                      (label PrintCString_Loop
                             (ld temp0 
                                 arg0)
                             (eqi p0 
                                  p1 
                                  temp0 
                                  0x00)
                             (bc p0 
                                 PrintCString_Done)
                             (putc temp0)
                             (incr arg0
                                   arg0)
                             (bi PrintCString_Loop))
                      (label PrintCString_Done)))
         (func PrintNewLine
               (using (save-to sp)
                      (temp0)
                      (set temp0
                           0x000a)
                      (putc temp0)))

         (func PrintCStringLine
               ; arg0 is the pointer as before
               (using (save-to sp)
                      (lr)
                      (call PrintCString)
                      (call PrintNewLine)))
         )




