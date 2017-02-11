(alias r32 as arg0)
(alias r33 as arg1)
(alias r34 as arg2)
(alias r35 as arg3)
(alias r36 as arg4)
(alias r64 as ret0)
(alias r65 as ret1)
(alias r96 as temp0)
(alias r97 as temp1)
(alias r98 as temp2)
(section code
         (func DecodeBits
               ; decode a series of bits out of a word using the following formula
               ; (input & bitmask) >> shiftcount
               ;
               ; arg0 -> input
               ; arg1 -> bitmask
               ; arg2 -> shiftcount
               ; ret0 -> decoded value from input
               (and ret0
                    arg0
                    arg1)
               (shr ret0
                    ret0
                    arg2))
          (func EncodeBits
                ; encode a series of bits into another word using the following formula
                ; (input & ~bitmask) | ((value << shiftcount) & bitmask)
                ; 
                ; arg0 -> input
                ; arg1 -> value
                ; arg2 -> bitmask
                ; arg3 -> shiftcount
                ; ret0 -> input with value encoded into it
                (using (save-to sp)
                       (temp0 temp1)
                       (not temp0
                            arg2) ; invert the bitmask so we can make a hole in the input to store our value
                       (and temp1
                            arg0
                            temp0) ; carve a hole out of the input value
                       (shl temp0
                            arg1
                            arg3) ; shift the value by the shiftcount to position it
                       (and temp0
                            temp0
                            arg2) ; mask the value with the bitmask
                       (or ret0
                           temp1
                           temp0))) ; insert the encoded value into the input, save it to the return register

                           )



