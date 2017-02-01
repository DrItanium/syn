(alias r0 as v0)
(alias r1 as v1)
(alias r2 as v2)
(alias r3 as v3)
(let ZeroAddress be 0x0000)

(section code
         (org ZeroAddress
              (label startup
                     (set sp to 0xfded)
                     (push r0 onto sp)
                     (pop sp into r0)
                     (add r0 r1 r2 r3)
                     (sub r0 r1 r2 r3)
                     (mul r0 r1 r2 r3 r4)
                     (cube r0 r1)
                     (square r0 r1)
                     (push lr into sp)
                     (pop sp into lr)
                     (memswap r32 r33)
                     (func foo
                           (using (save-to sp)
                                  (predicates r0 r1 r2 r3)
                                  (add r0 r1 r2)
                                  (add r2 r3 r3)))
                     )
                     (swap lr lr)
              ))

(section data
         (org 0x0000
              (word 0x0000))

         (org 0x00FF))


