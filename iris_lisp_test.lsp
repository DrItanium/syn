(alias r0 as v0)
(alias r1 as v1)
(alias r2 as v2)
(alias r3 as v3)

(section code
         (org 0x0000
              (label startup
                     (set sp to 0xfded)
                     (push r0 onto sp)
                     (pop sp into r0)
                     (add r0 r1 r2 r3)
                     (sub r0 r1 r2 r3)
                     (mul r0 r1 r2 r3 r4)
                     (cube r0 r1)
                     (square r0 r1)
                     (using (save-to sp)
                            (predicates r0 r1 r2 r3)
                            then
                            (add r0 r1 r2)
                            (add r2 r3 r3))
                     )))

(section data
         (org 0x0000
              (word 0x0000))

         (org 0x00FF))


