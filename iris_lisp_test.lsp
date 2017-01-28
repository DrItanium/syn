(alias r0 as v0)
(alias r1 as v1)
(alias r2 as v2)
(alias r3 as v3)
(alias r255 as sp)
(alias r254 as predicates)

(section code
         (org 0x0000
              (label startup
                     (set sp to 0xfded)
                     (push r0 onto sp)
                     (pop sp into r0)
                     )))

(section data
         (org 0x0000
              (word 0x0000))

         (org 0x00FF))


