; there are 8 temporary registers internally

;(alias r0 r1)
(alias v0 r0)
(alias v1 r1)
(alias v2 r2)
(alias v3 r3)
(alias a0 r32)
(alias a1 r33)
(alias a2 r34)
(alias a3 r35)
(alias a4 r36)
(alias a5 r37)
(alias a6 r38)
(alias a7 r39)
(alias t0 r96)
(alias t1 r97)
(alias t2 r98)
(alias t3 r99)
(alias t4 r100)
(alias t5 r101)
(alias t6 r102)
(alias t7 r103)


(alias return0 r64)
(alias return1 r65)
(alias return2 r66)
(alias return3 r67)

(alias sp r255)
(alias core-id r224)
(alias m0 r225)
(alias m1 r226)
(alias csp r227)
(alias dsp r228)
(alias usp r229)
(macro andfi
       (?dest ?src ?immediate)
       (set v0
            ?immediate)
       (and ?dest
            ?src
            v0))

(macro addfi
       (?dest ?src ?immediate)
       (set v0
            ?immediate)
       (add ?dest
            ?src
            v0))

(macro fnret
       ()
       (jlr))

(macro push-lr
       (?stack)
       (mflr v0)
       (push ?stack
             v0))

(macro pop-lr
       (?stack)
       (pop v0
            ?stack)
       (mtlr v0))
(macro ldioi
       (?dest ?immediate)
       (set v0
            ?immediate)
       (ldio ?dest
             v0))
(macro stioi
       (?addr ?immediate)
       (set v0
            ?immediate)
       (stio ?addr
             v0))

(section code
         (label print-code-as-hex
                (push-lr sp)
                (push sp
                      a0) ; save r32 to the stack since it is the lower half
                (move a0 
                      a1) ; r33 is now r32 since it is the upper half, needs to be printed first
                (call print-register-as-hex)
                (pop a0
                     sp) ; restore arg0
                (call print-register-as-hex)
                (pop-lr sp)
                (fnret))
         (label translate-lower4-to-hex
                ; translate the lower 4 bits of the register
                (andfi a0
                       a0
                       0x000F)
                (addfi a0
                       a0
                       hex-print-translation-table)
                (load return0
                      a0)
                (fnret))
         (label print-register-as-hex
                (push-lr sp)
                (push sp
                      t0)
                (push sp
                      t1)
                (push sp
                      t2)
                (push sp
                      t3)
                (shift-right t0 
                             a0 
                             12)
                (shift-right t1
                             a0
                             8)
                (shift-right t2
                             a0
                             4)
                (move t3
                      a0)
                (move a0
                      t0)
                (call print-hex-character-from-lower4)
                (move a0
                      t1)
                (call print-hex-character-from-lower4)
                (move a0
                      t2)
                (call print-hex-character-from-lower4)
                (move a0
                      t3)
                (call print-hex-character-from-lower4)
                (pop t3
                     sp)
                (pop t2
                     sp)
                (pop t1
                     sp)
                (pop t0
                     sp)
                (pop-lr sp)
                (fnret))
         (label print-hex-character-from-lower4
                (push-lr sp)
                (call translate-lower4-to-hex)
                (pop-lr sp)
                (move a0
                      return0)
                (goto print-character))
         (label print-newline
                (set a0 0xa)
                (goto print-character))
         (label print-character
                (set a1 0x0002)
                (goto io-write))
         (label terminate-execution
                (set a1 0x0000)
                (goto io-write))
         (label read-character
                (set a0 0x0003)
                (goto io-read))
         (label hex-char-to-number
                (andfi a0
                       a0
                       0x00FF) ; make sure we have not overflowed
                (addfi a0
                       a0
                       hex-print-translation-table) ; add the base translation table offset
                (load return0
                      a0) ; load the corresponding offset from memory 
                (fnret))
         (label io-read
                (ldio return0
                      a0)
                (fnret))
         (label io-write
                (stio a1 
                      a0)
                (fnret))
         )

(section data
         (org 0xF100
              (label hex-print-translation-table
                     (word 0x30)
                     (word 0x30)
                     (word 0x31)
                     (word 0x32)
                     (word 0x33)
                     (word 0x34)
                     (word 0x35)
                     (word 0x36)
                     (word 0x37)
                     (word 0x38)
                     (word 0x39)
                     (word 0x41)
                     (word 0x42)
                     (word 0x43)
                     (word 0x44)
                     (word 0x45)
                     (word 0x46)))
         ; 0xF110
         (org 0xF130
              (word 0x0)
              (word 0x1)
              (word 0x2)
              (word 0x3)
              (word 0x4)
              (word 0x5)
              (word 0x6)
              (word 0x7)
              (word 0x8)
              (word 0x9))
         (org 0xF141
              (word 0xA)
              (word 0xB)
              (word 0xC)
              (word 0xD)
              (word 0xE)
              (word 0xF)))
