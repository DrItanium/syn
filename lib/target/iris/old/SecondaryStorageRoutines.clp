; Sector load store routines
; This is the code that handles IrisCore secondary storage which uses an iris core itself!
; It is fairly simple, the only differences is that 

(alias r0 as fixed-register0)
(alias r1 as fixed-register1)
(alias r2 as fixed-register2)
(alias r3 as fixed-register3)
(alias r4 as fixed-register4)
(alias r5 as fixed-register5)
(alias r6 as fixed-register6)
(alias r7 as fixed-register7)
(alias r8 as fixed-register8)
(alias r9 as fixed-register9)
(alias r10 as fixed-register10)
(alias r11 as fixed-register11)
(alias r12 as fixed-register12)
(alias r13 as fixed-register13)
(alias r14 as fixed-register14)
(alias r15 as fixed-register15)
(alias r16 as fixed-register16)
(alias r17 as fixed-register17)
(alias r18 as fixed-register18)
(alias r19 as fixed-register19)
(alias r20 as fixed-register20)
(alias r21 as fixed-register21)
(alias r22 as fixed-register22)
(alias r23 as fixed-register23)
(alias r24 as fixed-register24)
(alias r25 as fixed-register25)
(alias r26 as fixed-register26)
(alias r27 as fixed-register27)
(alias r28 as fixed-register28)
(alias r29 as fixed-register29)
(alias r30 as fixed-register30)
(alias r31 as fixed-register31)

(alias v0 as fixed-register0)
(alias v1 as fixed-register1)
(alias v2 as fixed-register2)
(alias v3 as fixed-register3)
(alias return0 as fixed-register4)
(alias return1 as fixed-register5)
(alias return2 as fixed-register6)
(alias return3 as fixed-register7)
(alias arg0 as fixed-register8)
(alias arg1 as fixed-register9)
(alias arg2 as fixed-register10)
(alias arg3 as fixed-register11)
(alias arg4 as fixed-register12)
(alias arg5 as fixed-register13)
(alias arg6 as fixed-register14)
(alias arg7 as fixed-register15)
(alias scratch0 as fixed-register16)
(alias scratch1 as fixed-register17)
(alias scratch2 as fixed-register18)
(alias scratch3 as fixed-register19)
(alias scratch4 as fixed-register20)
(alias scratch5 as fixed-register21)
(alias scratch6 as fixed-register22)
(alias scratch7 as fixed-register23)
(alias io-sector as fixed-register24)
(alias io-offset as fixed-register25)
(alias io-result as fixed-register26)
(alias io-ready as fixed-register27)
(alias global0 as fixed-register28)
(alias global1 as fixed-register29)
(alias global2 as fixed-register30)
(alias global3 as fixed-register31)

(alias in-sector-address as scratch0)
(alias current-address as scratch1)
(alias io-secondary-storage-base-address as arg0)
(alias page-in-data-memory-base as arg1)

(alias sys-false as p15)
(alias sys-true as p14)
(let RawStorageBase be 0x000A)
(let CacheBase be 0x8000)
(section code
         (label Startup
                ; setup the stack as we only have a single stack to work with!
                (set sp 
                     0xFFFF)
                ; usually it is a good idea to load several sectors into memory
                ; cache 32 pages into memory so that we have extra data 
                (set global1
                     CacheBase)
                (set global2
                     0x00)
                (label Startup_Load192Pages
                       (set io-secondary-storage-base-address 
                            RawStorageBase)
                       (move page-in-data-memory-base
                             global1)
                       (bil CacheSector)
                       (addi global2
                             global2
                             0x01)
                       (eqi sys-true
                            sys-false
                            global2
                            0x20)
                       (bic sys-true
                            DoneWithInitialCache)
                       (set scratch0
                            0x100)
                       (add global1
                            global1
                            scratch0)
                       (bi Startup_Load192Pages))
                (label DoneWithInitialCache
                       )
                )
         (label Terminate
                (set scratch0
                     0x0000)
                (stio scratch0
                      scratch0))
         (label CommitSector
                (set in-sector-address 
                     0x0000)
                (label CommitWord
                       ; next load the current word into a register from data memory
                       (ld current-value
                           page-in-data-memory-base)
                       ; tell the io controller to point the secondary offset to the in-sector-address
                       (stiowo io-secondary-storage-base-address
                               in-sector-address
                               0x1)
                       ; store the value to secondary storage
                       (stiowo io-secondary-storage-base-address
                               current-value
                               0x2)
                       ; add one to the in-sector-address and do it again!
                       (addi in-sector-address
                             in-sector-address
                             0x1)
                       (addi page-in-data-memory-base
                             page-in-data-memory-base
                             0x01))
                ; keep going as long as the in-sector-address is less than or equal to 255
                (lei sys-true
                     sys-false
                     in-sector-address
                     0xFF)
                (bic sys-true
                     CommitWord)
                (blr))
         (label CacheSector
                ; by this point we already have the target sector set and the page-in-data-memory-base
                (set in-sector-address
                     0x0000)
                (label CacheWord
                       (stiowo io-secondary-storage-base-address
                               in-sector-address
                               0x1)
                       (ldiowo current-value
                               io-secondary-storage-base-address
                               0x2)
                       (st page-in-data-memory-base
                           current-value)
                       (addi in-sector-address
                             in-sector-address
                             0x1)
                       (addi page-in-data-memory-base
                             page-in-data-memory-base
                             0x1))
                (lei sys-true
                     sys-false
                     in-sector-address
                     0xFF)
                (bic sys-true
                     CacheWord)
                (blr)))



