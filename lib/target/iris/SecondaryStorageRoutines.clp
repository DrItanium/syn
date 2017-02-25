; Sector load store routines
; In the iris system, the pages 0xF0, 0xF1, 0xF2, and 0xF3 are used as a cache
; for the secondary storage. Registers r128, r129, r130, and r131 are used to
; keep track of the sector id inside the secondary storage! Register r132 is
; used as temporary storage during the page commit and extract routines.
(alias r128 as secondary-page-index0)
(alias r129 as secondary-page-index1)
(alias r130 as secondary-page-index2)
(alias r131 as secondary-page-index3)

(alias r132 as page-in-data-memory-base) ; 0xF000, 0xF100, 0xF200, 0xF300, etc
(alias r133 as in-sector-address)
(alias r134 as current-value)
(alias r135 as io-secondary-storage-base-address)

(alias r3 as v3)
(alias r2 as v2)
(alias r1 as v1)
(alias r0 as v0)
(alias sys-true as p14)
(alias sys-false as p15)


(section code
         (label CommitPage
                ; By this point, the sector id has already been set so all we need to do is
                ; store the offset into secondary storage!
                ; r132 is temporary storage and r133 base address of the secondary storage
                ; controller in IO space, that way this code can be used regardless of
                ; controller location
                ; first setup the sector counter
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
        (label CachePage
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



