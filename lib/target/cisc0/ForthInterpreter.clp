(let io-bus-start be 0xFF000000)
(let rng0 be 0xFF000001)
(let terminate-address be 0xFFFFFFFF)
(let ram3-base-address be 0x03FFFFFF)
(let ram3-half-way be 0x037FFFFF)
(alias ram3-base-address as call-stack-bottom)
(alias ram3-half-way as data-stack-bottom)
(alias io-bus-start as stdin-out)
(section code
         (org 0xFE000000
              (label Startup
                     ; use /dev/ram3 as the ram disk at boot up
                     (set32 cs
                            call-stack-bottom)
                     (set32 ds
                            data-stack-bottom)
                     ; shutdown
                     (set32 addr
                            terminate-address)
                     (set32 value
                            0xD0CEDB00)
                     (direct-store16l))))
