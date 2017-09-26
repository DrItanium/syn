(batch* cisc0_machine.clp)
(reset)
(parse-and-install-asm [c0]
                       ReadLine.cisc0)
(send [c0] run)
