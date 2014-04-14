This project contains multiple cpu cores for research purposes.
In the src folder you will find folders corresponding to the different cpu 
types. 

The primary core being worked on right now is the iris core. It is a 16-bit 
RISC/CISC cpu core with a ton of registers and a fixed memory space.

Branches
   - master [![Build Status](https://travis-ci.org/DrItanium/theoretical-architecture.svg?branch=)](https://travis-ci.org/DrItanium/theoretical-architecture)
   - iris [![Build Status](https://travis-ci.org/DrItanium/theoretical-architecture.svg?branch=)](https://travis-ci.org/DrItanium/theoretical-architecture)


Other cores:

phoenix - the original theoretical-architecture risc core (64-bit and tons of
          registers).

The phoenix core is the original core that started all of this (I implemented 
it in four hours during December 2013). It has not been updated in some time
but it is a good source of reference. It requires plan9port.
