This project contains multiple cpu cores for research purposes. All of them are
called iris with the generation starting at 16 because it was originally a
16-bit cpu and I am nothing but original :).

iris16 - Original RISC/CISC 16-bit harvard architecture CPU, designed with 64k
		 instruction, data, and stack areas. Has 256 registers
iris17 - 32-bit version of iris16 with 8 hardware threads which execute in an
		 interleaved fashion (each thread executes every eighth cycle). Has 256
		 registers.
iris18 - A 32-bit unconventional design with only 16 registers with some registers 
         having specific purposes (there was a dedicated address and value register)
		 Objective was to explore how easy it was to have a design with GPRs
		 that have specific purposes. Made the code far more dense. Used a
		 variable length instruction encoding.
iris19 - A 64-bit architecture with a variable length instruction encoding. 
		 Unlike previous designs, this core uses the upper two bits of the
		 register fields to denote if it is a register operation, stack
		 operation, or memory operation. On a memory operation it would be a
		 load if the register was a source register. A store would occur if the
		 register was in the destination position. A stack operation is a push
		 in the destination position and a pop in the source positions.
		 If tagged register then it would act like a normal three argument RISC
		 like cpu. The machine only has 64 registers because of this design.
		 For instance, the move operation is now extremely powerful. Move dest,
		 src is now used for load, store, push, pop, and a normal move. Because
		 this encoding is part of the register encoding, all instructions take
		 advantage of this feature without any special extra logic.
iris20 - An extension of iris19 which gets rid of the variable length
         architecture and replaces it with 64-bit instructions called
		 containers. Each container can hold two atoms (a 32-bit instruction)
		 or a single 64-bit molecule. The instruction pointer operates on
		 64-bit values and thus can execute two instructions per clock. Uses
		 the 64 register layout and the tagged operations to increase density.
		 The other objective of this architecture was to see how tenable it is
		 to write the assembler and code generator in CLIPS. The bootstrap was
		 very successful and will influence future iris designs. No longer will
		 an explicit assembler be required.
