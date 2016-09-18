DefFlags(MemoryFlags)
	Component(type,        0b00000011, 0, MemoryOperation)
	Component(bitmask,     0b11110000, 4, byte)
	Component(errorState,  0b00001100, 2, bool)
EndDefFlags(MemoryFlags)

