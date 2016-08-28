DefFlags(MemoryFlags)
	Component(type,        0b00000111, 0, MemoryOperation)
	Component(bitmask,     0b11110000, 4, byte)
	Component(errorState, 0b00001000, 3, bool)
EndDefFlags(MemoryFlags)

