/**
 * Define the initial program load for the iris20 architecture inside of C++
 * itself. This will construct binary as its output
 */
#include "iris20.h"

namespace iris20 {
// define the encoding operations!
InstructionMolecule molecule(InstructionAtom first, InstructionAtom second) {
    return encodeSecondAtom( encodeFirstAtom( encodeMoleculeContainsOneInstruction(0 , false), first), second);
}
byte encodeRegisterOperation(byte registerIndex, bool memory, bool stack) {
    auto descript = SectionType::Undefined;
    if (memory && stack) {
        throw iris::Problem("Register operation can't be both memory and stack");
    } else if (memory) {
        descript = SectionType::Memory;
    } else if (stack) {
        descript = SectionType::Stack;
    } else {
        descript = SectionType::Register;
    }
    return encodeSectionDescriptor( encodeSectionIndex(0, registerIndex), descript);
}
byte stackOperation(byte registerIndex) { return encodeRegisterOperation(registerIndex, false, true); }
byte memoryOperation(byte registerIndex) { return encodeRegisterOperation(registerIndex, true, false); }
byte registerOperation(byte registerIndex) { return encodeRegisterOperation(registerIndex, false, false); }

InstructionAtom returnInstruction(byte destination) {
    return encodeDestination( encodeOperation(0, Operation::BranchUnconditionalRegister), destination);
}

InstructionAtom returnFromStack(byte stackPointer) {
    return returnInstruction(stackOperation(stackPointer));
}

InstructionAtom returnFromMemory(byte memoryPointer) {
    return returnInstruction(memoryOperation(memoryPointer));
}

InstructionAtom returnWithRegister(byte registerIndex) {
    return returnInstruction(registerOperation(registerIndex));
}

InstructionAtom returnToLinkRegister() {
    return returnWithRegister(static_cast<byte>(ArchitectureConstants::LinkRegisterIndex));
}

InstructionAtom move(byte dest, byte src) noexcept {
    return encodeSource0(encodeDestination(encodeOperation(0, Operation::Move), dest), src);
}
InstructionAtom store(byte destReg, byte src) {
    return move(memoryOperation(destReg), src);
}
InstructionAtom load(byte dest, byte srcReg) {
    return move(dest, memoryOperation(srcReg));
}

InstructionAtom push(byte stackPointer, byte value) {
    return move(stackOperation(stackPointer), value);
}

InstructionAtom pop(byte stackPointer, byte destination) {
    return move(destination, stackOperation(stackPointer));
}

InstructionAtom saveLinkRegister(byte stackPointer) {
    return push(stackPointer, registerOperation(static_cast<byte>(ArchitectureConstants::LinkRegisterIndex)));
}

InstructionAtom restoreLinkRegister(byte stackPointer) {
    return pop(stackPointer, registerOperation(static_cast<byte>(ArchitectureConstants::LinkRegisterIndex)));
}


} // end namespace iris20

int main() {

}
