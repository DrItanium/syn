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

InstructionAtom zeroArgumentOperation(Operation op) noexcept {
    return encodeOperation(0, op);
}
InstructionAtom oneArgumentOperation(Operation op, byte dest) noexcept {
    return encodeDestination(zeroArgumentOperation(op), dest);
}
InstructionAtom twoArgumentOperation(Operation op, byte dest, byte src0) noexcept {
    return encodeSource0(oneArgumentOperation(op, dest), src0);
}
InstructionAtom threeArgumentOperation(Operation op, byte dest, byte src0, byte src1) noexcept {
    return encodeSource1(twoArgumentOperation(op, dest, src0), src1);
}

InstructionAtom returnInstruction(byte destination) {
    return oneArgumentOperation(Operation::BranchUnconditionalRegister, destination);
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
    return twoArgumentOperation(Operation::Move, dest, src);
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

InstructionAtom set16(byte destination, InstructionImmediate immediate) noexcept {
    return encodeImmediate(oneArgumentOperation(Operation::Set16, destination), immediate);
}

InstructionAtom push16(byte stackPointer, InstructionImmediate immediate) noexcept {
    return set16(stackOperation(stackPointer), immediate);
}

InstructionAtom push16(InstructionImmediate immediate) noexcept {
    return set16(static_cast<byte>(ArchitectureConstants::StackPointerIndex), immediate);
}

InstructionAtom store16(byte address, InstructionImmediate immediate) noexcept {
    return set16(memoryOperation(address), immediate);
}
#define makeThreeAddressOperation(title, fragment) \
    InstructionAtom title ( byte dest, byte s0, byte s1, bool imm = false) noexcept { \
        return threeArgumentOperation( imm ? Operation:: fragment ## Immediate : Operation:: fragment , dest, s0, s1); \
    }
makeThreeAddressOperation(add, Add);
makeThreeAddressOperation(sub, Sub);
makeThreeAddressOperation(mul, Mul);
makeThreeAddressOperation(div, Div);
makeThreeAddressOperation(rem, Rem);
makeThreeAddressOperation(shiftLeft, ShiftLeft);
makeThreeAddressOperation(shiftRight, ShiftRight);
makeThreeAddressOperation(eq, Eq);
makeThreeAddressOperation(neq, Neq);
makeThreeAddressOperation(lt, LessThan);
makeThreeAddressOperation(gt, GreaterThan);
makeThreeAddressOperation(le, LessThanOrEqualTo);
makeThreeAddressOperation(ge, GreaterThanOrEqualTo);
#undef makeThreeAddressOperation

InstructionAtom increment(byte destination, byte src) noexcept { return add(destination, src, 1, true); }
InstructionAtom increment(byte destination) noexcept { return increment(destination, destination); }
InstructionAtom decrement(byte destination, byte src) noexcept { return sub(destination, src, 1, true); }
InstructionAtom decrement(byte destination) noexcept { return decrement(destination, destination); }
InstructionAtom doubleVal(byte destination, byte src) noexcept { return shiftLeft(destination, src, 1, true); }
InstructionAtom doubleVal(byte destination) noexcept { return doubleVal(destination, destination); }
InstructionAtom halveVal(byte destination, byte src) noexcept { return shiftRight(destination, src, 1, true); }
InstructionAtom halveVal(byte destination) noexcept { return halveVal(destination, destination); }


} // end namespace iris20

int main() {

}
