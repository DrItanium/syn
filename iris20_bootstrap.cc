/**
 * Define the initial program load for the iris20 architecture inside of C++
 * itself. This will construct binary as its output
 */
#include "iris20.h"
#include <vector>
#include <iostream>
#include <bitset>

namespace iris20 {
    using InstructionBiCompound = std::tuple<InstructionMolecule, InstructionMolecule>;
    using InstructionTriCompound = std::tuple<InstructionMolecule, InstructionMolecule, InstructionMolecule>;
    constexpr byte temporaryRegister0 = 0;
    constexpr byte temporaryRegister1 = 1;
    constexpr byte temporaryRegister2 = 2;
    constexpr byte temporaryRegister3 = 3;
    constexpr byte temporaryRegister4 = 4;


    // define the encoding operations!
    InstructionMolecule molecule(InstructionAtom first, InstructionAtom second) {
        return encodeMoleculeContainsOneInstruction(encodeSecondAtom( encodeFirstAtom(0, first), second), false);
    }
    InstructionMolecule molecule(InstructionMolecule molecule) noexcept {
        return encodeMoleculeContainsOneInstruction(molecule, true);
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

    InstructionMolecule zeroArgumentMolecule(Operation op) noexcept {
        return molecule(encodeMoleculeOperation(0, op));
    }

    InstructionMolecule oneArgumentMolecule(Operation op, byte dest) noexcept {
        return encodeMoleculeDestination(zeroArgumentMolecule(op), dest);
    }

    InstructionMolecule twoArgumentMolecule(Operation op, byte dest, byte s0) noexcept {
        return encodeMoleculeSource0(oneArgumentMolecule(op, dest), s0);
    }

    InstructionMolecule threeArgumentMolecule(Operation op, byte dest, byte s0, byte s1) noexcept {
        return encodeMoleculeSource1(twoArgumentMolecule(op, dest, s0), s1);
    }

    InstructionAtom zeroArgumentAtom(Operation op) noexcept {
        return encodeOperation(0, op);
    }
    InstructionAtom oneArgumentAtom(Operation op, byte dest) noexcept {
        return encodeDestination(zeroArgumentAtom(op), dest);
    }
    InstructionAtom twoArgumentAtom(Operation op, byte dest, byte src0) noexcept {
        return encodeSource0(oneArgumentAtom(op, dest), src0);
    }
    InstructionAtom threeArgumentAtom(Operation op, byte dest, byte src0, byte src1) noexcept {
        return encodeSource1(twoArgumentAtom(op, dest, src0), src1);
    }

    InstructionAtom returnInstruction(byte destination) {
        return oneArgumentAtom(Operation::BranchUnconditionalRegister, destination);
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
        return twoArgumentAtom(Operation::Move, dest, src);
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
        return encodeImmediate(oneArgumentAtom(Operation::Set16, destination), immediate);
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
#define makeThreeAddressOperationWithImmediateAndStackVersion(title, fragment) \
    InstructionAtom title ( byte dest, byte s0, byte s1, bool imm = false) noexcept { \
        return threeArgumentAtom( imm ? Operation:: fragment ## Immediate : Operation:: fragment , dest, s0, s1); \
    } \
    InstructionAtom title ## Stack (byte d, byte s0, byte s1) noexcept { \
        return title ( stackOperation(d), stackOperation(s0), stackOperation(s1), false); \
    }
    makeThreeAddressOperationWithImmediateAndStackVersion(add, Add);
    makeThreeAddressOperationWithImmediateAndStackVersion(sub, Sub);
    makeThreeAddressOperationWithImmediateAndStackVersion(mul, Mul);
    makeThreeAddressOperationWithImmediateAndStackVersion(div, Div);
    makeThreeAddressOperationWithImmediateAndStackVersion(rem, Rem);
    makeThreeAddressOperationWithImmediateAndStackVersion(shiftLeft, ShiftLeft);
    makeThreeAddressOperationWithImmediateAndStackVersion(shiftRight, ShiftRight);
    makeThreeAddressOperationWithImmediateAndStackVersion(eq, Eq);
    makeThreeAddressOperationWithImmediateAndStackVersion(neq, Neq);
    makeThreeAddressOperationWithImmediateAndStackVersion(lt, LessThan);
    makeThreeAddressOperationWithImmediateAndStackVersion(gt, GreaterThan);
    makeThreeAddressOperationWithImmediateAndStackVersion(le, LessThanOrEqualTo);
    makeThreeAddressOperationWithImmediateAndStackVersion(ge, GreaterThanOrEqualTo);
    makeThreeAddressOperationWithImmediateAndStackVersion(binaryAnd, BinaryAnd);
    makeThreeAddressOperationWithImmediateAndStackVersion(binaryOr, BinaryOr);
    makeThreeAddressOperationWithImmediateAndStackVersion(binaryXor, BinaryXor);
    makeThreeAddressOperationWithImmediateAndStackVersion(binaryNand, BinaryNand);
#undef makeThreeAddressOperationWithImmediateAndStackVersion
    InstructionAtom unaryNot(byte dest, byte s0) noexcept { return twoArgumentAtom(Operation::BinaryNot, dest, s0); }
    InstructionAtom increment(byte destination, byte src) noexcept { return add(destination, src, 1, true); }
    InstructionAtom increment(byte destination) noexcept { return increment(destination, destination); }
    InstructionAtom decrement(byte destination, byte src) noexcept { return sub(destination, src, 1, true); }
    InstructionAtom decrement(byte destination) noexcept { return decrement(destination, destination); }
    InstructionAtom doubleVal(byte destination, byte src) noexcept { return shiftLeft(destination, src, 1, true); }
    InstructionAtom doubleVal(byte destination) noexcept { return doubleVal(destination, destination); }
    InstructionAtom halveVal(byte destination, byte src) noexcept { return shiftRight(destination, src, 1, true); }
    InstructionAtom halveVal(byte destination) noexcept { return halveVal(destination, destination); }
    InstructionAtom zeroRegister(byte reg) noexcept { return binaryXor(reg, reg, reg); }

    InstructionAtom ifThenElse(byte cond, byte onTrue, byte onFalse, bool link, bool checkIfFalse) noexcept {
        Operation op;
        if (!link && checkIfFalse) {
            op = Operation::BranchIfThenElseNormalPredFalse;
        } else if (!link && !checkIfFalse) {
            op = Operation::BranchIfThenElseNormalPredTrue;
        } else if (link && checkIfFalse) {
            op = Operation::BranchIfThenElseLinkPredFalse;
        } else {
            op = Operation::BranchIfThenElseLinkPredTrue;
        }
        return threeArgumentAtom(op, cond, onTrue, onFalse);
    }

    InstructionMolecule ifConditionTrueThenElse(InstructionAtom compare, byte compareDest, byte onTrue, byte onFalse, bool link) noexcept {
        return molecule(compare, ifThenElse(compareDest, onTrue, onFalse, link, false));
    }

    InstructionMolecule indirectLoadIntoRegister(byte destination, byte address) noexcept {
        return molecule(load(registerOperation(destination), address),
                load(registerOperation(destination), destination));
    }

    InstructionMolecule indirectStoreFromRegister(byte address, byte value) noexcept {
        // r0 is the temporary register of legend :D
        return molecule(load(temporaryRegister0, address),
                store(temporaryRegister0, value));
    }

    InstructionMolecule stackLoad(byte stackPointer, byte destination) noexcept {
        return molecule(pop(stackPointer, registerOperation(destination)),
                load(registerOperation(destination), destination));
    }

    // pop the top two
    InstructionMolecule stackStore(byte stackPointer) {
        // t0 => address
        return molecule(pop(stackPointer, registerOperation(temporaryRegister0)),
                store(temporaryRegister0, stackOperation(stackPointer)));
    }
    InstructionAtom swap(byte r0, byte r1) noexcept {
        return twoArgumentAtom(Operation::Swap, r0, r1);
    }
    InstructionAtom nop() noexcept {
        // if the two byte fields are the the same then nothing happens
        return swap(temporaryRegister0, temporaryRegister0);
    }

    InstructionMolecule set32(byte destination, word value) noexcept {
        return encodeImmediate32(oneArgumentMolecule(Operation::Set32, destination), value);
    }

    InstructionMolecule set48(byte destination, word value) noexcept {
        return encodeImmediate48(oneArgumentMolecule(Operation::Set48, destination), value);
    }

    InstructionAtom stackSwapTopElements(byte stackPointer) noexcept {
        return swap(stackOperation(stackPointer), stackOperation(stackPointer));
    }

    InstructionAtom swapMemory(byte r0, byte r1) noexcept {
        return swap(memoryOperation(r0), memoryOperation(r1));
    }

    InstructionTriCompound set64(byte dest, word value, InstructionAtom leftOverSlot) noexcept {
        return std::make_tuple(
                molecule(set16(registerOperation(temporaryRegister0),
                        iris::getUpperHalf(iris::getUpperHalf(value))),
                    shiftLeft(temporaryRegister0, temporaryRegister0, 48, true)),
                set48(dest, iris::decodeBits<word, word, 0x0000FFFFFFFFFFFF, 0>(value)),
                molecule(add(dest, dest, temporaryRegister0, false),
                    leftOverSlot));
    }

    InstructionTriCompound set64(byte dest, word value) noexcept {
        // put a nop in the left over slot
        return set64(dest, value, nop());
    }
    void unpack(std::vector<InstructionMolecule>& molecules, InstructionTriCompound compound) noexcept {
        InstructionMolecule a, b, c;
        std::tie(a, b, c) = compound;
        molecules.emplace_back(a);
        molecules.emplace_back(b);
        molecules.emplace_back(c);
    }

    InstructionMolecule singleMoleculeFunction(InstructionAtom op) noexcept {
        return molecule(op, returnToLinkRegister());
    }

    InstructionMolecule load32Shifted(byte destination, byte address, InstructionAtom next) noexcept {
        return molecule(shiftLeft(destination, memoryOperation(address), 32, true), next);
    }
    InstructionMolecule load32Shifted(byte destination, byte address) noexcept {
        return load32Shifted(destination, address, nop());
    }
    InstructionMolecule load32(byte destination, byte address) noexcept {
        return load32Shifted(destination, address, shiftRight(destination, destination, 32, true));
    }

    InstructionMolecule load48Shifted(byte destination, byte address, InstructionAtom op) noexcept {
        return molecule(shiftLeft(destination, memoryOperation(address), 16, true), op);
    }


    InstructionMolecule load48Shifted(byte destination, byte address) noexcept {
        return load48Shifted(destination, address, nop());
    }

    InstructionMolecule load48(byte destination, byte address) noexcept {
        return load48Shifted(destination, address, shiftRight(destination, memoryOperation(address), 16, true));
    }

    using AddressTable = std::map<std::string, word>;
    using MoleculeList = std::vector<InstructionMolecule>;

    constexpr byte StackMaxIndex = static_cast<byte>(ArchitectureConstants::RegisterCount) - 4;
    constexpr byte StackBottomIndex = static_cast<byte>(ArchitectureConstants::RegisterCount) - 5;
    constexpr byte MemorySection0Start = static_cast<byte>(ArchitectureConstants::RegisterCount) - 6;
    constexpr byte MemorySection0End = static_cast<byte>(ArchitectureConstants::RegisterCount) - 7;
    constexpr byte MemorySection1Start = static_cast<byte>(ArchitectureConstants::RegisterCount) - 8;
    constexpr byte MemorySection1End = static_cast<byte>(ArchitectureConstants::RegisterCount) - 9;
	constexpr byte CallStackBottom = static_cast<byte>(ArchitectureConstants::RegisterCount) - 10;
	constexpr byte CallStackTop = static_cast<byte>(ArchitectureConstants::RegisterCount) - 11;
	constexpr byte CodeEnd = static_cast<byte>(ArchitectureConstants::RegisterCount) - 12;
	constexpr byte CodeStart = static_cast<byte>(ArchitectureConstants::RegisterCount) - 13;
    // basic memory layout
    constexpr word stackBottom = ArchitectureConstants::AddressMax;
    constexpr word stackTop = stackBottom - (0x00FFFFFF / 2);
	constexpr word callStackBottom = stackTop - 1;
	constexpr word callStackTop = callStackBottom - (0x00FFFFFF / 2);
    constexpr word memory1End = callStackTop - 1;
    constexpr word memory1Start = memory1End - 0x00FFFFFF;
    constexpr word memory0End = memory1Start - 1;
    constexpr word memory0Start = memory0End - 0x00FFFFFF;
	constexpr word codeEnd = memory0Start - 1;
	constexpr word codeStart = codeEnd - 0x00FFFFFF;
	//static_assert(codeStart == 0, "Memory map is not properly laid out, code start isn't zero!");

    void stackInitCode(MoleculeList& molecules) {
        // carve the system up into four spaces (0-3). Space 3 is the stack for the
        // entire system
        // declare our registers
        auto sp = registerOperation(static_cast<byte>(ArchitectureConstants::StackPointerIndex));
        auto sb = registerOperation(StackBottomIndex);
        auto sm = registerOperation(StackMaxIndex);
		auto csb = registerOperation(CallStackBottom);
		auto cst = registerOperation(CallStackTop);
        unpack(molecules, set64(sb, stackBottom, move(sp, sb)));
        unpack(molecules, set64(sm, stackTop));
		unpack(molecules, set64(csb, callStackBottom));
		unpack(molecules, set64(cst, callStackTop));
    }

    void memoryBlockCode(MoleculeList& molecules) {
        auto ms0 = registerOperation(MemorySection0Start);
        auto me0 = registerOperation(MemorySection0End);
        auto ms1 = registerOperation(MemorySection1Start);
        auto me1 = registerOperation(MemorySection1End);
		auto cs = registerOperation(CodeStart);
		auto ce = registerOperation(CodeEnd);
        unpack(molecules, set64(ms0, memory0Start));
        unpack(molecules, set64(me0, memory0End));
        unpack(molecules, set64(ms1, memory1Start));
        unpack(molecules, set64(me1, memory1End));
		unpack(molecules, set64(cs, codeStart));
		unpack(molecules, set64(ce, codeEnd));
    }


	void emit(const MoleculeList& elements, std::ostream& out) noexcept {
		byte storage[sizeof(InstructionMolecule)] = { 0 };
		for (const auto& a : elements) {
			iris::decodeUint64LE(a, storage);
			out.write((char*)storage, sizeof(InstructionMolecule));
		}
	}

	void emit(const MoleculeList& elements) noexcept {
		emit(elements, std::cout);
	}

} // end namespace iris20


int main() {
    iris20::MoleculeList molecules;
    iris20::stackInitCode(molecules);
    iris20::memoryBlockCode(molecules);
	std::cerr << "Memory Map" << std::endl;
	std::cerr << "\tRegister  |  value  " << std::endl;
	std::cerr << "\tce:       |  " << std::hex << iris20::codeEnd << std::endl;
	std::cerr << "\tcs:       |  " << std::hex << iris20::codeStart << std::endl;
	std::cerr << "\tm0s:      |  " << std::hex << iris20::memory0Start << std::endl;
	std::cerr << "\tm0e:      |  " << std::hex << iris20::memory0End << std::endl;
	std::cerr << "\tm1s:      |  " << std::hex << iris20::memory1Start << std::endl;
	std::cerr << "\tm1e:      |  " << std::hex << iris20::memory1End << std::endl;
	std::cerr << "\tcst:      |  " << std::hex << iris20::callStackTop << std::endl;
	std::cerr << "\tcsb:      |  " << std::hex << iris20::callStackBottom << std::endl;
	std::cerr << "\tst:       |  " << std::hex << iris20::stackTop << std::endl;
	std::cerr << "\tsb:       |  " << std::hex << iris20::stackBottom << std::endl;

	iris20::emit(molecules);
    return 0;
}
