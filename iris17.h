#ifndef _TARGET_IRIS17_IRIS_H
#define _TARGET_IRIS17_IRIS_H
#include "iris_base.h"
#include "iris_alu.h"
#include "Core.h"
#include <cstdint>
#include <vector>
#include <memory>


namespace iris17 {
    using word = int32_t;
    using hword = int16_t;
    constexpr word encodeWord(byte, byte, byte, byte) noexcept;
    enum ArchitectureConstants  {
        RegisterCount = 256,
        AddressMax = 268435456 /* bytes */ / sizeof(word), // words
        InstructionPointerIndex = RegisterCount - 1,
        LinkRegisterIndex = RegisterCount - 2,
        StackPointerIndex = RegisterCount - 3,
        ConditionRegisterIndex = RegisterCount - 4,
        ThreadIndex = RegisterCount - 5,
		UserRegisterCount = ThreadIndex,

        GroupMask = 0b00000111,
        RestMask = ~GroupMask,
        MaxInstructionsPerGroup = RestMask >> 3,
        MaxGroups = 8,
    };
} // end namespace iris17

#include "iris17_defines.h"

namespace iris17 {
class DecodedInstruction {
    public:
        DecodedInstruction(word rinst) noexcept;
		virtual ~DecodedInstruction() noexcept;
        inline word getRawValue() const noexcept { return raw; }
        inline byte getDestination() const noexcept { return decodeDestination(raw); }
        inline byte getSource0() const noexcept { return decodeSource0(raw); }
        inline byte getSource1() const noexcept { return decodeSource1(raw); }
        inline hword getImmediate() const noexcept { return decodeImmediate(raw); }
        inline InstructionGroup getGroup() const noexcept { return static_cast<InstructionGroup>(decodeGroup(raw)); }
        inline byte getOperation() const noexcept { return decodeOperation(raw); }
        inline byte getControl() const noexcept { return decodeControl(raw); }
		template<typename Op>
		inline Op getSubtype() const noexcept { return static_cast<Op>(getOperation()); }
    private:
        word raw;
};
using RegisterFile = iris::FixedSizeLoadStoreUnit<word, word, ArchitectureConstants::UserRegisterCount>;
using ALU = iris::ALU<word>;
using CompareUnit = iris::Comparator<word>;
/// Represents the execution state of a thread of execution
struct ExecState {
	public:
		inline word& getStackPointer() noexcept { return _stackPointer; }
		inline word& getInstructionPointer() noexcept { return _instructionPointer; }
		inline word& getLinkRegister() noexcept { return _linkRegister; }
		inline word& getConditionRegister() noexcept { return _conditionRegister; }
		inline word& getThreadIndexRegister() noexcept { return _threadIndex; }
	public:
		bool advanceIp = true;
		RegisterFile gpr;
	private:
		word _instructionPointer = 0;
		word _linkRegister = 0;
		word _stackPointer = 0;
		word _conditionRegister = 0;
		word _threadIndex = 0;
};

using SharedExecState = std::shared_ptr<ExecState>;

class Core : public iris::Core {
    public:
        Core(word memorySize, byte numThreads) noexcept;
        ~Core() noexcept;
        virtual void initialize();
        virtual void installprogram(std::istream& stream);
        virtual void shutdown();
        virtual void dump(std::ostream& stream);
        virtual void run();
        virtual void link(std::istream& input);
        void write(word address, word value);
        word read(word address);
    private:
        void execBody();
        void decode();
        void dispatch();
        void systemCall(DecodedInstruction& inst);
    private:
        void compare(DecodedInstruction&& inst);
        void jump(DecodedInstruction&& inst);
        void move(DecodedInstruction&& inst);
        void arithmetic(DecodedInstruction&& inst);
        void misc(DecodedInstruction&& inst);
    private:
		ALU _alu;
		CompareUnit _compare;
		iris::LoadStoreUnit<word, word> memory;
		SharedExecState thread;
        std::vector<SharedExecState> threads;
        bool execute = true;
};

Core* newCore() noexcept;
void assemble(FILE* input, std::ostream* output);
} // end namespace iris17
#undef DefOp
#endif // end _TARGET_IRIS17_IRIS_H
