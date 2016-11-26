// define an ALU to cut down on the amount of repeated actions
#ifndef _IRIS_ALU_H
#define _IRIS_ALU_H
#include "iris_base.h"
namespace iris {

template<typename Word>
class ALU  {
    public:
        enum class Operation {
            Add,
            Subtract,
            Multiply,
            Divide,
            Remainder,
            ShiftLeft,
            ShiftRight,
            Count,
        };
    public:
        ALU() { }
        virtual ~ALU() { }
        inline Word performOperation(Operation op, Word a, Word b) const {
            switch(op) {
                case Operation::Add:
                    return iris::add<Word>(a, b);
                case Operation::Subtract:
                    return iris::sub<Word>(a, b);
                case Operation::Multiply:
                    return iris::mul<Word>(a, b);
                case Operation::Divide:
                    return iris::div<Word>(a, b);
                case Operation::Remainder:
                    return iris::rem<Word>(a, b);
                case Operation::ShiftLeft:
                    return iris::shiftLeft<Word>(a, b);
                case Operation::ShiftRight:
                    return iris::shiftRight<Word>(a, b);
                default:
                    throw iris::Problem("Undefined ALU operation!");
            }
        }
};

template<typename Word>
class Comparator {
    public:
        enum class Operation {
            Eq,
            Neq,
            LessThan,
            GreaterThan,
            LessThanOrEqualTo,
            GreaterThanOrEqualTo,
            Count,
        };
    public:
        Comparator() { }
        virtual ~Comparator() { }
        inline Word performOperation(Operation op, Word a, Word b) const {
            switch(op) {
                case Operation::Eq:
                    return iris::eq<Word>(a, b);
                case Operation::Neq:
                    return iris::neq<Word>(a, b);
                case Operation::LessThan:
                    return iris::lt<Word>(a, b);
                case Operation::GreaterThan:
                    return iris::gt<Word>(a, b);
                case Operation::LessThanOrEqualTo:
                    return iris::le<Word>(a, b);
                case Operation::GreaterThanOrEqualTo:
                    return iris::ge<Word>(a, b);
                default:
                    throw iris::Problem("Undefined Comparison operation!");
            }
        }
};

} // end namespace iris
#endif // end _IRIS_ALU_H
