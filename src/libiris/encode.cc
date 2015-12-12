#include <stdint.h>
#include "iris.h"

/* macros */
#define encode_bits(raw_instruction, mask, value, shiftcount) ((instruction & ~mask) | (value << shiftcount))
//#define decode_bits(instruction, mask, shiftcount) ((instruction & mask) >> shiftcount)
#define extract_byte0(a) ((byte)(a & 0x000000FF))
template<typename T, raw_instruction mask, int shiftcount>
void iris_encode_bits(raw_instruction* inst, T value) {
   *inst = ((*inst & ~mask) | (value << shiftcount));
}
template<typename T, raw_instruction mask, int shiftcount>
T iris_decode_bits(raw_instruction* inst) {
   return (T)((*inst & mask) >> shiftcount);
}

template<InstructionField field> 
struct RegisterDescription {
   static const bool isRegister = false;
};
template<InstructionField offset>
struct Shifter {
   static const int shiftcount = 0;
};
template<InstructionField offset>
struct FieldBits {
   static const raw_instruction mask = 0xFFFFFFFF;
};

template<InstructionField field, typename T = byte>
T iris_decode_field(raw_instruction* inst) {
   return iris_decode_bits<T, FieldBits<field>::mask, Shifter<field>::shiftcount>(inst);
}

template<InstructionField field, typename T = byte>
void iris_encode_field(raw_instruction* inst, T value) {
   iris_encode_bits<T, FieldBits<field>::mask, Shifter<field>::shiftcount>(inst, value);
}
template<InstructionField index>
byte iris_decode_register(raw_instruction* inst) {
   return (byte)*inst;
}
#define CASE_ON(cond) PRIMITIVE_CAT(CASE_ON_, cond)
#define CASE_ON_false(index, _)
#define CASE_ON_true(index, ret) \
   template<> \
   ret iris_decode_register<index>(raw_instruction* inst) { \
      return iris_decode_field<index, ret>(inst); \
   }
#define X(index, bitmask, shiftby, typ, isreg, postfix) \
   template<> \
   struct FieldBits<index> { \
      static const raw_instruction mask = bitmask; \
   }; \
   template<> \
   struct Shifter<index> { \
      static const int shiftcount = shiftby; \
   }; \
   template<> \
struct RegisterDescription<index> { \
   static const bool isRegister = isreg; \
}; \
typ iris_decode_ ## postfix (raw_instruction* inst) { \
   return iris_decode_field<index, typ>(inst); \
} \
void iris_encode_ ## postfix (raw_instruction* inst, typ value) { \
   iris_encode_field<index, typ>(inst, value); \
} \
CASE_ON(isreg)(index, typ)
#include "instruction.def"
#undef X
#undef CASE_ON_false
#undef CASE_ON_true

byte iris_decode_register(raw_instruction* inst, InstructionField field) { 
   switch(field) {
#define CASE_ON_false(index, _)
#define CASE_ON_true(index, ret) \
      case index: return iris_decode_field<index, ret>(inst);
#define X(index, bitmask, shiftby, typ, isreg, postfix) CASE_ON(isreg)(index, typ)
#include "instruction.def"
#undef X
#undef CASE_ON_false
#undef CASE_ON_true
      default:
                  iris_error((char*)"illegal field index", 1);
                  return 0;
   }
}
//void iris_encode_register(instruction* inst, byte index, byte value) {
//   switch(index) {
//      case 0:
//         iris_encode_bits<byte, FieldBits<0>::mask, Shifter<0>::shiftcount>(inst, value);
//         break;
//      case 1:
//         iris_encode_bits<byte, FieldBits<1>::mask, Shifter<1>::shiftcount>(inst, value);
//         break;
//      case 2:
//         iris_encode_bits<byte, FieldBits<2>::mask, Shifter<2>::shiftcount>(inst, value);
//         break;
//      case 3:
//         iris_encode_bits<byte, FieldBits<3>::mask, Shifter<3>::shiftcount>(inst, value);
//         break;
//   }
//}
//immediate iris_decode_immediate(raw_instruction* inst) {
//   return iris_decode_bits<immediate, FieldBits<4>::mask, Shifter<4>::shiftcount>(inst);
//}
//void iris_encode_immediate(raw_instruction* inst, byte index, word value) {
//   *inst = encode_bits(*inst, 0xFFFF0000, value, 16);
//}
#undef CASE_ON
#undef CAT
#undef PRIMITIVE_CAT
/* vim: set expandtab tabstop=3 shiftwidth=3: */
