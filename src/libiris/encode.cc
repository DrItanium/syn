#include <stdint.h>
#include "iris.h"

/* macros */
#define encode_bits(instruction, mask, value, shiftcount) ((instruction & ~mask) | (value << shiftcount))
//#define decode_bits(instruction, mask, shiftcount) ((instruction & mask) >> shiftcount)
#define extract_byte0(a) ((byte)(a & 0x000000FF))
template<typename T, instruction mask, int shiftcount>
T iris_decode_bits(instruction* inst) {
   return (T)((*inst & mask) >> shiftcount);
}

template<int offset>
struct Shifter {
   static const int shiftcount = 0;
};
#define defshiftcount(index, count) \
   template<> \
struct Shifter<index> { \
   static const int shiftcount = count; \
}
defshiftcount(0, 0);
defshiftcount(1, 8);
defshiftcount(2, 16);
defshiftcount(3, 24);
#undef defshiftcount

template<int offset>
struct FieldBits {
   static const instruction mask = 0xFFFFFFFF;
};
#define deffieldmask(index, bitmask) \
   template<> \
   struct FieldBits<index> { \
      static const instruction mask = bitmask; \
   }
deffieldmask(0, 0x000000FF);
deffieldmask(1, 0x0000FF00);
deffieldmask(2, 0x00FF0000);
deffieldmask(3, 0xFF000000);
#undef deffieldmask

byte iris_decode_group(instruction* inst) {
   return iris_decode_bits<byte, 0x7, 0>(inst);
}
void iris_encode_group(instruction* inst, byte group) {
   *inst = encode_bits(*inst, 0x7, group, 0);
}
byte iris_decode_op(instruction* inst) {
   return iris_decode_bits<byte, 0xF8, 3>(inst);
}
void iris_encode_op(instruction* inst, byte value) {
   *inst = encode_bits(*inst, 0xF8, value, 3);
}

byte iris_decode_register(instruction* inst, byte index) {
   switch(index) {
      case 0:
         return iris_decode_bits<byte, FieldBits<0>::mask, Shifter<0>::shiftcount>(inst);
      case 1:
         return iris_decode_bits<byte, FieldBits<1>::mask, Shifter<1>::shiftcount>(inst);
      case 2:
         return iris_decode_bits<byte, FieldBits<2>::mask, Shifter<2>::shiftcount>(inst);
      case 3:
         return iris_decode_bits<byte, FieldBits<3>::mask, Shifter<3>::shiftcount>(inst);
      default:
         iris_error((char*)"illegal field index", 1);
   }
}
void iris_encode_register(instruction* inst, byte index, byte value) {
   switch(index) {
      case 0:
         *inst = encode_bits(*inst, 0x0000FF00, value, 8);
         break;
      case 1:
         *inst = encode_bits(*inst, 0x00FF0000, value, 16);
         break;
      case 2:
         *inst = encode_bits(*inst, 0xFF000000, value, 24);
         break;
      default:
         break;
   }
}
word iris_decode_immediate(instruction* inst) {
   return (word)decode_bits(*inst, 0xFFFF0000, 16);
}
void iris_encode_immediate(instruction* inst, byte index, word value) {
   *inst = encode_bits(*inst, 0xFFFF0000, value, 16);
}
/* vim: set expandtab tabstop=3 shiftwidth=3: */
