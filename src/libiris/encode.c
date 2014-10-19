#include <stdint.h>
#include "iris.h"

/* macros */
#define encode_bits(instruction, mask, value, shiftcount) ((instruction & ~mask) | (value << shiftcount))
#define decode_bits(instruction, mask, shiftcount) ((instruction & mask) >> shiftcount)
#define extract_byte0(a) ((byte)(a & 0x000000FF))
byte iris_decode_group(instruction* inst) {
   return (byte)decode_bits(extract_byte0(*inst), 0x7, 0);
}
void iris_encode_group(instruction* inst, byte group) {
   *inst = encode_bits(*inst, 0x7, group, 0);
}
byte iris_decode_op(instruction* inst) {
   return (byte)decode_bits(extract_byte0(*inst), 0xF8, 3);
}
void iris_encode_op(instruction* inst, byte value) {
   *inst = encode_bits(*inst, 0xF8, value, 3);
}

byte iris_decode_register(instruction* inst, byte index) {
   switch(index) {
      case 1:
         return (byte)decode_bits(*inst, 0x0000FF00, 8);
      case 2:
         return (byte)decode_bits(*inst, 0x00FF0000, 16);
      case 3:
         return (byte)decode_bits(*inst, 0xFF000000, 24);
      default:
         return (byte)(*inst);
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
word iris_decode_immediate(instruction* inst, byte index) {
   return (word)decode_bits(*inst, 0xFFFF0000, 16);
}
void iris_encode_immediate(instruction* inst, byte index, word value) {
   *inst = encode_bits(*inst, 0xFFFF0000, value, 16);
}
/* vim: set expandtab tabstop=3 shiftwidth=3: */
