#include "iris.h"

/* macros */
#define encode_bits(instruction, mask, value, shiftcount) ((instruction & ~mask) | (value << shiftcount))
#define decode_bits(instruction, mask, shiftcount) ((byte)((instruction & mask) >> shiftcount))
byte iris_decode_group(instruction* inst) {
   return decode_bits(inst->words[0], 0x7, 0);
}
void iris_encode_group(instruction* inst, byte group) {
   inst->words[0] = encode_bits(inst->words[0], 0x7, group, 0);
}
byte iris_decode_op(instruction* inst) {
   return decode_bits(inst->bytes[0], 0xF8, 3);
}
void iris_encode_op(instruction* inst, byte value) {
   inst->words[0] = encode_bits(inst->bytes[0], 0xF8, value, 3);
}

byte iris_decode_register(instruction* inst, byte index) {
   return inst->bytes[index];
}
void iris_encode_register(instruction* inst, byte index, byte value) {
   inst->bytes[index] = value;
}
datum iris_decode_immediate(instruction* inst, byte index) {
   return inst->words[index];
}
void iris_encode_immediate(instruction* inst, byte index, datum value) {
   inst->words[index] = value;
}
