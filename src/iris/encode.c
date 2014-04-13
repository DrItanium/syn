#include "iris.h"

/* macros */
#define set_bits(instruction, mask, value, shiftcount) ((instruction & ~mask) | (value << shiftcount))
#define get_bits(instruction, mask, shiftcount) ((byte)((instruction & mask) >> shiftcount))
byte get_group(instruction* inst) {
   return get_bits(inst->words[0], 0x7, 0);
}
void set_group(Instruction* inst, byte group) {
   inst->words[0] = set_bits(inst->words[0], 0x7, group, 0);
}
byte get_op(instruction* inst) {
   return get_bits(inst->words[0], 0xF8, 3);
}
void set_op(instruction* inst, byte value) {
   inst->words[0] = set_bits(inst->words[0], 0xF8, value, 3);
}

byte get_register(instruction* inst, byte index) {
   return inst->bytes[index];
}
void set_register(instruction* inst, byte index, byte value) {
   inst->bytes[index] = value;
}
datum get_immediate(instruction* inst, byte index) {
   return inst->words[index];
}
void set_immediate(instruction* inst, byte index, datum value) {
   inst->words[index] = value;
}
