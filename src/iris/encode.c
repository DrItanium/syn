#include "iris.h"

byte get_group(instruction* inst) {
   return get_bits(inst->words[0], 0x7, 0);
}
void set_group(Instruction* inst, byte group) {
   inst->words[0] = set_bits(inst->words[0], 0x7, group, 0);
}
byte get_arithmetic_op(instruction* inst) {
   return get_bits(inst->words[0], 0x78, 3);
}
void set_arithmetic_op(instruction* inst, byte value) {
   inst->words[0] = set_bits(inst->words[0], 0x78, value, 3);
}
byte get_arithmetic_dest(instruction* inst) {
   return instruction->bytes[1];
}
void set_arithmetic_dest(instruction* inst, byte value) {
   instruction->bytes[1] = value;
}
byte get_arithmetic_source0(instruction* inst) {
   return instruction->bytes[2];
}
void set_arithmetic_source0(instruction* inst, byte value) {
   instruction->bytes[2] = value;
}
byte get_arithmetic_source1(instruction* inst) {
   return instruction->bytes[3];
}
void set_arithmetic_source1(instruction* inst, byte value) {
   instruction->bytes[3] = value;
}

byte get_move_op(instruction* inst) {
   return get_bits(instruction->words[0], 0xF8, 3);
}
void set_move_op(instruction* inst, byte value) {
   instruction->words[0] = set_bits(instruction->words[0], 0xF8, value, 3);
}
byte get_move_reg0(instruction* inst) {
   return instruction->bytes[1];
}
void set_move_reg0(instruction* inst, byte value) {
   instruction->bytes[1] = value;
}
ushort get_move_immediate(instruction* inst) {
   return instruction->words[1]; 
}
void set_move_immediate(instruction* inst, ushort value) {
   instruction->words[1] = value;
}
byte get_move_reg1(instruction* inst) {
   return instruction->words[2];
}
void set_move_reg1(instruction* inst, byte value) {
   instruction->words[2] = value;
}

byte get_move_reg2(instruction* inst) {
   return instruction->words[3];
}
void set_move_reg2(instruction* inst, byte value) {
   instruction->words[3] = value;
}
