#include "iris.h"

void unparse(char* unparsed, ushort insn);
void unparse_register(char* unparsed, byte index);
void unparse_arithmetic(char* unparsed, ushort insn);
void unparse_move(char* unparsed, ushort insn);
void unparse_jump(char* unparsed, ushort insn);
void unparse_normal_jump(char* unparsed, ushort insn);
void unparse_if_then_else(char* unparsed, ushort insn);
void unparse_compare(char* unparsed, ushort insn);
void unparse_bitstring(char* bits, ushort insn);
