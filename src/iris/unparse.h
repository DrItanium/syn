#include "iris.h"

void unparse(char* unparsed, ushort value);
void unparse_register(char* unparsed, byte index);
void unparse_arithmetic(char* unparsed, instruction i);
void unparse_move(char* unparsed, instruction i);
void unparse_jump(char* unparsed, instruction i);
void unparse_compare(char* unparsed, instruction i);
