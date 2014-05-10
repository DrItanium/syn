#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "iris.h"

int main() {
  instruction insn;
  uint i;
  for(i = 0; i < 65536; i++) {
    insn.full = i;
    switch(get_group(&insn)) {
      case InstructionGroupArithmetic:
        puts(iris_arithmetic_mnemonic(&insn));
        break;
      case InstructionGroupMove:
        puts(iris_move_mnemonic(&insn));
        break;
      case InstructionGroupJump:
        puts(iris_jump_mnemonic(&insn));
        break;
      case InstructionGroupCompare:
        puts(iris_compare_mnemonic(&insn));
        break;
      case InstructionGroupMisc:
        puts(iris_misc_mnemonic(&insn));
        break;
      default:
        puts("Unknown instruction group");
        break;
    }
  }
  return 0;
}
