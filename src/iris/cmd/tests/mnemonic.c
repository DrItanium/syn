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
        puts(arithmetic_mnemonic(&insn));
        break;
      case InstructionGroupMove:
        puts(move_mnemonic(&insn));
        break;
      case InstructionGroupJump:
        puts(jump_mnemonic(&insn));
        break;
      case InstructionGroupCompare:
        puts(compare_mnemonic(&insn));
        break;
      case InstructionGroupMisc:
        puts(misc_mnemonic(&insn));
        break;
      default:
        puts("Unknown instruction group");
        break;
    }
  }
  return 0;
}
