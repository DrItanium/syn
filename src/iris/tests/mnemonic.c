#include <stdio.h>
#include <stdlib.h>
#include "../iris.h"
#include "../mnemonic.h"

int main() {
  ushort insn;
  for(insn = 0; insn < 65535; insn++) {
    switch(getgroup(insn)) {
      case InstructionGroupArithmetic:
        puts(arithmetic_mnemonic(insn));
        break;
      case InstructionGroupMove:
        puts(move_mnemonic(insn));
        break;
      case InstructionGroupJump:
        puts(jump_mnemonic(insn));
        break;
      case InstructionGroupCompare:
        puts(compare_mnemonic(insn));
        break;
      default:
        puts("Unknown instruction group");
    }
  }
  return 0;
}
