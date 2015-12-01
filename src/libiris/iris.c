
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "iris.h"

void iris_dispatch(iris_core* proc, instruction* value) {
   /* reset the advancepc value */
   proc->advancepc = 1;
   switch(get_group(value)) {
      case InstructionGroupArithmetic:
         iris_arithmetic(proc, value);
         break;
      case InstructionGroupMove:
         iris_move(proc, value);
         break;
      case InstructionGroupJump:
         iris_jump(proc, value);
         break;
      case InstructionGroupCompare:
         iris_compare(proc, value);
         break;
      case InstructionGroupMisc:
         iris_misc(proc, value);
         break;
      case InstructionGroupLoadStore:
         iris_load_store(proc, value);
         break;
      default:
         iris_error("invalid instruction group provided", ErrorInvalidInstructionGroupProvided);
         break;
   }
}

void iris_put_register(iris_core* proc, byte index, word value) {
   if(index < RegisterCount) {
      proc->gpr[index] = value;
   } else {
      iris_error("attempted to store to a register out of range", ErrorPutRegisterOutOfRange);
   }
}

word iris_get_register(iris_core* proc, byte index) {
   if(index < RegisterCount) {
      return proc->gpr[index];
   } else {
      iris_error("attempted to retrieve a value from a register out of range", ErrorGetRegisterOutOfRange);
      return 0;
   }
}

void iris_arithmetic(iris_core* proc, instruction* inst) {
#define perform_operation(symbol) \
   (iris_put_register(proc, get_arithmetic_dest(inst), \
         (iris_get_register(proc, get_arithmetic_source0(inst))) \
          symbol \
         (iris_get_register(proc, get_arithmetic_source1(inst)))))
#define perform_operation_immediate(symbol) \
   (iris_put_register(proc, get_arithmetic_dest(inst), \
         (iris_get_register(proc, get_arithmetic_source0(inst))) \
          symbol \
         (get_arithmetic_source1(inst))))
#define defop(tag, symbol) \
      case tag: \
         perform_operation(symbol); \
         break 
#define defiop(tag, symbol) \
      case tag: \
         perform_operation_immediate(symbol); \
         break
   switch(get_arithmetic_op(inst)) {
      defop(ArithmeticOpAdd, +);
      defop(ArithmeticOpSub, -);
      defop(ArithmeticOpMul, *);
      defop(ArithmeticOpDiv, /);
      defop(ArithmeticOpRem, %);
      defop(ArithmeticOpShiftLeft, <<);
      defop(ArithmeticOpShiftRight, >>);
      defop(ArithmeticOpBinaryAnd, &);
      defop(ArithmeticOpBinaryOr, |);
      case ArithmeticOpBinaryNot:
         iris_put_register(proc, get_arithmetic_dest(inst), 
               ~(iris_get_register(proc, get_arithmetic_source0(inst))));
         break;
      defop(ArithmeticOpBinaryXor, ^);
      /* immediate operations */
      defiop(ArithmeticOpAddImmediate, +);
      defiop(ArithmeticOpSubImmediate, -);
      defiop(ArithmeticOpMulImmediate, *);
      defiop(ArithmeticOpDivImmediate, /);
      defiop(ArithmeticOpRemImmediate, %);
      defiop(ArithmeticOpShiftLeftImmediate, <<);
      defiop(ArithmeticOpShiftRightImmediate, >>);
      default:
         iris_error("invalid arithmetic operation", ErrorInvalidArithmeticOperation);
   }
#undef perform_operation
#undef perform_operation_immediate
#undef defop
#undef defiop
}

/* position masks */
enum {
   MaskBits0_15  = 0xFFFFFFFFFFFF0000,
   MaskBits16_31 = 0xFFFFFFFF0000FFFF,
   MaskBits32_47 = 0xFFFF0000FFFFFFFF,
   MaskBits48_63 = 0x0000FFFFFFFFFFFF,
   MaskBits0_31   = 0x00000000FFFFFFFF,
   MaskBits32_63  = 0xFFFFFFFF00000000,
};
void iris_movefull(iris_core* proc, instruction* inst) {
   iris_put_register(proc, get_move_reg0(inst), iris_get_register(proc, get_move_reg1(inst)));
}
void iris_swapfull(iris_core* proc, instruction* inst) {
   word a = iris_get_register(proc, get_move_reg0(inst));
   iris_put_register(proc, get_move_reg0(inst), iris_get_register(proc, get_move_reg1(inst)));
   iris_put_register(proc, get_move_reg1(inst), a);
}
void iris_swap_generic(iris_core* proc, instruction* inst, word mask) {
   word a = iris_get_register(proc, get_move_reg0(inst)),
        b = iris_get_register(proc, get_move_reg1(inst));
   word p0 = a & mask;
   word p1 = b & mask;
   word r0 = a & ~mask;
   word r1 = b & ~mask;
   iris_put_register(proc, get_move_reg0(inst), r0 | p1);
   iris_put_register(proc, get_move_reg1(inst), r1 | p0);
}
void iris_move_generic(iris_core* proc, instruction* inst, word mask) {
   iris_put_register(proc, get_move_reg0(inst), 
         (iris_get_register(proc, get_move_reg0(inst)) & ~mask) |
         (((iris_get_register(proc, get_move_reg1(inst))) & mask)));
}
void iris_moveupper32(iris_core* proc, instruction* inst) {
   iris_move_generic(proc, inst, MaskBits32_63);
}

void iris_movelower32(iris_core* proc, instruction* inst) {
   iris_move_generic(proc, inst, MaskBits0_31);
}
void iris_swapupper32(iris_core* proc, instruction* inst) {
   iris_swap_generic(proc, inst, MaskBits32_63);
}

void iris_swaplower32(iris_core* proc, instruction* inst) {
   iris_swap_generic(proc, inst, MaskBits0_31);
}
word generate_full_mask(byte mask) {
   return ((word)((mask & 0x01) ? 0xFF : 0x0)) |
          (((word)(((mask & 0x02) >> 1) ? 0xFF : 0x0)) << 8) |
          (((word)(((mask & 0x04) >> 2) ? 0xFF : 0x0)) << 16) |
          (((word)(((mask & 0x08) >> 3) ? 0xFF : 0x0)) << 24) |
          (((word)(((mask & 0x10) >> 4) ? 0xFF : 0x0)) << 32) |
          (((word)(((mask & 0x20) >> 5) ? 0xFF : 0x0)) << 40) |
          (((word)(((mask & 0x40) >> 6) ? 0xFF : 0x0)) << 48) |
          (((word)(((mask & 0x80) >> 7) ? 0xFF : 0x0)) << 56);
}
void iris_move(iris_core* proc, instruction* inst) {
   word mask = 0;
   switch(get_move_op(inst)) {
      case MoveOpMove:
         {
            // do nothing
            if (get_move_reg0(inst) == get_move_reg1(inst)) {
               break;
            }
            mask = (word)(byte)get_move_mask(inst);
            if (mask != 0) {
               switch(get_move_position(inst)) {
                  case MoveOp_Form_64bit_chunks:
                     {
                     // properly format the mask
                     mask &= 0x1;
                     if (mask == 1) {
                        iris_movefull(proc, inst);
                     } else {
                        iris_error("The bit mask is improperly formed for a 64bit move!", ErrorIllegalMoveBits);
                     }
                     break;
                     }
                  case MoveOp_Form_32bit_chunks:
                     {
                        mask &= 0x3;
                        switch (mask) {
                           case 0x1:
                              iris_movelower32(proc, inst);
                              break;
                           case 0x2:
                              iris_moveupper32(proc, inst);
                              break;
                           case 0x3:
                              iris_movefull(proc, inst);
                              break;
                           default:
                              iris_error("The bit mask is improperly formed for a 32bit move!", ErrorIllegalMoveBits);
                        }
                     break;
                     }
                  case MoveOp_Form_16bit_chunks:
                     {
                        mask &= 0xF;
                        switch (mask) {
                              case 0xF:
                                 iris_movefull(proc, inst);
                                 break;
                              case 0xE:
                                 iris_move_generic(proc, inst, 0xFFFFFFFFFFFF0000);
                                 break;
                              case 0xD:
                                 iris_move_generic(proc, inst, 0xFFFFFFFF0000FFFF);
                                 break;
                              case 0xC:
                                 iris_moveupper32(proc, inst);
                                 break;
                              case 0xB:
                                 iris_move_generic(proc, inst, 0xFFFF0000FFFFFFFF);
                                 break;
                              case 0xA:
                                 iris_move_generic(proc, inst, 0xFFFF0000FFFF0000);
                                 break;
                              case 0x9:
                                 iris_move_generic(proc, inst, 0xFFFF00000000FFFF);
                                 break;
                              case 0x8:
                                 iris_move_generic(proc, inst, 0xFFFF000000000000);
                                 break;
                              case 0x7:
                                 iris_move_generic(proc, inst, 0x0000FFFFFFFFFFFF);
                                 break;
                              case 0x6:
                                 iris_move_generic(proc, inst, 0x0000FFFFFFFF0000);
                                 break;
                              case 0x5:
                                 iris_move_generic(proc, inst, 0x0000FFFF0000FFFF);
                                 break;
                              case 0x4:
                                 iris_move_generic(proc, inst, 0x0000FFFF00000000);
                                 break;
                              case 0x3:
                                 iris_movelower32(proc, inst);
                                 break;
                              case 0x2:
                                 iris_move_generic(proc, inst, 0x00000000FFFF0000);
                                 break;
                              case 0x1:
                                 iris_move_generic(proc, inst, 0x000000000000FFFF);
                                 break;
                              default:
                                 iris_error("The bit mask is improperly formed for a 16bit move!", ErrorIllegalMoveBits);
                        }
                        break;
                     }
                  case MoveOp_Form_8bit_chunks:
                     {
                        if (mask == 0xFF) {
                           iris_movefull(proc, inst);
                        } else if (mask == 0xF0) {
                           iris_moveupper32(proc, inst);
                        } else if (mask == 0x0F) {
                           iris_movelower32(proc, inst);
                        } else if (mask == 0xC0) {
                           iris_move_generic(proc, inst, MaskBits48_63);
                        } else if (mask == 0x30) {
                           iris_move_generic(proc, inst, MaskBits32_47);
                        } else if (mask == 0x0C) {
                           iris_move_generic(proc, inst, MaskBits16_31);
                        } else if (mask == 0x03) {
                           iris_move_generic(proc, inst, MaskBits0_15);
                        } else {
                           iris_move_generic(proc, inst, generate_full_mask(mask));
                        }
                        break;
                     }
                  default:
                     iris_error("Illegal move operation position bits!", ErrorIllegalMoveBits);
               }
            }
            break;
         }
      case MoveOpSwap:
         {
            // do nothing
            if (get_move_reg0(inst) == get_move_reg1(inst)) {
               break;
            }
            mask = (word)(byte)get_move_mask(inst);
            if (mask != 0) {
               switch(get_move_position(inst)) {
                  case MoveOp_Form_64bit_chunks:
                     {
                     // properly format the mask
                     mask &= 0x1;
                     if (mask == 1) {
                        iris_swapfull(proc, inst);
                     } else {
                        iris_error("The bit mask is improperly formed for a 64bit move!", ErrorIllegalMoveBits);
                     }
                     break;
                     }
                  case MoveOp_Form_32bit_chunks:
                     {
                        mask &= 0x3;
                        switch (mask) {
                           case 0x1:
                              iris_swaplower32(proc, inst);
                              break;
                           case 0x2:
                              iris_swapupper32(proc, inst);
                              break;
                           case 0x3:
                              iris_swapfull(proc, inst);
                              break;
                           default:
                              iris_error("The bit mask is improperly formed for a 32bit move!", ErrorIllegalMoveBits);
                        }
                     break;
                     }
                  case MoveOp_Form_16bit_chunks:
                     {
                        mask &= 0xF;
                        switch (mask) {
                              case 0xF:
                                 iris_swapfull(proc, inst);
                                 break;
                              case 0xE:
                                 iris_swap_generic(proc, inst, 0xFFFFFFFFFFFF0000);
                                 break;
                              case 0xD:
                                 iris_swap_generic(proc, inst, 0xFFFFFFFF0000FFFF);
                                 break;
                              case 0xC:
                                 iris_swapupper32(proc, inst);
                                 break;
                              case 0xB:
                                 iris_swap_generic(proc, inst, 0xFFFF0000FFFFFFFF);
                                 break;
                              case 0xA:
                                 iris_swap_generic(proc, inst, 0xFFFF0000FFFF0000);
                                 break;
                              case 0x9:
                                 iris_swap_generic(proc, inst, 0xFFFF00000000FFFF);
                                 break;
                              case 0x8:
                                 iris_swap_generic(proc, inst, 0xFFFF000000000000);
                                 break;
                              case 0x7:
                                 iris_swap_generic(proc, inst, 0x0000FFFFFFFFFFFF);
                                 break;
                              case 0x6:
                                 iris_swap_generic(proc, inst, 0x0000FFFFFFFF0000);
                                 break;
                              case 0x5:
                                 iris_swap_generic(proc, inst, 0x0000FFFF0000FFFF);
                                 break;
                              case 0x4:
                                 iris_swap_generic(proc, inst, 0x0000FFFF00000000);
                                 break;
                              case 0x3:
                                 iris_swaplower32(proc, inst);
                                 break;
                              case 0x2:
                                 iris_swap_generic(proc, inst, 0x00000000FFFF0000);
                                 break;
                              case 0x1:
                                 iris_swap_generic(proc, inst, 0x000000000000FFFF);
                                 break;
                              default:
                                 iris_error("The bit mask is improperly formed for a 16bit move!", ErrorIllegalMoveBits);
                        }
                        break;
                     }
                  case MoveOp_Form_8bit_chunks:
                     {
                        if (mask == 0xFF) {
                           iris_swapfull(proc, inst);
                        } else if (mask == 0xF0) {
                           iris_swapupper32(proc, inst);
                        } else if (mask == 0x0F) {
                           iris_swaplower32(proc, inst);
                        } else if (mask == 0xC0) {
                           iris_swap_generic(proc, inst, MaskBits48_63);
                        } else if (mask == 0x30) {
                           iris_swap_generic(proc, inst, MaskBits32_47);
                        } else if (mask == 0x0C) {
                           iris_swap_generic(proc, inst, MaskBits16_31);
                        } else if (mask == 0x03) {
                           iris_swap_generic(proc, inst, MaskBits0_15);
                        } else {
                           iris_swap_generic(proc, inst, generate_full_mask(mask));
                        }
                        break;
                     }
                  default:
                     iris_error("Illegal move operation position bits!", ErrorIllegalMoveBits);
               }
            }
            break;
         }
      case MoveOpSet:
         {
            word value = (word)get_move_immediate(inst);
            word reg = iris_get_register(proc, get_move_reg0(inst));
            word mask = 0;
            word shift = 0;

            switch(get_move_position(inst)) {
               case MoveOpSet_Bits_0_15:
                  mask = MaskBits0_15;
                  shift = 0;
                  break;
               case MoveOpSet_Bits_16_31:
                  mask = MaskBits16_31;
                  shift = 16;
                  break;
               case MoveOpSet_Bits_32_47:
                  mask = MaskBits32_47;
                  shift = 32;
                  break;
               case MoveOpSet_Bits_48_63:
                  mask = MaskBits48_63;
                  shift = 48;
                  break;
               default:
                  iris_error("Illegal set operation bit position!", ErrorIllegalSetBits);
            }
            iris_put_register(proc, get_move_reg0(inst), (reg & mask) | ((value << shift) & (~mask)));
            break;
         }
      case MoveOpSlice:
         {
            // pull the contents of the source out
            byte target = 0,
                 mask = get_move_mask(inst),
                 result = 0;
            word contents = iris_get_register(proc, get_move_reg1(inst)),
                 dest = iris_get_register(proc, get_move_reg0(inst)),
                 shift = 0;
         switch(get_move_position(inst)) {
            case SliceOp_0to7:
               target = (byte)contents;
               dest &= 0xFFFFFFFFFFFFFF00;
               break;
            case SliceOp_8to15:
               target = (byte)(contents >> 8);
               dest &= 0xFFFFFFFFFFFF00FF;
               shift = 8;
               break;
            case SliceOp_16to23:
               target = (byte)(contents >> 16);
               dest &= 0xFFFFFFFFFF00FFFF;
               shift = 16;
               break;
            case SliceOp_24to31:
               target = (byte)(contents >> 24);
               dest &= 0xFFFFFFFF00FFFFFF;
               shift = 24;
               break;
            case SliceOp_32to39:
               target = (byte)(contents >> 32);
               dest &= 0xFFFFFF00FFFFFFFF;
               shift = 32;
               break;
            case SliceOp_40to47:
               target = (byte)(contents >> 40);
               dest &= 0xFFFF00FFFFFFFFFF;
               shift = 40;
               break;
            case SliceOp_48to55:
               target = (byte)(contents >> 48);
               dest &= 0xFF00FFFFFFFFFFFF;
               shift = 48;
               break;
            case SliceOp_56to63:
               target = (byte)(contents >> 56);
               dest &= 0x00FFFFFFFFFFFFFF;
               shift = 56;
               break;
            default:
               iris_error("illegal slice position defined!", ErrorInvalidMoveOperation);
         }
         if (mask != 0) {
            // put result into the destination
            result = target & mask;
         }
         iris_put_register(proc, get_move_reg0(inst), dest | (((word)result) << shift));
         break;
         }
      default:
         iris_error("Illegal move operation!", ErrorInvalidMoveOperation);
   }
}
void iris_jump(iris_core* proc, instruction* inst) {
   word a = 0;
   proc->advancepc = false;
   switch(get_jump_form(inst)) {
      case JumpOpUnconditional: 
         {
            if (get_jump_link_flag(inst)) {
               a = proc->pc + 1;
               if (get_jump_immediate_flag(inst)) {
                  proc->pc = get_jump_immediate(inst);
               } else {
                  proc->pc = iris_get_register(proc, get_jump_reg1(inst));
               }
               iris_put_register(proc, get_jump_reg0(inst), a);
            } else {
              if (get_jump_immediate_flag(inst)) {
                 proc->pc = get_jump_immediate(inst);
              } else {
                 proc->pc = iris_get_register(proc, get_jump_reg0(inst));
              }
            }
            break;
         }
      case JumpOpConditional: 
         {
            if (get_jump_true_false_flag(inst)) { 
               /* true form */
               if (get_jump_link_flag(inst)) {
                  /* link form */
                  if (get_jump_immediate_flag(inst)) {
                     /* immediate form */
                     if ((iris_get_register(proc, PredicateRegisterIndex) != 0)) {
                        iris_put_register(proc, get_jump_reg0(inst), proc->pc + 1);
                        proc->pc = get_jump_immediate(inst); 
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  } else {
                     /* register form */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) != 0)) {
                        // cache this result first in-case the same register is
                        // used in both address to jump to and link register...
                        a = iris_get_register(proc, get_jump_reg2(inst)); 
                        iris_put_register(proc, get_jump_reg1(inst), proc->pc + 1);
                        proc->pc = a;
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  }
               } else {
                  /* non link form */
                  if (get_jump_immediate_flag(inst)) {
                     /* immediate form */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) != 0)) {
                        proc->pc = get_jump_immediate(inst);
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  } else {
                     /* register form */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) != 0)) {
                        proc->pc = iris_get_register(proc, get_jump_reg1(inst));
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  }
               }
            } else {
               /* false form */
               if (get_jump_link_flag(inst)) {
                  /* link */
                  if (get_jump_immediate_flag(inst)) {
                     /* immediate */
                     if ((iris_get_register(proc, PredicateRegisterIndex) == 0)) {
                        iris_put_register(proc, get_jump_reg0(inst), proc->pc + 1);
                        proc->pc = get_jump_immediate(inst); 
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  } else {
                     /* register */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) == 0)) {
                        // cache this result first in-case the same register is
                        // used in both address to jump to and link register...
                        a = iris_get_register(proc, get_jump_reg2(inst));
                        iris_put_register(proc, get_jump_reg1(inst), proc->pc + 1);
                        proc->pc = a;
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  }
               } else {
                  /* non-link */
                  if (get_jump_immediate_flag(inst)) {
                     /* immediate */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) == 0)) {
                        proc->pc = get_jump_immediate(inst);
                     } else {
                        proc->pc = proc->pc + 1;
                     }

                  } else {
                     /* register */
                     if ((iris_get_register(proc, get_jump_reg0(inst)) == 0)) {
                        proc->pc = iris_get_register(proc, get_jump_reg1(inst));
                     } else {
                        proc->pc = proc->pc + 1;
                     }
                  }
               }
            }
            break;
         }
      case JumpOpIfThenElse:
         {
            byte onTrue = 0,
                 onFalse = 0;

            if (get_jump_true_false_flag(inst)) {
               /* true */
               onTrue = get_jump_reg1(inst);
               onFalse = get_jump_reg2(inst);
            } else {
               /* false, just reverse the registers internally so that we can
                * use the same code for both paths */
               onTrue = get_jump_reg2(inst);
               onFalse = get_jump_reg1(inst);
            }
            if (get_jump_link_flag(inst)) {
               a = iris_get_register(proc, PredicateRegisterIndex);
               /* do this second in case the predicate register is reg0  as
                * well.... */
               iris_put_register(proc, get_jump_reg0(inst), proc->pc + 1);
            } else {
               a = iris_get_register(proc, get_jump_reg0(inst));
            }
            if (a != 0) {
               proc->pc = iris_get_register(proc, onTrue);
            } else {
               proc->pc = iris_get_register(proc, onFalse);
            }
            break;
         }
      default:
         iris_error("invalid jump operation", ErrorInvalidJumpOperation);
         break;
   }
}

void iris_compare(iris_core* proc, instruction* inst) {
   /* grab the appropriate value */
   word tmp = 0;
   switch(get_compare_op(inst)) {
#define X(class, symbol, _) \
      case class: \
                  tmp = (iris_get_register(proc, get_compare_reg1(inst))) symbol (iris_get_register(proc, get_compare_reg2(inst))); \
      break; 
#include "moveops.def"
#undef X
      default:
         iris_error("invalid compare operation", ErrorInvalidCompareOperation);
   }
   switch(get_combine_op(inst)) {
      case CombineOpSet:
         iris_put_register(proc, get_compare_reg0(inst), tmp);
         break;
      case CombineOpAnd:
         iris_put_register(proc, get_compare_reg0(inst), tmp & iris_get_register(proc, get_compare_reg0(inst)));
         break;
      case CombineOpOr:
         iris_put_register(proc, get_compare_reg0(inst), tmp | iris_get_register(proc, get_compare_reg0(inst)));
         break;
      case CombineOpXor:
         iris_put_register(proc, get_compare_reg0(inst), tmp ^ iris_get_register(proc, get_compare_reg0(inst)));
         break;
      default:
         iris_error("invalid combine operation", ErrorInvalidCombineOperation);
   }
}
static void iris_system_call(iris_core* proc, instruction* j);
void iris_misc(iris_core* proc, instruction* j) {
   /* implement system commands */
   switch(get_misc_op(j)) {
      case MiscOpSystemCall:
         iris_system_call(proc, j);
         break;
      default:
         iris_error("invalid misc operation", ErrorInvalidMiscOperation);
   }
}

void iris_system_call(iris_core* proc, instruction* j) {
   byte reg0 = get_misc_reg0(j);
   //byte reg1 = get_misc_reg1(j); // currently unused
   switch(get_misc_index(j)) {
      case SystemCommandTerminate: /* init 0 */
         proc->terminateexecution = 1;
         break;
      case SystemCommandGetC:
         iris_put_register(proc, reg0, (word)getchar());
         break;
      case SystemCommandPutC:
         putchar((word)iris_get_register(proc, reg0));
         break;
      default:
         iris_error("invalid system command provided", ErrorInvalidSystemCommand);
   }
}

void iris_error(char* message, int code) {
   fprintf(stderr, "%s\n", message);
   exit(code);
}

void iris_rom_init(iris_core* proc) {
   proc->pc = 0;
   proc->terminateexecution = 0;
   proc->advancepc = 1;
   /* clear out processor space to be sure everything is okay */
   for(int i = 0; i < RegisterCount; i++) {
      proc->gpr[i] = 0;
   }
   /*
   for(int i = 0; i < MemorySize; ++i) {
      proc->data[i] = 0;
      proc->stack[i] = 0;
      proc->code[i] = 0;
   }
   */
   /* by default we use these registers for stack and predicate so set them as such */
   proc->gpr[PredicateRegisterIndex] = 0;
   proc->gpr[StackPointerRegisterIndex] = 0xFFFF;
}

void iris_shutdown(iris_core* c) {
   /* do nothing right now */
   free(c->memory);
   c->memory = 0;
   c->memorysize = 0;
}
void iris_new_core(iris_core* proc, hword memorysize) {
   proc->memory = calloc(memorysize, sizeof(byte));
   proc->memorysize = memorysize;
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
static word iris_merge_load(iris_core*, word, word, byte);
static void iris_full_store(iris_core* proc, word addr, word value) {
   // need to decompose it into multiple pieces
   if ((addr + 7) >= proc->memorysize) {
      iris_error("MEMORY PROTECTION ERROR! Attempted to access outside memory boundaries", 8);
   }
   proc->memory[addr + 0] = (byte)(value);
   proc->memory[addr + 1] = (byte)(value >> 8);
   proc->memory[addr + 2] = (byte)(value >> 16);
   proc->memory[addr + 3] = (byte)(value >> 24);
   proc->memory[addr + 4] = (byte)(value >> 32);
   proc->memory[addr + 5] = (byte)(value >> 40);
   proc->memory[addr + 6] = (byte)(value >> 48);
   proc->memory[addr + 7] = (byte)(value >> 56);
}
static word iris_full_load(iris_core* proc, word addr) {
   if ((addr + 7) >= proc->memorysize) {
      iris_error("MEMORY PROTECTION ERROR! Attempted to access outside memory boundaries", 8);
   }
   return ((word)(proc->memory[addr])) |
      (((word)(proc->memory[addr+1])) << 8)  |
      (((word)(proc->memory[addr+2])) << 16)  |
      (((word)(proc->memory[addr+3])) << 24)  |
      (((word)(proc->memory[addr+4])) << 32)  |
      (((word)(proc->memory[addr+5])) << 40) |
      (((word)(proc->memory[addr+6])) << 48) |
      (((word)(proc->memory[addr+7])) << 56);
}
static void iris_store_overwrite(iris_core* proc, word addr, word value, byte mask) {
   if (mask == 0x00) {
      // since none of the bits are active in the mask it must be zero that we
      // want to store in there
      iris_full_store(proc, addr, 0);
   } else if (mask == 0xFF) {
      iris_full_store(proc, addr, value);
   } else {
      iris_full_store(proc, addr, value & generate_full_mask(mask));
   }
}

static void iris_store_merge(iris_core* proc, word addr, word value, byte mask) {
   if (mask == 0x00) {
      // do nothing since this is a merge!
   } else if (mask == 0xFF) {
      iris_full_store(proc, addr, value);
   } else {
      iris_full_store(proc, addr, iris_merge_load(proc, addr, value, mask));
   }
}


static word iris_load_memory(iris_core* proc, word addr, byte mask) {
   if (mask == 0x00) {
      return 0;
   } else if (mask == 0xFF) {
      return iris_full_load(proc, addr);
   } else {
      return iris_full_load(proc, addr) & generate_full_mask(mask);
   }
}

static word iris_merge_load(iris_core* proc, word addr, word value, byte mask) {
   if (mask == 0x00) {
      return value;
   } else if (mask == 0xFF) {
      return iris_full_load(proc, addr);
   } else {
      return (value & ~generate_full_mask(mask)) | (iris_full_load(proc, addr) & generate_full_mask(mask)) ;
   }
}
static void iris_load(iris_core* proc, bool merge, word addr, word value, byte mask, byte dest) {
   iris_put_register(proc, dest, merge ?  iris_merge_load(proc, addr, value, mask) : iris_load_memory(proc, addr, mask));
}
static void iris_store(iris_core* proc, bool merge, word addr, word value, byte mask) {
   if (merge) {
      iris_store_merge(proc, addr, value, mask);
   } else {
      iris_store_overwrite(proc, addr, value, mask);
   }
}
void iris_load_store(iris_core* proc, instruction* inst) {
   // load a word's worth of data and then modify it according to the bit mask
   word addr0 = iris_get_register(proc, get_load_store_reg0(inst)),
        addr1 = iris_get_register(proc, get_load_store_reg1(inst));
   switch(get_load_store_op(inst)) {
      case LoadStoreOp_Load:
         //addr1 is address
         //addr0 is value
         iris_load(proc, (bool)get_load_store_merge_flag(inst), addr1, addr0, get_load_store_mask(inst), get_load_store_reg0(inst));
         break;
      case LoadStoreOp_Store:
         // addr0 is address
         // addr1 is value
         iris_store(proc, (bool)get_load_store_merge_flag(inst), addr0, addr1, get_load_store_mask(inst));
         break;
      default:
         iris_error("invalid load/store operation provided", ErrorInvalidLoadStoreOperation);
   }
}
