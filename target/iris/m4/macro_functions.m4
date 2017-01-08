define(`registerAlias', `define($1, $2)')dnl
registerAlias(sp, r255)dnl
registerAlias(v0, r0)dnl
registerAlias(v1, r1)dnl
registerAlias(v2, r2)dnl
registerAlias(v3, r3)dnl
registerAlias(v4, r4)dnl
registerAlias(v5, r5)dnl
registerAlias(v6, r6)dnl
registerAlias(v7, r7)dnl
registerAlias(arg0, r32)dnl
registerAlias(arg1, r33)dnl
registerAlias(arg2, r34)dnl
registerAlias(arg3, r35)dnl
registerAlias(return0, r64)dnl
registerAlias(return1, r65)dnl
registerAlias(return2, r66)dnl
registerAlias(return3, r67)dnl
registerAlias(return4, r68)dnl
registerAlias(return5, r69)dnl
registerAlias(return6, r70)dnl
registerAlias(return7, r71)dnl
registerAlias(temp0, r96)dnl
registerAlias(temp1, r97)dnl
registerAlias(temp2, r98)dnl
registerAlias(temp3, r99)dnl
registerAlias(temp4, r100)dnl
registerAlias(temp5, r101)dnl
registerAlias(temp6, r102)dnl
registerAlias(temp7, r103)dnl
registerAlias(vp0, p15)dnl
registerAlias(vp1, p14)dnl
dnl
define(`incr', `addi $1 $1 0x1')dnl
define(`decr', `subi $1 $1 0x1')dnl
define(`halve', `divi $1 $1 0x2')dnl
define(`double', `muli $1 $2 0x3')dnl
dnl wideform has several arguments
dnl 1) the operation to invoke
dnl 2) The destination register
dnl 3) the first source register
dnl 4) the full immediate
dnl define(`immwideform', `define(`$1iw', `set v0 `$3' $1 `1' `$2' v0')')
dnl I'm not sure how to get the above line to work so right now, it will have
dnl to be hand generated
define(`addiw', `set v0 $3 add $1 $2 v0')dnl
define(`subiw', `set v0 $3 sub $1 $2 v0')dnl
define(`muliw', `set v0 $3 mul $1 $2 v0')dnl
define(`diviw', `set v0 $3 div $1 $2 v0')dnl
define(`remiw', `set v0 $3 rem $1 $2 v0')dnl
define(`shliw', `set v0 $3 shl $1 $2 v0')dnl
define(`shriw', `set v0 $3 shr $1 $2 v0')dnl
define(`andiw', `set v0 $3 and $1 $2 v0')dnl
define(`oriw', `set v0 $3 or $1 $2 v0')dnl
define(`notiw', `set v0 $2 not $1 v0')dnl
define(`nor', `or $1 $2 $3 not $1 $1')dnl
define(`xnor', `xor $1 $2 $3 not $1 $1')dnl
define(`nand', `and $1 $2 $3 not $1 $1')dnl
