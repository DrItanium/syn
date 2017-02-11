dnl encode value into the provided input and store the result into destination
dnl 1) destination
dnl 2) input
dnl 3) value
dnl 4) bitmask
dnl 5) shift
define(`encodeBits', `not v0 $4 and v0 $2 v0 shl v1 $3 $5 or $1 v0 v1')dnl
dnl decode a value from an input given a mask and shift values
dnl 1) destination
dnl 2) input
dnl 3) bitmask
dnl 4) shift
define(`decodeBits', `and $1 $2 $3 shr $1 $1 $4')dnl

