include(`core.m4')
include(`registers.m4')
include(`macro_functions.m4')
include(`extended_functions.m4')
dnl Initial Program Load of an Iris system
dnl this should always be the first code that is execed
@code 
@org 0x0000
clear(zero)
set one 0x1
