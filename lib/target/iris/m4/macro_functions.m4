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
define(`immset', `set v0 $1')dnl
define(`addiw', `immset($3) add $1 $2 v0')dnl
define(`subiw', `immset($3) sub $1 $2 v0')dnl
define(`muliw', `immset($3) mul $1 $2 v0')dnl
define(`diviw', `immset($3) div $1 $2 v0')dnl
define(`remiw', `immset($3) rem $1 $2 v0')dnl
define(`shliw', `immset($3) shl $1 $2 v0')dnl
define(`shriw', `immset($3) shr $1 $2 v0')dnl
define(`andiw', `immset($3) and $1 $2 v0')dnl
define(`oriw', `immset($3) or $1 $2 v0')dnl
define(`notiw', `immset($2) not $1 v0')dnl
define(`nor', `or $1 $2 $3 not $1 $1')dnl
define(`xnor', `xor $1 $2 $3 not $1 $1')dnl
define(`nand', `and $1 $2 $3 not $1 $1')dnl
dnl
define(`immcmp', `$1 vp0 vp0 $2 $3')
define(`bigen', `immcmp($1, $2, $3) bit vp0 $4')dnl
define(`bilgen', `immcmp($1, $2, $3) bitl vp0 $4')dnl
define(`bgen', `immcmp($1, $2, $3) bt vp0 $4')dnl
define(`blgen', `immcmp($1, $2, $3) btl vp0 $4')dnl
define(`blrgen', `immcmp($1, $2, $3) blrt vp0')dnl
define(`blrlgen', `immcmp($1, $2, $3) blrtl vp0')dnl
define(`bieq', `bigen(eq, $1, $2, $3)')dnl
define(`bieql', `bilgen(eq, $1, $2, $3)')dnl
define(`beq', `bgen(eq, $1, $2, $3)')dnl
define(`beql', `blgen(eq, $1, $2, $3)')dnl
define(`beqlr', `blrgen(eq, $1, $2, $3)')dnl
define(`beqlrl', `blrlgen(eq, $1, $2, $3)')dnl
define(`bieqi', `bigen(eqi, $1, $2, $3)')dnl
define(`bieqil', `bilgen(eqi, $1, $2, $3)')dnl
define(`beqi', `bgen(eqi, $1, $2, $3)')dnl
define(`beqil', `blgen(eqi, $1, $2, $3)')dnl
define(`beqilr', `blrgen(eqi, $1, $2, $3)')dnl
define(`beqilrl', `blrlgen(eqi, $1, $2, $3)')dnl
define(`bine', `bigen(ne, $1, $2, $3)')dnl
define(`binel', `bilgen(ne, $1, $2, $3)')dnl
define(`bne', `bgen(ne, $1, $2, $3)')dnl
define(`bnel', `blgen(ne, $1, $2, $3)')dnl
define(`bnelr', `blrgen(ne, $1, $2, $3)')dnl
define(`bnelrl', `blrlgen(ne, $1, $2, $3)')dnl
define(`binei', `bigen(nei, $1, $2, $3)')dnl
define(`bineil', `bilgen(nei, $1, $2, $3)')dnl
define(`bnei', `bgen(nei, $1, $2, $3)')dnl
define(`bneil', `blgen(nei, $1, $2, $3)')dnl
define(`bneilr', `blrgen(nei, $1, $2, $3)')dnl
define(`bneilrl', `blrlgen(nei, $1, $2, $3)')dnl
define(`bilt', `bigen(lt, $1, $2, $3)')dnl
define(`biltl', `bilgen(lt, $1, $2, $3)')dnl
define(`blt', `bgen(lt, $1, $2, $3)')dnl
define(`bltl', `blgen(lt, $1, $2, $3)')dnl
define(`bltlr', `blrgen(lt, $1, $2, $3)')dnl
define(`bltlrl', `blrlgen(lt, $1, $2, $3)')dnl
define(`bilti', `bigen(lti, $1, $2, $3)')dnl
define(`biltil', `bilgen(lti, $1, $2, $3)')dnl
define(`blti', `bgen(lti, $1, $2, $3)')dnl
define(`bltil', `blgen(lti, $1, $2, $3)')dnl
define(`bltilr', `blrgen(lti, $1, $2, $3)')dnl
define(`bltilrl', `blrlgen(lti, $1, $2, $3)')dnl
define(`bigt', `bigen(gt, $1, $2, $3)')dnl
define(`bigtl', `bilgen(gt, $1, $2, $3)')dnl
define(`bgt', `bgen(gt, $1, $2, $3)')dnl
define(`bgtl', `blgen(gt, $1, $2, $3)')dnl
define(`bgtlr', `blrgen(gt, $1, $2, $3)')dnl
define(`bgtlrl', `blrlgen(gt, $1, $2, $3)')dnl
define(`bigti', `bigen(gti, $1, $2, $3)')dnl
define(`bigtil', `bilgen(gti, $1, $2, $3)')dnl
define(`bgti', `bgen(gti, $1, $2, $3)')dnl
define(`bgtil', `blgen(gti, $1, $2, $3)')dnl
define(`bgtilr', `blrgen(gti, $1, $2, $3)')dnl
define(`bgtilrl', `blrlgen(gti, $1, $2, $3)')dnl
define(`bile', `bigen(le, $1, $2, $3)')dnl
define(`bilel', `bilgen(le, $1, $2, $3)')dnl
define(`ble', `bgen(le, $1, $2, $3)')dnl
define(`blel', `blgen(le, $1, $2, $3)')dnl
define(`blelr', `blrgen(le, $1, $2, $3)')dnl
define(`blelrl', `blrlgen(le, $1, $2, $3)')dnl
define(`bilei', `bigen(lei, $1, $2, $3)')dnl
define(`bileil', `bilgen(lei, $1, $2, $3)')dnl
define(`blei', `bgen(lei, $1, $2, $3)')dnl
define(`bleil', `blgen(lei, $1, $2, $3)')dnl
define(`bleilr', `blrgen(lei, $1, $2, $3)')dnl
define(`bleilrl', `blrlgen(lei, $1, $2, $3)')dnl
define(`bige', `bigen(ge, $1, $2, $3)')dnl
define(`bigel', `bilgen(ge, $1, $2, $3)')dnl
define(`bge', `bgen(ge, $1, $2, $3)')dnl
define(`bgel', `blgen(ge, $1, $2, $3)')dnl
define(`bgelr', `blrgen(ge, $1, $2, $3)')dnl
define(`bgelrl', `blrlgen(ge, $1, $2, $3)')dnl
define(`bigei', `bigen(gei, $1, $2, $3)')dnl
define(`bigeil', `bilgen(gei, $1, $2, $3)')dnl
define(`bgei', `bgen(gei, $1, $2, $3)')dnl
define(`bgeil', `blgen(gei, $1, $2, $3)')dnl
define(`bgeilr', `blrgen(gei, $1, $2, $3)')dnl
define(`bgeilrl', `blrlgen(gei, $1, $2, $3)')dnl
dnl
define(`crnand', `crand $1 $1 $3 $4 crnot $1 $2 $1')dnl
define(`crnor', `cror $1 $1 $3 $4 crnot $1 $2 $1')dnl
define(`clear', `set $1 0x0000')dnl
define(`ret', `blr')dnl
define(`savelr', `mflr v0 push $1 v0')dnl
define(`restorelr', `pop v0 $1 mtlr v0')dnl
