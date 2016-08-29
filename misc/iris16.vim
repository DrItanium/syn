" Vim syntax file
" Language:	iris16 assembler
" Maintainer:	Joshua Scoggins <theoretically.x64@gmail.com

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn case ignore

" storage types and declarators
syn match asmType "@declare"
syn match asmType "@org"
syn match asmSection "@code"
syn match asmSection "@data"
syn match asmType "@label"
syn keyword asmOpcode ld ldi ldm ldc
syn keyword asmOpcode st stc set pop push swap move pushi
syn keyword asmOpcode memset
syn keyword asmOpcode j jl jr jrl
syn keyword asmOpcode jt jtl jrt jrtl
syn keyword asmOpcode jf jfl jrf jrfl
syn keyword asmOpcode ift iff iftl iffl
syn keyword asmOpcode add addi sub subi mul muli div divi rem remi 
syn keyword asmOpcode and or not xor shl shr shli shri 
syn keyword asmOpcode halve double incr decr
syn keyword asmOpcode eq ne lt gt le ge 
syn keyword asmOpcode eqi nei lti gti lei gei
syn keyword asmOpcode system
syn keyword asmOpcode stx ldx

syn keyword asmRegister r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31 r32 r33 r34 r35 r36 r37 r38 r39 r40 r41 r42 r43 r44 r45 r46 r47 r48 r49 r50 r51 r52 r53 r54 r55 r56 r57 r58 r59 r60 r61 r62 r63 r64 r65 r66 r67 r68 r69 r70 r71 r72 r73 r74 r75 r76 r77 r78 r79 r80 r81 r82 r83 r84 r85 r86 r87 r88 r89 r90 r91 r92 r93 r94 r95 r96 r97 r98 r99 r100 r101 r102 r103 r104 r105 r106 r107 r108 r109 r110 r111 r112 r113 r114 r115 r116 r117 r118 r119 r120 r121 r122 r123 r124 r125 r126 r127 r128 r129 r130 r131 r132 r133 r134 r135 r136 r137 r138 r139 r140 r141 r142 r143 r144 r145 r146 r147 r148 r149 r150 r151 r152 r153 r154 r155 r156 r157 r158 r159 r160 r161 r162 r163 r164 r165 r166 r167 r168 r169 r170 r171 r172 r173 r174 r175 r176 r177 r178 r179 r180 r181 r182 r183 r184 r185 r186 r187 r188 r189 r190 r191 r192 r193 r194 r195 r196 r197 r198 r199 r200 r201 r202 r203 r204 r205 r206 r207 r208 r209 r210 r211 r212 r213 r214 r215 r216 r217 r218 r219 r220 r221 r222 r223 r224 r225 r226 r227 r228 r229 r230 r231 r232 r233 r234 r235 r236 r237 r238 r239 r240 r241 r242 r243 r244 r245 r246 r247 r248 r249 r250 r251 r252 r253 r254 r255
syn keyword asmMacroRegister outptr inptr fdcur zero stmp7 wlen stmp6 stmp5 stmp4 stmp3 stmp2 stmp1 stmp0
syn keyword asmMacroRegister sres3 sres2 sres1 sres0 spar3 spar2 spar1 spar0 rend rtop pend ptop sp lr ip 
syn keyword asmMacroRegister space
syn keyword asmMacroRegister xstorage dataLower dataUpper localStorage toCall dataCacheStart dataCacheEnd
syn keyword asmMacroRegister storageSizeLower storageSizeUpper storageBandSize dataCacheAddressLower dataCacheAddressUpper

syn match asmIdentifier		"[a-z_-][a-z0-9_-]*"

syn match decNumber		"[0-9]\d*"
syn match hexNumber		"0x[0-9a-fA-F]\+"
syn keyword asmTodo		contained TODO
syn match asmComment		";.*" contains=asmTodo
syn match asmDirective		"@[A-Za-z][0-9A-Za-z-_]*"
syn case match

if version >= 508 || !exists("did_asm_syntax_inits")
  if version < 508
    let did_asm_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " The default methods for highlighting.  Can be overridden later
  HiLink asmSection	Special
  HiLink asmLabel	Label
  HiLink asmComment	Comment
  HiLink asmTodo	Todo
  HiLink asmDirective	Statement

  HiLink asmInclude	Include
  HiLink asmCond	PreCondit
  HiLink asmMacro	Macro

  HiLink hexNumber	Number
  HiLink decNumber	Number

  HiLink asmIdentifier	Identifier
  HiLink asmType	Type

  HiLink asmRegister Identifier
  HiLink asmOpcode Statement
  HiLink asmMacroRegister Macro

  delcommand HiLink
endif

let b:current_syntax = "iris16"

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: ts=8
