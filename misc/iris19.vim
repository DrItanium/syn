" Vim syntax file
" Language:	iris19 assembler
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
syn match asmType "@word"
syn match asmType "@org"
syn match asmType "@label"
syn match asmType "@dword"
syn match asmType "@constant"
syn match asmType "@startup"
syn keyword asmOperation nop arithmetic shift logical compare branch return system move set swap memory complex
syn keyword asmOperation increment decrement double halve zero copy
syn keyword asmOpcode load store push pop 
syn keyword asmOpcode add sub mul div rem 
syn keyword asmOpcode left right
syn keyword asmOpcode encoding bitset bitunset encode decode
syn keyword asmOpcode == ! < <= > >=
syn keyword asmOpcode none and or xor not nand
syn keyword asmOpcode call if cond indirect
syn keyword asmOpcode immediate

syn keyword asmRegister r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 
syn keyword asmMacroRegister value addr field mask ip cr sp shift_width
" syn keyword asmMacroRegister outptr inptr fdcur zero stmp7 wlen stmp6 stmp5 stmp4 stmp3 stmp2 stmp1 stmp0
" syn keyword asmMacroRegister sres3 sres2 sres1 sres0 spar3 spar2 spar1 spar0 rend rtop pend ptop sp lr ip 
" syn keyword asmMacroRegister space

syn match asmIdentifier		"[a-z_-][a-z0-9_-]*"

syn match decNumber		"[0-9]\d*"
syn match hexNumber		"0x[0-9a-fA-F]\+"
syn match binNumber		"0b[0-1]*"
syn match bitmaskNumber         "0m[01][01][01][01]"
syn keyword asmTodo		contained TODO
syn match asmComment		";.*" contains=asmTodo
syn match asmDirective		"@[A-Za-z][0-9A-Za-z-_]*"
syn match asmAlias		"[?][A-Za-z0-9]\+"

syn case match

if version >= 508 || !exists("did_asm_syntax_inits")
  if version < 508
    let did_asm_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " The default methods for highlighting.  Can be overridden later
  HiLink asmLabel	Label
  HiLink asmComment	Comment
  HiLink asmTodo	Todo
  HiLink asmDirective	Statement

  HiLink asmInclude	Include
  HiLink asmCond	PreCondit
  HiLink asmMacro	Macro
  HiLink asmAlias	Macro

  HiLink hexNumber	Number
  HiLink decNumber	Number
  HiLink bitmaskNumber  Number
  HiLink binNumber      Number

  HiLink asmIdentifier	Identifier
  HiLink asmType	Type

  HiLink asmRegister 	  Macro
  HiLink asmOpcode 	  Type
  HiLink asmOperation     Special
  HiLink asmMacroRegister Macro

  delcommand HiLink
endif

let b:current_syntax = "iris19"

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: ts=8
