" Creating syntax groups
" syntax keyword fBasicType Qubit Vec Nat VNil VCons Circ
syntax keyword fKeyword else in let case of then if module where import do
syntax keyword fSpecial dynlift
syntax match fOperator "[\*!=:]"
syntax match fOperator "->"
syntax match fType "\l\@<!\<\u\w\+"
syntax match fNumber /\c\<\%(\d\+\%(e[+-]\=\d\+\)\=\|0b[01]\+\|0o\o\+\|0x\%(\x\|_\)\+\)n\=\>/
syntax match fDouble /\c\<\%(\d\+\.\d\+\|\d\+\.\|\.\d\+\)\%(e[+-]\=\d\+\)\=\>/
syntax region fString start=+"\|c"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell
syntax match fInlineComment "--.*\n"
syntax match fComment "{-.*-}"

" Linking highlighting
highlight link fKeyword Keyword
highlight link fType Type
highlight link fBasicType Type
highlight link fOperator Operator
highlight link fSpecial Identifier
highlight link fNumber Number
highlight link fDouble Double
highlight link fString String
highlight link fInlineComment Comment
highlight link fComment Comment
