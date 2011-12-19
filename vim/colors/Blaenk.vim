" Vim color file
" Converted from Textmate theme Blaenk using Coloration v0.2.5 (http://github.com/sickill/coloration)

set background=dark
highlight clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "Blaenk"

hi Cursor  guifg=#224D70 guibg=#82A3BF gui=NONE
hi Visual  guifg=NONE guibg=#373b41 gui=NONE
hi CursorLine  guifg=NONE guibg=#282a2e gui=NONE
hi CursorColumn  guifg=NONE guibg=#282a2e gui=NONE
hi LineNr  guifg=#717474 guibg=#282a2e gui=NONE
hi VertSplit  guifg=#3d3f40 guibg=#3d3f40 gui=NONE
hi MatchParen guifg=#224d70 guibg=#82a3bf
hi StatusLine  guifg=#c5c8c6 guibg=#3d3f40 
hi StatusLineNC  guifg=#c5c8c6 guibg=#3d3f40 gui=NONE
hi Pmenu  guifg=NONE  gui=NONE
hi PmenuSel  guifg=NONE guibg=#373b41 gui=NONE
hi IncSearch  guibg=#224D70 guifg=#82A3BF gui=NONE
hi Search  guibg=#224D70 guifg=#82A3BF gui=NONE
hi Directory  guifg=#b5bd68  
hi Folded  guifg=#969896 guibg=#1d1f21 gui=NONE
hi Delimiter guifg=#cc6666

hi DiffDelete guifg=#822021 guibg=#df5f5f
hi diffRemoved guifg=#822021 guibg=#df5f5f

hi DiffAdd guifg=#5c622b guibg=#c1c780
hi diffAdded guifg=#5c622b guibg=#c1c780

"hi diffOldFile
"hi diffNewFile
"hi diffLine
"hi diffFile

hi Normal  guifg=#c5c8c6 guibg=#1d1f21 gui=NONE
hi Boolean  guifg=#de935f  
hi Character  guifg=#de935f  
hi Comment  guifg=#969896  
hi Conditional  guifg=#b294bb  
hi Constant  guifg=NONE  gui=NONE
hi Define  guifg=#b294bb  
hi Error guifg=#822021 guibg=#df5f5f
hi ErrorMsg  guifg=#822021 guibg=#df5f5f
hi WarningMsg  guifg=#822021 guibg=#df5f5f
hi Float  guifg=#de935f  
hi Function  guifg=#81a2be  
hi Identifier  guifg=#CC6666
hi Keyword  guifg=#81A2BE
hi Label  guifg=#b5bd68  
hi NonText  guifg=#4b4e55 gui=NONE
hi Number  guifg=#de935f  
hi Operator  guifg=#81a2be  
hi PreProc  guifg=#b294bb  
hi Special  guifg=#CC6666 gui=NONE
"hi SpecialChar guifg=#CC6666
hi SpecialKey  guifg=#4b4e55 guibg=#282a2e gui=NONE
hi Statement  guifg=#b294bb  
hi StorageClass  guifg=#b294bb  
hi String  guifg=#b5bd68  
hi Structure guifg=#F0C674
hi Tag  guifg=#cc6666  
hi Title  guifg=#c5c8c6  
hi Todo  guifg=#969896  gui=bold
hi Type  guifg=NONE  gui=NONE
hi Underlined  guifg=NONE  gui=underline

hi rubyClass  guifg=#b294bb  
hi rubyFunction  guifg=#81a2be  
hi rubyInterpolationDelimiter  guifg=NONE  gui=NONE
hi rubySymbol  guifg=#b5bd68  
hi rubyConstant  guifg=#f0c674  
hi rubyStringDelimiter  guifg=#b5bd68  
hi rubyBlockParameter  guifg=#de935f  
hi rubyInstanceVariable  guifg=#cc6666  
hi rubyInclude  guifg=#81a2be  
hi rubyGlobalVariable  guifg=#cc6666  
hi rubyRegexp  guifg=#cc6666  
hi rubyRegexpDelimiter  guifg=#cc6666  
hi rubyEscape  guifg=#de935f  
hi rubyControl  guifg=#b294bb  
hi rubyClassVariable  guifg=#cc6666  
hi rubyOperator  guifg=#81a2be  
hi rubyException  guifg=#81a2be  
hi rubyPseudoVariable  guifg=#cc6666  
hi rubyRailsUserClass  guifg=#f0c674  
hi rubyRailsARAssociationMethod  guifg=#81a2be  
hi rubyRailsARMethod  guifg=#81a2be  
hi rubyRailsRenderMethod  guifg=#81a2be  
hi rubyRailsMethod  guifg=#81a2be  
hi erubyDelimiter  guifg=#de935f  
hi erubyComment  guifg=#969896  
hi erubyRailsMethod  guifg=#81a2be  
hi htmlTag  guifg=NONE  gui=NONE
hi htmlEndTag  guifg=NONE  gui=NONE
hi htmlTagName  guifg=NONE  gui=NONE
hi htmlArg  guifg=NONE  gui=NONE
hi htmlSpecialChar  guifg=#de935f  
hi javaScriptFunction  guifg=#b294bb  
hi javaScriptRailsFunction  guifg=#81a2be  
hi javaScriptBraces  guifg=#cc6666  
hi yamlKey  guifg=#cc6666  
hi yamlAnchor  guifg=#cc6666  
hi yamlAlias  guifg=#cc6666  
hi yamlDocumentHeader  guifg=#b5bd68  
hi cssURL  guifg=#de935f  
hi cssFunctionName  guifg=#81a2be  
hi cssColor  guifg=#81a2be  
hi cssPseudoClassId  guifg=#cc6666  
hi cssClassName  guifg=#cc6666  
hi cssValueLength  guifg=#de935f  
hi cssCommonAttr  guifg=#de935f  
hi cssBraces  guifg=NONE  gui=NONE
