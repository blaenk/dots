" Excelsior - Full Colour and 256 Colour
" http://blaenkdenum.com
"
" Forked from Chris Kempson's Tomorrow-Night: https://github.com/chriskempson/tomorrow-theme
" Hex colour conversion functions borrowed from the theme "Desert256""

if exists("syntax_on")
  syntax reset
endif

set background=dark
hi clear

let g:colors_name = "Excelsior"

if has("gui_running") || &t_Co == 88 || &t_Co == 256
  " Returns an approximate grey index for the given grey level
  fun <SID>grey_number(x)
    if &t_Co == 88
      if a:x < 23
        return 0
      elseif a:x < 69
        return 1
      elseif a:x < 103
        return 2
      elseif a:x < 127
        return 3
      elseif a:x < 150
        return 4
      elseif a:x < 173
        return 5
      elseif a:x < 196
        return 6
      elseif a:x < 219
        return 7
      elseif a:x < 243
        return 8
      else
        return 9
      endif
    else
      if a:x < 14
        return 0
      else
        let l:n = (a:x - 8) / 10
        let l:m = (a:x - 8) % 10
        if l:m < 5
          return l:n
        else
          return l:n + 1
        endif
      endif
    endif
  endfun

  " Returns the actual grey level represented by the grey index
  fun <SID>grey_level(n)
    if &t_Co == 88
      if a:n == 0
        return 0
      elseif a:n == 1
        return 46
      elseif a:n == 2
        return 92
      elseif a:n == 3
        return 115
      elseif a:n == 4
        return 139
      elseif a:n == 5
        return 162
      elseif a:n == 6
        return 185
      elseif a:n == 7
        return 208
      elseif a:n == 8
        return 231
      else
        return 255
      endif
    else
      if a:n == 0
        return 0
      else
        return 8 + (a:n * 10)
      endif
    endif
  endfun

  " Returns the palette index for the given grey index
  fun <SID>grey_colour(n)
    if &t_Co == 88
      if a:n == 0
        return 16
      elseif a:n == 9
        return 79
      else
        return 79 + a:n
      endif
    else
      if a:n == 0
        return 16
      elseif a:n == 25
        return 231
      else
        return 231 + a:n
      endif
    endif
  endfun

  " Returns an approximate colour index for the given colour level
  fun <SID>rgb_number(x)
    if &t_Co == 88
      if a:x < 69
        return 0
      elseif a:x < 172
        return 1
      elseif a:x < 230
        return 2
      else
        return 3
      endif
    else
      if a:x < 75
        return 0
      else
        let l:n = (a:x - 55) / 40
        let l:m = (a:x - 55) % 40
        if l:m < 20
          return l:n
        else
          return l:n + 1
        endif
      endif
    endif
  endfun

  " Returns the actual colour level for the given colour index
  fun <SID>rgb_level(n)
    if &t_Co == 88
      if a:n == 0
        return 0
      elseif a:n == 1
        return 139
      elseif a:n == 2
        return 205
      else
        return 255
      endif
    else
      if a:n == 0
        return 0
      else
        return 55 + (a:n * 40)
      endif
    endif
  endfun

  " Returns the palette index for the given R/G/B colour indices
  fun <SID>rgb_colour(x, y, z)
    if &t_Co == 88
      return 16 + (a:x * 16) + (a:y * 4) + a:z
    else
      return 16 + (a:x * 36) + (a:y * 6) + a:z
    endif
  endfun

  " Returns the palette index to approximate the given R/G/B colour levels
  fun <SID>colour(r, g, b)
    " Get the closest grey
    let l:gx = <SID>grey_number(a:r)
    let l:gy = <SID>grey_number(a:g)
    let l:gz = <SID>grey_number(a:b)

    " Get the closest colour
    let l:x = <SID>rgb_number(a:r)
    let l:y = <SID>rgb_number(a:g)
    let l:z = <SID>rgb_number(a:b)

    if l:gx == l:gy && l:gy == l:gz
      " There are two possibilities
      let l:dgr = <SID>grey_level(l:gx) - a:r
      let l:dgg = <SID>grey_level(l:gy) - a:g
      let l:dgb = <SID>grey_level(l:gz) - a:b
      let l:dgrey = (l:dgr * l:dgr) + (l:dgg * l:dgg) + (l:dgb * l:dgb)
      let l:dr = <SID>rgb_level(l:gx) - a:r
      let l:dg = <SID>rgb_level(l:gy) - a:g
      let l:db = <SID>rgb_level(l:gz) - a:b
      let l:drgb = (l:dr * l:dr) + (l:dg * l:dg) + (l:db * l:db)
      if l:dgrey < l:drgb
        " Use the grey
        return <SID>grey_colour(l:gx)
      else
        " Use the colour
        return <SID>rgb_colour(l:x, l:y, l:z)
      endif
    else
      " Only one possibility
      return <SID>rgb_colour(l:x, l:y, l:z)
    endif
  endfun

  " Returns the palette index to approximate the 'rrggbb' hex string
  fun <SID>rgb(rgb)
    let l:r = ("0x" . strpart(a:rgb, 0, 2)) + 0
    let l:g = ("0x" . strpart(a:rgb, 2, 2)) + 0
    let l:b = ("0x" . strpart(a:rgb, 4, 2)) + 0

    return <SID>colour(l:r, l:g, l:b)
  endfun

  " Sets the highlighting for the given group
  fun <SID>X(group, fg, bg, attr)
    if a:fg != ""
      exec "hi " . a:group . " guifg=#" . a:fg . " ctermfg=" . <SID>rgb(a:fg)
    endif
    if a:bg != ""
      exec "hi " . a:group . " guibg=#" . a:bg . " ctermbg=" . <SID>rgb(a:bg)
    endif
    if a:attr != ""
      exec "hi " . a:group . " gui=" . a:attr . " cterm=" . a:attr
    endif
  endfun

  fun <SID>L(from, to)
    exec "hi def link " . a:from . " " . a:to
  endfun

  " Vim Highlighting

  " Default GUI Colours
  let s:foreground = "c5c8c6"
  let s:background = "1d1f21"
  let s:selection = "373b41"
  let s:line = "282a2e"
  let s:comment = "8e908c"

  let s:red = "c82829"
  let s:dark_red = "b32d47"
  let s:tomorrow_red = "cc6666"
  let s:light_red = "cc6666"

  let s:orange = "f5871f"
  let s:dark_orange = "cf6a4c"

  let s:yellow = "eab700"

  let s:green = "718c00"
  let s:light_green = "99ad6a"
  let s:lights_green = "00cd00"

  let s:aqua = "3e999f"
  let s:blue = "4271ae"
  let s:threadless_blue = "3a89c9"
  let s:light_blue = "799dcc"
  let s:purple = "a986bf"
  let s:window = "efefef"
  let s:search_fg = "224d70"
  let s:search_bg = "82a3bf"
  let s:delimiter = "787876"

  if !has("gui_running")
    let s:background = "303030"
    let s:window = "5e5e5e"
    let s:selection = "585858"
    let s:line = "3a3a3a"
  end

  highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=#717474 guibg=#282a2e

  call <SID>X("Directory", s:blue, "", "")
  call <SID>X("FoldColumn", "", s:background, "")
  call <SID>X("Folded", s:comment, s:background, "")
  call <SID>X("IncSearch", s:search_bg, s:search_fg, "")
  call <SID>X("MatchParen", "822021", "df5f5f", "")
  call <SID>X("ModeMsg", s:green, "", "")
  call <SID>X("MoreMsg", s:green, "", "")
  call <SID>X("NonText", s:selection, "", "")
  call <SID>X("Normal", s:foreground, s:background, "")
  call <SID>X("Question", s:green, "", "")
  call <SID>X("Search", s:background, s:orange, "")
  call <SID>X("SpecialKey", s:selection, "", "")
  call <SID>X("StatusLine", s:window, s:yellow, "reverse")
  call <SID>X("StatusLineNC", s:window, s:foreground, "reverse")
  call <SID>X("TabLine", s:foreground, s:background, "reverse")
  call <SID>X("VertSplit", s:window, s:window, "none")
  call <SID>X("Visual", "", s:selection, "")
  call <SID>X("WarningMsg", s:dark_red, "", "")

  if version >= 700
    call <SID>X("Cursor", s:search_fg, s:search_bg, "none")
    call <SID>X("CursorColumn", "", s:line, "none")
    call <SID>X("CursorLine", "", s:line, "none")
    call <SID>X("PMenu", s:foreground, s:selection, "none")
    call <SID>X("PMenuSel", s:foreground, s:selection, "reverse")
  end
  if version >= 703
    call <SID>X("ColorColumn", "", s:line, "none")
  end

  " Standard Highlighting
  call <SID>X("Comment", s:threadless_blue, "", "")
  call <SID>X("Conditional", s:foreground, "", "")
  call <SID>X("Constant", s:red, "", "")
  call <SID>X("Define", s:purple, "", "none")
  call <SID>X("Delimiter", s:delimiter, s:background, "")
  call <SID>X("Function", s:blue, "", "")
  call <SID>X("Identifier", s:dark_red, "", "none")
  call <SID>X("Include", s:blue, "", "")
  call <SID>X("Operator", s:light_blue, "", "none")
  call <SID>X("PreProc", s:purple, "", "")
  call <SID>X("Repeat", s:foreground, "", "")
  call <SID>X("Special", s:foreground, "", "")
  call <SID>X("Statement", s:foreground, "", "")
  call <SID>X("String", s:green, "", "")
  call <SID>X("Structure", s:purple, "", "")
  call <SID>X("Title", s:comment, "", "")
  call <SID>X("Todo", s:comment, s:background, "")
  call <SID>X("Type", s:blue, "", "none")

  " Vim Highlighting
  call <SID>X("vimCommand", s:dark_red, "", "none")
  call <SID>X("vimFuncVar", s:yellow, "", "none")
  call <SID>X("vimNotFunc", s:purple, "", "none")

  " C Highlighting
  call <SID>X("cConditional", s:purple, "", "")
  call <SID>X("cCustomClass", s:aqua, "", "")
  call <SID>X("cCustomFunc", s:blue, "", "")
  call <SID>X("cCustomVar", s:yellow, "", "")
  call <SID>X("cLabel", s:purple, "", "")
  call <SID>X("cRepeat", s:purple, "", "")
  call <SID>X("cStorageClass", s:purple, "", "")
  call <SID>X("cType", s:yellow, "", "")
  call <SID>X("cUserLabel", s:purple, "", "")

  " PHP Highlighting
  call <SID>X("phpConditional", s:purple, "", "")
  call <SID>X("phpKeyword", s:purple, "", "")
  call <SID>X("phpMemberSelector", s:foreground, "", "")
  call <SID>X("phpRepeat", s:purple, "", "")
  call <SID>X("phpStatement", s:purple, "", "")
  call <SID>X("phpVarSelector", s:dark_red, "", "")

  " Ruby Highlighting
  call <SID>X("rubyAttribute", s:blue, "", "")
  call <SID>X("rubyBlock", s:orange, "", "")
  call <SID>X("rubyBlockParameter", s:dark_red, "", "")
  call <SID>X("rubyBlockParameterList", s:delimiter, "", "")
  call <SID>X("rubyClass", s:purple, "", "")
  call <SID>X("rubyConditional", s:purple, "", "")
  call <SID>X("rubyConstant", s:aqua, "", "")
  call <SID>X("rubyControl", s:purple, "", "")
  call <SID>X("rubyCurlyBlock", s:foreground, "", "")
  call <SID>X("rubyException", s:purple, "", "")
  call <SID>X("rubyExceptional", s:purple, "", "")
  call <SID>X("rubyInclude", s:blue, "", "")
  call <SID>X("rubyInstanceVariable", s:dark_red, "", "")
  call <SID>X("rubyInteger", s:red, "", "")
  call <SID>X("rubyInterpolationDelimiter", s:light_green, "", "")
  call <SID>X("rubyLocalVariableOrMethod", s:orange, "", "")
  call <SID>X("rubyPseudoVariable", s:dark_red, "", "")
  call <SID>X("rubyRailsUserClass", s:aqua, "", "")
  call <SID>X("rubyRepeat", s:purple, "", "")
  call <SID>X("rubyStringDelimiter", "99ad6a", "", "")
  call <SID>X("rubyStringDelimiter", s:green, "", "")
  call <SID>X("rubyStringEscape", s:light_red, "", "")
  call <SID>X("rubySymbol", s:green, "", "")

  " Haskell
  call <SID>X("hsStatement", s:purple, "", "")
  call <SID>X("hsConditional", s:purple, "", "")
  call <SID>X("hsType", s:aqua, "", "")
  call <SID>X("hsCharacter", s:light_green, "", "")
  call <SID>X("hsDelimiter", s:delimiter, "", "")

  " Python Highlighting
  call <SID>X("pythonConditional", s:purple, "", "")
  call <SID>X("pythonInclude", s:purple, "", "")
  call <SID>X("pythonDot", s:light_blue, "", "")
  call <SID>X("pythonNumber", s:red, "", "")
  call <SID>X("pythonPreCondit", s:purple, "", "")
  call <SID>X("pythonRepeat", s:purple, "", "")
  call <SID>X("pythonStatement", s:purple, "", "")

  " JavaScript Highlighting
  call <SID>X("javaScriptBraces", s:foreground, "", "")
  call <SID>X("javaScriptConditional", s:purple, "", "")
  call <SID>X("javaScriptMember", s:orange, "", "")
  call <SID>X("javaScriptNumber", s:orange, "", "")
  call <SID>X("javaScriptRepeat", s:purple, "", "")

  " HTML Highlighting
  call <SID>X("htmlArg", s:red, "", "")
  call <SID>X("htmlScriptTag", s:red, "", "")
  call <SID>X("htmlTag", s:red, "", "")
  call <SID>X("htmlTagName", s:red, "", "")

  " Diff Highlighting
  call <SID>X("diffAdded", s:green, "", "")
  call <SID>X("diffRemoved", s:red, "", "")

  " Delete Functions
  delf <SID>X
  delf <SID>L
  delf <SID>rgb
  delf <SID>colour
  delf <SID>rgb_colour
  delf <SID>rgb_level
  delf <SID>rgb_number
  delf <SID>grey_colour
  delf <SID>grey_level
  delf <SID>grey_number
endif
