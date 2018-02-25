" vi: fdm=marker

" github.com/blaenk :: @blaenk
" http://blaenkdenum.com

" Initialization: {{{
set nocompatible
set encoding=utf-8
set fenc=utf-8
set termencoding=utf-8

if has('win32')
  set rtp+=~/.vim
endif
" }}}

" Plugins: {{{
set nocompatible

let install_plug = 0

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

  let install_plug = 1
  :echom "Plug bootstrapped!"
endif

call plug#begin('~/.vim/plugged')

" sort the bundles by plugin name with
" vim: sort i /\/\zs.\+\ze'/ r
" emacs: (sort-regexp-fields nil "^Plug '.+/\\(.+\\)'$" "\\1" (region-beginning) (region-end))

Plug 'vim-scripts/a.vim'
Plug 'rking/ag.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'Raimondi/delimitMate'
Plug 'mattn/gist-vim'
Plug 'sjl/gundo.vim'
Plug 'unblevable/quick-scope'
Plug 'kien/rainbow_parentheses.vim'
Plug 'rust-lang/rust.vim'
Plug 'cakebaker/scss-syntax.vim'
Plug 'AndrewRadev/sideways.vim'
Plug 'godlygeek/tabular'
Plug 'tomtom/tlib_vim'
Plug 'SirVer/ultisnips'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'PeterRincker/vim-argumentative'
Plug 'guns/vim-clojure-highlight'
Plug 'guns/vim-clojure-static'
Plug 'altercation/vim-colors-solarized'
Plug 'hail2u/vim-css3-syntax'
Plug 'tommcdo/vim-exchange'
Plug 'airblade/vim-gitgutter'
Plug 'jnwhiteh/vim-golang'
Plug 'wlangstroth/vim-haskell'
Plug 'henrik/vim-indexed-search'
Plug 'groenewege/vim-less'
Plug 'tommcdo/vim-lion'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'derekwyatt/vim-scala'
Plug 'guns/vim-sexp'
Plug 'toyamarinyon/vim-swift'
Plug 'kana/vim-textobj-indent'
Plug 'kana/vim-textobj-user'
Plug 'rhysd/vim-textobj-anyblock'
Plug 'milkypostman/vim-togglelist'
Plug 'cespare/vim-toml'
Plug 'bronson/vim-visual-star-search'
Plug 'Shougo/vimproc.vim'
Plug 'mattn/webapi-vim'
Plug 'vim-scripts/yaml.vim'

if has('unix')
  if empty($SSH_CONNECTION)
    " Plug 'Valloric/YouCompleteMe'
  endif
endif

" tpope chorus
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-leiningen'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-ragtag'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

call plug#end()

if install_plug == 1
  PlugInstall --sync
  :echom "Plugins installed!"
endif

" }}}

" Colors: {{{
set t_Co=256
syntax enable

if $USE_SOLARIZED_DARK
  set background=dark
else
  set background=light
endif

colorscheme solarized

" Solarized Color Variables: {{{2

" Remove this if (ever) PR 32 is merged
" https://github.com/altercation/vim-colors-solarized/pull/32

" Terminals that support italics
let s:terms_italic=[
            \"rxvt",
            \"gnome-terminal"
            \]
" For reference only, terminals are known to be incomptible.
" Terminals that are in neither list need to be tested.
let s:terms_noitalic=[
            \"iTerm.app",
            \"Apple_Terminal"
            \]
if has("gui_running")
    let s:terminal_italic=1 " TODO: could refactor to not require this at all
else
    let s:terminal_italic=0 " terminals will be guilty until proven compatible
    for term in s:terms_italic
        if $TERM_PROGRAM =~ term
            let s:terminal_italic=1
        endif
    endfor
endif

if (has("gui_running") && g:solarized_degrade == 0)
    let s:vmode       = "gui"
    let s:base03      = "#002b36"
    let s:base02      = "#073642"
    let s:base01      = "#586e75"
    let s:base00      = "#657b83"
    let s:base0       = "#839496"
    let s:base1       = "#93a1a1"
    let s:base2       = "#eee8d5"
    let s:base3       = "#fdf6e3"
    let s:yellow      = "#b58900"
    let s:orange      = "#cb4b16"
    let s:red         = "#dc322f"
    let s:magenta     = "#d33682"
    let s:violet      = "#6c71c4"
    let s:blue        = "#268bd2"
    let s:cyan        = "#2aa198"
    "let s:green       = "#859900" "original
    let s:green       = "#719e07" "experimental
elseif (has("gui_running") && g:solarized_degrade == 1)
    " These colors are identical to the 256 color mode. They may be viewed
    " while in gui mode via "let g:solarized_degrade=1", though this is not
    " recommened and is for testing only.
    let s:vmode       = "gui"
    let s:base03      = "#1c1c1c"
    let s:base02      = "#262626"
    let s:base01      = "#4e4e4e"
    let s:base00      = "#585858"
    let s:base0       = "#808080"
    let s:base1       = "#8a8a8a"
    let s:base2       = "#d7d7af"
    let s:base3       = "#ffffd7"
    let s:yellow      = "#af8700"
    let s:orange      = "#d75f00"
    let s:red         = "#af0000"
    let s:magenta     = "#af005f"
    let s:violet      = "#5f5faf"
    let s:blue        = "#0087ff"
    let s:cyan        = "#00afaf"
    let s:green       = "#5f8700"
elseif g:solarized_termcolors != 256 && &t_Co >= 16
    let s:vmode       = "cterm"
    let s:base03      = "8"
    let s:base02      = "0"
    let s:base01      = "10"
    let s:base00      = "11"
    let s:base0       = "12"
    let s:base1       = "14"
    let s:base2       = "7"
    let s:base3       = "15"
    let s:yellow      = "3"
    let s:orange      = "9"
    let s:red         = "1"
    let s:magenta     = "5"
    let s:violet      = "13"
    let s:blue        = "4"
    let s:cyan        = "6"
    let s:green       = "2"
elseif g:solarized_termcolors == 256
    let s:vmode       = "cterm"
    let s:base03      = "234"
    let s:base02      = "235"
    let s:base01      = "239"
    let s:base00      = "240"
    let s:base0       = "244"
    let s:base1       = "245"
    let s:base2       = "187"
    let s:base3       = "230"
    let s:yellow      = "136"
    let s:orange      = "166"
    let s:red         = "124"
    let s:magenta     = "125"
    let s:violet      = "61"
    let s:blue        = "33"
    let s:cyan        = "37"
    let s:green       = "64"
else
    let s:vmode       = "cterm"
    let s:bright      = "* term=bold cterm=bold"
"   let s:base03      = "0".s:bright
"   let s:base02      = "0"
"   let s:base01      = "2".s:bright
"   let s:base00      = "3".s:bright
"   let s:base0       = "4".s:bright
"   let s:base1       = "6".s:bright
"   let s:base2       = "7"
"   let s:base3       = "7".s:bright
"   let s:yellow      = "3"
"   let s:orange      = "1".s:bright
"   let s:red         = "1"
"   let s:magenta     = "5"
"   let s:violet      = "5".s:bright
"   let s:blue        = "4"
"   let s:cyan        = "6"
"   let s:green       = "2"
    let s:base03      = "DarkGray"      " 0*
    let s:base02      = "Black"         " 0
    let s:base01      = "LightGreen"    " 2*
    let s:base00      = "LightYellow"   " 3*
    let s:base0       = "LightBlue"     " 4*
    let s:base1       = "LightCyan"     " 6*
    let s:base2       = "LightGray"     " 7
    let s:base3       = "White"         " 7*
    let s:yellow      = "DarkYellow"    " 3
    let s:orange      = "LightRed"      " 1*
    let s:red         = "DarkRed"       " 1
    let s:magenta     = "DarkMagenta"   " 5
    let s:violet      = "LightMagenta"  " 5*
    let s:blue        = "DarkBlue"      " 4
    let s:cyan        = "DarkCyan"      " 6
    let s:green       = "DarkGreen"     " 2

endif

" Formatting options and null values for passthrough effect "{{{3
" ---------------------------------------------------------------------
    let s:none            = "NONE"
    let s:none            = "NONE"
    let s:t_none          = "NONE"
    let s:n               = "NONE"
    let s:c               = ",undercurl"
    let s:r               = ",reverse"
    let s:s               = ",standout"
    let s:ou              = ""
    let s:ob              = ""
"}}}
" Background value based on termtrans setting "{{{3
" ---------------------------------------------------------------------
if (has("gui_running") || g:solarized_termtrans == 0)
    let s:back        = s:base03
else
    let s:back        = "NONE"
endif
"}}}
" Alternate light scheme "{{{3
" ---------------------------------------------------------------------
if &background == "light"
    let s:temp03      = s:base03
    let s:temp02      = s:base02
    let s:temp01      = s:base01
    let s:temp00      = s:base00
    let s:base03      = s:base3
    let s:base02      = s:base2
    let s:base01      = s:base1
    let s:base00      = s:base0
    let s:base0       = s:temp00
    let s:base1       = s:temp01
    let s:base2       = s:temp02
    let s:base3       = s:temp03
    if (s:back != "NONE")
        let s:back    = s:base03
    endif
endif
"}}}
" Optional contrast schemes "{{{3
" ---------------------------------------------------------------------
if g:solarized_contrast == "high"
    let s:base01      = s:base00
    let s:base00      = s:base0
    let s:base0       = s:base1
    let s:base1       = s:base2
    let s:base2       = s:base3
    let s:back        = s:back
endif
if g:solarized_contrast == "low"
    let s:back        = s:base02
    let s:ou          = ",underline"
endif
"}}}
" Overrides dependent on user specified values and environment "{{{3
" ---------------------------------------------------------------------
if (g:solarized_bold == 0 || &t_Co == 8 )
    let s:b           = ""
    let s:bb          = ",bold"
else
    let s:b           = ",bold"
    let s:bb          = ""
endif

if g:solarized_underline == 0
    let s:u           = ""
else
    let s:u           = ",underline"
endif

if g:solarized_italic == 0 || s:terminal_italic == 0
    let s:i           = ""
else
    let s:i           = ",italic"
endif
"}}}
" Highlighting primitives"{{{3
" ---------------------------------------------------------------------

exe "let s:bg_none      = ' ".s:vmode."bg=".s:none   ."'"
exe "let s:bg_back      = ' ".s:vmode."bg=".s:back   ."'"
exe "let s:bg_base03    = ' ".s:vmode."bg=".s:base03 ."'"
exe "let s:bg_base02    = ' ".s:vmode."bg=".s:base02 ."'"
exe "let s:bg_base01    = ' ".s:vmode."bg=".s:base01 ."'"
exe "let s:bg_base00    = ' ".s:vmode."bg=".s:base00 ."'"
exe "let s:bg_base0     = ' ".s:vmode."bg=".s:base0  ."'"
exe "let s:bg_base1     = ' ".s:vmode."bg=".s:base1  ."'"
exe "let s:bg_base2     = ' ".s:vmode."bg=".s:base2  ."'"
exe "let s:bg_base3     = ' ".s:vmode."bg=".s:base3  ."'"
exe "let s:bg_green     = ' ".s:vmode."bg=".s:green  ."'"
exe "let s:bg_yellow    = ' ".s:vmode."bg=".s:yellow ."'"
exe "let s:bg_orange    = ' ".s:vmode."bg=".s:orange ."'"
exe "let s:bg_red       = ' ".s:vmode."bg=".s:red    ."'"
exe "let s:bg_magenta   = ' ".s:vmode."bg=".s:magenta."'"
exe "let s:bg_violet    = ' ".s:vmode."bg=".s:violet ."'"
exe "let s:bg_blue      = ' ".s:vmode."bg=".s:blue   ."'"
exe "let s:bg_cyan      = ' ".s:vmode."bg=".s:cyan   ."'"

exe "let s:fg_none      = ' ".s:vmode."fg=".s:none   ."'"
exe "let s:fg_back      = ' ".s:vmode."fg=".s:back   ."'"
exe "let s:fg_base03    = ' ".s:vmode."fg=".s:base03 ."'"
exe "let s:fg_base02    = ' ".s:vmode."fg=".s:base02 ."'"
exe "let s:fg_base01    = ' ".s:vmode."fg=".s:base01 ."'"
exe "let s:fg_base00    = ' ".s:vmode."fg=".s:base00 ."'"
exe "let s:fg_base0     = ' ".s:vmode."fg=".s:base0  ."'"
exe "let s:fg_base1     = ' ".s:vmode."fg=".s:base1  ."'"
exe "let s:fg_base2     = ' ".s:vmode."fg=".s:base2  ."'"
exe "let s:fg_base3     = ' ".s:vmode."fg=".s:base3  ."'"
exe "let s:fg_green     = ' ".s:vmode."fg=".s:green  ."'"
exe "let s:fg_yellow    = ' ".s:vmode."fg=".s:yellow ."'"
exe "let s:fg_orange    = ' ".s:vmode."fg=".s:orange ."'"
exe "let s:fg_red       = ' ".s:vmode."fg=".s:red    ."'"
exe "let s:fg_magenta   = ' ".s:vmode."fg=".s:magenta."'"
exe "let s:fg_violet    = ' ".s:vmode."fg=".s:violet ."'"
exe "let s:fg_blue      = ' ".s:vmode."fg=".s:blue   ."'"
exe "let s:fg_cyan      = ' ".s:vmode."fg=".s:cyan   ."'"

exe "let s:fmt_none     = ' ".s:vmode."=NONE".          " term=NONE".    "'"
exe "let s:fmt_bold     = ' ".s:vmode."=NONE".s:b.      " term=NONE".s:b."'"
exe "let s:fmt_bldi     = ' ".s:vmode."=NONE".s:b.      " term=NONE".s:b."'"
exe "let s:fmt_undr     = ' ".s:vmode."=NONE".s:u.      " term=NONE".s:u."'"
exe "let s:fmt_undb     = ' ".s:vmode."=NONE".s:u.s:b.  " term=NONE".s:u.s:b."'"
exe "let s:fmt_undi     = ' ".s:vmode."=NONE".s:u.      " term=NONE".s:u."'"
exe "let s:fmt_uopt     = ' ".s:vmode."=NONE".s:ou.     " term=NONE".s:ou."'"
exe "let s:fmt_curl     = ' ".s:vmode."=NONE".s:c.      " term=NONE".s:c."'"
exe "let s:fmt_ital     = ' ".s:vmode."=NONE".s:i.      " term=NONE".s:i."'"
exe "let s:fmt_stnd     = ' ".s:vmode."=NONE".s:s.      " term=NONE".s:s."'"
exe "let s:fmt_revr     = ' ".s:vmode."=NONE".s:r.      " term=NONE".s:r."'"
exe "let s:fmt_revb     = ' ".s:vmode."=NONE".s:r.s:b.  " term=NONE".s:r.s:b."'"
" revbb (reverse bold for bright colors) is only set to actual bold in low 
" color terminals (t_co=8, such as OS X Terminal.app) and should only be used 
" with colors 8-15.
exe "let s:fmt_revbb    = ' ".s:vmode."=NONE".s:r.s:bb.   " term=NONE".s:r.s:bb."'"
exe "let s:fmt_revbbu   = ' ".s:vmode."=NONE".s:r.s:bb.s:u." term=NONE".s:r.s:bb.s:u."'"

if has("gui_running")
    exe "let s:sp_none      = ' guisp=".s:none   ."'"
    exe "let s:sp_back      = ' guisp=".s:back   ."'"
    exe "let s:sp_base03    = ' guisp=".s:base03 ."'"
    exe "let s:sp_base02    = ' guisp=".s:base02 ."'"
    exe "let s:sp_base01    = ' guisp=".s:base01 ."'"
    exe "let s:sp_base00    = ' guisp=".s:base00 ."'"
    exe "let s:sp_base0     = ' guisp=".s:base0  ."'"
    exe "let s:sp_base1     = ' guisp=".s:base1  ."'"
    exe "let s:sp_base2     = ' guisp=".s:base2  ."'"
    exe "let s:sp_base3     = ' guisp=".s:base3  ."'"
    exe "let s:sp_green     = ' guisp=".s:green  ."'"
    exe "let s:sp_yellow    = ' guisp=".s:yellow ."'"
    exe "let s:sp_orange    = ' guisp=".s:orange ."'"
    exe "let s:sp_red       = ' guisp=".s:red    ."'"
    exe "let s:sp_magenta   = ' guisp=".s:magenta."'"
    exe "let s:sp_violet    = ' guisp=".s:violet ."'"
    exe "let s:sp_blue      = ' guisp=".s:blue   ."'"
    exe "let s:sp_cyan      = ' guisp=".s:cyan   ."'"
else
    let s:sp_none      = ""
    let s:sp_back      = ""
    let s:sp_base03    = ""
    let s:sp_base02    = ""
    let s:sp_base01    = ""
    let s:sp_base00    = ""
    let s:sp_base0     = ""
    let s:sp_base1     = ""
    let s:sp_base2     = ""
    let s:sp_base3     = ""
    let s:sp_green     = ""
    let s:sp_yellow    = ""
    let s:sp_orange    = ""
    let s:sp_red       = ""
    let s:sp_magenta   = ""
    let s:sp_violet    = ""
    let s:sp_blue      = ""
    let s:sp_cyan      = ""
endif

" }}}

" Tweaks: {{{2
exe "hi! Comment"        .s:fmt_none   .s:fg_base01 .s:bg_none
exe "hi! CursorLineNR"   .s:fmt_uopt   .s:fg_magenta   .s:bg_base02  .s:fmt_none
exe "hi! StatusLine"     .s:fmt_none   .s:fg_base1 .s:bg_base02
exe "hi! StatusLineNC"   .s:fmt_none   .s:fg_base01 .s:bg_base02

exe "hi! pandocStrong"   .s:fg_base0  .s:bg_none  .s:fmt_bold

if (g:solarized_visibility!="high" && g:solarized_visibility!="low")
  exe "hi! SpecialKey" .s:fmt_bold   .s:fg_base00 .s:bg_none
endif
" }}}

" Status Line Colors: {{{2
exe "hi SLColumn"   .s:fg_blue    .s:bg_base03
exe "hi SLLineNr"   .s:fg_magenta .s:bg_base02
exe "hi SLBranch"   .s:fg_green   .s:bg_base02
exe "hi SLArrows"   .s:fg_cyan    .s:bg_base02
exe "hi SLProgress" .s:fg_base02  .s:bg_orange
exe "hi SLHelp"     .s:fg_base02  .s:bg_red
" }}}

" Cursor Colors: {{{2
exe "hi InsertCursor"  .s:fg_base03 .s:bg_cyan
exe "hi VisualCursor"  .s:fg_base03 .s:bg_magenta
exe "hi ReplaceCursor" .s:fg_base03 .s:bg_red
exe "hi CommandCursor" .s:fg_base03 .s:bg_blue
" }}}

" }}}

" Settings: {{{
" don't allow files with the same name to overwrite each other
set writebackup
set backupdir=~/.vim/backups
set undofile
set undodir=~/.vim/undo
set directory=~/.vim/tmp
set tags=./tags;

set display=lastline
set backspace=indent,eol,start
set laststatus=2
set expandtab
set tabstop=2
set shiftwidth=0
set autoindent
set textwidth=0

set gcr=a:block

" mode aware cursors
set gcr+=o:hor50-Cursor
set gcr+=n:Cursor
set gcr+=i-ci-sm:InsertCursor
set gcr+=r-cr:ReplaceCursor-hor20
set gcr+=c:CommandCursor
set gcr+=v-ve:VisualCursor

set gcr+=a:blinkon0

set shortmess=atToOI
set viewoptions+=unix,slash

set wildmenu
set wildmode=list:longest

set showmode
set showcmd
set hidden
set visualbell
set relativenumber
set number
set ttyfast

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch

set splitbelow
set splitright
set wrap
set listchars=tab:▸\ ,eol:¬,trail:·
set list
" }}}

" OS Specific: {{{

" Windows: {{{2
if has('win32')
  set gfn=Consolas:h10
  set clipboard=unnamed
" }}}

" UNIX: {{{2
else
  let s:kernel = system('echo -n "$(uname -s)"')

" Mac: {{{3
  if s:kernel == 'Darwin'
    set gfn=Menlo:h12
    " this doesn't work in mountain lion's default vim (does on macvim)
    set clipboard=unnamed
" }}}

" Linux: {{{3
  elseif s:kernel == 'Linux'
    set guifont=DejaVu\ Sans\ Mono\ 10

    " make the default clipboard be X11's
    set clipboard=unnamedplus
  endif
" }}}

endif
" }}}

" }}}

" Tabs: {{{

function! TabLabels()
  return gettabvar(v:lnum, 'label')
endfunction

set gtl=%{TabLabels()}

function! LabelTab(label)
  let t:label = a:label
  exe 'set showtabline=2'
endfunction

command! -nargs=1 LabelTab call LabelTab(<q-args>)

function! TabTip()
  let bufnrlist = tabpagebuflist(v:lnum)
  let tip = ''
  for bufnr in bufnrlist
    let name = bufname(bufnr)
    if !empty(name)
      if !empty(tip)
        let tip .= " \n "
      endif

      let tip .= name

      if getbufvar(bufnr, "&modified")
        let tip .= ' [+]'
      endif
    endif
  endfor
  return tip
endfunction

set gtt=%{TabTip()}

" }}}

" Status Line: {{{

" Status Function: {{{2
function! Status(winnum)
  let active = a:winnum == winnr()
  let bufnum = winbufnr(a:winnum)

  let stat = ''

  " this function just outputs the content colored by the
  " supplied colorgroup number, e.g. num = 2 -> User2
  " it only colors the input if the window is the currently
  " focused one

  function! Color(active, group, content)
    if a:active
      return '%#' . a:group . '#' . a:content . '%*'
    else
      return a:content
    endif
  endfunction

  " this handles alternative statuslines
  let usealt = 0

  let type = getbufvar(bufnum, '&buftype')
  let name = bufname(bufnum)

  let altstat = ''

  if type ==# 'help'
    let altstat .= '%#SLHelp# HELP %* ' . fnamemodify(name, ':t:r')
    let usealt = 1
  elseif name ==# '__Gundo__'
    let altstat .= ' Gundo'
    let usealt = 1
  elseif name ==# '__Gundo_Preview__'
    let altstat .= ' Gundo Preview'
    let usealt = 1
  endif

  if usealt
    return altstat
  endif

  " column
  "   this might seem a bit complicated but all it amounts to is
  "   a calculation to see how much padding should be used for the
  "   column number, so that it lines up nicely with the line numbers

  "   an expression is needed because expressions are evaluated within
  "   the context of the window for which the statusline is being prepared
  "   this is crucial because the line and virtcol functions otherwise
  "   operate on the currently focused window

  function! Column()
    let vc = virtcol('.')
    let ruler_width = max([strlen(line('$')), (&numberwidth - 1)]) + &l:foldcolumn
    let column_width = strlen(vc)
    let padding = ruler_width - column_width
    let column = ''

    if padding <= 0
      let column .= vc
    else
      " + 1 because for some reason vim eats one of the spaces
      let column .= repeat(' ', padding + 1) . vc
    endif

    return column . ' '
  endfunction

  let stat .= '%#SLColumn#'
  let stat .= '%{Column()}'
  let stat .= '%*'

  if getwinvar(a:winnum, 'statusline_progress', 0)
    let stat .= Color(active, 'SLProgress', ' %p ')
  endif

  " file name
  let stat .= Color(active, 'SLArrows', active ? ' »' : ' «')
  let stat .= ' %<'
  let stat .= '%f'
  let stat .= ' ' . Color(active, 'SLArrows', active ? '«' : '»')

  " file modified
  let modified = getbufvar(bufnum, '&modified')
  let stat .= Color(active, 'SLLineNr', modified ? ' +' : '')

  " read only
  let readonly = getbufvar(bufnum, '&readonly')
  let stat .= Color(active, 'SLLineNR', readonly ? ' ‼' : '')

  " paste
  if active
    if getwinvar(a:winnum, '&spell')
      let stat .= Color(active, 'SLLineNr', ' S')
    endif

    if getwinvar(a:winnum, '&paste')
      let stat .= Color(active, 'SLLineNr', ' P')
    endif
  endif

  " right side
  let stat .= '%='

  " git branch
  if exists('*fugitive#head')
    let head = fugitive#head()

    if empty(head) && exists('*fugitive#detect') && !exists('b:git_dir')
      call fugitive#detect(getcwd())
      let head = fugitive#head()
    endif

    if !empty(head)
      let stat .= Color(active, 'SLBranch', ' ← ') . head . ' '
    endif
  endif

  return stat
endfunction
" }}}

" Status AutoCMD: {{{
function! s:ToggleStatusProgress()
  if !exists('w:statusline_progress')
    let w:statusline_progress = 0
  endif

  let w:statusline_progress = !w:statusline_progress
endfunction

command! ToggleStatusProgress :call s:ToggleStatusProgress()

nnoremap <silent> ,p :ToggleStatusProgress<CR>

function! s:IsDiff()
  let result = 0

  for nr in range(1, winnr('$'))
    let result = result || getwinvar(nr, '&diff')

    if result
      return result
    endif
  endfor

  return result
endfunction

function! s:RefreshStatus()
  for nr in range(1, winnr('$'))
    call setwinvar(nr, '&statusline', '%!Status(' . nr . ')')
  endfor
endfunction

command! RefreshStatus :call <SID>RefreshStatus()

augroup status
  autocmd!
  autocmd VimEnter,VimLeave,WinEnter,WinLeave,BufWinEnter,BufWinLeave * :RefreshStatus
augroup END
" }}}

" }}}

" Mappings: {{{
let mapleader = ","

" Modes: {{{2
nnoremap <silent> <leader>l :noh \| diffupdate<CR>
nnoremap <silent> <leader>c :set list!<CR>
nnoremap <silent> <leader>n :set rnu!<CR>
nnoremap <silent> <leader>t :set rnu! list! number!<CR>
nnoremap <silent> <leader>s :set spell!<CR>

inoremap jj <ESC>
" }}}

" Editing: {{{2
nnoremap ZS :w<CR>

command! CloneBuffer new | 0put =getbufline('#',1,'$')

" 'force' write
cmap w!! %!sudo tee > /dev/null %

map <Leader>o o<Esc>ko

nmap gp `[v`]

vnoremap > >gv
vnoremap < <gv

nnoremap <M-c> y
nnoremap <M-v> p

vnoremap <M-c> y
vnoremap <M-v> p

imap <M-v> <C-r>+
cmap <M-v> <C-r>+

inoremap <S-Tab> <C-d>

" go to end of line from insert mode
inoremap <C-l> <ESC>A

function! BindH(lang)
  let b:highlight_lang = matchstr(a:lang, "^[^=]*")

  exe 'PandocHighlight ' . a:lang
  inoremap <silent> <buffer> <c-p> code<C-R>=UltiSnips#Anon(
    \"\\`\\`\\` " . b:highlight_lang . "\n" .
    \"${1:${VISUAL}}\n" .
    \"\\`\\`\\`$0", 'code')<cr>
endfunction

autocmd FileType pandoc command! -nargs=1 -complete=syntax Highlight call BindH(<f-args>)

" }}}

" Navigation: {{{2
nnoremap <C-J> <C-W>-
nnoremap <C-K> <C-W>+
nnoremap <C-L> <C-W>>
nnoremap <C-H> <C-W><

nnoremap <M-h> gT
nnoremap <M-l> gt

nnoremap <silent> <M-j> :tabmove -1<CR>
nnoremap <silent> <M-k> :tabmove +1<CR>
" }}}

" Plugins_Misc: {{{2
map <F10> :echo HighlightGroups()<CR>

function! HighlightGroups()
  let l:h = synIDattr(synID(line('.'), col('.'), 1), 'name')
  let l:t = synIDattr(synID(line('.'), col('.'), 0), 'name')
  let l:l = synIDattr(synIDtrans(synID(line('.'), col('.'), 1)), 'name')

  let l:fg = synIDattr(synIDtrans(synID(line('.'), col('.'), 1)), 'fg#')
  let l:bg = synIDattr(synIDtrans(synID(line('.'), col('.'), 1)), 'bg#')

  let msg = ''

  let msg .= !empty(l:fg) ? (l:fg . ' ') : ''
  let msg .= !empty(l:bg) ? ((!empty(l:fg) ? '/' : '') . l:bg . ' ') : ''

  let msg .= !empty(l:h) ? (l:h . ' ') : ''
  let msg .= (!empty(l:t) && l:t != l:h) ? ('-> ' . l:t . ' ') : ''
  let msg .= !empty(l:l) ? ('-> ' . l:l . ' ') : ''

  return msg
endfunction

nnoremap <leader>r :RainbowParenthesesToggle<CR>

nnoremap <silent> <leader>u :GundoToggle<CR>
" }}}

" }}}

" Plugin Configuration: {{{

" Fugitive: {{{2
autocmd User Fugitive
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif

autocmd VimResized * if <SID>IsDiff() | wincmd = | endif
autocmd FilterWritePre *
  \ if <SID>IsDiff() |
  \   set columns=180 lines=55 | wincmd = |
  \ else |
  \   set columns=85 lines=25 |
  \ endif

cnoreabbrev <expr> dp ((getcmdtype() is# ':' && getcmdline() is# "'<,'>dp")?('diffput'):('dp'))
cnoreabbrev <expr> do ((getcmdtype() is# ':' && getcmdline() is# "'<,'>do")?('diffget'):('do'))

autocmd BufReadPost fugitive://* set bufhidden=delete
autocmd QuickFixCmdPost *grep* cwindow

function! ToggleGStatus()
    if buflisted(bufname('.git/index'))
        bd .git/index
    else
        Gstatus
    endif
endfunction

command! ToggleGStatus :call ToggleGStatus()

cnoreabbrev <expr> Gcommit ((getcmdtype() is# ':' && getcmdline() is# 'Gcommit')?('Gcommit -v'):('Gcommit'))
cnoreabbrev <expr> Gbrowse ((getcmdtype() is# ':' && getcmdline() is# 'Gbrowse')?('Gbrowse!'):('Gbrowse'))
cnoreabbrev <expr> Gdiff ((getcmdtype() is# ':' && getcmdline() is# 'Gdiff')?('Gvdiff'):('Gdiff'))

nnoremap ,gs :ToggleGStatus<CR>
nnoremap ,gg :Ggrep ""<left>
nnoremap ,gc :Gcommit -v -q<CR>
nnoremap ,gh :silent! Glog<CR>
nnoremap ,gl :silent! Glog --<CR>
nnoremap ,gd :Gvdiff<CR>
nnoremap ,gb :Gblame<CR>
nnoremap ,gn :Gbrowse!<CR>

" optional revision; no revision = index copy
nnoremap ,ge :Gedit<CR>
nnoremap ,gr :Gread<CR>
nnoremap ,gw :Gwrite<CR>

nnoremap ,go :Git checkout<space>

" }}}

" ToggleList: {{{2
let g:toggle_list_no_mappings = 0
nnoremap <script> <silent> <leader>q :call ToggleQuickfixList()<CR>
" }}}

" Pandoc: {{{2
let g:pandoc#filetypes#pandoc_markdown = 1
let g:pandoc#folding#mode = 'relative'
let g:pandoc#folding#fdc = 0

let g:pandoc#modules#enabled = [
  \"formatting",
  \"keyboard",
  \"folding"
  \]

let g:pandoc#syntax#codeblocks#ignore = ['definition', 'delimited']
let g:pandoc#syntax#conceal#blacklist = [
  \"titleblock",
  \"image",
  \"superscript",
  \"subscript",
  \"strikeout",
  \"atx",
  \"codeblock_start",
  \"codeblock_delim",
  \"definition",
  \"list",
  \"newline",
  \"hrule"
  \]

hi link pandocNoLabel Statement
" }}}

" UltiSnips: {{{2
let g:UltiSnipsSnippetDirectories = ["snips"]
let g:UltiSnipsExpandTrigger = '<C-p>'
" }}}

" YCM: {{{2
let g:ycm_filetype_whitelist = {'cpp': 1, 'c': 1}
let g:ycm_key_list_select_completion = ['<C-y>', '<Down>']
" }}}

" GitGutter: {{{2
let g:gitgutter_enabled = 0
let g:gitgutter_sign_modified = '#'

nnoremap <leader>gm :GitGutterToggle<CR>
" }}}

" vim-sexp: {{{2
let g:sexp_enable_insert_mode_mappings = 0
" }}}

" CtrlP: {{{2

" Settings: {{{3
let g:ctrlp_map = '<leader>f'
let g:ctrlp_show_hidden = 1

let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/]((\.(git|hg|svn))|build)$',
  \ 'file': '\v\.(DS_Store)$',
  \ }

let g:ctrlp_working_path_mode = 'ra'

if executable('ag')
  let g:ctrlp_use_caching = 0
  let g:ctrlp_user_command = "ag --hidden --nocolor --ignore .git -l -g '' %s"
endif

" let s:ctrlp_fallback =
"   \ has('win32') ?
"     \ 'dir %s /-n /b /s /a-d' :
"     \ 'find %s -type f'
"
" let g:ctrlp_user_command = [
"   \ '.git',
"   \ 'git --git-dir=%s/.git ls-files -co --exclude-standard',
"   \ s:ctrlp_fallback
"   \ ]

map <leader>b :CtrlPBuffer<cr>
" }}}

" StatusLine: {{{3
" Arguments: focus, byfname, s:regexp, prv, item, nxt, marked
"            a:1    a:2      a:3       a:4  a:5   a:6  a:7
fu! CtrlP_main_status(...)
  let regex = a:3 ? '%2*regex %*' : ''
  let prv = '%#StatusLineNC# '.a:4.' %*'
  let item = ' ' . (a:5 == 'mru files' ? 'mru' : a:5) . ' '
  let nxt = '%#StatusLineNC# '.a:6.' %*'
  let byfname = '%2* '.a:2.' %*'
  let dir = '%#SLBranch# ← %*%#StatusLineNC#' . fnamemodify(getcwd(), ':~') . '%* '

  " only outputs current mode
  retu ' %#SLArrows#»%*' . item . '%#SLArrows#«%* ' . '%=%<' . dir

  " outputs previous/next modes as well
  " retu prv . '%4*»%*' . item . '%4*«%*' . nxt . '%=%<' . dir
endf
 
" Argument: len
"           a:1
fu! CtrlP_progress_status(...)
  let len = '%#Function# '.a:1.' %*'
  let dir = ' %=%<%#LineNr# '.getcwd().' %*'
  retu len.dir
endf

hi CtrlP_Purple  ctermfg=255 guifg=#ffffff  ctermbg=54  guibg=#5f5faf
hi CtrlP_IPurple ctermfg=54  guifg=#5f5faf  ctermbg=255 guibg=#ffffff
hi CtrlP_Violet  ctermfg=54  guifg=#5f5faf  ctermbg=104 guibg=#aeaed7

let g:ctrlp_status_func = {
  \ 'main': 'CtrlP_main_status',
  \ 'prog': 'CtrlP_progress_status'
  \}
" }}}

" }}}

" Gist: {{{2
let g:gist_detect_filetype = 1
let g:gist_post_private = 1
let g:gist_show_privates = 1
" let g:gist_open_browser_after_post = 1
" }}}

" Ag: {{{2
let g:aghighlight = 1
let g:ag_mapping_message = 0
nnoremap <leader>a :Ag!<space>
" }}}

" Tabularize: {{{2
vmap <leader>t= :Tabularize /=<CR>
vmap <leader>t: :Tabularize /:\zs/l0l1<CR>
" }}}

" }}}

" AutoCMDs: {{{
augroup filespecific
  autocmd!
  au BufRead,BufNewFile *.json set ft=javascript
  au FileType go set noet nolist
  au FileType python setl ts=4
  au FileType rust setl sw=0 sts=0
augroup END

augroup cursorline
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END
" }}}
