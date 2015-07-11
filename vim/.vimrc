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

" Colors: {{{
set t_Co=256
syntax enable
set background=light
colorscheme solarized
" }}}

" Vundle: {{{
set nocompatible

let install_vundle = 0

if !isdirectory($HOME . "/.vim/bundle/Vundle.vim")
  silent !mkdir -p $HOME/.vim/bundle
  silent !git clone https://github.com/gmarik/Vundle.vim $HOME/.vim/bundle/Vundle.vim
  let install_vundle = 1
endif

filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

" sort the bundles by plugin name with
"   sort i /\/\zs.\+\ze'/ r

Plugin 'vim-scripts/a.vim'
Plugin 'rking/ag.vim'
Plugin 'msanders/cocoa.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'Raimondi/delimitMate'
Plugin 'mattn/gist-vim'
Plugin 'sjl/gundo.vim'
Plugin 'vim-scripts/python.vim--Vasiliev'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'rust-lang/rust.vim'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'godlygeek/tabular'
Plugin 'tomtom/tlib_vim'
Plugin 'SirVer/ultisnips'
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'PeterRincker/vim-argumentative'
Plugin 'guns/vim-clojure-highlight'
Plugin 'guns/vim-clojure-static'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'tommcdo/vim-exchange'
Plugin 'airblade/vim-gitgutter'
Plugin 'jnwhiteh/vim-golang'
Plugin 'wlangstroth/vim-haskell'
Plugin 'henrik/vim-indexed-search'
Plugin 'groenewege/vim-less'
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'vim-pandoc/vim-pandoc-syntax'
Plugin 'derekwyatt/vim-scala'
Plugin 'guns/vim-sexp'
Plugin 'toyamarinyon/vim-swift'
Plugin 'milkypostman/vim-togglelist.git'
Plugin 'cespare/vim-toml'
Plugin 'bronson/vim-visual-star-search'
Plugin 'Shougo/vimproc.vim'
Plugin 'mattn/webapi-vim'
Plugin 'vim-scripts/yaml.vim'

if has('unix')
  if empty($SSH_CONNECTION)
    " Plugin 'Valloric/YouCompleteMe'
  endif
endif

" tpope chorus
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-fireplace'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-leiningen'
Plugin 'tpope/vim-projectionist'
Plugin 'tpope/vim-ragtag'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-rsi'
Plugin 'tpope/vim-sexp-mappings-for-regular-people'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'

call vundle#end()
filetype plugin indent on

if install_vundle == 1
  :silent! PluginInstall
  :qa
endif

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
  endif

  if !empty(head)
    let stat .= Color(active, 'SLBranch', ' ← ') . head . ' '
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

command! CloneBuffer new | put =getbufline('#',1,'$') | 1d_

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
call togglebg#map("<F5>")

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

" Plugins: {{{

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
