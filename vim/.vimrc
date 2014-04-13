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

" Vundle Bootstrap: {{{
source ~/.vim/conf/bundles.vim
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

hi InsertCursor  ctermfg=15 guifg=#fdf6e3 ctermbg=37  guibg=#2aa198
hi VisualCursor  ctermfg=15 guifg=#fdf6e3 ctermbg=125 guibg=#d33682
hi ReplaceCursor ctermfg=15 guifg=#fdf6e3 ctermbg=65  guibg=#dc322f
hi CommandCursor ctermfg=15 guifg=#fdf6e3 ctermbg=33  guibg=#268bd2

set shortmess=atToOI
set viewoptions+=unix,slash

set wildmenu
set wildmode=list:longest

set scrolloff=5
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

  function! Color(active, num, content)
    if a:active
      return '%' . a:num . '*' . a:content . '%*'
    else
      return a:content
    endif
  endfunction

  " this handles alternative statuslines
  let usealt = 0
  let altstat = Color(active, 4, ' »')

  let type = getbufvar(bufnum, '&buftype')
  let name = bufname(bufnum)

  if type ==# 'help'
    let altstat .= ' ' . fnamemodify(name, ':t:r')
    let usealt = 1
  elseif name ==# '__Gundo__'
    let altstat .= ' Gundo'
    let usealt = 1
  elseif name ==# '__Gundo_Preview__'
    let altstat .= ' Gundo Preview'
    let usealt = 1
  endif

  if usealt
    let altstat .= Color(active, 4, ' «')
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
    let ruler_width = max([strlen(line('$')), (&numberwidth - 1)])
    let column_width = strlen(vc)
    let padding = ruler_width - column_width
    let column = ''

    if padding <= 0
      let column .= vc
    else
      " + 1 becuase for some reason vim eats one of the spaces
      let column .= repeat(' ', padding + 1) . vc
    endif

    return column . ' '
  endfunction

  let stat .= '%1*'
  let stat .= '%{Column()}'
  let stat .= '%*'

  " file name
  let stat .= Color(active, 4, active ? ' »' : ' «')
  let stat .= ' %<'
  let stat .= '%f'
  let stat .= ' ' . Color(active, 4, active ? '«' : '»')

  " file modified
  let modified = getbufvar(bufnum, '&modified')
  let stat .= Color(active, 2, modified ? ' +' : '')

  " read only
  let readonly = getbufvar(bufnum, '&readonly')
  let stat .= Color(active, 2, readonly ? ' ‼' : '')

  " paste
  if active && &paste
    let stat .= ' %2*' . 'P' . '%*'
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
    let stat .= Color(active, 3, ' ← ') . head . ' '
  endif

  return stat
endfunction
" }}}

" Status AutoCMD: {{{

function! s:RefreshStatus()
  for nr in range(1, winnr('$'))
    call setwinvar(nr, '&statusline', '%!Status(' . nr . ')')
  endfor
endfunction

augroup status
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * call <SID>RefreshStatus()
augroup END
" }}}

" Status Colors: {{{
hi User1 ctermfg=33  guifg=#268bd2  ctermbg=15 guibg=#fdf6e3
hi User2 ctermfg=125 guifg=#d33682  ctermbg=7  guibg=#eee8d5
hi User3 ctermfg=64  guifg=#719e07  ctermbg=7  guibg=#eee8d5
hi User4 ctermfg=37  guifg=#2aa198  ctermbg=7  guibg=#eee8d5
" }}}

" }}}

" Mappings: {{{
let mapleader = ","

" Modes: {{{2
nnoremap <silent> <M-p> :set paste!<CR>
nnoremap <silent> <leader>l :noh<CR>
nnoremap <silent> <leader>c :set list!<CR>
nnoremap <silent> <leader>n :set rnu!<CR>
nnoremap <silent> <leader>t :set rnu! list! number!<CR>
nnoremap <silent> <leader>s :set spell!<CR>

nnoremap <leader>ph :PandocHighlight<space>
nnoremap <leader>pu :PandocUnhighlight<space>

inoremap jj <ESC>
" }}}

" Editing: {{{2
" 'force' write
cmap w!! %!sudo tee > /dev/null %

map <Leader>o o<Esc>ko

nmap gy `[v`]

vnoremap > >gv
vnoremap < <gv

nnoremap <M-c> y
nnoremap <M-v> p

vnoremap <M-c> y
vnoremap <M-v> p

imap <M-v> <C-r>+
cmap <M-v> <C-r>+

" go to end of line from insert mode
inoremap <C-l> <ESC>A
" }}}

" Navigation: {{{2
noremap H ^
noremap L $

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

nmap <leader>ht <Plug>HexHighlightToggle
nmap <leader>hr <Plug>HexHighlightRefresh

nnoremap <silent> <leader>u :GundoToggle<CR>
" }}}

" }}}

" Plugins: {{{

" Pandoc: {{{2
let g:pantondoc_use_pandoc_markdown = 1

let g:pantondoc_enabled_modules = [
  \"folding"
  \]

let g:pandoc_no_empty_implicits = 1
let g:pandoc_syntax_ignore_codeblocks = ['definition', 'delimited']
let g:pandoc_syntax_dont_use_conceal_for_rules = [
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
let g:gitgutter_realtime = 0
let g:gitgutter_sign_modified = '#'

nnoremap <leader>g :GitGutterToggle<CR>
" }}}

" CtrlP: {{{2

" Settings: {{{3
let g:ctrlp_map = '<leader>f'
let g:ctrlp_show_hidden = 1
" this is ignored since we're using ag
let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/]((\.(git|hg|svn))|build)$',
  \ 'file': '\v\.(DS_Store)$',
  \ }
let g:ctrlp_working_path_mode = 'ra'
" let g:ctrlp_open_new_file = 't'
" let g:ctrlp_open_multiple_files = 't'
" let g:ctrlp_prompt_mappings = {
"   \ 'AcceptSelection("t")': ['<cr>'],
"   \ 'AcceptSelection("e")': ['<s-cr>']
"   \ }

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_user_command = 'ag -l --hidden --nocolor --ignore-dir .git . %s'
  let g:ctrlp_use_caching = 0
endif

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
  let dir = '%3* ← %*%#StatusLineNC#' . fnamemodify(getcwd(), ':~') . '%* '

  " only outputs current mode
  retu ' %4*»%*' . item . '%4*«%* ' . '%=%<' . dir

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
let g:gist_open_browser_after_post = 1
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
filetype plugin indent on

augroup filespecific
  autocmd!
  au BufRead,BufNewFile *.go setl noet | setl nolist
  au BufRead,BufNewFile *.json set ft=javascript
  au BufRead,BufNewFile *.py setl ts=4
augroup END

augroup cursorline
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END
" }}}
