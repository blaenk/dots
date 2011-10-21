call pathogen#infect()
set nocompatible
syntax on

colorscheme Tomorrow-Night

let g:CommandTAcceptSelectionMap = '<C-t>'
let g:CommandTAcceptSelectionTabMap = '<CR>'
let g:CommandTMaxHeight=10

let g:solarized_termcolors=256

" objective-c
let g:alternateExtensions_m = "h"
let g:alternateExtensions_h = "m"

let g:delimitMate_expand_cr = 1

let g:gist_detect_filetype = 1
let g:gist_open_browser_after_post = 1

set paste

set backup
set backupdir=~/.vim/backups
set directory=~/.vim/tmp
set undodir=~/.vim/undo

cmap w!! %!sudo tee > /dev/null %

let mapleader = ","
nnoremap <silent> <Leader>l :TlistToggle<CR>
map <Leader><CR> o<Esc>ko
cmap <C-P> <C-R>=expand("%:p:h") . "/" <CR>
map <Leader>n :NERDTreeToggle<CR>
set laststatus=2
set statusline=%F%m%r%h%w\ %y\ [%l/%L,%c]\ (%p%%)\ %{fugitive#statusline()}
set nowrap
set tabstop=2
set shiftwidth=2
set expandtab
set softtabstop=2
set smartindent
set autoindent
set gfn=Menlo:h14
set undofile

filetype on
filetype plugin indent on
au BufRead,BufNewFile *.json set filetype=javascript
au BufRead,BufNewFile *.zsh-theme set filetype=zsh

set wildmenu
set wildmode=list:longest

set encoding=utf-8
set showmode
set showcmd
set hidden
set visualbell
set cursorline
set ttyfast
set ruler
set relativenumber

nnoremap / /\v
vnoremap / /\v
set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch

set wrap

set list
set listchars=tab:▸\ ,eol:¬

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
nnoremap j gj
nnoremap k gk

inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

nnoremap ; :

nnoremap <leader>a :Ack

inoremap jj <ESC>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <silent> <C-l> :noh<CR><C-l>
nmap <Leader>x <Plug>ToggleAutoCloseMappings

nnoremap <leader>p :set rnu! list!<CR>
