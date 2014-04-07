set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Bundle 'gmarik/vundle'

" sort the bundles by plugin name with
"   sort i /\/\zs.\+\ze'/ r

Bundle 'a.vim'
Bundle 'rking/ag.vim'
Bundle 'msanders/cocoa.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'Raimondi/delimitMate'
Bundle 'mattn/gist-vim'
Bundle 'sjl/gundo.vim'
Bundle 'yurifury/hexHighlight'
Bundle 'ddollar/nerdcommenter'
Bundle 'python.vim--Vasiliev'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'wting/rust.vim'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'godlygeek/tabular'
Bundle 'tomtom/tlib_vim'
Bundle 'SirVer/ultisnips'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'VimClojure'
Bundle 'airblade/vim-gitgutter'
Bundle 'jnwhiteh/vim-golang'
Bundle 'wlangstroth/vim-haskell'
Bundle 'henrik/vim-indexed-search'
Bundle 'groenewege/vim-less'
Bundle 'vim-pandoc/vim-pandoc-syntax'
Bundle 'vim-pandoc/vim-pantondoc'
Bundle 'derekwyatt/vim-scala'
Bundle 'Shougo/vimproc.vim'
Bundle 'mattn/webapi-vim'
Bundle 'yaml.vim'

if has('unix')
  if empty($SSH_CONNECTION)
    Bundle 'Valloric/YouCompleteMe'
  endif
endif

" tpope chorus
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-ragtag'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'

filetype plugin indent on
