set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Plugin 'gmarik/vundle'

" sort the bundles by plugin name with
"   sort i /\/\zs.\+\ze'/ r

Plugin 'vim-scripts/a.vim'
Plugin 'rking/ag.vim'
Plugin 'msanders/cocoa.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'Raimondi/delimitMate'
Plugin 'mattn/gist-vim'
Plugin 'sjl/gundo.vim'
Plugin 'yurifury/hexHighlight'
Plugin 'ddollar/nerdcommenter'
Plugin 'vim-scripts/python.vim--Vasiliev'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'wting/rust.vim'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'godlygeek/tabular'
Plugin 'tomtom/tlib_vim'
Plugin 'SirVer/ultisnips'
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'airblade/vim-gitgutter'
Plugin 'jnwhiteh/vim-golang'
Plugin 'wlangstroth/vim-haskell'
Plugin 'henrik/vim-indexed-search'
Plugin 'groenewege/vim-less'
Plugin 'vim-pandoc/vim-pandoc-syntax'
Plugin 'vim-pandoc/vim-pantondoc'
Plugin 'derekwyatt/vim-scala'
Plugin 'vim-scripts/VimClojure'
Plugin 'Shougo/vimproc.vim'
Plugin 'mattn/webapi-vim'
Plugin 'vim-scripts/yaml.vim'

if has('unix')
  if empty($SSH_CONNECTION)
    Plugin 'Valloric/YouCompleteMe'
  endif
endif

" tpope chorus
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-ragtag'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'

filetype plugin indent on
