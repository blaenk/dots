set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'bling/vim-airline'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'airblade/vim-gitgutter'
Bundle 'vim-scripts/Align'
Bundle 'vim-pandoc/vim-pandoc'
Bundle 'yurifury/hexHighlight'
Bundle 'ddollar/nerdcommenter'
Bundle 'groenewege/vim-less'
Bundle 'kien/ctrlp.vim'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'
Bundle 'mileszs/ack.vim'
Bundle 'msanders/cocoa.vim'
Bundle 'sjl/gundo.vim'
Bundle 'Raimondi/delimitMate'
Bundle 'tomtom/tlib_vim'
Bundle 'vim-scripts/a.vim'
Bundle 'vim-scripts/python.vim--Vasiliev'
Bundle 'wlangstroth/vim-haskell'
Bundle 'godlygeek/tabular'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'henrik/vim-indexed-search'
Bundle 'SirVer/ultisnips'
Bundle 'yurifury/hexHighlight'

if has('unix')
  Bundle 'Valloric/YouCompleteMe'
  Bundle 'rking/ag.vim'
endif

" tpope chorus
"Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-ragtag'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'

filetype plugin indent on
