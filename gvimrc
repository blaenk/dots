if has("gui_macvim")
  colorscheme Excelsior
  set guioptions-=T
  set fuoptions=maxhorz,maxvert
  " macmenu Window.Toggle\ Full\ Screen\ Mode key=<D-CR>
  map <D-F> :Ack<space>
  vmap <D-]> >gv
  vmap <D-[> <gv
  imap <D-CR> <Esc>o
  macmenu &File.New\ Tab key=<D-T>
  macmenu &File.Open\ Tab\.\.\. key=<D-O>
  map <D-t> :CommandT<CR>
  imap <D-t> <Esc>:CommandT<CR>

  "set shell=bash
endif
