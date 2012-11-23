" gui configurations
set guioptions-=T
set guioptions-=m
set guioptions-=rL

if has('gui_macvim')
  set fuoptions=maxhorz,maxvert
  " macmenu Window.Toggle\ Full\ Screen\ Mode key=<D-CR>
  map <D-F> :Ack<space>
  vmap <D-]> >gv
  vmap <D-[> <gv
  imap <D-CR> <Esc>o
  macmenu &File.New\ Tab key=<D-T>
  macmenu &File.Open\ Tab\.\.\. key=<D-O>
endif

if has('gui_gtk')
  " gvim specifics
endif

if has('gui_win64')
endif
