" gui configurations
set guioptions-=T
set guioptions-=m
set guioptions-=rL

set lines=25 columns=85

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

if has('gui_win32')
endif
