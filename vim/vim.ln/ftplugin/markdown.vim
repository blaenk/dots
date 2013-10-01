let b:surround_{char2nr("i")} = "_\r_"
let b:surround_{char2nr("s")} = "**\r**"

nnoremap <buffer> j gj
nnoremap <buffer> k gk

nnoremap <buffer> gj j
nnoremap <buffer> gk k

setlocal nolist
setlocal linebreak
