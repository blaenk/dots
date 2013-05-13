# tmux
alias t="tmux"
alias ta="tmux a -t"
alias ":q"="exit"

function manopt() { man -P "less -p \"^ +$2\"" $1 }
