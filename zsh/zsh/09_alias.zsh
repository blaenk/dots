# tmux
alias t="tmux"
alias tn="tmux new -s"
alias ta="tmux a -t"
alias tmr="tmux a -rt"

# ;)
alias ":q"="exit"
alias ":qa"='[[ -n $TMUX ]] && tmux confirm-before kill-session'

# fixes weird problem in tmux and ssh with zsh-syntax-highlighting
alias sudo='sudo '

# prompt if deleting more than 3 files
alias rm='rm -I'

if (( $+commands[xsel] )); then
  alias cbc='xsel -i -b'
  alias cbp='xsel -o -b'
fi

if (( $+commands[gist] )); then
  alias gist='gist -c -o'
fi

if (( $+commands[systemctl] )); then
  alias sc="systemctl"
fi

if (( $+commands[pacman] )); then
  alias pacup="sudo pacman -Syu"
  alias pacin="sudo pacman -S"
  alias pacun="sudo pacman -Rs"
  alias pacau="sudo pacman -U"

  # search remote/local for package
  alias pacss="pacman -Ss"
  alias pacqs="pacman -Qs"

  # information about a package
  alias pacqi="pacman -Qi"
  alias pacsi="pacman -Si"

  # list files owned by package
  alias pacql="pacman -Ql"

  # who owns this file
  alias pacqo="pacman -Qo"

  # all foreign packages
  alias pacqm="pacman -Qm"
fi

if (( $+commands[aura] )); then
  alias aura="aura -x"
  alias auru="sudo aura -Ayu"
fi
