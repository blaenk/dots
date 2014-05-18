# tmux
alias t="tmux"
alias tn="tmux new -s"
alias ta="tmux a -t"
alias taro="tmux a -rt"

# ;)
alias ":q"="exit"
alias ":qa"='[[ -n $TMUX ]] && tmux confirm-before kill-session'

# fixes weird problem in tmux and ssh with zsh-syntax-highlighting
alias sudo='sudo '

# don't interpret brackets in arguments as glob patterns
alias rake='noglob rake'

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
  alias packg="sudo pacman -U"
  alias pacun="sudo pacman -Rs"

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

  alias aurup="sudo aura -Ayu"

  alias aurin="sudo aura -A"
  alias aured="aura -A --hotedit"

  alias aurai="aura -Ai"
  alias auras="aura -As"
fi
