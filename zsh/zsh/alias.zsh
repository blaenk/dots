# tmux
alias t="tmux"
alias tn="tmux new -s"
alias ta="tmux a -t"
alias tmr="tmux a -rt"

# ;)
alias ":q"="exit"
alias ":qa"='[[ -n $TMUX ]] && tmux confirm-before kill-session'

# sprinkle the dots!
alias sprinkle="~/.dots/sprinkle"

# fixes weird problem in tmux and ssh with zsh-syntax-highlighting
alias sudo='sudo '

# prompt if deleting more than 3 files
alias rm='rm -I'

if [[ -x `which pacman` ]]; then
  alias pacup="sudo pacman -Syu"
  alias pacin="sudo pacman -S"
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
fi

if [[ -x `which aura` ]]; then
  alias aura="aura -x"
  alias auru="sudo aura -Ayu"
fi
