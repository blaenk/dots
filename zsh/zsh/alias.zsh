# tmux
alias t="tmux"
alias ta="tmux a -t"

# ;)
alias ":q"="exit"
alias ":qa"="tmux confirm-before kill-session"

# sprinkle the dots!
alias sprinkle="~/.dots/sprinkle"

# fixes weird problem in tmux and ssh with zsh-syntax-highlighting
alias sudo='sudo '

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
  alias aura="sudo aura -x"
  alias auru="sudo aura -Ayu"
fi
