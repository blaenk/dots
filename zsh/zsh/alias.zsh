# tmux
alias t="tmux"
alias ta="tmux a -t"

# ;)
alias ":q"="exit"

# fixes weird problem in tmux and ssh with zsh-syntax-highlighting
alias sudo='sudo '

if [[ -x `which pacman` ]]; then
  alias pacup="sudo pacman -Syu"
  alias pacin="sudo pacman -S"
  alias pacun="sudo pacman -Rs"

  alias pacss="pacman -Ss"
  alias pacqs="pacman -Qs"

  alias pacqi="pacman -Qi"
  alias pacsi="pacman -Si"

  alias pacorphans="pacman -Qtdq"

  if [[ -x `which expac` ]]; then
    alias pacorphans="expac "%n:%N:%d" -Q $(expac "%n %G" | grep -v ' base') | awk -F: '$2 == "" {printf "%s: %s\n", $1, $3}"
  fi
fi

if [[ -x `which aura` ]]; then
  alias aura="sudo aura -x"
  alias auru="sudo aura -Ayu"
fi
