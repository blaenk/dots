# tmux
alias tmux="tmux -2"
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

  alias aurin="sudo aura -A"

  # update packages, show pkgbuild diffs, remove orphan make deps
  alias aurup="sudo aura -Akua"

  # same as above but also update repo-sourced packages
  alias aurud="sudo aura -Aka --devel"

  # edit pkgbuild before install
  alias aured="aura -A --hotedit"

  alias aurai="aura -Ai"

  # only show first 10 results
  alias auras="aura -As --head"

  # check pkgbuild. useful when piped to vim -
  alias aurap="aura -Ap"

  # download tarball only
  alias auraw="aura -Aw"

  # downgrade specific packages
  alias aurdg="aura -C"

  # save n package versions, remove the rest from cache
  alias aurcc="aura -Cc"

  # show pacman log
  alias aurlg="aura -L"

  # show pacman log for certain package
  alias aurli="aura -Li"
fi
