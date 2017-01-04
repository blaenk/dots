alias ":q"="exit"

alias ls="ls -lh --color=auto"

alias tsup="sudo ntpd -qg"

# fixes weird problem in tmux and ssh with zsh-syntax-highlighting
alias sudo='sudo '

alias sedit='sudoedit'

# prompt if deleting more than 3 files
alias rm='rm -I'

command_exists() {
  (( $+commands[$1]))
}

if command_exists git; then
  alias git='noglob git'
  alias g='git'
fi

if command_exists tlmgr; then
  alias tlmgr-search-file='tlmgr search --global --file'
fi

if command_exists vim; then
  # update vundle
  alias vimup='vim +PluginInstall! +qall'
fi

if command_exists ag; then
  alias agq="ag -Q"
fi

if command_exists emacs; then
  alias ec="emacsclient -nc"
fi

if command_exists tmux; then
  alias t="tmux"
  alias tmux="tmux -2"
  alias tn="tmux new -s"
  alias ta="tmux a -t"
  alias taro="tmux a -rt"
  alias ":qa"='[[ -n $TMUX ]] && tmux confirm-before kill-session'
fi

if command_exists rake; then
  # don't interpret brackets in arguments as glob patterns
  alias rake='noglob rake'
fi

if command_exists xsel; then
  alias cbc='xsel -i -b'
  alias cbp='xsel -o -b'
elif command_exists pbcopy; then
  alias cbc='pbcopy'
  alias cbp='pbpaste'
fi

if command_exists rustup; then
  alias rup='rustup'
  alias clippy='rustup run nightly cargo clippy'
fi

if (( $+commands[npm] )); then
  alias n="npm"
  alias nr="npm run"
  alias nis="npm --save install"
  alias nisd="npm --save-dev install"
  alias nus="npm --save uninstall"
  alias nusd="npm --save-dev uninstall"
  alias nex='PATH=$(npm bin):$PATH'
fi

if command_exists gist; then
  alias gist='gist -c -o'
fi

if command_exists systemctl; then
  alias sc="systemctl"
  alias scs="systemctl status"
  alias scr="systemctl restart"

  alias scu="systemctl --user"
  alias scus="systemctl --user status"
  alias scur="systemctl --user restart"

  alias jc="journalctl"
  alias jcu="journalctl --user-unit"
fi

if command_exists pacman; then
  alias p="pacman"

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

if command_exists aura; then
  alias aura="aura -x"

  alias aurin="sudo aura -A"

  # update packages, show pkgbuild diffs, remove orphan make deps
  alias aurup="sudo aura -Aua"

  # same as above but also update repo-sourced packages
  alias aurud="sudo aura -Aua --devel"

  # edit pkgbuild before install
  alias aured="sudo aura -A --hotedit"

  alias aurai="aura -Ai"

  # only show first 10 results
  alias auras="aura -As --head=5"

  # check pkgbuild. useful when piped to vim -
  alias aurpb="aura -Ap"

  # download tarball only
  alias aurdl="aura -Aw"

  # downgrade specific packages
  alias aurdg="sudo aura -C"

  # save n package versions, remove the rest from cache
  alias aurcc="sudo aura -Cc"

  # show pacman log
  alias aurlg="aura -L"

  # show pacman log for certain package
  alias aurli="aura -Li"
fi
