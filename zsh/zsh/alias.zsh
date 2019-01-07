# There are some ways to alter invocations of programs:
#
# * nocorrect: don't suggest a correction
# * noglob: don't interpret glob patterns

alias ":q"='exit'

alias reload="killall -USR1 -- zsh -zsh"

alias ls='ls -lh --color=always'

alias less='less -FXr'
alias -g eless='2>&1 | less'

alias tsup='sudo ntpd -qg'

# fixes weird problem in tmux and ssh with zsh-syntax-highlighting
alias sudo='sudo '

alias sedit='sudoedit'

# using `type` is much nicer and provides information for various layers
alias which='type -a'

# prompt if deleting more than 3 files
alias rm='rm -I'

alias human-readable='numfmt --to=iec --suffix=B'

if command_exists git; then
  alias g='git'
  alias git='noglob git'
fi

if command_exists tlmgr; then
  alias tlmgr-search-file='tlmgr search --global --file'
fi

if command_exists vim; then
  # update packages
  alias vimup='vim +PlugInstall +qall'
fi

if command_exists ag; then
  alias agq='ag -Q'
fi

if command_exists tree; then
  alias tree='tree -I .git -a'
fi

if command_exists emacs; then
  alias ec='emacsclient -nc'
fi

if command_exists tmux; then
  alias t='tmux'
  alias tmux='tmux -2 -u'

  alias tn='tmux new-session -s'
  alias tc='tmux new-session -t'
  alias ta='tmux attach-session -t'

  alias taro='tmux attach-session -rt'

  alias ":qa"='[[ -n $TMUX ]] && tmux confirm-before kill-session'
  alias ":wqa"='[[ -n $TMUX ]] && [[ -f ~/.tmux/plugins/tmux-resurrect/scripts/save.sh ]] && tmux run-shell ~/.tmux/plugins/tmux-resurrect/scripts/save.sh && :qa'
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
  alias rsup='rustup'
  alias rsupdate='rustup update'
  alias clippy='cargo +nightly clippy'
fi

if command_exists cargo; then
  alias c='cargo'
  alias cb='cargo build'
  alias cbl='cargo build --color=always 2>&1 | less -K +F'
  alias ct='cargo test'
  alias ctl='cargo test --color=always 2>&1 | less -K +F'
  alias cup='cargo +nightly install-update -a'
fi

if command_exists npm; then
  alias n='npm'
  alias nr='npm run'
  alias nis='npm --save install'
  alias nisd='npm --save-dev install'
  alias nus='npm --save uninstall'
  alias nusd='npm --save-dev uninstall'
  alias nex='PATH=$(npm bin):$PATH'
fi

if command_exists yarn; then
  alias y='yarn'
  alias yr='yarn run'
  alias yt='yarn test'
  alias yup='yarn upgrade'
  alias yout='yarn outdated'
  alias yex='yarn exec --'

  alias ya='yarn add'
  alias yr='yarn remove'
  function yad { yarn add "$@" --dev; }
  function yrd { yarn remove "$@" --dev; }
fi

if command_exists gist; then
  alias gist='gist -c -o'
fi

if command_exists systemctl; then
  alias sc='systemctl'
  alias scs='systemctl status'
  alias scr='systemctl restart'

  alias scu='systemctl --user'
  alias scus='systemctl --user status'
  alias scur='systemctl --user restart'
fi

if command_exists journalctl; then
  alias jc='journalctl'
  alias jcu='journalctl --user-unit'
fi

if command_exists docker; then
  alias d='docker'
fi

if command_exists docker-compose; then
  # NOTE
  # dc is 'an arbitrary precision calculator' from package 'bc'
  # I'm overriding it because I don't care about it.
  alias dc='docker-compose'
fi

if command_exists kubectl; then
  alias k='kubectl'
  alias kc='kubectl'
fi

if command_exists minikube; then
  alias mk='minikube'
fi

if command_exists helm; then
  alias h='helm'
fi

if command_exists pacman; then
  alias p='pacman'

  alias pacup='sudo pacman -Syu'
  alias pacin='sudo pacman -S'
  alias packg='sudo pacman -U'
  alias pacun='sudo pacman -Rs'

  # search remote/local for package
  alias pacss='pacman -Ss'
  alias pacqs='pacman -Qs'

  # information about a package
  alias pacqi='pacman -Qi'
  alias pacsi='pacman -Si'

  # list files owned by package
  alias pacql='pacman -Ql'

  # who owns this file
  alias pacqo='pacman -Qo'

  # all foreign packages
  alias pacqm='pacman -Qm'
fi

if command_exists aura; then
  alias aura='aura -x'

  alias aurin='sudo aura -A'

  # update packages, show pkgbuild diffs, remove orphan make deps
  alias aurup='sudo aura -Aua'

  # same as above but also update repo-sourced packages
  alias aurud='sudo aura -Aua --devel'

  # edit pkgbuild before install
  alias aured='sudo aura -A --hotedit'

  alias aurai='aura -Ai'

  # only show first 10 results
  alias auras='aura -As --head=5'

  # check pkgbuild. useful when piped to vim -
  alias aurpb='aura -Ap'

  # download tarball only
  alias aurdl='aura -Aw'

  # downgrade specific packages
  alias aurdg='sudo aura -C'

  # save n package versions, remove the rest from cache
  alias aurcc='sudo aura -Cc'

  # show pacman log
  alias aurlg='aura -L'

  # show pacman log for certain package
  alias aurli='aura -Li'
fi
