if [[ "$OSTYPE" == linux* ]]; then
  # fixes weird problem in tmux and ssh with zsh-syntax-highlighting
  alias sudo='sudo '

  alias sysd='systemd'
  alias sysctl='systemctl'
  alias jourctl='journalctl'
  alias ls='ls --color=auto'

  # add texlive netinstall to path
  path+=('/usr/local/texlive/2013/bin/x86_64-linux')
  export PATH

  export GOPATH=/home/jorge/code/go

  # nvidia fix
  export __GL_SYNC_TO_VBLANK=1
  export __GL_SYNC_DISPLAY_DEVICE=DFP-0
  export __VDPAU_NVIDIA_SYNC_DISPLAY_DEVICE=DFP-0

  # command not found; install pkgfile
  [[ -e /usr/share/doc/pkgfile/command-not-found.zsh ]] && source /usr/share/doc/pkgfile/command-not-found.zsh
fi

