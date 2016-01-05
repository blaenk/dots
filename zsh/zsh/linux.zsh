if [[ "$OSTYPE" == linux* ]]; then
  # nvidia fix
  # export __GL_SYNC_TO_VBLANK=1
  # export __GL_SYNC_DISPLAY_DEVICE=DFP-0
  # export __VDPAU_NVIDIA_SYNC_DISPLAY_DEVICE=DFP-0

  # systemctl --user env imports
  systemctl --user import-environment PATH SSH_AUTH_SOCK GTAGSCONF GTAGSLABEL VM

  # for man --html etc.
  export BROWSER=chromium

  # command not found; install pkgfile
  [[ -e /usr/share/doc/pkgfile/command-not-found.zsh ]] &&\
    source /usr/share/doc/pkgfile/command-not-found.zsh

  [[ -e /etc/zsh_command_not_found ]] &&\
    source /etc/zsh_command_not_found

  if [[ "$TERM" == xterm ]]; then
    export TERM=xterm-256color
  fi
fi
