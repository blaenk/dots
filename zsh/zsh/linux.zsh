if [[ "$OSTYPE" == linux* ]]; then
  eval $(keychain --eval --agents ssh -Q --quiet id_rsa)
  # fixes weird problem in tmux and ssh with zsh-syntax-highlighting
  export TERM=rxvt-256color
fi

