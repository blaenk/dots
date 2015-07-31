if (( $+commands[gnome-keyring-daemon] )); then
  eval $(gnome-keyring-daemon -s)
  export SSH_AUTH_SOCK
fi
