if [[ "$OSTYPE" == linux* ]]; then
  eval $(keychain --eval --agents ssh -Q --quiet id_rsa)
  # fixes weird problem in tmux and ssh with zsh-syntax-highlighting
  export TERM=rxvt-256color
  alias sudo='sudo '
  alias sysd='systemd'
  alias sysctl='systemctl'
  alias journ='journalctl'
  alias ls='ls --color'

  c16(){
    x=`tput op`
    y=`printf %76s`
    for i in {0..16}
    do
      o=00$i
      echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x
    done
  }
fi

