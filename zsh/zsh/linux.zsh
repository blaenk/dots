if [[ "$OSTYPE" == linux* ]]; then
  # fixes weird problem in tmux and ssh with zsh-syntax-highlighting
  alias sudo='sudo '

  # only ask for ssh password on first use of ssh
  alias ssh='eval $(keychain --eval --agents ssh -Q --quiet id_rsa) && ssh'

  alias sysd='systemd'
  alias sysctl='systemctl'
  alias journ='journalctl'
  alias ls='ls --color'
  alias pacman='pacman-color'

  c16(){
    x=`tput op`
    y=`printf %76s`
    for i in {0..16}
    do
      o=00$i
      echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x
    done
  }

  c256(){
    x=`tput op`
    y=`printf %76s`
    for i in {0..256}
    do
      o=00$i
      echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x
    done
  }
fi

