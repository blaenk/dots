if [[ "$OSTYPE" == linux* ]]; then
  # fixes weird problem in tmux and ssh with zsh-syntax-highlighting
  alias sudo='sudo '

  # only ask for passphrase on first use of ssh/git
  # perhaps add SSH_CONNECTION check here
  if [[ -n $SSH_CONNECTION ]]; then
    alias ssh='eval $(keychain --eval --agents ssh -Q --quiet id_rsa) && ssh'
  # alias git='eval $(keychain --eval --agents ssh -Q --quiet id_rsa) && git'
  fi

  alias sysd='systemd'
  alias sysctl='systemctl'
  alias journ='journalctl'
  alias ls='ls --color'

  eval `dircolors ~/.dircolors`

  export GOPATH=/home/jorge/code/go
  export __GL_SYNC_TO_VBLANK=1
  export __GL_SYNC_DISPLAY_DEVICE=DFP-0
  export __VDPAU_NVIDIA_SYNC_DISPLAY_DEVICE=DFP-0

  label() {
    print -Pn "\e]2;$1\a"
  }

  c16 (){
    x=`tput op`
    y=`printf %76s`
    for i in {0..16}
    do
      o=00$i
      echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x
    done
  }

  c256 (){
    x=`tput op`
    y=`printf %76s`
    for i in {0..256}
    do
      o=00$i
      echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x
    done
  }
fi

