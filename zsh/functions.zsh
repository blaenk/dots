wt() {
  if [ $# -eq 0 ]; then
    local dir
    dir=$(git worktree list --porcelain | awk '/^worktree /{path=$2; branch=""} /^branch /{branch=substr($2,12)} /^$/{n++; paths[n]=path; branches[n]=(branch ? branch : "detached"); l=length(branches[n])+2; if(l>max) max=l} END{for(i=1;i<=n;i++) printf "\033[36m%-*s\033[0m %s\n", max, "["branches[i]"]", paths[i]}' | fzf --ansi --header="worktrees" --preview 'git -C {-1} log --oneline --decorate --color=always -10' | awk '{print $NF}')
    if [ -n "$dir" ]; then
      local name=$(basename "$dir")
      if [ -n "$TMUX" ]; then
        tmux new-session -d -s "$name" -c "$dir" 2>/dev/null
        tmux switch-client -t "$name"
      else
        tmux new-session -s "$name" -c "$dir"
      fi
    fi
  else
    local repo name wtdir reporoot
    repo=$(basename "$(git rev-parse --show-toplevel)") || return 1
    reporoot=$(git rev-parse --show-toplevel)
    name=$1
    wtdir="$HOME/code/worktrees/$repo/$name"
    if [ ! -d "$wtdir" ]; then
      git worktree add -b "${GIT_USER_BRANCH_PREFIX}${name}" "$wtdir" 2>/dev/null || git worktree add "$wtdir" "${GIT_USER_BRANCH_PREFIX}${name}" || return 1
      # Symlink devbox.json if the source repo has one
      [ -f "$reporoot/devbox.json" ] && ln -s "$reporoot/devbox.json" "$wtdir/devbox.json"
    fi
    # Create tmux session+window and cd there (or attach if session exists)
    if [ -n "$TMUX" ]; then
      tmux new-session -d -s "$name" -c "$wtdir" 2>/dev/null
      tmux switch-client -t "$name"
    else
      tmux new-session -s "$name" -c "$wtdir"
    fi
  fi
}

# open man page and jump to specific option
# $ manf ls -l
manf() {
  man -P "less -p \"^( +\K$2| +-[^ ]+( or|,) \K$2)\"" $1
}

cheat() {
  curl "cht.sh/${1}"
}

message-logs() {
  kc logs -f -n messaging--platform $(kc get pods -n messaging--platform | grep topic-listener | cut -f1 -d ' ' | head -1) -c topic-listener | zr-log-deturder
}

# open man page and jump to examples section
eg() {
  man -P "less -p \"^EXAMPLES?\"" $1
}

# create a directory and cd into it
mkcd() {
  mkdir $1 && cd $_
}

# run emacs in a new session
# this way closing the shell it was started from won't
# kill emacs, nor will it usurp the shell
e() {
  if [[ "$OSTYPE" == darwin* ]]; then
    emacs $* > /dev/null 2>&1 &!
  else
    ( setsid emacs $* > /dev/null 2>&1 & );
  fi
}

magit() {
  e --eval '(magit-status)'
}

alias mg='magit'

# html man pages
manh() {
  file=$(mktemp)
  man --html=cat $1 > $file 2>/dev/null

  if [[ -s $file ]]; then
    $BROWSER $file 2>/dev/null
  else
    echo "no man page for '$1'"
  fi
}

# find the zsh file that backs a command
# $ funcpath ls
# /usr/share/zsh/functions/Completion/Unix/_ls
funcpath() {
  echo ${^fpath}/_${1}(N)
}

# label the current window/tab
label() {
  print -Pn "\e]2;$1\a"
}

# serve an application with vnc
streamapp() {
  x11vnc -id pick -display :0 -passwd $1 -viewonly -shared -forever
}

# print colors
# $ clist 16
clist(){
  x=`tput op`
  y=`printf %76s`
  for i in {0..$1}
  do
    o=00$i
    echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x
  done
}

colortest(){
  #!/bin/bash
  #
  #   This file echoes a bunch of color codes to the
  #   terminal to demonstrate what's available.  Each
  #   line is the color code of one forground color,
  #   out of 17 (default + 16 escapes), followed by a
  #   test use of that color on all nine background
  #   colors (default + 8 escapes).
  #

  T='gYw'   # The test text

  echo -e "\n                 40m     41m     42m     43m\
       44m     45m     46m     47m";

  for FGs in '    m' '   1m' '  30m' '1;30m' '  31m' '1;31m' '  32m' \
             '1;32m' '  33m' '1;33m' '  34m' '1;34m' '  35m' '1;35m' \
             '  36m' '1;36m' '  37m' '1;37m';
    do FG=${FGs// /}
    echo -en " $FGs \033[$FG  $T  "
    for BG in 40m 41m 42m 43m 44m 45m 46m 47m;
      do echo -en "$EINS \033[$FG\033[$BG  $T  \033[0m";
    done
    echo;
  done
  echo
}

# print numerical permissions before each item in ls
lsp() {
  command ls -lh --time-style '+%m/%d/%y %I:%M %p' --color=always $@ |\
    awk '{k=0;for(i=0;i<=8;i++)k+=((substr($1,i+2,1)~/[rwx]/)\
         *2^(8-i));if(k)printf("%0o ",k);print}'
}

# list pacman packages not required by another package
# also print their package description
pacorphans() {
  expac "%n:%N:%d" -Q $(expac "%n %G" | grep -v ' base') |\
    awk -F: '$2 == "" {printf "%s: %s\n", $1, $3}'
}

# print the package's version
pacqv() {
  echo $(pacman -Qi $1 | grep Version | tr -s ' ' | cut -d ' ' -f 3)
}

# enter a docker container
dexec() {
  docker exec -it "${1}" "${2:-bash}"
}

drun() {
  docker run --rm -it "${1}" "${2:-bash}"
}

klogs() {
  kubectl logs -n "$1" -c "$2" "$3"
}

# what is my ip? useful for syncplay and mumble
# $ ip get
#   copied <ip> to clipboard
ip() {
  emulate -LR zsh

  if [[ $1 == 'get' ]]; then
    res=$(curl -s ipinfo.io/ip)
    echo -n $res | xsel --clipboard
    echo "copied $res to clipboard"
  # only run ip if it exists
  elif (( $+commands[ip] )); then
    command ip $*
  fi
}

# open alias for xdg-open
# it ignores stdout and stderr

if (( $+commands[xdg-open] )); then
  open() {
    emulate -LR zsh

    xdg-open $* > /dev/null 2>&1
  }
fi

# Inspiration: https://github.com/petertriho/dotfiles/blob/main/scripts/.local/bin/update
update_plugins() {
  echo "updating zsh plugins"
  zinit update

  echo "updating tmux plugins"
  ~/.tmux/plugins/tpm/bin/update_plugins all

  # Reload tmux configuration if there are running sessions. Note that this
  # simply "overlays" the new configuration over the previously loaded one,
  # meaning for example that key binds that were removed in a newer
  # configuration version will remain in the running session until it's
  # restarted.
  #
  # It's possible to unbind all binds with `unbind -a` and then load a "clean
  # slate" configuration of out-of-the-box tmux binds which can be retrieved
  # using:
  #
  # $ tmux -f /dev/null -L temp start-server \; list-keys
  #
  # See https://unix.stackexchange.com/questions/57641
  if tmux info &> /dev/null; then
    tmux source-file ~/.tmux.conf \; display "Reloaded!"
  fi

  echo "updating vim plugins"
  vim '+PlugInstall' '+PlugUpdate' '+PlugClean!' '+PlugUpdate' '+qall'

  # echo "Updating neovim plugins ..."
  # nvim +TSUpdateSync +qall

  # nvim +"autocmd User PackerComplete sleep 100m | write! /tmp/packer-sync-result.txt | quitall" \
  #      +PackerSync

  # cargo install-update --all

  brew update
  brew upgrade
}

# deploy the dotfiles
install_plugins() {
  echo "installing tmux plugins"
  ~/.tmux/plugins/tpm/bin/install_plugins

  echo "installing vim plugins"
  vim +PlugInstall +qall

  # cargo install cargo-update
}

texi-to-epub() {
    name=${1%.*}
    makeinfo --docbook $1 -o "${name}.docbook"
    xsltproc /usr/share/xml/docbook/xsl-stylesheets-1.78.1/epub/docbook.xsl "${name}.docbook"
    echo "application/epub+zip" > mimetype
    zip -0Xq "${name}.epub" mimetype
    zip -Xr9D "${name}.epub" META-INF OEBPS
}

if [ "$(type -w assume)" = 'assume: alias' ]; then
  unalias assume

  function assume() {
    echo "Truncating ~/.aws/credentials..."
    rm ~/.aws/credentials

    export GRANTED_ALIAS_CONFIGURED=true
    . assume --ex "$@"

    if [ $? -eq 0 ]; then
      echo "Setting the assumed role as the [default] profile in ~/.aws/credentials..."
      sed -i '' '1s/\[.*\]/[default]/' ~/.aws/credentials
    fi
  }
fi
