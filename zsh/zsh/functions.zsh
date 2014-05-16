# open man page and jump to specific option
# $ manf ls -l
function manf() {
  man -P "less -p \"^ +$2\"" $1
}

# find the zsh file that backs a command
# $ funcpath ls
# /usr/share/zsh/functions/Completion/Unix/_ls
function funcpath() {
  echo ${^fpath}/_${1}(N)
}

# label the current window/tab
function label() {
  print -Pn "\e]2;$1\a"
}

# print colors
# $ clist 16
function clist(){
  x=`tput op`
  y=`printf %76s`
  for i in {0..$1}
  do
    o=00$i
    echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x
  done
}

# print numerical permissions before each item in ls
function ls() {
  command ls -lh --time-style '+%m/%d/%y %I:%M %p' --color=always $@ |\
    awk '{k=0;for(i=0;i<=8;i++)k+=((substr($1,i+2,1)~/[rwx]/)\
         *2^(8-i));if(k)printf("%0o ",k);print}'
}

# move back arbitrary number of directories
# $ cd b...
# $ cd ../../../
function cd() {
  emulate -LR zsh

  if [[ $1 == 'b.'* ]]; then
    builtin cd ${${1/"b"}//"."/"../"}
  else
    builtin cd $*
  fi
}

# list pacman packages not required by another package
# also print their package description
function pacorphans() {
  expac "%n:%N:%d" -Q $(expac "%n %G" | grep -v ' base') |\
    awk -F: '$2 == "" {printf "%s: %s\n", $1, $3}'
}

# what is my ip? useful for syncplay and mumble
# $ ip get
#   copied <ip> to clipboard
function ip() {
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
# pass-through for os x

function open() {
  emulate -LR zsh

  # linux
  if (( $+commands[xdg-open] )); then
    xdg-open $* > /dev/null 2>&1
  # mac
  elif (( $+commands[open] )); then
    open $*
  fi
}

# update the dotfiles
function get_dots() {
  pushd $DOTSPATH

  pre=$(git log -1 HEAD --pretty-format:%h)

  msg_info "updating from $pre"

  if git pull -q; then
    post=$(git log -1 HEAD --pretty-format:%h)

    if [[ $pre -eq $post ]]; then
      msg_info "no updates available"
    else
      msg_success "updated to $post\n"
      git log --oneline --format='  %C(green)+%Creset %C(black)%h%Creset %s' $(pre)..HEAD
    fi
  else
    msg_fail "there was an error with updating"
  fi

  popd
}

# deploy the dotfiles
function put_dots() {
  echo ''
  msg_info "deploying dots from $DOTSPATH!"
  msg_info "help: "\
"$(tput bold)b$(tput sgr0)ackup, "\
"$(tput bold)o$(tput sgr0)verwrite, "\
"$(tput bold)r$(tput sgr0)emove, "\
"$(tput bold)s$(tput sgr0)kip\n"\
"          capitalize to apply to all remaining\n"

  overwrite_all=false
  backup_all=false
  skip_all=false
  remove_all=false

  for src in `find "$DOTSPATH" -mindepth 2 -maxdepth 2  -name .\* ! -path "$DOTSPATH/.git*"`; do
    dest="$HOME/`basename \"$src\"`"

    if [ -e $dest ] || [ -L $dest ]; then
      overwrite=false
      backup=false
      skip=false
      remove=false
      fname="$(tput bold)`basename $dest`$(tput sgr0)"

      if [ "$overwrite_all" == "false" ] &&\
         [ "$backup_all" == "false" ] &&\
         [ "$remove_all" == "false" ] &&\
         [ "$skip_all" == "false" ]; then
        if [ ! -L $dest ]; then
          msg_user "$fname exists non-linked:"
        else
          link=`readlink -mn "$dest"`
          msg_user "$fname is already linked to $link:"
        fi

        read -n 1 action

        case "$action" in
          o )
            overwrite=true;;
          O )
            overwrite_all=true;;
          b )
            backup=true;;
          B )
            backup_all=true;;
          s )
            skip=true;;
          S )
            skip_all=true;;
          r )
            remove=true;;
          R )
            remove_all=true;;
          * )
            ;;
        esac
      fi

      if [ "$skip" == "false" ] && [ "$skip_all" == "false" ]; then
        if [ "$overwrite" == "true" ] || [ "$overwrite_all" == "true" ] ||\
           [ "$remove" == "true" ] || [ "$remove_all" == "true" ]; then
          rm -rf $dest
          msg_fail "removed $fname"
        fi

        if [ "$backup" == "true" ] || [ "$backup_all" == "true" ]; then
          mv $dest{,.bak}
          msg_success "moved $fname to $fname.bak"
        fi

        if [ "$overwrite" == "true" ] || [ "$overwrite_all" == "true" ] ||\
           [ "$backup" == "true" ] || [ "$backup_all" == "true" ]; then
          link_files $src $dest
        fi
      else
        msg_info "skipped $fname"
      fi

    else
      link_files $src $dest
    fi
  done
}

# message functions
function tput_msg() {
  printf "\r$(tput el)  $(tput setaf $1)$2$(tput sgr0) $3\n"
}

function msg_info() {
  printf "\r$(tput el)  $(tput setaf 4)·$(tput sgr0) $1\n"
  # tput_msg "4" "·" $1
}

function msg_success() {
  printf "\r$(tput el)  $(tput setaf 2)+$(tput sgr0) $1\n"
  # tput_msg "2" "+" $1
}

function msg_fail() {
  printf "\r$(tput el)  $(tput setaf 1)-$(tput sgr0) $1\n"
  # tput_msg "1" "-" $1
}

function msg_user() {
  printf "\r  $(tput setaf 5)?$(tput sgr0) $1 "
}

function link_files() {
  ln -s $1 $2
  msg_success "linked $1 $(tput setaf 2)→$(tput sgr0) $2"
}

# update and deploy dots
function dots() {
  emulate -LR zsh

  case "$1" in
    get )
      get_dots;;
    put )
      put_dots;;
    * )
      echo "use the 'get' or 'put' commands";;
  esac
}

