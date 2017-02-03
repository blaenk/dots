# open man page and jump to specific option
# $ manf ls -l
manf() {
  man -P "less -p \"^ +$2\"" $1
}

# open man page and jump to examples section
eg() {
  man -P "less -p \"^EXAMPLES?\"" $1
}

# run emacs in a new session
# this way closing the shell it was started from won't
# kill emacs, nor will it usurp the shell
e() {
  ( setsid emacs $* > /dev/null 2>&1 & );
}

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
# pass-through for os x

open() {
  emulate -LR zsh

  # linux
  if (( $+commands[xdg-open] )); then
    xdg-open $* > /dev/null 2>&1
  # mac
  elif (( $+commands[open] )); then
    open $*
  fi
}

# go to the dotfiles directory

go_dots() {
  emulate -LR zsh
  cd $DOTSPATH
}

# load the given theme variant
set_theme_dots() {
  case "$1" in
    light )
      touch "$DOTSPATH/.theme.light"
      xrdb -DUSE_SOLARIZED_LIGHT=1 -merge ~/.Xresources

      msg_success "theme set to Solarized Light"
      msg_success "you may need to restart your terminal"
      ;;
    dark )
      rm -f "$DOTSPATH/.theme.light"
      xrdb -UUSE_SOLARIZED_LIGHT -merge ~/.Xresources

      msg_success "theme set to Solarized Dark"
      msg_success "you may need to restart your terminal"
      ;;
    * )
      msg_user "valid theme variants are 'light' and 'dark'"
      echo ''
      ;;
  esac
}

# edit the dotfiles

edit_dots() {
  emulate -LR zsh

  # this might need customization
  # but I don't want to use $EDITOR cause I prefer gvim
  gvim --cmd "cd $DOTSPATH"
}

# update the dotfiles
get_dots() {
  emulate -LR zsh

  pushd $DOTSPATH > /dev/null

  pre=$(git log -1 HEAD --pretty=format:%h)

  msg_info "checking for updates since $pre"

  if git pull > /dev/null 2>&1; then
    post=$(git log -1 HEAD --pretty=format:%h)

    if [[ "$pre" == "$post" ]]; then
      msg_info "no updates available"
    else
      msg_info "updated to $post\n"
      git log --oneline --format='  %C(green)+%Creset %C(bold)%h%Creset %s' $pre..HEAD
    fi
  else
    msg_fail "there was an error with updating"
  fi

  # msg_info "updating vim plugins"
  # vim +PluginInstall +qall

  popd > /dev/null

  msg_info "reloading zsh"
  exec zsh
}

# deploy the dotfiles
put_dots() {
  emulate -LR zsh

  msg_info "deploying dots from $DOTSPATH"
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

    if [[ -e $dest ]] || [[ -L $dest ]]; then
      overwrite=false
      backup=false
      skip=false
      remove=false
      fname="$(tput bold)`basename $dest`$(tput sgr0)"

      if [[ "$overwrite_all" == "false" ]] &&\
         [[ "$backup_all" == "false" ]] &&\
         [[ "$remove_all" == "false" ]] &&\
         [[ "$skip_all" == "false" ]]; then
        if [[ ! -L $dest ]]; then
          msg_user "$fname exists non-linked:"
        else
          link=`readlink -mn "$dest"`
          msg_user "$fname is already linked to $link:"
        fi

        read -k 1 action

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

      if [[ "$skip" == "false" ]] && [[ "$skip_all" == "false" ]]; then
        if [[ "$overwrite" == "true" ]] || [[ "$overwrite_all" == "true" ]] ||\
           [[ "$remove" == "true" ]] || [[ "$remove_all" == "true" ]]; then
          rm -rf $dest
          msg_fail "removed $fname"
        fi

        if [[ "$backup" == "true" ]] || [[ "$backup_all" == "true" ]]; then
          mv $dest{,.bak}
          msg_success "moved $fname to $fname.bak"
        fi

        if [[ "$overwrite" == "true" ]] || [[ "$overwrite_all" == "true" ]] ||\
           [[ "$backup" == "true" ]] || [[ "$backup_all" == "true" ]]; then
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
tput_msg() {
  printf "\r$(tput el)  $(tput setaf $1)$2$(tput sgr0) $3\n"
}

msg_info() {
  printf "\r$(tput el)  $(tput setaf 4)·$(tput sgr0) $1\n"
  # tput_msg "4" "·" $1
}

msg_success() {
  printf "\r$(tput el)  $(tput setaf 2)+$(tput sgr0) $1\n"
  # tput_msg "2" "+" $1
}

msg_fail() {
  printf "\r$(tput el)  $(tput setaf 1)-$(tput sgr0) $1\n"
  # tput_msg "1" "-" $1
}

msg_user() {
  printf "\r  $(tput setaf 5)?$(tput sgr0) $1 "
}

link_files() {
  ln -s $1 $2
  msg_success "linked $1 $(tput setaf 2)→$(tput sgr0) $2"
}

# update and deploy dots
dots() {
  emulate -LR zsh

  echo ''

  case "$1" in
    set-theme )
      shift
      set_theme_dots "$@";;
    get )
      get_dots;;
    put )
      put_dots;;
    edit )
      edit_dots;;
    go )
      go_dots;;
    * )
      msg_user "use the 'get' or 'put' commands"
      echo ''
      ;;
  esac
}

texi-to-epub() {
    name=${1%.*}
    makeinfo --docbook $1 -o "${name}.docbook"
    xsltproc /usr/share/xml/docbook/xsl-stylesheets-1.78.1/epub/docbook.xsl "${name}.docbook"
    echo "application/epub+zip" > mimetype
    zip -0Xq "${name}.epub" mimetype
    zip -Xr9D "${name}.epub" META-INF OEBPS
}
