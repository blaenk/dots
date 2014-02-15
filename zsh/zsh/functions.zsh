# open man page and jump to specific option
# $ manopt ls -l
function manopt() {
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
function lsn() {
  ls -lh --color=always $@ | \
    awk '{k=0;for(i=0;i<=8;i++)k+=((substr($1,i+2,1)~/[rwx]/) \
         *2^(8-i));if(k)printf("%0o ",k);print}'
}

# numerical permissions
alias ls='lsn'

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
  else
    # only run ip if it exists
    if (( $+commands[ip] )); then
      command ip $*
    fi
  fi
}
