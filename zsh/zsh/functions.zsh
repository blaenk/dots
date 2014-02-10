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
label() {
  print -Pn "\e]2;$1\a"
}

# print 16 colors
c16(){
  x=`tput op`
  y=`printf %76s`
  for i in {0..16}
  do
    o=00$i
    echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x
  done
}

# print numerical permissions before each item in ls
lsn() {
  ls -lh --color=always $@ | \
    awk '{k=0;for(i=0;i<=8;i++)k+=((substr($1,i+2,1)~/[rwx]/) \
         *2^(8-i));if(k)printf("%0o ",k);print}'
}

# numerical permissions
alias ls='lsn'

# print 256 colors
c256(){
  x=`tput op`
  y=`printf %76s`
  for i in {0..256}
  do
    o=00$i
    echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x
  done
}

# move back arbitrary number of directories
# $ cd b...
# $ cd ../../../
cd() {
  emulate -LR zsh

  if [[ $1 == 'b.'* ]]; then
    builtin cd ${${1//"b"/""}//"."/"../"}
  else
    builtin cd $*
  fi
}
