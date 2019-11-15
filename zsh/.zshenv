# Signal USR1 will be taken to mean that all interactive zsh shells should
# "reload" by re-executing zsh.
#
# This should be triggered by sending the signal using kill.
#
# $ killall -USR1 zsh
# $ reload # alias
#
# See https://superuser.com/questions/852912/
TRAPUSR1() {
  if [[ -o INTERACTIVE ]]; then
     {echo; echo execute a new shell instance } 1>&2
     exec zsh
  fi
}

# for man --html etc.
# export BROWSER=chromium
