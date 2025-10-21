export ENHANCD_ENABLE_HYPHEN=false
export ENHANCD_ENABLE_DOUBLE_DOT=false
export ENHANCD_ENABLE_SINGLE_DOT=false
export ENHANCD_ENABLE_HOME=true
export ENHANCD_USE_ABBREV=true

export ENHANCD_ARG_HYPHEN=--
export ENHANCD_ARG_DOUBLE_DOT=...
export ENHANCD_ARG_HOME=''
export ENHANCD_DOT_SHOW_FULLPATH=1
export ENHANCD_USE_FUZZY_MATCH=0

# compdef _cd __enhancd::cd

# Checks if the command is `cd`
# If it is and there's one space or more after it, e.g. `cd `, then go $HOME
# Else use existing `cd` (i.e. enhancd)
_my_cd_wrapper() {
  if [[ $BUFFER =~ ^cd[[:space:]]+$ ]]; then
    BUFFER='cd ~'
  fi

  zle .accept-line
}

zle -N accept-line _my_cd_wrapper
