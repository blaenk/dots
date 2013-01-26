# completions
unsetopt menu_complete
unsetopt flowcontrol
unsetopt nomatch

setopt correct nocorrectall
setopt complete_in_word
setopt always_to_end
setopt complete_aliases

# directories
setopt auto_name_dirs
setopt auto_cd

zmodload -i zsh/complist

zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
cdpath=(.)

# case-insensitive substring completion
if [ "x$CASE_SENSITIVE" = "xtrue" ]; then
  zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
  unset CASE_SENSITIVE
else
  zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
fi

