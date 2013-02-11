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

zstyle ':completion:*::::' completer _expand _complete _ignored _approximate
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
cdpath=(.)

# case-insensitive substring completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
