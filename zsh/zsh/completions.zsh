# completions
unsetopt menu_complete
unsetopt flowcontrol

setopt auto_menu
setopt complete_in_word
setopt always_to_end

setopt correct nocorrectall
setopt complete_aliases

# directories
setopt auto_name_dirs
setopt auto_cd

zmodload -i zsh/complist

eval $(dircolors ~/.dircolors)
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*::::' completer _expand _complete _ignored _approximate
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
cdpath=(.)

# case-insensitive substring completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*:*:*:*:*' menu select select=1 _complete _ignored _approximate

# use a cache
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.dots/zsh/zsh/cache

# ignore _functions
zstyle ':completion:*:functions' ignored-patterns '_*'
