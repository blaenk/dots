# completions
unsetopt menu_complete
unsetopt flowcontrol

setopt auto_menu
setopt complete_in_word
setopt always_to_end

setopt correct nocorrectall

# directories
setopt auto_name_dirs
setopt auto_cd

zmodload -i zsh/complist

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' rehash true

zstyle ':completion:*::::' completer _expand _complete _ignored _approximate
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
cdpath=(.)

zstyle ':completion:*:directory-stack' list-colors '=(#b) #([0-9]#)*( *)==95=38;5;12'

# case-insensitive substring completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*:*:*:*:*' menu select=1 _complete _ignored _approximate

# use a cache
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path $DOTSPATH/zsh/zsh/cache

# ignore _functions
zstyle ':completion:*:functions' ignored-patterns '_*'

zstyle ':completion:*:*:docker:*' option-stacking yes
zstyle ':completion:*:*:docker-*:*' option-stacking yes

zstyle '*' single-ignored complete

if [[ "$OSTYPE" == 'msys' ]]; then
  local drives=($(mount | command grep --perl-regexp '^\w: on /\w ' | cut --delimiter=' ' --fields=3))
  zstyle ':completion:*' fake-files "/:${(j. .)drives//\//}"
fi
