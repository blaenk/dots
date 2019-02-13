# key bindings

# support for ci( etc

autoload -U select-bracketed
zle -N select-bracketed

for m in visual viopp; do
  for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
    bindkey -M $m $c select-bracketed
  done
done

# support ci" etc

autoload -U select-quoted
zle -N select-quoted

for m in visual viopp; do
  for c in {a,i}{\',\",\`}; do
    bindkey -M $m $c select-quoted
  done
done

# support surrounding

autoload -Uz surround
zle -N delete-surround surround
zle -N add-surround surround
zle -N change-surround surround
bindkey -a cs change-surround
bindkey -a ds delete-surround
bindkey -a ys add-surround
bindkey -M visual S add-surround

local _cursor_magenta _cursor_cyan

if [[ "$OSTYPE" == darwin* ]]; then
  _cursor_magenta="\033]Pld33682\033\\"
  _cursor_cyan="\033]Pl2aa198\033\\"

  if [[ -n "${TMUX}" ]]; then
    _cursor_magenta="\033Ptmux;\033${_cursor_magenta}"
    _cursor_cyan="\033Ptmux;\033${_cursor_cyan}"
  fi
else
  _cursor_magenta="\033]12;5\007"
  _cursor_cyan="\033]12;6\007"
fi

function zle-keymap-select {
  zle reset-prompt

  if [[ $KEYMAP = "vicmd" ]]; then
    echo -ne "${_cursor_magenta}"
  else
    echo -ne "${_cursor_cyan}"
  fi
}

echo -ne "${_cursor_cyan}"

function zle-line-finish {
  zle reset-prompt

  echo -ne "${_cursor_cyan}"
}

autoload -U edit-command-line
zle -N zle-keymap-select
zle -N zle-line-finish
zle -N edit-command-line

bindkey -v
bindkey -M vicmd V edit-command-line # ESC-v to edit in an external editor.

bindkey '\e.' insert-last-word

# Home key variants
bindkey '\e[1~' vi-beginning-of-line
bindkey '\eOH' vi-beginning-of-line

# End key variants
bindkey '\e[4~' vi-end-of-line
bindkey '\eOF' vi-end-of-line

bindkey '^[[3~' delete-char
bindkey '^[3;5~' delete-char

bindkey ' ' magic-space
bindkey -M vicmd "gg" beginning-of-history
bindkey -M vicmd "G" end-of-history
bindkey -M vicmd "k" up-line-or-history # history-search-backward
bindkey -M vicmd "j" down-line-or-history # history-search-forward
bindkey -M vicmd "/" history-incremental-search-backward
bindkey -M vicmd "?" history-incremental-search-forward
bindkey -M vicmd "u" undo
bindkey -M vicmd "U" redo
bindkey -M vicmd "_" beginning-of-line
bindkey -M vicmd "g_" end-of-line

zle -A .backward-kill-word vi-backward-kill-word
zle -A .backward-delete-char vi-backward-delete-char

bindkey -M viins 'jj' vi-cmd-mode

bindkey -M viins "^I" expand-or-complete-prefix
bindkey -M viins "^L" clear-screen
bindkey -M viins "^P" up-line-or-history
bindkey -M viins "^N" down-line-or-history
bindkey -M viins "^R" history-incremental-search-backward
bindkey -M viins "^W" backward-kill-word
bindkey -M viins "^A" beginning-of-line
bindkey -M viins "^E" end-of-line
bindkey -M viins "^H" backward-delete-char  # vi-backward-delete-char
bindkey -M viins "^U" backward-kill-line    # vi-kill-line
bindkey -M viins "^?" backward-delete-char  # vi-backward-delete-char

function vi-paste-x-selection () {
  CUTBUFFER=$(eval "cbp")
  zle yank
}

zle -N vi-paste-x-selection
# bindkey -M vicmd 'p' vi-paste-x-selection
bindkey -M viins "^Y" vi-paste-x-selection

function vi-yank-x-selection () {
  zle vi-yank
  print -rn -- $CUTBUFFER | eval "cbc"
}

# zle -N vi-yank-x-selection
# bindkey -M vicmd "y" vi-yank-x-selection
# bindkey -M visual "y" vi-yank-x-selection

function vi-yank-whole-line-x-selection () {
  zle vi-yank-whole-line
  print -rn -- $BUFFER | eval "cbc"
}

# zle -N vi-yank-whole-line-x-selection
# bindkey -M vicmd "yy" vi-yank-whole-line-x-selection

# allow interactive incr search, ^G or ^C to exit
bindkey -M isearch "^P" history-incremental-search-backward
bindkey -M isearch "^N" history-incremental-search-forward

bindkey -M menuselect "^M" .accept-line # enter command by default
bindkey -M menuselect "^G" accept-line # accept completion, don't enter
bindkey -M menuselect "+" accept-and-menu-complete # accept completion, stay in menu
bindkey -M menuselect "^[[Z" reverse-menu-complete

bindkey -M menuselect "^P" reverse-menu-complete
bindkey -M menuselect "^N" menu-complete
