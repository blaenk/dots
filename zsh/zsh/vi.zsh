# key bindings

function accept_line {
  if [[ -n "$rprompt_cached" ]]; then
    RPROMPT="$rprompt_cached"
    rprompt_cached=""
  fi
  builtin zle .accept-line
}

function zle-line-init zle-keymap-select { zle reset-prompt }

autoload -U edit-command-line
zle -N zle-line-init
zle -N zle-keymap-select
zle -N accept_line
zle -N edit-command-line

bindkey -v
bindkey -M vicmd "^M" accept_line # Alow RETURN in vi command.
bindkey -M vicmd v edit-command-line # ESC-v to edit in an external editor.

bindkey ' ' magic-space 
bindkey -M vicmd "gg" beginning-of-history
bindkey -M vicmd "G" end-of-history
bindkey -M vicmd "k" history-search-backward
bindkey -M vicmd "j" history-search-forward
bindkey -M vicmd "/" history-incremental-search-backward
bindkey -M vicmd "?" history-incremental-search-forward

zle -A .backward-kill-word vi-backward-kill-word
zle -A .backward-delete-char vi-backward-delete-char

bindkey -M viins 'jj' vi-cmd-mode
bindkey -M viins "^L" clear-screen
bindkey -M viins "^W" backward-kill-word
bindkey -M viins "^A" beginning-of-line
bindkey -M viins "^E" end-of-line
bindkey -M viins "^R" history-incremental-search-backward # allow interactive incr search, ^G or ^C to exit
bindkey -M viins "^S" history-incremental-search-forward
bindkey -M viins "^H" backward-delete-char  # vi-backward-delete-char
bindkey -M viins "^U" vi-kill-line             # vi-kill-line
bindkey -M viins "^?" backward-delete-char  # vi-backward-delete-char

