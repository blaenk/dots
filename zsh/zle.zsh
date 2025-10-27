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
  [[ $KEYMAP = vicmd ]] && print -n -- "${_cursor_magenta}" || print -n -- "${_cursor_cyan}"
}

print -n -- "${_cursor_cyan}"

function zle-line-finish {
  print -n -- "${_cursor_cyan}"
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

# (Updated Function)
# Function to delete a path component by temporarily redefining WORDCHARS and word style.
# backward-kill-path-component() {
#   # Use `local` to create temporary, function-scoped settings.
#   # They automatically revert to their original values when the function ends.

#   # 1. Temporarily switch to the 'zsh' word style, which respects WORDCHARS.
#   select-word-style bash

#   # 2. Temporarily remove the '/' character from WORDCHARS.
#   # local WORDCHARS=${WORDCHARS//\//}

#   # Now, execute the standard backward-kill-word widget.
#   # It will use our temporary, local settings.
#   zle backward-kill-word

#   select-word-style normal
# }

# Deletes the last path component, with a fallback to deleting the last
# word while preserving the trailing space.
# backward-kill-path-component() {
#   # Save the original buffer so we can see if our first attempt changed anything.
#   local original_lbuffer=$LBUFFER

#   # --- Part 1: Try to delete as a path component ---
#   LBUFFER=${LBUFFER%/}
#   LBUFFER=${LBUFFER%/*}

#   # --- Part 2: The Robust Fallback ---
#   # Check if the buffer is unchanged.
#   if [[ $LBUFFER == $original_lbuffer ]]; then
#     # Fall back using a more explicit, two-step nested expansion.
#     # This isolates the last word and then removes it from the original string.
#     LBUFFER=${LBUFFER%${LBUFFER##* }}
#   fi
# }

backward-kill-path-component() {
  local original_lbuffer=$LBUFFER

  # --- Part 1: Try to delete as a path component ---
  # Normalize by removing a trailing slash first.
  local temp_buffer=${LBUFFER%/}

  # Isolate the last component.
  local last_comp=${temp_buffer##*/}

  # GUARDED DELETION:
  # Proceed only if a component was isolated AND it's not the whole string.
  # This prevents 'cd /' from becoming empty.
  if [[ -n "$last_comp" && "$last_comp" != "$temp_buffer" ]]; then
    LBUFFER=${temp_buffer%$last_comp}
    return # Successful deletion, so we are done.
  fi

  # --- Part 2: The Robust Fallback ---
  # If we've reached this point, the path logic did nothing.
  # Isolate the last word.
  local last_word=${LBUFFER##* }

  # GUARDED DELETION:
  # Proceed only if a word was isolated AND it's not the whole string.
  # This prevents 'cd ' or 'cd' from becoming empty.
  if [[ -n "$last_word" && "$last_word" != "$LBUFFER" ]]; then
    LBUFFER=${LBUFFER%$last_word}
  fi
}

# Register the function as a ZLE widget
zle -N backward-kill-path-component

# Bind the new widget to a key combination.
# Alt-Backspace is a common and safe choice.
# On macOS, you may need to configure your terminal to use Option as Meta.
# In iTerm2: Preferences -> Profiles -> Keys -> General -> "Left Option Key" -> Esc+
# bindkey '\e\x7f' backward-kill-path-component

# If Alt-Backspace doesn't work, you can try Alt-W
bindkey -M viins '\ew' backward-kill-path-component