export FZF_DEFAULT_OPTS='
  --color fg:11,hl:3,fg+:11,bg+:7,hl+:3
  --color info:5,spinner:6,pointer:6,marker:6
'

_fzf_compgen_path() {
  ag -g "" "$1"
}

# use fzf to select from all of the descendants
fzf-cd-down() {
  local dir
  dir=$(find ${1} -type d -printf '%P\n' 2> /dev/null \
          | fzf-tmux +m) && cd "$dir"
  zle reset-prompt
}

# M-j to cd down
zle -N fzf-cd-down
bindkey '^[j' fzf-cd-down

# use fzf to select from all of the ancestors
fzf-cd-up() {
  __enhancd::cd::builtin "$(__enhancd::arguments::dot "$2")"
  zle reset-prompt
}

# M-k to cd up
zle -N fzf-cd-up
bindkey '^[k' fzf-cd-up

# fs [FUZZY PATTERN] - Select selected tmux session
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fzf-tmux-select-session() {
  local session

  if ! tmux info &> /dev/null; then
    echo "no tmux session available"
    return
  fi

  session=$(tmux list-sessions -F "#{session_name}" | \
    fzf-tmux --query="$1" --select-1 --exit-0) &&
    [ -z "$TMUX" ] && tmux attach-session -t "$session" ||
      tmux switch-client -t "$session"
}

# M-. to fzf sessions
bindkey -s '^[.' 'fzf-tmux-select-session\n'

# ftw - switch window
fzf-tmux-select-window() {
  [ -z "$TMUX" ] && return

  local windows current_window target target_window
  windows=$(tmux list-windows -F '#{window_index}: #{window_name}')
  current_window=$(tmux display-message -p '#{window_index}: #{window_name}')

  target=$(echo "$windows" | grep -v "$current_window" |
              fzf-tmux --query="$1" --select-1 +m --reverse --exit-0) || return

  target_window=$(echo $target | awk 'BEGIN{FS=":"} {print$1}')

  tmux select-window -t $target_window
}

# M-, to fzf windows in current session
bindkey -s '^[,' 'fzf-tmux-select-window\n'

# ftwa - switch window all
fzf-tmux-select-all-window() {
  if ! tmux info &> /dev/null; then
    echo "no tmux session available"
    return
  fi

  local windows current_window target target_window
  windows=$(tmux list-windows -a -F '#{session_name}:#{window_index}: #{window_name}')

  if [ ! -z "$TMUX" ]; then
    current_window=$(tmux display-message -p '#{session_name}:#{window_index}: #{window_name}')
    windows=$(echo "$windows" | grep -v "$current_window")
  fi

  target=$(echo "$windows" | fzf-tmux --query="$1" --select-1 +m --reverse --exit-0) || return

  target_session=$(echo $target | awk 'BEGIN{FS=":"} {print$1}')
  target_window=$(echo $target | awk 'BEGIN{FS=":"} {print$2}')

  tmux select-window -t "$target_session:$target_window"

  if [ -z "$TMUX" ]; then
    tmux attach-session -t "$target_session"
  else
    tmux switch-client -t "$target_session"
  fi
}

# M-, to fzf all windows in every session
bindkey -s '^[<' 'fzf-select-all-window\n'

if [[ -f /usr/share/fzf/key-bindings.zsh ]]; then
  source /usr/share/fzf/key-bindings.zsh
fi
