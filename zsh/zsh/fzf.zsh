if [ -z "$USE_SOLARIZED_DARK" ]; then
  export FZF_DEFAULT_OPTS='
    --color fg:11,hl:3,fg+:11,bg+:7,hl+:3
    --color info:5,spinner:6,pointer:6,marker:6
  '
else
  export FZF_DEFAULT_OPTS='
    --color fg:12,hl:3,fg+:12,bg+:0,hl+:3
    --color info:5,spinner:6,pointer:6,marker:6
  '
fi

_fzf_compgen_path() {
  ag -g "" "$1"
}

# use fzf to select from the previous command's arguments
fzf-select-history-argument() {
  local argument
  argument=$(print -r -- "${(@pj.\n.)${(z)history[$((HISTCMD-1))]}[2,-1]}" \
               | fzf-tmux +m --reverse --header="select historical argument" --exit-0)

  if [[ -z "${argument}" ]]; then
    return
  fi

  BUFFER="$BUFFER${argument}" && zle end-of-line
}

zle -N fzf-select-history-argument
bindkey '^[o' fzf-select-history-argument

# use fzf to select from all of the descendants
fzf-cd-down() {
  local dir
  dir=$(find ${1} ! -path . -type d -printf '%P\n' 2> /dev/null \
          | fzf-tmux +m --header="cd ↓ from $PWD" --exit-0) && cd "$dir"
  zle reset-prompt
}

# M-j to cd down
zle -N fzf-cd-down
bindkey '^[j' fzf-cd-down

# use fzf to select from all of the ancestors
fzf-cd-up() {
  local saved
  saved="${FZF_DEFAULT_OPTS}"
  FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --header=\"cd ↑ from $PWD\""

  __enhancd::cd::builtin "$(__enhancd::arguments::dot "$2")"

  FZF_DEFAULT_OPTS="${saved}"
  zle reset-prompt
}

# M-k to cd up
zle -N fzf-cd-up
bindkey '^[k' fzf-cd-up

# select a tmux window from among all windows in every session
fzf-tmux-select-all-window() {
  if ! tmux info &> /dev/null; then
    zle reset-prompt
    return
  fi

  local windows current_window target target_window
  windows=$(tmux list-windows -a -F '#{session_name}:#{window_index}: #{window_name}')

  if [ ! -z "$TMUX" ]; then
    current_window=$(tmux display-message -p '#{session_name}:#{window_index}: #{window_name}')
    windows=$(echo "$windows" | grep -v "$current_window")
  fi

  target=$(echo "$windows" | fzf-tmux --query="$1" --header="tmux windows" --select-1 +m --exit-0)

  if [[ -z "$target" ]]; then
    zle reset-prompt
    return
  fi

  target_session=$(echo $target | awk 'BEGIN{FS=":"} {print$1}')
  target_window=$(echo $target | awk 'BEGIN{FS=":"} {print$2}')

  tmux select-window -t "$target_session:$target_window"

  if [ -z "$TMUX" ]; then
    BUFFER="tmux attach-session -t \"$target_session\""
  else
    BUFFER="tmux switch-client -t \"$target_session\""
  fi

  zle accept-line
}

# M-, to fzf all windows in every session
zle -N fzf-tmux-select-all-window
bindkey '^[,' fzf-tmux-select-all-window

if [[ -f /usr/share/fzf/key-bindings.zsh ]]; then
  source /usr/share/fzf/key-bindings.zsh
fi

if [[ -f /usr/share/fzf/completion.zsh ]]; then
  source /usr/share/fzf/completion.zsh
fi
