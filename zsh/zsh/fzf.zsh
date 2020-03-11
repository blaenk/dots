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

FZF_TAB_COMMAND=(
    fzf
    --ansi   # Enable ANSI color support, necessary for showing groups
    --expect='$continuous_trigger' # For continuous completion
    '--color=hl:$(( $#headers == 0 ? 108 : 255 ))'
    --color fg:11,hl:3,fg+:11,bg+:7,hl+:3
    --color info:5,spinner:6,pointer:6,marker:6
    --nth=2,3 --delimiter='\x00'  # Don't search prefix
    --layout=reverse --height='${FZF_TMUX_HEIGHT:=75%}'
    --tiebreak=begin -m --bind=tab:down,ctrl-j:accept,change:top,ctrl-space:toggle --cycle
    '--query=$query'   # $query will be expanded to query string at runtime.
    '--header-lines=$#headers' # $#headers will be expanded to lines of headers at runtime
)

zstyle ':fzf-tab:*' prefix '·'
zstyle ':fzf-tab:*' no-group-color $'\033[38;5;11m'
zstyle ':fzf-tab:*' command $FZF_TAB_COMMAND

_fzf_compgen_path() {
  ag -g "" "$1"
}

# use fzf to select from the previous command's arguments
fzf-select-history-argument() {
  local argument
  argument=$(print -r -- "${(@pj.\n.)${(z)history[$((HISTCMD-1))]}[2,-1]}" \
               | fzf-tmux +m --reverse --header="select historical argument" --select-1 --exit-0)

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

  __enhancd::cd::builtin "$(__enhancd::sources::go_up "$2")"

  FZF_DEFAULT_OPTS="${saved}"
  zle reset-prompt
}

# M-k to cd up
zle -N fzf-cd-up
bindkey '^[k' fzf-cd-up

# use fzf to show all aliases
# selecting an alias inserts the alias' target into the prompt without accepting
# it, allowing the user to edit the line before entering
fzf-aliases() {
  local selection expanded
  selection=$(alias | fzf-tmux +m --query="$1" --header="aliases" --exit-0 --select-1)

  if [[ -n "$selection" ]]; then
    expanded=$(echo "$selection" | cut -d"'" -f2)
    BUFFER="$BUFFER${expanded}" && zle end-of-line
  fi
}

# M-? to list all aliases
zle -N fzf-aliases
bindkey '^[?' fzf-aliases

# fzf the git aliases and print the result
fzf-git-aliases() {
  git config --get-regexp ^alias\. | \
    sed -e "s/^alias\.//" -e "s/\ /\ =\ /" | \
    fzf-tmux +m --query="$1" --header="aliases" --exit-0 --select-1
}

tmux_panes_list_format='#{session_name}:#{window_index}.#{pane_index} #{window_name}:#{pane_current_command}'

# select a tmux window from among all windows in every session
fzf-tmux-list-all-panes() {
  if ! tmux ls &> /dev/null; then
    zle reset-prompt
    return
  fi

  local windows current_window target target_window list_format
  windows=$(tmux list-panes -a -F "${tmux_panes_list_format}")

  if [ -n "$TMUX" ]; then
    current_pane=$(tmux display-message -p "${tmux_panes_list_format}")
    windows=$(echo "$windows" | grep -v "$current_pane" | grep -v "${1}")
  fi

  echo "$windows" | fzf --header="tmux panes" --select-1 +m --exit-0 | awk '{print$1}'
}

# NOTE
# When inside of tmux, a tmux binding for this takes precedence, which allows us
# to invoke it while another program is running.

_fzf-tmux-switch-panes() {
  target=$(fzf-tmux-list-all-panes "${1}")

  if [ -z "$TMUX" ]; then
    BUFFER="tmux attach-session -t \"$target\""
    zle accept-line
  else
    tmux switch-client -t "$target"
  fi
}

# M-, to fzf all windows in every session
zle -N _fzf-tmux-switch-panes
bindkey '^[,' _fzf-tmux-switch-panes

fzf-tmux-switch-panes() {
  current_pane=$(tmux display-message -p "${tmux_panes_list_format}")
  tmux split-window -f "TMUX_FZF=1 zsh -ci '_fzf-tmux-switch-panes \"$current_pane\"'"
}

# bring pane from other window into this window's split
# join-pane -s other-window.pane-number
_fzf-tmux-bring-pane() {
  target=$(fzf-tmux-list-all-panes "${1}")

  tmux join-pane -s "${target}"
}

fzf-tmux-bring-pane() {
  current_pane=$(tmux display-message -p "${tmux_panes_list_format}")
  tmux split-window -f "TMUX_FZF=1 zsh -ci '_fzf-tmux-bring-pane \"$current_pane\"'"
}

fzf_path=(
  # Standard install, Ubuntu
  $HOME/.fzf/shell
  /usr/share/doc/fzf/examples/
  # Archlinux package
  /usr/share/fzf
  # Homebrew
  /usr/local/opt/fzf/shell
)

typeset -a fzf_path
fzf_path=($^fzf_path(N))

if [[ -f "${fzf_path}/key-bindings.zsh" ]]; then
  source "${fzf_path}/key-bindings.zsh"

  bindkey -M vicmd "/" fzf-history-widget
  bindkey '^T' fzf-file-widget
  bindkey '^[f' fzf-file-widget
  bindkey '^[/' fzf-file-widget
fi

if [[ -f "${fzf_path}/completion.zsh" ]]; then
  source "${fzf_path}/completion.zsh"
fi

function mans(){
  apropos '.' | \
    fzf --preview-window=up:50% --preview "echo {} | cut -f 1 -d \" \" | sed -E 's/(.+)\((.+)\),?/\2 \1/g' | xargs man" | \
    cut -f 1 -d " " | \
    sed -E 's/(.+)\((.+)\),?/\2 \1/g' | \
    xargs man
}

if (( $+commands[brew])); then
  function bip() {
    local inst=$(brew search | eval "fzf -m --header='[brew:install]'")

    if [[ $inst ]]; then
      for prog in $(echo $inst)
      do brew install $prog
      done
    fi
  }
fi
