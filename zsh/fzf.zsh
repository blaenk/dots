export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git'

export FZF_DEFAULT_OPTS='
  --ansi
  --color fg:7,hl:3,fg+:-1,bg+:8,hl+:3
  --color info:5,spinner:6,pointer:6,marker:6
  --preview-window=up:wrap
'

export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window wrap:down:2"

export FZF_ALT_C_OPTS="--preview 'tree -C {}'"

FZF_TAB_FLAGS=(
  --ansi   # Enable ANSI color support, necessary for showing groups
  --color fg:7,hl:3,fg+:-1,bg+:8,hl+:3
  --color info:5,spinner:6,pointer:6,marker:6
)

zstyle ':fzf-tab:*' default-color $'\033[93m'
# preview directory's content with eza when completing cd
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'
zstyle ':fzf-tab:*' fzf-flags $FZF_TAB_FLAGS

# bindkey -M viins '^I^I' fzf-tab-complete

# Use fd (https://github.com/sharkdp/fd) for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
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
# fzf-cd-down() {
#   local dir
#   dir=$(gfind ${1} ! -path . -type d -printf '%P\n' -maxdepth 1 2> /dev/null \
#         | fzf-tmux +m --header="cd ↓ from $PWD" --exit-0)
#   cd "$dir"
#   zle reset-prompt
# }

fzf-cd-down() {
  local initial_target_dir="${1}" # The directory to start searching in, passed recursively
  local current_search_path
  local selected_fzf_output key selection chosen_item_relative_path chosen_item_full_path

  # Determine the absolute, real path for the current search
  if [[ -n "$initial_target_dir" ]]; then
    current_search_path=$(realpath "$initial_target_dir" 2>/dev/null)
    if [[ -z "$current_search_path" || ! -d "$current_search_path" ]]; then
      echo "fzf-cd-down: Invalid or non-directory path specified: '$initial_target_dir'." >&2
      echo "fzf-cd-down: Defaulting to current directory: '$PWD'." >&2
      current_search_path=$(realpath "$PWD") # Fallback to PWD
    fi
  else
    current_search_path=$(realpath "$PWD") # Default to PWD if no argument
  fi

  # Updated header to include the new Ctrl-G binding
  local fzf_header
  fzf_header="Dir: $current_search_path (^/:descend sel, Enter:cd sel, ^G:cd here, Esc:cancel)"

  selected_fzf_output=$(gfind "$current_search_path" -mindepth 1 -maxdepth 1 -type d -printf '%P\n' 2>/dev/null | \
    fzf-tmux +m --header="$fzf_header" \
               --expect=/,enter,ctrl-g --print-query --exit-0 \
               --preview="ls -p --color=always '$current_search_path/{}'")

  if [[ -z "$selected_fzf_output" ]]; then
    zle reset-prompt
    return 0
  fi

  local -a lines
  lines=("${(@f)selected_fzf_output}")

  key="${lines[2]}"
  chosen_item_relative_path="${lines[3]}" # This might be empty if Ctrl-G was pressed without a selection focused

  # Handle Ctrl-G: "cd to current directory being listed"
  # This action takes precedence and doesn't depend on a selection.
  if [[ "$key" == "ctrl-g" ]]; then
    cd "$current_search_path"
    zle reset-prompt
    return 0
  fi

  # For '/' (descend) and 'enter' (cd into selection), a selection is required.
  if [[ -z "$chosen_item_relative_path" ]]; then
    # No item was selected (e.g., pressed Enter or / on an empty filtered list)
    zle reset-prompt
    return 0
  fi

  chosen_item_full_path="$current_search_path/$chosen_item_relative_path"
  chosen_item_full_path=$(realpath "$chosen_item_full_path" 2>/dev/null)

  if [[ -z "$chosen_item_full_path" || ! -d "$chosen_item_full_path" ]]; then
      echo "fzf-cd-down: Selected path is not a valid directory: '$current_search_path/$chosen_item_relative_path'" >&2
      zle reset-prompt
      return 1
  fi

  if [[ "$key" == "/" ]]; then
    fzf-cd-down "$chosen_item_full_path"
  elif [[ "$key" == "enter" ]]; then
    cd "$chosen_item_full_path"
    zle reset-prompt
  else
    # Other key pressed or unexpected scenario
    zle reset-prompt
  fi
}

# M-j to cd down
zle -N fzf-cd-down
bindkey '^[j' fzf-cd-down

# use fzf to select from all of the ancestors
fzf-cd-up() {
  local declare dirs=()
  get_parent_dirs() {
    if [[ -d "${1}" ]]; then dirs+=("$1"); else return; fi
    if [[ "${1}" == '/' ]]; then
      for _dir in "${dirs[@]}"; do echo $_dir; done
    else
      get_parent_dirs $(dirname "$1")
    fi
  }
  DIR=$(get_parent_dirs $(realpath $(dirname "${1:-$(pwd)}")) | fzf-tmux +m --header="cd ↓ from $PWD" --exit-0) && cd "$DIR"
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
  git config --get-regexp "^alias." | \
    gsed -e "s/^alias\.//" -e "s/\ /\ =\ /" | \
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

bindkey -M vicmd "/" fzf-history-widget
bindkey '^T' fzf-file-widget
bindkey '^[f' fzf-file-widget
bindkey '^[/' fzf-file-widget

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
