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

  # gfind options:
  #   "$current_search_path": The directory to search within.
  #   -mindepth 1: Do not include the starting directory itself.
  #   -maxdepth 1: Do not go deeper than one level.
  #   -type d: Only list directories.
  #   -printf '%P\n': Print the found directory names relative to current_search_path, one per line.
  #
  # fzf-tmux options:
  #   +m: Disable multi-selection (original widget had this).
  #   --header: Informative text displayed in fzf.
  #   --expect=/,enter: Tell fzf to exit and report if '/' or 'Enter' is pressed.
  #                     Other keys like 'Esc' will also exit fzf.
  #   --print-query: The first line of fzf's output will be the query typed by the user.
  #   --exit-0: fzf exits with status 0 even if no selection is made (e.g., Esc is pressed).
  #   --preview: Show a preview of the contents of the currently highlighted directory.
  selected_fzf_output=$(gfind "$current_search_path" -mindepth 1 -maxdepth 1 -type d -printf '%P\n' 2>/dev/null | \
    fzf-tmux +m --header="Navigate: $current_search_path ('/' to descend, Enter to select, Esc to cancel)" \
               --expect=/,enter --print-query --exit-0 \
               --preview="ls -p --color=always '$current_search_path/{}'")

  # If fzf was exited (e.g., Esc) or produced no output (e.g. no directories found and Esc)
  if [[ -z "$selected_fzf_output" ]]; then
    zle reset-prompt
    return 0
  fi

  # Parse fzf's output:
  # With --print-query and --expect, the output is typically:
  # Line 1: query text (which we ignore for now)
  # Line 2: key pressed ('/' or 'enter', or empty if Enter was pressed on an empty selection list after typing a query)
  # Line 3: selected item text (the directory name, relative to current_search_path)
  local lines
  lines=(${(f)selected_fzf_output}) # Zsh array split by newlines

  # Zsh arrays are 1-indexed
  key="${lines[2]}"
  chosen_item_relative_path="${lines[3]}"

  # If no item was actually selected (e.g., pressed Enter on an empty filtered list, or just a key without a selection)
  if [[ -z "$chosen_item_relative_path" ]]; then
    zle reset-prompt
    return 0
  fi

  # Construct the full path to the selected item
  # chosen_item_relative_path is relative to current_search_path (e.g., "subdir" if current_search_path is "/foo")
  chosen_item_full_path="$current_search_path/$chosen_item_relative_path"
  # Normalize the constructed path (e.g., resolve '/./', '/../', symlinks)
  chosen_item_full_path=$(realpath "$chosen_item_full_path" 2>/dev/null)

  # Check if the resolved path is a valid directory
  if [[ -z "$chosen_item_full_path" || ! -d "$chosen_item_full_path" ]]; then
      echo "fzf-cd-down: Selected path is not a valid directory: '$current_search_path/$chosen_item_relative_path'" >&2
      zle reset-prompt
      return 1 # Indicate an error
  fi

  if [[ "$key" == "/" ]]; then
    # User pressed '/', call this function recursively for the newly selected directory path
    fzf-cd-down "$chosen_item_full_path"
  elif [[ "$key" == "enter" ]]; then
    # User pressed Enter, change to the selected directory
    cd "$chosen_item_full_path"
    zle reset-prompt
  else
    # Other key pressed (e.g. Esc after selection, though --exit-0 usually means empty output for plain Esc)
    # or an unexpected key if --expect was different. For /,enter this path is less likely.
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
