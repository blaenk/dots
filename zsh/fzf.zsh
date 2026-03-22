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

# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# set descriptions format to enable group support
# NOTE: don't use escape sequences (like '%F{red}%d%f') here, fzf-tab will ignore them
zstyle ':completion:*:descriptions' format '[%d]'
# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# force zsh not to show completion menu, which allows fzf-tab to capture the unambiguous prefix
zstyle ':completion:*' menu no
# To make fzf-tab follow FZF_DEFAULT_OPTS.
# NOTE: This may lead to unexpected behavior since some flags break this plugin. See Aloxaf/fzf-tab#455.
zstyle ':fzf-tab:*' use-fzf-default-opts yes
# switch group using `<` and `>`
zstyle ':fzf-tab:*' switch-group '<' '>'

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

cdd() {
  local dir
  dir=$(fd --type d | fzf-tmux +m --scheme path --header="cd ↓ from $PWD" --exit-0)
  if [[ -n "$dir" ]]; then
    cd "$dir"
  fi
}

cdu() {
  local dir=$PWD result
  result=$({ while dir=${dir%/*}; [[ -n "$dir" ]]; do echo "$dir"; done; echo /; } |
    fzf-tmux +m --header="cd ↑ from $PWD" --exit-0) && cd "$result"
}

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

# select a tmux session, previewing the active pane's current contents
fzf-tmux-list-sessions() {
  if ! tmux ls &> /dev/null; then
    zle reset-prompt 2>/dev/null
    return
  fi

  local sessions current_session
  sessions=$(tmux list-sessions -F "#{session_name} · #{session_windows} windows")

  if [ -n "$TMUX" ]; then
    current_session=$(tmux display-message -p "#{session_name}")
    sessions=$(echo "$sessions" | grep -v "^${current_session} ")
  fi

  echo "$sessions" | fzf --header="tmux sessions" --select-1 +m --exit-0 --cycle \
    --preview 'tmux capture-pane -t {1}: -ep | tac | awk "NF{found=1} found" | tac | tail -n $FZF_PREVIEW_LINES' \
    --preview-window down:80%:nowrap | awk '{print$1}'
}

_fzf-tmux-switch-sessions() {
  local target
  target=$(fzf-tmux-list-sessions)

  if [ -z "$target" ]; then
    zle reset-prompt 2>/dev/null
    return
  fi

  if [ -z "$TMUX" ]; then
    BUFFER="tmux attach-session -t \"$target\""
    zle accept-line
  else
    tmux switch-client -t "$target"
  fi
}

zle -N _fzf-tmux-switch-sessions
bindkey '^[;' _fzf-tmux-switch-sessions

# select a tmux window from among all windows in every session
fzf-tmux-list-all-panes() {
  if ! tmux ls &> /dev/null; then
    zle reset-prompt 2>/dev/null
    return
  fi

  local windows current_window target target_window list_format
  windows=$(tmux list-panes -a -F "${tmux_panes_list_format}")

  if [ -n "$TMUX" ]; then
    current_pane=$(tmux display-message -p "${tmux_panes_list_format}")
    windows=$(echo "$windows" | grep -v "$current_pane")
    [ -n "${1}" ] && windows=$(echo "$windows" | grep -v "${1}")
  fi

  echo "$windows" | fzf --header="tmux panes (alt-enter to bring)" --select-1 +m --exit-0 --cycle \
    --bind 'alt-enter:execute(tmux join-pane -s {1})+abort' \
    --preview 'tmux capture-pane -t {1} -ep | tac | awk "NF{found=1} found" | tac | tail -n $FZF_PREVIEW_LINES' \
    --preview-window down:80%:nowrap | awk '{print$1}'
}

# NOTE
# When inside of tmux, a tmux binding for this takes precedence, which allows us
# to invoke it while another program is running.

_fzf-tmux-switch-panes() {
  target=$(fzf-tmux-list-all-panes "${1}")

  if [ -z "$target" ]; then
    zle reset-prompt 2>/dev/null
    return
  fi

  if [ -z "$TMUX" ]; then
    [ -n "$target" ] || return
    BUFFER="tmux attach-session -t \"$target\""
    zle accept-line
  else
    tmux switch-client -t "$target"
  fi
}

# M-, to fzf all windows in every session
zle -N _fzf-tmux-switch-panes
bindkey '^[,' _fzf-tmux-switch-panes

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
