_fzf_compgen_path() {
  ag -g "" "$1"
}

# fd - cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-*} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# M-SPC to go down
bindkey -s '^[ ' 'fd\n'

# fda - including hidden directories
fda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

# fdr - cd to selected parent directory
fdr() {
  local declare dirs=()
  get_parent_dirs() {
    if [[ -d "${1}" ]]; then dirs+=("$1"); else return; fi
    if [[ "${1}" == '/' ]]; then
      for _dir in "${dirs[@]}"; do echo $_dir; done
    else
      get_parent_dirs $(dirname "$1")
    fi
  }
  local DIR=$(get_parent_dirs $(realpath "${1:-$(pwd)}") | fzf-tmux --tac)
  cd "$DIR"
}

# C-SPC to go up
bindkey -s '^@' 'fdr\n'

# cf - fuzzy cd from anywhere
# ex: cf word1 word2 ... (even part of a file name)
# zsh autoload function
cf() {
  local file

  file="$(locate -Ai -0 $@ | grep -z -vE '~$' | fzf --read0 -0 -1)"

  if [[ -n $file ]]
  then
     if [[ -d $file ]]
     then
        cd -- $file
     else
        cd -- ${file:h}
     fi
  fi
}

# cdf - cd into the directory of the selected file
cdf() {
   local file
   local dir
   file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

# fs [FUZZY PATTERN] - Select selected tmux session
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fs() {
  local session

  if ! tmux info &> /dev/null; then
    echo "no tmux session available"
    return
  fi
  
  session=$(tmux list-sessions -F "#{session_name}" | \
    fzf --query="$1" --select-1 --exit-0) &&
    [ -z "$TMUX" ] && tmux attach-session -t "$session" ||
      tmux switch-client -t "$session"
}

# M-. to fzf sessions
bindkey -s '^[.' 'fs\n'

# ftw - switch window
ftw() {
  [ -z "$TMUX" ] && return

  local windows current_window target target_window
  windows=$(tmux list-windows -F '#{window_index}: #{window_name}')
  current_window=$(tmux display-message -p '#{window_index}: #{window_name}')

  target=$(echo "$windows" | grep -v "$current_window" |
              fzf --query="$1" --select-1 +m --reverse --exit-0) || return

  target_window=$(echo $target | awk 'BEGIN{FS=":"} {print$1}')

  tmux select-window -t $target_window
}

# M-, to fzf windows in current session
bindkey -s '^[,' 'ftw\n'

# ftwa - switch window all
ftwa() {
  local windows current_window target target_window
  windows=$(tmux list-windows -a -F '#{session_name}:#{window_index}: #{window_name}')

  if [ ! -z "$TMUX" ]; then
    current_window=$(tmux display-message -p '#{session_name}:#{window_index}: #{window_name}')
    windows=$(echo "$windows" | grep -v "$current_window")
  fi

  target=$(echo "$windows" | fzf --query="$1" --select-1 +m --reverse --exit-0) || return

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
bindkey -s '^[<' 'ftwa\n'

if [[ -f /usr/share/fzf/key-bindings.zsh ]]; then
  source /usr/share/fzf/key-bindings.zsh
fi
