#!/usr/bin/env bash
# Parse the cheatsheet comment block at the top of ~/.tmux.conf and display it.

conf="$HOME/.tmux.conf"

bold=$'\033[1m'
cyan=$'\033[36m'
reset=$'\033[0m'

inside=false
while IFS= read -r line; do
  if [[ "$line" == "# START BINDINGS" ]]; then inside=true; continue; fi
  if [[ "$line" == "# END BINDINGS" ]]; then break; fi
  $inside || continue

  [[ -z "$line" ]] && { echo; continue; }

  # Strip leading "# " or "#"
  text="${line#\# }"
  text="${text#\#}"

  if [[ "$text" == *" -- "* ]]; then
    key="${text%% -- *}"
    desc="${text#* -- }"
    printf "  ${cyan}%-24s${reset} %s\n" "$key" "$desc"
  elif [[ -n "$text" ]]; then
    printf "\n${bold}${cyan}%s${reset}\n" "$text"
  fi
done < "$conf" | less -R
