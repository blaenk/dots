export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

mvn-or-mvnw() {
  local dir="$PWD"
  while [[ ! -x "$dir/mvnw" && "$dir" != / ]]; do
    dir="${dir:h}"
  done

  if [[ -x "$dir/mvnw" ]]; then
    echo "Running \`$dir/mvnw\`..." >&2
    "$dir/mvnw" "$@"
    return $?
  fi

  command mvn "$@"
}

alias mvn="mvn-or-mvnw"
