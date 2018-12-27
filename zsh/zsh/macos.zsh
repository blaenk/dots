if [[ -z "$MACOS" ]]; then
  iterm2_prompt_mark() {}

  return 0
fi

export IS_MACOS=true

ITERM_INTEGRATION="$DOTSPATH/zsh/zsh/iterm.zsh"

if [ ! -f "${ITERM_INTEGRATION}" ]; then
  curl -s -L 'https://iterm2.com/shell_integration/zsh' -o "${ITERM_INTEGRATION}"

  # Apply iterm2_prompt_mark patch
  patch -s -d "$DOTSPATH/zsh/zsh/" < <(curl -s -L 'https://git.io/fhkwE')
fi

source "${ITERM_INTEGRATION}"

if ! command_exists brew; then
  return 0
fi

if ! brew command command-not-found-init > /dev/null 2>&1; then
  brew tap homebrew/command-not-found
fi

eval "$(brew command-not-found-init)"
