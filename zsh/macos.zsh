if [[ -z "$MACOS" ]]; then
  iterm2_prompt_mark() {}

  iterm2_prompt_mark() {}

  iterm2_prompt_end() {}

  return 0
fi

iterm2_prompt_mark() {
  printf "\033]133;A\007"
}

# Mark end of prompt
iterm2_prompt_end() {
  printf "\033]133;B\007"
}

export IS_MACOS=true

# ITERM_INTEGRATION="$DOTSPATH/zsh/zsh/iterm.zsh"

# if [ ! -f "${ITERM_INTEGRATION}" ]; then
#   curl -s -L 'https://iterm2.com/shell_integration/zsh' -o "${ITERM_INTEGRATION}"

#   # Apply iterm2_prompt_mark patch
#   patch -s -d "$DOTSPATH/zsh/zsh/" < <(curl -s -L 'https://git.io/fhkwE')
# fi

# source "${ITERM_INTEGRATION}"

if ! command_exists brew; then
  return 0
fi

HB_CNF_HANDLER="/opt/homebrew/Library/Taps/homebrew/homebrew-command-not-found/handler.sh"

if [ -f "$HB_CNF_HANDLER" ]; then
  source "$HB_CNF_HANDLER";
fi
